{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Control.Concurrent (threadDelay)
import Network.Wai (Request, Response, defaultRequest)
import qualified Network.Wai as WAI
import Network.HTTP.Types (methodGet)
import Network.Socket (SockAddr(..))
import Network.HTTP.Types.Status (statusCode)
import Keter.RateLimiter.Cache
import Keter.RateLimiter.WAI

main :: IO ()
main = do
  store <- createInMemoryStore
  let cache :: Cache (InMemoryStore "counter")
      cache = newCache "counter" store
  putStrLn ""
  v1 <- incStore cache "test" 10 :: IO Int
  print v1
  v2 <- incStore cache "test" 10 :: IO Int
  print v2
  v3 <- incStore cache "test" 10 :: IO Int
  print v3
  defaultMain tests

tests :: TestTree
tests = testGroup "Rate Limiter and IP Zones Tests"
  [ rateLimiterTests
  , ipZoneTests
  , cacheApiTests
  ]

-- IPs and Zone Identifiers for testing
testIPZoneA, testIPZoneB :: IPZoneIdentifier
testIPZoneA = "testZoneA"
testIPZoneB = "testZoneB"
ipForZoneA, ipForZoneB, ipForDefaultZone, genericTestIP, genericTestIP2 :: Text
ipForZoneA = "10.0.0.1"
ipForZoneB = "20.0.0.1"
ipForDefaultZone = "30.0.0.1"
genericTestIP = "192.168.1.1"
genericTestIP2 = "192.168.1.2"

-- Helper function to extract IP from WAI Request for zone determination
testGetRequestIPZone :: Request -> IPZoneIdentifier
testGetRequestIPZone req
  | getClientIP req == ipForZoneA = testIPZoneA
  | getClientIP req == ipForZoneB = testIPZoneB
  | getClientIP req == genericTestIP = defaultIPZone
  | getClientIP req == genericTestIP2 = defaultIPZone
  | getClientIP req == ipForDefaultZone = defaultIPZone
  | otherwise = defaultIPZone

-- Helper to create a mock WAI Request with specified IP and path
makeRequest :: Text -> Text -> Request
makeRequest ip path = defaultRequest
  { WAI.requestMethod = methodGet
  , WAI.rawPathInfo = TE.encodeUtf8 path
  , WAI.requestHeaderHost = Just "example.com"
  , WAI.remoteHost = mockSockAddr ip
  , WAI.requestHeaders = [("x-real-ip", TE.encodeUtf8 ip)]  -- Set IP via header for testing
  }

-- Helper to create a mock SockAddr from IP string (simplified for testing)
mockSockAddr :: Text -> SockAddr
mockSockAddr ip = SockAddrInet 80 0  -- Simplified - in real tests you might parse the IP properly

-- Mock application that always returns "Success"
mockApp :: WAI.Application
mockApp _ respond = respond $ WAI.responseLBS 
  (toEnum 200) 
  [("Content-Type", "text/plain")] 
  "Success"

-- Helper to extract response body as Text
getResponseBody :: Response -> IO Text
getResponseBody response = do
  case response of
    -- This is a simplified approach - in practice you'd need to handle the response streaming
    _ -> return "Success"  -- For now, assume success responses

-- Helper to check if response is rate limited
isRateLimited :: Response -> Bool
isRateLimited response = 
  case response of
    -- Check status code - simplified approach
    _ -> False  -- You'll need to implement proper response checking

executeRequests :: Env -> [Request] -> [Text] -> IO ()
executeRequests env requests expectedResponses = do
  actualResponses <- mapM (processRequest env) requests
  assertEqual "Responses should match expected" expectedResponses actualResponses
  where
    processRequest :: Env -> Request -> IO Text
    processRequest testEnv req = do
      blocked <- instrument testEnv req
      return $ if blocked then "Too Many Requests" else "Success"

--------------------------------------------------------------------------------
-- Rate Limiter Tests

rateLimiterTests :: TestTree
rateLimiterTests = testGroup "General Rate Limiter Tests (with Default IP Zone)"
  [ testCase "Fixed Window Rate Limiting" testFixedWindow
  , testCase "Sliding Window Rate Limiting" testSlidingWindow
  , testCase "Token Bucket Rate Limiting" testTokenBucket
  , testCase "Leaky Bucket Rate Limiting" testLeakyBucket
  , testCase "Path-specific Rate Limiting" testPathSpecificThrottle
  , testCase "Multiple Throttles" testMultipleThrottles
  , testCase "Original Reset After Period (New Env)" testOriginalResetAfterPeriod
  , testCase "Time-based Reset (Same Env)" testTimeBasedReset
  ]

testFixedWindow :: IO ()
testFixedWindow = do
  env <- initConfig testGetRequestIPZone
  let throttleConfig = ThrottleConfig
        { throttleLimit = 3
        , throttlePeriod = 10
        , throttleAlgorithm = FixedWindow
        , throttleIdentifier = Just . getClientIP  -- Use WAI helper
        , throttleTokenBucketTTL = Nothing
        }
      envWithThrottle = addThrottle env "test-throttle" throttleConfig
      requests = replicate 5 $ makeRequest genericTestIP "/test"
      expectedResponses = replicate 3 "Success" ++ replicate 2 "Too Many Requests"
  executeRequests envWithThrottle requests expectedResponses

testSlidingWindow :: IO ()
testSlidingWindow = do
  env <- initConfig testGetRequestIPZone
  let throttleConfig = ThrottleConfig
        { throttleLimit = 3
        , throttlePeriod = 10
        , throttleAlgorithm = SlidingWindow
        , throttleIdentifier = Just . getClientIP
        , throttleTokenBucketTTL = Nothing
        }
      envWithThrottle = addThrottle env "test-throttle" throttleConfig
      requests = replicate 5 $ makeRequest genericTestIP "/test"
      expectedResponses = replicate 3 "Success" ++ replicate 2 "Too Many Requests"
  executeRequests envWithThrottle requests expectedResponses

testTokenBucket :: IO ()
testTokenBucket = do
  let ttlSeconds = 3
  env <- initConfig testGetRequestIPZone
  let throttleConfig = ThrottleConfig
        { throttleLimit = 3
        , throttlePeriod = 1
        , throttleAlgorithm = TokenBucket
        , throttleIdentifier = Just . getClientIP
        , throttleTokenBucketTTL = Just ttlSeconds
        }
      envWithThrottle = addThrottle env "test-throttle" throttleConfig
      requests = replicate 5 $ makeRequest genericTestIP "/test"
      expectedResponses = replicate 3 "Success" ++ replicate 2 "Too Many Requests"
  executeRequests envWithThrottle requests expectedResponses
  threadDelay ((ttlSeconds + 1) * 1000000)
  blocked <- instrument envWithThrottle (makeRequest genericTestIP "/test")
  assertEqual "Request should succeed after TTL expiry and refill" False blocked
  b1 <- instrument envWithThrottle (makeRequest genericTestIP "/test")
  b2 <- instrument envWithThrottle (makeRequest genericTestIP "/test")
  b3 <- instrument envWithThrottle (makeRequest genericTestIP "/test")
  assertEqual "Next requests after refill" [False, False, True] [b1, b2, b3]

testLeakyBucket :: IO ()
testLeakyBucket = do
  env <- initConfig testGetRequestIPZone
  let throttleConfig = ThrottleConfig
        { throttleLimit = 3
        , throttlePeriod = 10
        , throttleAlgorithm = LeakyBucket
        , throttleIdentifier = Just . getClientIP
        , throttleTokenBucketTTL = Nothing
        }
      envWithThrottle = addThrottle env "test-throttle" throttleConfig
      requests = replicate 5 $ makeRequest genericTestIP "/test"
      expectedResponses = replicate 3 "Success" ++ replicate 2 "Too Many Requests"
  executeRequests envWithThrottle requests expectedResponses
  threadDelay (4 * 1000000)
  blocked <- instrument envWithThrottle (makeRequest genericTestIP "/test")
  assertEqual "Request should succeed after leaking" False blocked

testPathSpecificThrottle :: IO ()
testPathSpecificThrottle = do
  env <- initConfig testGetRequestIPZone
  let throttleConfig = ThrottleConfig
        { throttleLimit = 2
        , throttlePeriod = 10
        , throttleAlgorithm = FixedWindow
        , throttleIdentifier = \req -> 
            if getRequestPath req == "/login" 
            then Just (getClientIP req <> ":" <> getRequestPath req) 
            else Nothing
        , throttleTokenBucketTTL = Nothing
        }
      envWithThrottle = addThrottle env "login-throttle" throttleConfig
      loginRequests = replicate 3 $ makeRequest genericTestIP "/login"
      homeRequests = replicate 3 $ makeRequest genericTestIP "/home"
      expectedLoginResponses = replicate 2 "Success" ++ ["Too Many Requests"]
      expectedHomeResponses = replicate 3 "Success"
  executeRequests envWithThrottle loginRequests expectedLoginResponses
  executeRequests envWithThrottle homeRequests expectedHomeResponses

testMultipleThrottles :: IO ()
testMultipleThrottles = do
  env <- initConfig testGetRequestIPZone
  let ipThrottleConfig = ThrottleConfig
        { throttleLimit = 5
        , throttlePeriod = 10
        , throttleAlgorithm = FixedWindow
        , throttleIdentifier = Just . getClientIP
        , throttleTokenBucketTTL = Nothing
        }
      loginThrottleConfig = ThrottleConfig
        { throttleLimit = 2
        , throttlePeriod = 10
        , throttleAlgorithm = SlidingWindow
        , throttleIdentifier = \req -> 
            if getRequestPath req == "/login" 
            then Just (getClientIP req <> ":" <> getRequestPath req) 
            else Nothing
        , throttleTokenBucketTTL = Nothing
        }
      envWithIpThrottle = addThrottle env "ip-throttle" ipThrottleConfig
      envWithBothThrottles = addThrottle envWithIpThrottle "login-throttle" loginThrottleConfig
      requests = [ makeRequest genericTestIP "/login"
                 , makeRequest genericTestIP "/login"
                 , makeRequest genericTestIP "/login"
                 , makeRequest genericTestIP "/home"
                 , makeRequest genericTestIP "/about"
                 , makeRequest genericTestIP "/contact"
                 ]
      expectedResponses = [ "Success", "Success", "Too Many Requests"
                         , "Success", "Success", "Too Many Requests"
                         ]
  executeRequests envWithBothThrottles requests expectedResponses

testOriginalResetAfterPeriod :: IO ()
testOriginalResetAfterPeriod = do
  env1 <- initConfig testGetRequestIPZone
  let throttleConfig = ThrottleConfig
        { throttleLimit = 2
        , throttlePeriod = 1
        , throttleAlgorithm = FixedWindow
        , throttleIdentifier = Just . getClientIP
        , throttleTokenBucketTTL = Nothing
        }
      envWithThrottle1 = addThrottle env1 "test-throttle" throttleConfig
      requests1 = replicate 3 $ makeRequest genericTestIP "/test"
      expectedResponses1 = replicate 2 "Success" ++ ["Too Many Requests"]
  executeRequests envWithThrottle1 requests1 expectedResponses1
  threadDelay (1100000)
  env2 <- initConfig testGetRequestIPZone
  let envWithThrottle2 = addThrottle env2 "test-throttle" throttleConfig
      requests2 = replicate 3 $ makeRequest genericTestIP2 "/test"
      expectedResponses2 = replicate 2 "Success" ++ ["Too Many Requests"]
  executeRequests envWithThrottle2 requests2 expectedResponses2

testTimeBasedReset :: IO ()
testTimeBasedReset = do
  let periodSec = 1
      limit = 1
  env <- initConfig testGetRequestIPZone
  let throttleConfig = ThrottleConfig
        { throttleLimit = limit
        , throttlePeriod = periodSec
        , throttleAlgorithm = FixedWindow
        , throttleIdentifier = Just . getClientIP
        , throttleTokenBucketTTL = Nothing
        }
      envWithThrottle = addThrottle env "reset-throttle" throttleConfig
  blocked1 <- instrument envWithThrottle (makeRequest ipForZoneA "/reset_test")
  assertEqual "First request to zone A should succeed" False blocked1
  blocked2 <- instrument envWithThrottle (makeRequest ipForZoneA "/reset_test")
  assertEqual "Second request to zone A should be blocked" True blocked2
  threadDelay ((periodSec * 1000000) + 200000)
  blocked3 <- instrument envWithThrottle (makeRequest ipForZoneA "/reset_test")
  assertEqual "Request after period to zone A should succeed" False blocked3

--------------------------------------------------------------------------------
-- IP Zone Tests

ipZoneTests :: TestTree
ipZoneTests = testGroup "IP Zone Functionality Tests"
  [ testCase "IP Zone Isolation" testIPZoneIsolation
  , testCase "IP Zone Default Fallback" testIPZoneDefaultFallback
  , testCase "Cache Reset All Across Zones" testIPZoneCacheResetAll
  ]

testIPZoneIsolation :: IO ()
testIPZoneIsolation = do
  let limit = 1
      period = 10
  env <- initConfig testGetRequestIPZone
  let throttleCfg = ThrottleConfig
        { throttleLimit = limit
        , throttlePeriod = period
        , throttleAlgorithm = FixedWindow
        , throttleIdentifier = Just . getClientIP
        , throttleTokenBucketTTL = Nothing
        }
      envWithThrottle = addThrottle env "test-throttle" throttleCfg
      reqA1 = makeRequest ipForZoneA "/test"
      reqB1 = makeRequest ipForZoneB "/test"
  b1 <- instrument envWithThrottle reqA1
  assertEqual "Zone A first request allowed" False b1
  b2 <- instrument envWithThrottle reqA1
  assertEqual "Zone A second request blocked" True b2
  b3 <- instrument envWithThrottle reqB1
  assertEqual "Zone B first request allowed" False b3

testIPZoneDefaultFallback :: IO ()
testIPZoneDefaultFallback = do
  env <- initConfig testGetRequestIPZone
  let throttleConfig = ThrottleConfig
        { throttleLimit = 1
        , throttlePeriod = 10
        , throttleAlgorithm = FixedWindow
        , throttleIdentifier = Just . getClientIP
        , throttleTokenBucketTTL = Nothing
        }
      envWithThrottle = addThrottle env "test-throttle" throttleConfig
      reqDefault = makeRequest ipForDefaultZone "/test"
      reqGeneric = makeRequest genericTestIP "/test"
  b1 <- instrument envWithThrottle reqDefault
  assertEqual "Default zone first request allowed" False b1
  b2 <- instrument envWithThrottle reqGeneric
  assertEqual "Generic IP default zone first request allowed" False b2

testIPZoneCacheResetAll :: IO ()
testIPZoneCacheResetAll = do
  env <- initConfig testGetRequestIPZone
  let throttleConfig = ThrottleConfig
        { throttleLimit = 1
        , throttlePeriod = 10
        , throttleAlgorithm = FixedWindow
        , throttleIdentifier = Just . getClientIP
        , throttleTokenBucketTTL = Nothing
        }
      envWithThrottle = addThrottle env "test-throttle" throttleConfig
      reqA = makeRequest ipForZoneA "/test"
      reqB = makeRequest ipForZoneB "/test"
  _ <- instrument envWithThrottle reqA
  _ <- instrument envWithThrottle reqB
  cacheResetAll envWithThrottle
  b1 <- instrument envWithThrottle reqA
  b2 <- instrument envWithThrottle reqB
  assertEqual "Zone A after reset" False b1
  assertEqual "Zone B after reset" False b2

--------------------------------------------------------------------------------
-- Cache API Tests (wrappers and manual keys)

cacheApiTests :: TestTree
cacheApiTests = testGroup "Cache API (wrappers and customisable)"
  [ testCase "incStoreWithZone wrapper increments independently" testIncStoreWithZone
  , testCase "incStore manual key increments independently" testIncStoreManual
  , testCase "readCacheWithZone and writeCacheWithZone" testReadWriteCacheWithZone
  , testCase "readCache and writeCache manual" testReadWriteCacheManual
  ]

testIncStoreWithZone :: IO ()
testIncStoreWithZone = do
  store <- createInMemoryStore
  let cache :: Cache (InMemoryStore "counter")
      cache = newCache "counter" store
      throttleName = "login-throttle"
      ipZone = "zoneX"
      userKey = "userX"
  v1 <- incStoreWithZone cache throttleName ipZone userKey 10 :: IO Int
  v2 <- incStoreWithZone cache throttleName ipZone userKey 10 :: IO Int
  assertEqual "First increment (wrapper)" 1 v1
  assertEqual "Second increment (wrapper)" 2 v2

testIncStoreManual :: IO ()
testIncStoreManual = do
  store <- createInMemoryStore
  let cache :: Cache (InMemoryStore "counter")
      cache = newCache "counter" store
      customKey = "login-throttle:zoneY:userY:extra"
  v1 <- incStore cache customKey 10 :: IO Int
  v2 <- incStore cache customKey 10 :: IO Int
  assertEqual "First increment (manual)" 1 v1
  assertEqual "Second increment (manual)" 2 v2

testReadWriteCacheWithZone :: IO ()
testReadWriteCacheWithZone = do
  store <- createInMemoryStore
  let cache :: Cache (InMemoryStore "counter")
      cache = newCache "counter" store
      throttleName = "api-throttle"
      ipZone = "zoneZ"
      userKey = "userZ"
      val = 42 :: Int
  writeCacheWithZone cache throttleName ipZone userKey val 10
  mVal <- readCacheWithZone cache throttleName ipZone userKey :: IO (Maybe Int)
  assertEqual "Read after write (wrapper)" (Just val) mVal

testReadWriteCacheManual :: IO ()
testReadWriteCacheManual = do
  store <- createInMemoryStore
  let cache :: Cache (InMemoryStore "counter")
      cache = newCache "counter" store
      customKey = "api-throttle:zoneW:userW:custom"
      val = 99 :: Int
  writeCache cache customKey val 10
  mVal <- readCache cache customKey :: IO (Maybe Int)
  assertEqual "Read after write (manual)" (Just val) mVal
