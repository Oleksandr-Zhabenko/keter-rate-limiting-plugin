{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Control.Concurrent (threadDelay)
import Keter.RateLimiter.Cache
import Keter.RateLimiter

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

testGetRequestIPZone :: Request -> IPZoneIdentifier
testGetRequestIPZone req
  | requestIP req == ipForZoneA = testIPZoneA
  | requestIP req == ipForZoneB = testIPZoneB
  | requestIP req == genericTestIP = defaultIPZone
  | requestIP req == genericTestIP2 = defaultIPZone
  | requestIP req == ipForDefaultZone = defaultIPZone
  | otherwise = defaultIPZone

makeRequest :: Text -> Text -> Request
makeRequest ip path = Request
  { requestMethod = "GET"
  , requestPath = path
  , requestHost = "example.com"
  , requestIP = ip
  , requestHeaders = Map.empty
  }

executeRequests :: Env -> [Request] -> [Text] -> IO ()
executeRequests env requests expectedResponses = do
  actualResponses <- mapM (attackMiddleware env (const (return "Success"))) requests
  assertEqual "Responses should match expected" expectedResponses actualResponses

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
  let throttleConfig = ThrottleConfig 3 10 FixedWindow (\req -> Just (requestIP req))
      envWithThrottle = addThrottle env "test-throttle" throttleConfig
      requests = replicate 5 $ makeRequest genericTestIP "/test"
      expectedResponses = replicate 3 "Success" ++ replicate 2 "Too Many Requests"
  executeRequests envWithThrottle requests expectedResponses

testSlidingWindow :: IO ()
testSlidingWindow = do
  env <- initConfig testGetRequestIPZone
  let throttleConfig = ThrottleConfig 3 10 SlidingWindow (\req -> Just (requestIP req))
      envWithThrottle = addThrottle env "test-throttle" throttleConfig
      requests = replicate 5 $ makeRequest genericTestIP "/test"
      expectedResponses = replicate 3 "Success" ++ replicate 2 "Too Many Requests"
  executeRequests envWithThrottle requests expectedResponses

testTokenBucket :: IO ()
testTokenBucket = do
  env <- initConfig testGetRequestIPZone
  let throttleConfig = ThrottleConfig 3 1 TokenBucket (\req -> Just (requestIP req))
      envWithThrottle = addThrottle env "test-throttle" throttleConfig
      requests = replicate 5 $ makeRequest genericTestIP "/test"
      expectedResponses = replicate 3 "Success" ++ replicate 2 "Too Many Requests"
  executeRequests envWithThrottle requests expectedResponses
  threadDelay (2 * 1000000)
  response <- attackMiddleware envWithThrottle (const (return "Success")) (makeRequest genericTestIP "/test")
  assertEqual "Request should succeed after refill" "Success" response

testLeakyBucket :: IO ()
testLeakyBucket = do
  env <- initConfig testGetRequestIPZone
  let throttleConfig = ThrottleConfig 3 10 LeakyBucket (\req -> Just (requestIP req))
      envWithThrottle = addThrottle env "test-throttle" throttleConfig
      requests = replicate 5 $ makeRequest genericTestIP "/test"
      expectedResponses = replicate 3 "Success" ++ replicate 2 "Too Many Requests"
  executeRequests envWithThrottle requests expectedResponses
  threadDelay (4 * 1000000)
  response <- attackMiddleware envWithThrottle (const (return "Success")) (makeRequest genericTestIP "/test")
  assertEqual "Request should succeed after leaking" "Success" response

testPathSpecificThrottle :: IO ()
testPathSpecificThrottle = do
  env <- initConfig testGetRequestIPZone
  let throttleConfig = ThrottleConfig 2 10 FixedWindow (\req -> if requestPath req == "/login" then Just (requestIP req <> ":" <> requestPath req) else Nothing)
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
  let ipThrottleConfig = ThrottleConfig 5 10 FixedWindow (\req -> Just (requestIP req))
      loginThrottleConfig = ThrottleConfig 2 10 SlidingWindow (\req -> if requestPath req == "/login" then Just (requestIP req <> ":" <> requestPath req) else Nothing)
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
  let throttleConfig = ThrottleConfig 2 1 FixedWindow (\req -> Just (requestIP req))
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
  let throttleConfig = ThrottleConfig limit periodSec FixedWindow (\req -> Just (requestIP req))
      envWithThrottle = addThrottle env "reset-throttle" throttleConfig
  resp1 <- attackMiddleware envWithThrottle (const (return "Success")) (makeRequest ipForZoneA "/reset_test")
  assertEqual "First request to zone A should succeed" "Success" resp1
  resp2 <- attackMiddleware envWithThrottle (const (return "Success")) (makeRequest ipForZoneA "/reset_test")
  assertEqual "Second request to zone A should be blocked" "Too Many Requests" resp2
  threadDelay ((periodSec * 1000000) + 200000)
  resp3 <- attackMiddleware envWithThrottle (const (return "Success")) (makeRequest ipForZoneA "/reset_test")
  assertEqual "Request after period to zone A should succeed" "Success" resp3

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
  let throttleCfg = ThrottleConfig limit period FixedWindow (\req -> Just (requestIP req))
      envWithThrottle = addThrottle env "test-throttle" throttleCfg
      reqA1 = makeRequest ipForZoneA "/test"
      reqB1 = makeRequest ipForZoneB "/test"
  r1 <- attackMiddleware envWithThrottle (const (return "Success")) reqA1
  assertEqual "Zone A first request allowed" "Success" r1
  r2 <- attackMiddleware envWithThrottle (const (return "Success")) reqA1
  assertEqual "Zone A second request blocked" "Too Many Requests" r2
  r3 <- attackMiddleware envWithThrottle (const (return "Success")) reqB1
  assertEqual "Zone B first request allowed" "Success" r3

testIPZoneDefaultFallback :: IO ()
testIPZoneDefaultFallback = do
  env <- initConfig testGetRequestIPZone
  let throttleConfig = ThrottleConfig 1 10 FixedWindow (\req -> Just (requestIP req))
      envWithThrottle = addThrottle env "test-throttle" throttleConfig
      reqDefault = makeRequest ipForDefaultZone "/test"
      reqGeneric = makeRequest genericTestIP "/test"
  r1 <- attackMiddleware envWithThrottle (const (return "Success")) reqDefault
  assertEqual "Default zone first request allowed" "Success" r1
  r2 <- attackMiddleware envWithThrottle (const (return "Success")) reqGeneric
  assertEqual "Generic IP default zone first request allowed" "Success" r2

testIPZoneCacheResetAll :: IO ()
testIPZoneCacheResetAll = do
  env <- initConfig testGetRequestIPZone
  let throttleConfig = ThrottleConfig 1 10 FixedWindow (\req -> Just (requestIP req))
      envWithThrottle = addThrottle env "test-throttle" throttleConfig
      reqA = makeRequest ipForZoneA "/test"
      reqB = makeRequest ipForZoneB "/test"
  _ <- attackMiddleware envWithThrottle (const (return "Success")) reqA
  _ <- attackMiddleware envWithThrottle (const (return "Success")) reqB
  cacheResetAll envWithThrottle
  rA <- attackMiddleware envWithThrottle (const (return "Success")) reqA
  rB <- attackMiddleware envWithThrottle (const (return "Success")) reqB
  assertEqual "Zone A request allowed after reset" "Success" rA
  assertEqual "Zone B request allowed after reset" "Success" rB

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
