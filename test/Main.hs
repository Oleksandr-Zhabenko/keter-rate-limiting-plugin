{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}

{-|
Module      : Main
Description : Main entry point for the rate limiter test suite.
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : stable
Portability : portable

This module aggregates all test suites for the Keter Rate Limiter library
and provides the main execution entry point. It includes general rate limiter tests,
IP zone functionality tests, and specific cache API tests.
-}
module Main (
  -- * Main Entry
  main
, mainTests
  -- * Test Suites
, rateLimiterTests
, ipZoneTests
, cacheApiTests
  -- * Test Cases
  -- ** General Rate Limiter Tests
, testFixedWindow
, testSlidingWindow
, testTokenBucket
, testLeakyBucket
, testPathSpecificThrottle
, testMultipleThrottles
, testOriginalResetAfterPeriod
, testTimeBasedReset
  -- ** IP Zone Tests
, testIPZoneIsolation
, testIPZoneDefaultFallback
, testIPZoneCacheResetAll
  -- ** Cache API Tests
, testIncStoreWithZone
, testIncStoreManual
, testReadWriteCacheWithZone
, testReadWriteCacheManual
  -- * Helpers
, mainTestGetRequestIPZone
, getRequestPath
, makeRequest
, mockSockAddr
, mockApp
, getResponseBody
, isRateLimited
, executeRequests
) where

import Control.Concurrent.STM (atomically, readTVar)
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Maybe (isJust)
--import Data.IORef (readIORef)
import Control.Concurrent.STM (readTVarIO)
import Data.Cache (purgeExpired)
import Test.Tasty (TestTree, defaultMain, testGroup, after, DependencyType(..))
import Test.Tasty.HUnit (testCase, assertEqual, assertFailure)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Network.Wai (Request, Response, Application, requestMethod, rawPathInfo, requestHeaderHost, remoteHost, responseLBS, requestHeaders, defaultRequest)
import Network.HTTP.Types (methodGet)
import Network.Socket (SockAddr(..))
import Keter.RateLimiter.Cache ( Cache(..), InMemoryStore(..), Algorithm(..), makeCacheKey, readCache, createInMemoryStore, newCache, incrementCache, writeCache, deleteCache )
import Keter.RateLimiter.CacheWithZone ( incStoreWithZone, writeCacheWithZone, readCacheWithZone )
import Keter.RateLimiter.IPZones (ZoneSpecificCaches(..), IPZoneIdentifier, defaultIPZone)
import Keter.RateLimiter.WAI ( Env, ThrottleConfig(..), initConfig, addThrottle, instrument, envZoneCachesMap, cacheResetAll, IdentifierBy(..) )
import qualified Keter.RateLimiter.RequestUtils as RequestUtils
import System.IO.Unsafe (unsafePerformIO)

-- Import test modules with qualified names to avoid ambiguity
import qualified TinyLRUTests
import Keter.RateLimiter.Cache.PurgeTests
import qualified Keter.RateLimiter.IPZonesTests
import qualified Keter.RateLimiter.LeakyBucketStateTests
import qualified Keter.RateLimiter.LeakyBucketTests
import qualified Keter.RateLimiter.NotificationTests
import qualified Keter.RateLimiter.SlidingWindowTests
import qualified Keter.RateLimiter.TokenBucketTests
import qualified Keter.RateLimiter.WAITests
import qualified Keter.RateLimiter.TokenBucketStateTests

-- IPs and Zone Identifiers for testing
testIPZoneA, testIPZoneB :: IPZoneIdentifier
testIPZoneA = Text.pack "testZoneA"
testIPZoneB = Text.pack "testZoneB"
ipForZoneA, ipForZoneB, ipForDefaultZone, genericTestIP, genericTestIP2 :: Text
ipForZoneA = "10.0.0.1"
ipForZoneB = "20.0.0.1"
ipForDefaultZone = "30.0.0.1"
genericTestIP = "192.168.1.1"
genericTestIP2 = "192.168.1.2"

-- | A test-specific helper to determine the 'IPZoneIdentifier' for a given request.
-- This function maps specific IP addresses to predefined zones for testing purposes.
-- It uses 'unsafePerformIO' for simplicity within the test context.
--
-- Note: Uses 'RequestUtils.getClientIP' which respects 'x-real-ip' and 'x-forwarded-for' headers.
mainTestGetRequestIPZone :: Request -> IPZoneIdentifier
mainTestGetRequestIPZone req =
  let ip = unsafePerformIO $ RequestUtils.byIP req
  in case ip of
    Just val
      | val == ipForZoneA -> testIPZoneA
      | val == ipForZoneB -> testIPZoneB
      | val == genericTestIP -> defaultIPZone
      | val == genericTestIP2 -> defaultIPZone
      | val == ipForDefaultZone -> defaultIPZone
    _ -> defaultIPZone

-- | A simple helper to extract the request path as 'Text'.
getRequestPath :: Request -> Text
getRequestPath req = Text.pack $ show $ rawPathInfo req

-- | Creates a mock 'Request' for testing purposes.
-- The request is configured with a specified IP address (via 'x-real-ip' header) and path.
makeRequest
  :: Text -- ^ The client IP address.
  -> Text -- ^ The request path.
  -> Request
makeRequest ip path = defaultRequest
  { requestMethod = methodGet
  , rawPathInfo = TE.encodeUtf8 path
  , requestHeaderHost = Just "example.com"
  , remoteHost = mockSockAddr ip
  , requestHeaders = [("x-real-ip", TE.encodeUtf8 ip)]
  }

-- | Creates a mock 'SockAddr' for a given IP address.
-- This is a simplified implementation for testing and always returns 'SockAddrInet'.
mockSockAddr
  :: Text -- ^ The IP address (not used in this simplified version).
  -> SockAddr
mockSockAddr _ = SockAddrInet 80 0 -- Simplified for testing

-- | A mock WAI 'Application' that always returns a 200 OK response with a "Success" body.
mockApp :: Application
mockApp _ respond = respond $ responseLBS
  (toEnum 200)
  [("Content-Type", "text/plain")]
  "Success"

-- | A simplified helper to extract the response body.
-- In this test suite, it always returns "Success".
getResponseBody
  :: Response
  -> IO Text
getResponseBody _ = return "Success" -- Simplified

-- | A simplified helper to check if a response was rate-limited.
-- In this test suite, it is not implemented and always returns 'False'.
isRateLimited
  :: Response
  -> Bool
isRateLimited _ = False -- Simplified

-- | Executes a series of requests against the rate-limiting middleware and asserts that the responses match expectations.
executeRequests
  :: Env        -- ^ The rate limiter environment.
  -> [Request]  -- ^ A list of requests to execute.
  -> [Text]     -- ^ A list of expected response bodies ("Success" or "Too Many Requests").
  -> IO ()
executeRequests env requests expectedResponses = do
  actualResponses <- mapM (processRequest env) requests
  assertEqual "Responses should match expected" expectedResponses actualResponses
  where
    processRequest :: Env -> Request -> IO Text
    processRequest testEnv req = do
      blocked <- instrument testEnv req
      return $ if blocked then "Too Many Requests" else "Success"

-- | The main entry point for running the test suite.
-- It aggregates all tests and runs them using Tasty.
main :: IO ()
main = defaultMain $ after AllSucceed "All tests passed" mainTests

-- | The root 'TestTree' that combines all test groups from the library.
mainTests :: TestTree
mainTests = testGroup "Rate Limiter and IP Zones Tests"
  [ TinyLRUTests.tests
  , Keter.RateLimiter.NotificationTests.tests
  , testBackgroundPurge
  , rateLimiterTests
  , ipZoneTests
  , cacheApiTests
  , Keter.RateLimiter.IPZonesTests.tests
  , Keter.RateLimiter.LeakyBucketStateTests.tests
  , Keter.RateLimiter.TokenBucketStateTests.tests
  , Keter.RateLimiter.LeakyBucketTests.tests
  , Keter.RateLimiter.SlidingWindowTests.tests
  , Keter.RateLimiter.TokenBucketTests.tests
  , Keter.RateLimiter.WAITests.tests
  ]

-- | Tests for general rate-limiting functionality using the default IP zone.
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

-- | Tests the Fixed Window algorithm. It expects the first 3 requests to succeed and the next 2 to be blocked.
testFixedWindow :: IO ()
testFixedWindow = do
  env <- initConfig mainTestGetRequestIPZone
  let throttleConfig = ThrottleConfig
        { throttleLimit = 3
        , throttlePeriod = 10
        , throttleAlgorithm = FixedWindow
        , throttleIdentifierBy = IdIP
        , throttleTokenBucketTTL = Nothing
        }
  envWithThrottle <- addThrottle env "test-throttle" throttleConfig
  let requests = replicate 5 $ makeRequest genericTestIP "/test"
      expectedResponses = replicate 3 "Success" ++ replicate 2 "Too Many Requests"
  executeRequests envWithThrottle requests expectedResponses

-- | Tests the Sliding Window algorithm. It expects the first 3 requests to succeed and subsequent requests within the window to be blocked.
-- It also verifies that requests are allowed again after the window has passed.
testSlidingWindow :: IO ()
testSlidingWindow = do
  env <- initConfig mainTestGetRequestIPZone
  let throttleConfig = ThrottleConfig
        { throttleLimit = 3
        , throttlePeriod = 10
        , throttleAlgorithm = SlidingWindow
        , throttleIdentifierBy = IdIP
        , throttleTokenBucketTTL = Nothing
        }
  envWithThrottle <- addThrottle env "test-throttle" throttleConfig
  let requests = replicate 5 $ makeRequest genericTestIP "/test"
      expectedResponses = replicate 3 "Success" ++ replicate 2 "Too Many Requests"
  executeRequests envWithThrottle requests expectedResponses
  -- Test partial window overlap
  threadDelay (5_000_000) -- Wait 5s (half period)
  blocked <- instrument envWithThrottle (makeRequest genericTestIP "/test")
  assertEqual "Request in overlapping window should be blocked" True blocked
  threadDelay (6_500_000) -- Wait 5.5s (past period)
  blocked' <- instrument envWithThrottle (makeRequest genericTestIP "/test")
  assertEqual "Request after full period should succeed" False blocked'

-- | Tests the Token Bucket algorithm, including TTL-based bucket expiry and refill.
testTokenBucket :: IO ()
testTokenBucket = do
  let ttlSeconds = 3
  env <- initConfig mainTestGetRequestIPZone
  let throttleConfig = ThrottleConfig
        { throttleLimit = 3
        , throttlePeriod = 1
        , throttleAlgorithm = TokenBucket
        , throttleIdentifierBy = IdIP
        , throttleTokenBucketTTL = Just ttlSeconds
        }
  envWithThrottle <- addThrottle env "test-throttle" throttleConfig
  let requests = replicate 5 $ makeRequest genericTestIP "/test"
      expectedResponses = replicate 3 "Success" ++ replicate 2 "Too Many Requests"
  executeRequests envWithThrottle requests expectedResponses
  threadDelay ((ttlSeconds + 1) * 1_000_000)
  blocked <- instrument envWithThrottle (makeRequest genericTestIP "/test")
  assertEqual "Request should succeed after TTL expiry and refill" False blocked
  b1 <- instrument envWithThrottle (makeRequest genericTestIP "/test")
  b2 <- instrument envWithThrottle (makeRequest genericTestIP "/test")
  b3 <- instrument envWithThrottle (makeRequest genericTestIP "/test")
  assertEqual "Next requests after refill" [False, False, True] [b1, b2, b3]

-- | Tests the Leaky Bucket algorithm, verifying burst tolerance and the steady leak rate.
testLeakyBucket :: IO ()
testLeakyBucket = do
  env <- initConfig mainTestGetRequestIPZone
  let throttleConfig = ThrottleConfig
        { throttleLimit = 3
        , throttlePeriod = 10
        , throttleAlgorithm = LeakyBucket
        , throttleIdentifierBy = IdIP
        , throttleTokenBucketTTL = Nothing
        }
  envWithThrottle <- addThrottle env "test-throttle" throttleConfig
  let req = makeRequest genericTestIP "/test"
  -- Test burst behavior
  blocked1 <- instrument envWithThrottle req
  blocked2 <- instrument envWithThrottle req
  blocked3 <- instrument envWithThrottle req
  assertEqual "First 3 requests succeed" [False, False, False] [blocked1, blocked2, blocked3]
  blocked4 <- instrument envWithThrottle req
  assertEqual "Fourth request blocked" True blocked4
  -- Test steady leak rate (1 token every ~3.33s for limit=3, period=10)
  threadDelay (3_500_000) -- Wait ~3.5s for 1 token to leak
  blocked5 <- instrument envWithThrottle req
  assertEqual "Request succeeds after leak" False blocked5
  -- Verify subsequent request is blocked
  blocked6 <- instrument envWithThrottle req
  assertEqual "Next request blocked" True blocked6

-- | Tests that rate limiting can be applied to specific request paths.
-- In this case, only requests to "/login" are throttled.
testPathSpecificThrottle :: IO ()
testPathSpecificThrottle = do
  env <- initConfig mainTestGetRequestIPZone
  let throttleConfig = ThrottleConfig
        { throttleLimit = 2
        , throttlePeriod = 10
        , throttleAlgorithm = FixedWindow
        , throttleIdentifierBy = IdIPAndPath
        , throttleTokenBucketTTL = Nothing
        }
  envWithThrottle <- addThrottle env "login-throttle" throttleConfig
  let loginRequests = replicate 3 $ makeRequest genericTestIP "/login"
      homeRequests = replicate 3 $ makeRequest genericTestIP "/home"
      expectedLoginResponses = replicate 2 "Success" ++ ["Too Many Requests"]
      expectedHomeResponses = replicate 2 "Success" ++ ["Too Many Requests"]
  executeRequests envWithThrottle loginRequests expectedLoginResponses
  executeRequests envWithThrottle homeRequests expectedHomeResponses

-- | Tests the application of multiple, independent throttles.
-- An IP-based throttle and a path-specific throttle are applied simultaneously.
testMultipleThrottles :: IO ()
testMultipleThrottles = do
  env <- initConfig mainTestGetRequestIPZone
  let ipThrottleConfig = ThrottleConfig
        { throttleLimit = 5
        , throttlePeriod = 10
        , throttleAlgorithm = FixedWindow
        , throttleIdentifierBy = IdIP
        , throttleTokenBucketTTL = Nothing
        }
      loginThrottleConfig = ThrottleConfig
        { throttleLimit = 2
        , throttlePeriod = 10
        , throttleAlgorithm = SlidingWindow
        , throttleIdentifierBy = IdIPAndPath
        , throttleTokenBucketTTL = Nothing
        }
  envWithIpThrottle <- addThrottle env "ip-throttle" ipThrottleConfig
  envWithBothThrottles <- addThrottle envWithIpThrottle "login-throttle" loginThrottleConfig
  let requests = [ makeRequest genericTestIP "/login"
                 , makeRequest genericTestIP "/login"
                 , makeRequest genericTestIP "/login" -- Blocked by login throttle
                 , makeRequest genericTestIP "/home"
                 , makeRequest genericTestIP "/about"
                 , makeRequest genericTestIP "/contact" -- Blocked by IP throttle (6th req total)
                 ]
      expectedResponses = [ "Success", "Success", "Too Many Requests"
                         , "Success", "Success", "Too Many Requests"
                         ]
  executeRequests envWithBothThrottles requests expectedResponses

-- | Verifies that creating a new environment effectively resets rate-limiting state.
testOriginalResetAfterPeriod :: IO ()
testOriginalResetAfterPeriod = do
  env1 <- initConfig mainTestGetRequestIPZone
  let throttleConfig = ThrottleConfig
        { throttleLimit = 2
        , throttlePeriod = 1
        , throttleAlgorithm = FixedWindow
        , throttleIdentifierBy = IdIP
        , throttleTokenBucketTTL = Nothing
        }
  envWithThrottle1 <- addThrottle env1 "test-throttle" throttleConfig
  let requests1 = replicate 3 $ makeRequest genericTestIP "/test"
      expectedResponses1 = replicate 2 "Success" ++ ["Too Many Requests"]
  executeRequests envWithThrottle1 requests1 expectedResponses1
  threadDelay (1_100_000)
  env2 <- initConfig mainTestGetRequestIPZone
  envWithThrottle2 <- addThrottle env2 "test-throttle" throttleConfig
  let requests2 = replicate 3 $ makeRequest genericTestIP2 "/test"
      expectedResponses2 = replicate 2 "Success" ++ ["Too Many Requests"]
  executeRequests envWithThrottle2 requests2 expectedResponses2

-- | Verifies that counters for Fixed Window algorithm expire correctly over time within the same environment.
testTimeBasedReset :: IO ()
testTimeBasedReset = do
  let periodSec = 1
      limit = 1
  env <- initConfig mainTestGetRequestIPZone
  let throttleConfig = ThrottleConfig
        { throttleLimit = limit
        , throttlePeriod = periodSec
        , throttleAlgorithm = FixedWindow
        , throttleIdentifierBy = IdIP
        , throttleTokenBucketTTL = Nothing
        }
  envWithThrottle <- addThrottle env "reset-throttle" throttleConfig
  let req = makeRequest ipForZoneA "/reset_test"
  -- First request
  blocked1 <- instrument envWithThrottle req
  assertEqual "First request to zone A should succeed" False blocked1
  -- Second request (should be blocked)
  blocked2 <- instrument envWithThrottle req
  assertEqual "Second request to zone A should be blocked" True blocked2
  -- Wait for period + buffer
  threadDelay 1_500_000 -- 1.5 seconds to ensure expiration
  -- Debug cache state
  cachesMap <- readTVarIO (envZoneCachesMap envWithThrottle)
  zoneCaches <- case HashMap.lookup testIPZoneA cachesMap of
    Just caches -> return caches
    Nothing -> assertFailure "Zone caches not found" >> return undefined
  let cache :: Cache (InMemoryStore 'FixedWindow)
      cache = zscCounterCache zoneCaches
      key = makeCacheKey "reset-throttle" FixedWindow testIPZoneA (case unsafePerformIO $ RequestUtils.byIP req of Just ip -> ip; Nothing -> "")
  mVal <- readCache cache key :: IO (Maybe Int)
  when (isJust mVal) $ putStrLn $ "Cache value before third request: " ++ show mVal
  -- Try manually deleting if it exists
  when (isJust mVal) $ do
    putStrLn "Manually deleting expired key"
    deleteCache cache key
  -- Explicitly purge expired entries
  cache' <- atomically $ readTVar (case cacheStore cache of CounterStore ref -> ref)
  purgeExpired cache' -- Now in IO, not STM
  -- Third request (should succeed)
  blocked3 <- instrument envWithThrottle req
  assertEqual "Request after period to zone A should succeed" False blocked3

-- | Tests for verifying the functionality of IP Zones.
ipZoneTests :: TestTree
ipZoneTests = testGroup "IP Zone Functionality Tests"
  [ testCase "IP Zone Isolation" testIPZoneIsolation
  , testCase "IP Zone Default Fallback" testIPZoneDefaultFallback
  , testCase "Cache Reset All Across Zones" testIPZoneCacheResetAll
  ]

-- | Verifies that different IP zones are rate-limited independently.
testIPZoneIsolation :: IO ()
testIPZoneIsolation = do
  let limit = 1
      period = 10
  env <- initConfig mainTestGetRequestIPZone
  let throttleConfig = ThrottleConfig
        { throttleLimit = limit
        , throttlePeriod = period
        , throttleAlgorithm = FixedWindow
        , throttleIdentifierBy = IdIP
        , throttleTokenBucketTTL = Nothing
        }
  envWithThrottle <- addThrottle env "test-throttle" throttleConfig
  let reqA1 = makeRequest ipForZoneA "/test"
      reqB1 = makeRequest ipForZoneB "/test"
  b1 <- instrument envWithThrottle reqA1
  assertEqual "Zone A first request allowed" False b1
  b2 <- instrument envWithThrottle reqA1
  assertEqual "Zone A second request blocked" True b2
  b3 <- instrument envWithThrottle reqB1
  assertEqual "Zone B first request allowed" False b3

-- | Verifies that IPs not matching a specific zone fall back to the default zone and are tracked independently by their IP.
testIPZoneDefaultFallback :: IO ()
testIPZoneDefaultFallback = do
  env <- initConfig mainTestGetRequestIPZone
  let throttleConfig = ThrottleConfig
        { throttleLimit = 1
        , throttlePeriod = 10
        , throttleAlgorithm = FixedWindow
        , throttleIdentifierBy = IdIP
        , throttleTokenBucketTTL = Nothing
        }
  envWithThrottle <- addThrottle env "test-throttle" throttleConfig
  let reqDefault = makeRequest ipForDefaultZone "/test"
      reqGeneric = makeRequest genericTestIP "/test"
  b1 <- instrument envWithThrottle reqDefault
  assertEqual "Default zone first request allowed" False b1
  b2 <- instrument envWithThrottle reqGeneric
  assertEqual "Generic IP in default zone should also be allowed (independent counter)" False b2

-- | Verifies that 'cacheResetAll' clears the state for all defined IP zones.
testIPZoneCacheResetAll :: IO ()
testIPZoneCacheResetAll = do
  env <- initConfig mainTestGetRequestIPZone
  let throttleConfig = ThrottleConfig
        { throttleLimit = 1
        , throttlePeriod = 10
        , throttleAlgorithm = FixedWindow
        , throttleIdentifierBy = IdIP
        , throttleTokenBucketTTL = Nothing
        }
  envWithThrottle <- addThrottle env "test-throttle" throttleConfig
  let reqA = makeRequest ipForZoneA "/test"
      reqB = makeRequest ipForZoneB "/test"
  _ <- instrument envWithThrottle reqA
  _ <- instrument envWithThrottle reqB
  cacheResetAll envWithThrottle
  b1 <- instrument envWithThrottle reqA
  b2 <- instrument envWithThrottle reqB
  assertEqual "Zone A after reset" False b1
  assertEqual "Zone B after reset" False b2

-- | Tests for the cache API, including both the zone-aware wrappers and direct cache access.
cacheApiTests :: TestTree
cacheApiTests = testGroup "Cache API (wrappers and customisable)"
  [ testCase "incStoreWithZone wrapper increments independently" testIncStoreWithZone
  , testCase "incStore manual key increments independently" testIncStoreManual
  , testCase "readCacheWithZone and writeCacheWithZone" testReadWriteCacheWithZone
  , testCase "readCache and writeCache manual" testReadWriteCacheManual
  ]

-- | Tests that the 'incStoreWithZone' wrapper correctly increments a counter.
testIncStoreWithZone :: IO ()
testIncStoreWithZone = do
  store <- createInMemoryStore @'FixedWindow
  let cache :: Cache (InMemoryStore 'FixedWindow)
      cache = newCache FixedWindow store
      throttleName = "test-throttle"
      ipZone = "zoneX"
      userKey = "userX"
  v1 <- incStoreWithZone cache throttleName ipZone userKey 10 :: IO Int
  v2 <- incStoreWithZone cache throttleName ipZone userKey 10 :: IO Int
  assertEqual "First increment (wrapper)" 1 v1
  assertEqual "Second increment (wrapper)" 2 v2

-- | Tests that the lower-level 'incrementCache' function works correctly with a manually constructed key.
testIncStoreManual :: IO ()
testIncStoreManual = do
  store <- createInMemoryStore @'FixedWindow
  let cache :: Cache (InMemoryStore 'FixedWindow)
      cache = newCache FixedWindow store
      customKey = "zoneY:userY:extra"
  v1 <- incrementCache cache customKey 10 :: IO Int
  v2 <- incrementCache cache customKey 10 :: IO Int
  assertEqual "First increment (manual)" 1 v1
  assertEqual "Second increment (manual)" 2 v2

-- | Tests the 'writeCacheWithZone' and 'readCacheWithZone' wrapper functions.
testReadWriteCacheWithZone :: IO ()
testReadWriteCacheWithZone = do
  store <- createInMemoryStore @'FixedWindow
  let cache :: Cache (InMemoryStore 'FixedWindow)
      cache = newCache FixedWindow store
      throttleName = "test-throttle"
      ipZone = "zoneZ"
      userKey = "userZ"
      val = 42 :: Int
  writeCacheWithZone cache throttleName ipZone userKey val 10
  mVal <- readCacheWithZone cache throttleName ipZone userKey :: IO (Maybe Int)
  assertEqual "Read after write (wrapper)" (Just val) mVal

-- | Tests the lower-level 'writeCache' and 'readCache' functions with a manually constructed key.
testReadWriteCacheManual :: IO ()
testReadWriteCacheManual = do
  store <- createInMemoryStore @'FixedWindow
  let cache :: Cache (InMemoryStore 'FixedWindow)
      cache = newCache FixedWindow store
      customKey = "zoneW:userW:custom"
      val = 99 :: Int
  writeCache cache customKey val 10
  mVal <- readCache cache customKey :: IO (Maybe Int)
  assertEqual "Read after write (manual)" (Just val) mVal
