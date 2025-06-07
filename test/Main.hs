{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (replicateM, forM_)
import Control.Concurrent (threadDelay)

import Keter.RateLimiter
import Keter.RateLimiter.Notifications -- Assuming this is still relevant, though not directly tested here
-- We expect IPZoneIdentifier and defaultIPZone to be exported by Keter.RateLimiter
-- (or Keter.RateLimiter.IPZones, but typically re-exported)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Rate Limiter and IP Zones Tests"
  [ rateLimiterTests
  , ipZoneTests -- New group for IP Zone specific tests
  ]

-- IPs and Zone Identifiers for testing
testIPZoneA :: IPZoneIdentifier
testIPZoneA = "testZoneA"

testIPZoneB :: IPZoneIdentifier
testIPZoneB = "testZoneB"

ipForZoneA :: Text
ipForZoneA = "10.0.0.1"

ipForZoneB :: Text
ipForZoneB = "20.0.0.1"

ipForDefaultZone :: Text
ipForDefaultZone = "30.0.0.1"

-- General IP used in older tests, will fall into defaultIPZone
genericTestIP :: Text
genericTestIP = "192.168.1.1"
genericTestIP2 :: Text
genericTestIP2 = "192.168.1.2"


-- Test specific IP zone resolution function
-- This function will be passed to initConfig for all tests
testGetRequestIPZone :: Request -> IPZoneIdentifier
testGetRequestIPZone req
  | requestIP req == ipForZoneA = testIPZoneA
  | requestIP req == ipForZoneB = testIPZoneB
  -- Add other specific IPs if needed for particular original tests,
  -- otherwise they fall into defaultIPZone.
  | requestIP req == genericTestIP = defaultIPZone
  | requestIP req == genericTestIP2 = defaultIPZone
  | requestIP req == ipForDefaultZone = defaultIPZone
  | otherwise                   = defaultIPZone -- Fallback for any other IPs

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

ipZoneTests :: TestTree
ipZoneTests = testGroup "IP Zone Functionality Tests"
  [ testCase "IP Zone Isolation" testIPZoneIsolation
  , testCase "IP Zone Default Fallback" testIPZoneDefaultFallback
  , testCase "Cache Reset All Across Zones" testIPZoneCacheResetAll
  ]

-- Helper function to create a test request
makeRequest :: Text -> Text -> Request
makeRequest ip path = Request
  { requestMethod = "GET"
  , requestPath = path
  , requestHost = "example.com"
  , requestIP = ip
  , requestHeaders = Map.empty
  }

-- Helper function to execute multiple requests and assert expected responses
executeRequests :: Env -> [Request] -> [Text] -> IO ()
executeRequests env requests expectedResponses = do
  actualResponses <- mapM (\req -> attackMiddleware env req (return "Success")) requests
  assertEqual "Responses should match expected" expectedResponses actualResponses

-- ===== Modified Rate Limiter Tests to use testGetRequestIPZone =====

testFixedWindow :: IO ()
testFixedWindow = do
  let throttleConfig = ThrottleConfig 3 10 (const True) requestIP FixedWindow
      config = addThrottle "test-throttle" throttleConfig defaultConfiguration
  env <- initConfig config testGetRequestIPZone
  let requests = replicate 5 $ makeRequest genericTestIP "/test"
      expectedResponses = replicate 3 "Success" ++ replicate 2 "Rate limit exceeded"
  executeRequests env requests expectedResponses

testSlidingWindow :: IO ()
testSlidingWindow = do
  let throttleConfig = ThrottleConfig 3 10 (const True) requestIP SlidingWindow
      config = addThrottle "test-throttle" throttleConfig defaultConfiguration
  env <- initConfig config testGetRequestIPZone
  let requests = replicate 5 $ makeRequest genericTestIP "/test"
      expectedResponses = replicate 3 "Success" ++ replicate 2 "Rate limit exceeded"
  executeRequests env requests expectedResponses

testTokenBucket :: IO ()
testTokenBucket = do
  let throttleConfig = ThrottleConfig 3 1 (const True) requestIP TokenBucket
      config = addThrottle "test-throttle" throttleConfig defaultConfiguration
  env <- initConfig config testGetRequestIPZone
  let requests = replicate 5 $ makeRequest genericTestIP "/test"
      expectedResponses = replicate 3 "Success" ++ replicate 2 "Rate limit exceeded"
  executeRequests env requests expectedResponses
  threadDelay (2 * 1000000) -- Wait for tokens to refill
  response <- attackMiddleware env (makeRequest genericTestIP "/test") (return "Success")
  assertEqual "Request should succeed after refill" "Success" response

testLeakyBucket :: IO ()
testLeakyBucket = do
  let throttleConfig = ThrottleConfig 3 10 (const True) requestIP LeakyBucket
      config = addThrottle "test-throttle" throttleConfig defaultConfiguration
  env <- initConfig config testGetRequestIPZone
  let requests = replicate 5 $ makeRequest genericTestIP "/test"
      expectedResponses = replicate 3 "Success" ++ replicate 2 "Rate limit exceeded"
  executeRequests env requests expectedResponses
  threadDelay (4 * 1000000) -- Wait for bucket to leak
  response <- attackMiddleware env (makeRequest genericTestIP "/test") (return "Success")
  assertEqual "Request should succeed after leaking" "Success" response

testPathSpecificThrottle :: IO ()
testPathSpecificThrottle = do
  let throttleConfig = ThrottleConfig 2 10 (\req -> requestPath req == "/login") (\req -> requestIP req <> ":" <> requestPath req) FixedWindow
      config = addThrottle "login-throttle" throttleConfig defaultConfiguration
  env <- initConfig config testGetRequestIPZone
  let loginRequests = replicate 3 $ makeRequest genericTestIP "/login"
      homeRequests = replicate 3 $ makeRequest genericTestIP "/home"
      expectedLoginResponses = replicate 2 "Success" ++ ["Rate limit exceeded"]
      expectedHomeResponses = replicate 3 "Success"
  executeRequests env loginRequests expectedLoginResponses
  executeRequests env homeRequests expectedHomeResponses

testMultipleThrottles :: IO ()
testMultipleThrottles = do
  let ipThrottleConfig = ThrottleConfig 5 10 (const True) requestIP FixedWindow
      loginThrottleConfig = ThrottleConfig 2 10 (\req -> requestPath req == "/login") (\req -> requestIP req <> ":" <> requestPath req) SlidingWindow
      baseConfig = defaultConfiguration
      configWithIpThrottle = addThrottle "ip-throttle" ipThrottleConfig baseConfig
      config = addThrottle "login-throttle" loginThrottleConfig configWithIpThrottle
  env <- initConfig config testGetRequestIPZone
  let requests = [ makeRequest genericTestIP "/login"    -- Login #1 (IP #1)
                 , makeRequest genericTestIP "/login"    -- Login #2 (IP #2)
                 , makeRequest genericTestIP "/login"    -- Login #3 (Blocked by login-throttle) (IP #3)
                 , makeRequest genericTestIP "/home"     -- Home #1 (IP #4)
                 , makeRequest genericTestIP "/about"    -- About #1 (IP #5)
                 , makeRequest genericTestIP "/contact"  -- Contact #1 (Blocked by ip-throttle) (IP #6)
                 ]
      expectedResponses = [ "Success", "Success", "Rate limit exceeded",
                            "Success", "Success", "Rate limit exceeded" ]
  executeRequests env requests expectedResponses

testOriginalResetAfterPeriod :: IO ()
testOriginalResetAfterPeriod = do
  let throttleConfig = ThrottleConfig 2 1 (const True) requestIP FixedWindow
      config = addThrottle "test-throttle" throttleConfig defaultConfiguration
  env1 <- initConfig config testGetRequestIPZone
  let requests1 = replicate 3 $ makeRequest genericTestIP "/test"
      expectedResponses1 = replicate 2 "Success" ++ ["Rate limit exceeded"]
  executeRequests env1 requests1 expectedResponses1

  threadDelay (1100000) -- Wait longer than period (1s)

  -- This tests a new environment, which should always be fresh
  env2 <- initConfig config testGetRequestIPZone
  let requests2 = replicate 3 $ makeRequest genericTestIP2 "/test" -- Different IP, but also default zone
      expectedResponses2 = replicate 2 "Success" ++ ["Rate limit exceeded"]
  executeRequests env2 requests2 expectedResponses2

testTimeBasedReset :: IO ()
testTimeBasedReset = do
  let periodSec = 1
      limit = 1
      throttleConfig = ThrottleConfig limit periodSec (const True) requestIP FixedWindow
      -- Ensure testIPZoneA is configured for this test
      config = addThrottle "reset-throttle" throttleConfig (defaultConfiguration { configIPZones = [testIPZoneA] })
  env <- initConfig config testGetRequestIPZone

  -- Hit limit for ipForZoneA
  resp1 <- attackMiddleware env (makeRequest ipForZoneA "/reset_test") (return "Success")
  assertEqual "First request to zone A should succeed" "Success" resp1
  resp2 <- attackMiddleware env (makeRequest ipForZoneA "/reset_test") (return "Success")
  assertEqual "Second request to zone A should be blocked" "Rate limit exceeded" resp2

  -- Wait for period to elapse
  threadDelay ( (periodSec * 1000000) + 200000 ) -- period + buffer

  -- Try again, should succeed
  resp3 <- attackMiddleware env (makeRequest ipForZoneA "/reset_test") (return "Success")
  assertEqual "Request after period to zone A should succeed" "Success" resp3


-- ===== New IP Zone Tests =====

-- Configuration helper for IP Zone tests
ipZoneTestingConfig :: [IPZoneIdentifier] -> ThrottleConfig -> Configuration
ipZoneTestingConfig zones throttleCfg =
  addThrottle "zone-test-throttle" throttleCfg (defaultConfiguration { configIPZones = zones })

testIPZoneIsolation :: IO ()
testIPZoneIsolation = do
  let limit = 1
      period = 10
      throttleCfg = ThrottleConfig limit period (const True) requestIP FixedWindow
      config = ipZoneTestingConfig [testIPZoneA, testIPZoneB] throttleCfg
  env <- initConfig config testGetRequestIPZone

  -- Test Zone A
  executeRequests env (replicate (limit + 1) $ makeRequest ipForZoneA "/zone_test")
                      (replicate limit "Success" ++ ["Rate limit exceeded"])
  -- Test Zone B (should be independent)
  executeRequests env (replicate (limit + 1) $ makeRequest ipForZoneB "/zone_test")
                      (replicate limit "Success" ++ ["Rate limit exceeded"])
  -- Test Default Zone (should be independent)
  executeRequests env (replicate (limit + 1) $ makeRequest ipForDefaultZone "/zone_test")
                      (replicate limit "Success" ++ ["Rate limit exceeded"])

testIPZoneDefaultFallback :: IO ()
testIPZoneDefaultFallback = do
  let limit = 1
      period = 10
      throttleCfg = ThrottleConfig limit period (const True) requestIP FixedWindow
      -- Only explicitly configure testIPZoneA. ipForDefaultZone will use the default zone.
      config = ipZoneTestingConfig [testIPZoneA] throttleCfg
  env <- initConfig config testGetRequestIPZone

  -- Hit limit in Zone A
  executeRequests env (replicate (limit + 1) $ makeRequest ipForZoneA "/path1")
                      (replicate limit "Success" ++ ["Rate limit exceeded"])

  -- Requests to default zone should be independent
  executeRequests env (replicate (limit + 1) $ makeRequest ipForDefaultZone "/path2")
                      (replicate limit "Success" ++ ["Rate limit exceeded"])
  
  -- Verify Zone A is still blocked
  responseZoneA <- attackMiddleware env (makeRequest ipForZoneA "/path1") (return "Success")
  assertEqual "Zone A should still be blocked" "Rate limit exceeded" responseZoneA

testIPZoneCacheResetAll :: IO ()
testIPZoneCacheResetAll = do
  let limit = 1
      period = 10
      throttleCfg = ThrottleConfig limit period (const True) requestIP FixedWindow
      config = ipZoneTestingConfig [testIPZoneA, testIPZoneB] throttleCfg
  env <- initConfig config testGetRequestIPZone

  -- Hit limits in all relevant zones
  forM_ [ipForZoneA, ipForZoneB, ipForDefaultZone] $ \ip -> do
    executeRequests env (replicate (limit + 1) $ makeRequest ip "/reset_all_test")
                        (replicate limit "Success" ++ ["Rate limit exceeded"])

  -- Reset all caches
  cacheResetAll env

  -- All zones should now allow requests again
  forM_ [ipForZoneA, ipForZoneB, ipForDefaultZone] $ \ip -> do
    response <- attackMiddleware env (makeRequest ip "/reset_all_test") (return "Success")
    assertEqual ("Request from " ++ T.unpack ip ++ " after cacheResetAll should succeed") "Success" response