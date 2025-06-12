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
import Control.Monad (replicateM, forM_)
import Control.Concurrent (threadDelay)

import Keter.RateLimiter.Cache
import Keter.RateLimiter
import Keter.RateLimiter.Notifications -- Assuming this is still relevant, though not directly tested here

-- We expect IPZoneIdentifier and defaultIPZone to be exported by Keter.RateLimiter

main :: IO ()
main = do
  store <- createInMemoryStore
  let cache :: Cache (InMemoryStore "counter")
      cache = newCache "counter" store
  putStrLn ""
  v1 <- incStore cache "test" 10
  print v1
  v2 <- incStore cache "test" 10
  print v2
  v3 <- incStore cache "test" 10
  print v3
  defaultMain tests

tests :: TestTree
tests = testGroup "Rate Limiter and IP Zones Tests"
  [ rateLimiterTests
  , ipZoneTests
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
testGetRequestIPZone :: Request -> IPZoneIdentifier
testGetRequestIPZone req
  | requestIP req == ipForZoneA = testIPZoneA
  | requestIP req == ipForZoneB = testIPZoneB
  | requestIP req == genericTestIP = defaultIPZone
  | requestIP req == genericTestIP2 = defaultIPZone
  | requestIP req == ipForDefaultZone = defaultIPZone
  | otherwise = defaultIPZone

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
  actualResponses <- mapM (attackMiddleware env (const (return "Success"))) requests
  assertEqual "Responses should match expected" expectedResponses actualResponses

-- ===== Rate Limiter Tests =====

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
  threadDelay (2 * 1000000) -- Wait for tokens to refill
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
  threadDelay (4 * 1000000) -- Wait for bucket to leak
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
      requests =
        [ makeRequest genericTestIP "/login"
        , makeRequest genericTestIP "/login"
        , makeRequest genericTestIP "/login"
        , makeRequest genericTestIP "/home"
        , makeRequest genericTestIP "/about"
        , makeRequest genericTestIP "/contact"
        ]
      expectedResponses =
        [ "Success", "Success", "Too Many Requests"
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
  threadDelay (1100000) -- Wait longer than period (1s)
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

-- ===== New IP Zone Tests =====

testIPZoneIsolation :: IO ()
testIPZoneIsolation = do
  let limit = 1
      period = 10
  env <- initConfig testGetRequestIPZone
  let throttleCfg = ThrottleConfig limit period FixedWindow (\req -> Just (requestIP req))
      envWithThrottle = addThrottle env "zone-test-throttle" throttleCfg
  executeRequests envWithThrottle (replicate (limit + 1) $ makeRequest ipForZoneA "/zone_test")
    (replicate limit "Success" ++ ["Too Many Requests"])
  executeRequests envWithThrottle (replicate (limit + 1) $ makeRequest ipForZoneB "/zone_test")
    (replicate limit "Success" ++ ["Too Many Requests"])
  executeRequests envWithThrottle (replicate (limit + 1) $ makeRequest ipForDefaultZone "/zone_test")
    (replicate limit "Success" ++ ["Too Many Requests"])

testIPZoneDefaultFallback :: IO ()
testIPZoneDefaultFallback = do
  let limit = 2
      period = 10
  env <- initConfig testGetRequestIPZone
  let throttleCfg = ThrottleConfig limit period FixedWindow (\req -> Just (requestIP req))
      envWithThrottle = addThrottle env "zone-fallback-throttle" throttleCfg
  executeRequests envWithThrottle (replicate (limit + 1) $ makeRequest genericTestIP "/default_zone")
    (replicate limit "Success" ++ ["Too Many Requests"])

testIPZoneCacheResetAll :: IO ()
testIPZoneCacheResetAll = do
  let limit = 1
      period = 10
  env <- initConfig testGetRequestIPZone
  let throttleCfg = ThrottleConfig limit period FixedWindow (\req -> Just (requestIP req))
      envWithThrottle = addThrottle env "zone-reset-throttle" throttleCfg
  executeRequests envWithThrottle (replicate (limit + 1) $ makeRequest ipForZoneA "/reset_zone")
    (replicate limit "Success" ++ ["Too Many Requests"])
  env2 <- initConfig testGetRequestIPZone
  let envWithThrottle2 = addThrottle env2 "zone-reset-throttle" throttleCfg
  executeRequests envWithThrottle2 (replicate (limit + 1) $ makeRequest ipForZoneA "/reset_zone")
    (replicate limit "Success" ++ ["Too Many Requests"])

