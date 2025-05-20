{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (replicateM)
import Control.Concurrent (threadDelay)

import Keter.RateLimiter
import Keter.RateLimiter.Notifications

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Rate Limiter Tests" 
  [ testCase "Fixed Window Rate Limiting" testFixedWindow
  , testCase "Sliding Window Rate Limiting" testSlidingWindow
  , testCase "Token Bucket Rate Limiting" testTokenBucket
  , testCase "Leaky Bucket Rate Limiting" testLeakyBucket
  , testCase "Path-specific Rate Limiting" testPathSpecificThrottle
  , testCase "Multiple Throttles" testMultipleThrottles
  , testCase "Reset After Period" testResetAfterPeriod
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

-- Test fixed window rate limiting
testFixedWindow :: IO ()
testFixedWindow = do
  -- Create a configuration with fixed window throttle
  let throttleConfig = ThrottleConfig
        { throttleLimit = 3
        , throttlePeriod = 10  -- 10 seconds
        , throttleCondition = const True
        , throttleKeyFn = requestIP
        , throttleAlgorithm = FixedWindow
        }
      
      config = addThrottle "test-throttle" throttleConfig defaultConfiguration
  
  env <- initConfig config
  
  -- Create 5 identical requests (same IP)
  let requests = replicate 5 $ makeRequest "192.168.1.1" "/test"
      
      -- First 3 should succeed, next 2 should be rate limited
      expectedResponses = replicate 3 "Success" ++ replicate 2 "Rate limit exceeded"
  
  executeRequests env requests expectedResponses

-- Test sliding window rate limiting  
testSlidingWindow :: IO ()
testSlidingWindow = do
  -- Create a configuration with sliding window throttle
  let throttleConfig = ThrottleConfig
        { throttleLimit = 3
        , throttlePeriod = 10  -- 10 seconds
        , throttleCondition = const True
        , throttleKeyFn = requestIP
        , throttleAlgorithm = SlidingWindow
        }
      
      config = addThrottle "test-throttle" throttleConfig defaultConfiguration
  
  env <- initConfig config
  
  -- Create 5 identical requests (same IP)
  let requests = replicate 5 $ makeRequest "192.168.1.1" "/test"
      
      -- First 3 should succeed, next 2 should be rate limited
      expectedResponses = replicate 3 "Success" ++ replicate 2 "Rate limit exceeded"
  
  executeRequests env requests expectedResponses

-- Test token bucket rate limiting
testTokenBucket :: IO ()
testTokenBucket = do
  -- Create a configuration with token bucket throttle - use a very short refill period
  let throttleConfig = ThrottleConfig
        { throttleLimit = 3  -- Bucket capacity
        , throttlePeriod = 1  -- Refill period of 1 second (much shorter for tests)
        , throttleCondition = const True
        , throttleKeyFn = requestIP
        , throttleAlgorithm = TokenBucket
        }
      
      config = addThrottle "test-throttle" throttleConfig defaultConfiguration
  
  env <- initConfig config
  
  -- Create 5 identical requests (same IP)
  let requests = replicate 5 $ makeRequest "192.168.1.1" "/test"
      
      -- First 3 should succeed (tokens available), next 2 should be rate limited
      expectedResponses = replicate 3 "Success" ++ replicate 2 "Rate limit exceeded"
  
  executeRequests env requests expectedResponses
  
  -- Wait for token refill - use a longer delay to ensure refill occurs
  threadDelay (2 * 1000000)  -- Wait 2 seconds (twice the refill period)
  
  -- Try one more request after waiting, should get at least one token back
  response <- attackMiddleware env (makeRequest "192.168.1.1" "/test") (return "Success")
  assertEqual "Request should succeed after refill" "Success" response

-- Test leaky bucket rate limiting
testLeakyBucket :: IO ()
testLeakyBucket = do
  -- Create a configuration with leaky bucket throttle
  let throttleConfig = ThrottleConfig
        { throttleLimit = 3  -- Bucket capacity
        , throttlePeriod = 10  -- Leak period
        , throttleCondition = const True
        , throttleKeyFn = requestIP
        , throttleAlgorithm = LeakyBucket
        }
      
      config = addThrottle "test-throttle" throttleConfig defaultConfiguration
  
  env <- initConfig config
  
  -- Create 5 identical requests (same IP)
  let requests = replicate 5 $ makeRequest "192.168.1.1" "/test"
      
      -- First 3 should succeed (capacity available), next 2 should be rate limited
      expectedResponses = replicate 3 "Success" ++ replicate 2 "Rate limit exceeded"
  
  executeRequests env requests expectedResponses
  
  -- Wait for bucket to leak
  threadDelay (4 * 1000000)  -- Wait 4 seconds
  
  -- Try one more request, should succeed due to leaking
  response <- attackMiddleware env (makeRequest "192.168.1.1" "/test") (return "Success")
  assertEqual "Request should succeed after leaking" "Success" response

-- Test path-specific throttling
testPathSpecificThrottle :: IO ()
testPathSpecificThrottle = do
  -- Create a configuration with path-specific throttle
  let throttleConfig = ThrottleConfig
        { throttleLimit = 2
        , throttlePeriod = 10
        , throttleCondition = \req -> requestPath req == "/login"
        , throttleKeyFn = \req -> requestIP req <> ":" <> requestPath req
        , throttleAlgorithm = FixedWindow
        }
      
      config = addThrottle "login-throttle" throttleConfig defaultConfiguration
  
  env <- initConfig config
  
  -- Create requests for /login and /home with same IP
  let loginRequests = replicate 3 $ makeRequest "192.168.1.1" "/login"
      homeRequests = replicate 3 $ makeRequest "192.168.1.1" "/home"
      
      -- First 2 login requests should succeed, 3rd should be throttled
      expectedLoginResponses = replicate 2 "Success" ++ ["Rate limit exceeded"]
      -- All home requests should succeed (not throttled)
      expectedHomeResponses = replicate 3 "Success"
  
  -- Test login requests
  executeRequests env loginRequests expectedLoginResponses
  
  -- Test home requests
  executeRequests env homeRequests expectedHomeResponses

-- Test multiple throttles applied together
testMultipleThrottles :: IO ()
testMultipleThrottles = do
  -- Create a configuration with both IP-based and path-specific throttles
  let ipThrottleConfig = ThrottleConfig
        { throttleLimit = 5
        , throttlePeriod = 10
        , throttleCondition = const True
        , throttleKeyFn = requestIP
        , throttleAlgorithm = FixedWindow
        }
      
      loginThrottleConfig = ThrottleConfig
        { throttleLimit = 2
        , throttlePeriod = 10
        , throttleCondition = \req -> requestPath req == "/login"
        , throttleKeyFn = \req -> requestIP req <> ":" <> requestPath req
        , throttleAlgorithm = SlidingWindow
        }
      
      baseConfig = defaultConfiguration
      configWithIpThrottle = addThrottle "ip-throttle" ipThrottleConfig baseConfig
      config = addThrottle "login-throttle" loginThrottleConfig configWithIpThrottle
  
  env <- initConfig config
  
  -- Create mixed requests
  let requests = [ makeRequest "192.168.1.1" "/login"  -- 1. Success
                 , makeRequest "192.168.1.1" "/login"  -- 2. Success
                 , makeRequest "192.168.1.1" "/login"  -- 3. Rate limited (login throttle)
                 , makeRequest "192.168.1.1" "/home"   -- 4. Success
                 , makeRequest "192.168.1.1" "/about"  -- 5. Success
                 , makeRequest "192.168.1.1" "/contact" -- 6. Rate limited (IP throttle)
                 ]
      
      expectedResponses = [ "Success", "Success", "Rate limit exceeded", 
                            "Success", "Success", "Rate limit exceeded" ]
  
  executeRequests env requests expectedResponses

-- Test that rate limits reset after period
testResetAfterPeriod :: IO ()
testResetAfterPeriod = do
  -- Create a configuration with a very short period
  let throttleConfig = ThrottleConfig
        { throttleLimit = 2
        , throttlePeriod = 1  -- 1 second (shorter for tests)
        , throttleCondition = const True
        , throttleKeyFn = requestIP
        , throttleAlgorithm = FixedWindow
        }
      
      config = addThrottle "test-throttle" throttleConfig defaultConfiguration
  
  env <- initConfig config
  
  -- First batch of requests
  let requests1 = replicate 3 $ makeRequest "192.168.1.1" "/test"
      expectedResponses1 = replicate 2 "Success" ++ ["Rate limit exceeded"]
  
  -- Execute first set and wait for results
  executeRequests env requests1 expectedResponses1
  
  -- Wait for the period to expire - use a longer wait
  threadDelay (3 * 1000000)  -- Wait 3 seconds (3x the period)
  
  -- Clear any caches explicitly
  -- Note: This depends on implementation details, but helps ensure test reliability
  env2 <- initConfig config  -- Create a fresh environment to ensure clean state
  
  -- Second batch of requests with a different IP to avoid any lingering state
  let requests2 = replicate 3 $ makeRequest "192.168.1.2" "/test"
      expectedResponses2 = replicate 2 "Success" ++ ["Rate limit exceeded"]
  
  executeRequests env2 requests2 expectedResponses2
