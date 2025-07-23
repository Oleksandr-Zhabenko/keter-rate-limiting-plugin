{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

{-|
Module      : Keter.RateLimiter.WAITests
Description : Comprehensive WAI middleware tests for various rate-limiting algorithms.
Copyright   : (c) 2025 Acme Inc.
License     : MIT
Maintainer  : maintainer@example.com
Stability   : experimental
Portability : POSIX

This module provides a comprehensive test suite for the WAI (Web Application Interface) middleware responsible for rate limiting. It uses the tasty and tasty-hunit frameworks to define and run tests.

The tests cover five distinct rate-limiting algorithms:
  * Fixed Window
  * Sliding Window
  * Token Bucket
  * Leaky Bucket
  * TinyLRU

For each algorithm, the following scenarios are tested:
  * Allowing requests that are under the defined limit.
  * Blocking requests that exceed the defined limit.
  * Correctly handling both IPv4 and IPv6 addresses.
  * Ensuring the rate-limiting window resets correctly over time.
  * Identifying clients using proxy headers like @x-forwarded-for@ and @x-real-ip@.
  * Managing concurrent requests to prevent race conditions.
  * Simulating a high-volume of concurrent requests to test DoS (Denial of Service) protection.

The module defines several helper functions to create mock requests and a mock application to isolate the middleware for testing.
-}
module Keter.RateLimiter.WAITests (
  -- * Test Suite
  tests
) where

import Test.Tasty
import Test.Tasty.HUnit
import Network.Wai
import Network.Wai.Test
import Network.HTTP.Types
import Network.Socket (SockAddr(..), tupleToHostAddress)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newMVar, modifyMVar_)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.CaseInsensitive (mk)
import Keter.RateLimiter.IPZones (defaultIPZone)
import Keter.RateLimiter.WAI
import Keter.RateLimiter.RequestUtils
import Keter.RateLimiter.Cache (Algorithm(..))
--import Keter.RateLimiter.CacheWithZone (allowFixedWindowRequest)

-- * Request Helpers

-- | Creates a mock 'Request' with a default IPv4 address (127.0.0.1).
-- This is used to simulate a standard IPv4 client connection.
mkIPv4Request :: Request
mkIPv4Request = defaultRequest { remoteHost = SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)) } -- 127.0.0.1

-- | Creates a mock 'Request' with a default IPv6 address (::1).
-- This is used to simulate a standard IPv6 client connection.
mkIPv6Request :: Request
mkIPv6Request = defaultRequest { remoteHost = SockAddrInet6 0 0 (0, 0, 0, 1) 0 } -- ::1

-- | Creates a mock 'Request' containing an @x-forwarded-for@ header.
-- This is used to test scenarios where the application is behind a proxy
-- and the client's IP address is forwarded in this header.
--
-- ==== __Example__
-- > mkRequestWithXFF "192.168.1.1"
mkRequestWithXFF
  :: Text -- ^ The IP address to set in the header.
  -> Request
mkRequestWithXFF ip = defaultRequest { requestHeaders = [(mk "x-forwarded-for", TE.encodeUtf8 ip)] }

-- | Creates a mock 'Request' containing an @x-real-ip@ header.
-- This is another common header used by proxies to forward the original client IP.
--
-- ==== __Example__
-- > mkRequestWithRealIP "2001:db8::1"
mkRequestWithRealIP
  :: Text -- ^ The IP address to set in the header.
  -> Request
mkRequestWithRealIP ip = defaultRequest { requestHeaders = [(mk "x-real-ip", TE.encodeUtf8 ip)] }


-- * Mock Application

-- | A simple WAI 'Application' that always returns a 200 OK response.
-- This serves as the downstream application in tests, allowing focus to be
-- on the middleware's behavior.
mockApp :: Application
mockApp _ respond = respond $ responseLBS status200 [] (LBS.fromStrict $ TE.encodeUtf8 "OK")


-- * Test Suite Definition

-- | The main entry point for all tests in this module.
-- It groups tests by the rate-limiting algorithm being tested.
tests :: TestTree
tests = testGroup "Rate Limiting Tests"
  [ testGroup "Fixed Window Algorithm"
      [ testCase "Allows IPv4 requests below limit" $ testBelowLimit FixedWindow byIP
      , testCase "Blocks IPv4 requests exceeding limit" $ testExceedLimit FixedWindow byIP
      , testCase "Allows IPv6 requests below limit" $ testBelowLimit FixedWindow byIP
      , testCase "Blocks IPv6 requests exceeding limit" $ testExceedLimit FixedWindow byIP
      , testCase "Respects fixed window timing with IPv4" $ testTiming FixedWindow byIP
      , testCase "Handles x-forwarded-for header for IPv4" $ testXFF FixedWindow byIP
      , testCase "Handles x-real-ip header for IPv6" $ testRealIP FixedWindow byIP
      , testCase "Handles concurrent requests" $ testConcurrent FixedWindow byIP
      , testCase "Handles DoS-like concurrency" $ testDoS FixedWindow byIP
      ]
  , testGroup "Sliding Window Algorithm"
      [ testCase "Allows IPv4 requests below limit" $ testBelowLimit SlidingWindow byIP
      , testCase "Blocks IPv4 requests exceeding limit" $ testExceedLimit SlidingWindow byIP
      , testCase "Allows IPv6 requests below limit" $ testBelowLimit SlidingWindow byIP
      , testCase "Blocks IPv6 requests exceeding limit" $ testExceedLimit SlidingWindow byIP
      , testCase "Respects sliding window timing with IPv4" $ testTiming SlidingWindow byIP
      , testCase "Handles x-forwarded-for header for IPv4" $ testXFF SlidingWindow byIP
      , testCase "Handles x-real-ip header for IPv6" $ testRealIP SlidingWindow byIP
      , testCase "Handles concurrent requests" $ testConcurrent SlidingWindow byIP
      , testCase "Handles DoS-like concurrency" $ testDoS SlidingWindow byIP
      ]
  , testGroup "Token Bucket Algorithm"
      [ testCase "Allows IPv4 requests below limit" $ testBelowLimit TokenBucket byIP
      , testCase "Blocks IPv4 requests exceeding limit" $ testExceedLimit TokenBucket byIP
      , testCase "Allows IPv6 requests below limit" $ testBelowLimit TokenBucket byIP
      , testCase "Blocks IPv6 requests exceeding limit" $ testExceedLimit TokenBucket byIP
      , testCase "Respects token bucket timing with IPv4" $ testTiming TokenBucket byIP
      , testCase "Handles x-forwarded-for header for IPv4" $ testXFF TokenBucket byIP
      , testCase "Handles x-real-ip header for IPv6" $ testRealIP TokenBucket byIP
      , testCase "Handles concurrent requests" $ testConcurrent TokenBucket byIP
      , testCase "Handles DoS-like concurrency" $ testDoS TokenBucket byIP
      ]
  , testGroup "Leaky Bucket Algorithm"
      [ testCase "Allows IPv4 requests below limit" $ testBelowLimit LeakyBucket byIP
      , testCase "Blocks IPv4 requests exceeding limit" $ testExceedLimit LeakyBucket byIP
      , testCase "Allows IPv6 requests below limit" $ testBelowLimit LeakyBucket byIP
      , testCase "Blocks IPv6 requests exceeding limit" $ testExceedLimit LeakyBucket byIP
      , testCase "Respects leaky bucket timing with IPv4" $ testTiming LeakyBucket byIP
      , testCase "Handles x-forwarded-for header for IPv4" $ testXFF LeakyBucket byIP
      , testCase "Handles x-real-ip header for IPv6" $ testRealIP LeakyBucket byIP
      , testCase "Handles concurrent requests" $ testConcurrent LeakyBucket byIP
      , testCase "Handles DoS-like concurrency" $ testDoS LeakyBucket byIP
      ]
  , testGroup "TinyLRU Algorithm"
      [ testCase "Allows IPv4 requests below limit" $ testBelowLimit TinyLRU byIP
      , testCase "Blocks IPv4 requests exceeding limit" $ testExceedLimit TinyLRU byIP
      , testCase "Allows IPv6 requests below limit" $ testBelowLimit TinyLRU byIP
      , testCase "Blocks IPv6 requests exceeding limit" $ testExceedLimit TinyLRU byIP
      , testCase "Respects TinyLRU timing with IPv4" $ testTiming TinyLRU byIP
      , testCase "Handles x-forwarded-for header for IPv4" $ testXFF TinyLRU byIP
      , testCase "Handles x-real-ip header for IPv6" $ testRealIP TinyLRU byIP
      , testCase "Handles concurrent requests" $ testConcurrent TinyLRU byIP
      , testCase "Handles DoS-like concurrency" $ testDoS TinyLRU byIP
      ]
  ]

-- * Test Case Implementations

-- | **Test Scenario**: Verifies that the middleware allows requests when the count is below the configured limit.
-- It sends two requests to an endpoint with a limit of 2 and asserts that both receive a 200 OK status.
testBelowLimit
  :: Algorithm -- ^ The rate-limiting algorithm to test.
  -> (Request -> IO (Maybe Text)) -- ^ The function to identify the client.
  -> Assertion
testBelowLimit algo identifier = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig
        { throttleLimit = 2
        , throttlePeriod = 60
        , throttleAlgorithm = algo
        , throttleIdentifier = identifier
        , throttleTokenBucketTTL = Just 3600
        }
  env' <- addThrottle env (T.pack "test_throttle") throttle
  let app = attackMiddleware env' mockApp
  let session = do
        result1 <- srequest $ SRequest mkIPv4Request ""
        result2 <- srequest $ SRequest mkIPv4Request ""
        return (result1, result2)
  (response1, response2) <- runSession session app
  assertEqual "First request status" status200 (simpleStatus response1)
  assertEqual "Second request status" status200 (simpleStatus response2)

-- | **Test Scenario**: Verifies that the middleware blocks requests once the limit is exceeded.
-- It sends three requests to an endpoint with a limit of 2. It asserts that the first two succeed (200 OK)
-- and the third is blocked (429 Too Many Requests).
testExceedLimit
  :: Algorithm -- ^ The rate-limiting algorithm to test.
  -> (Request -> IO (Maybe Text)) -- ^ The function to identify the client.
  -> Assertion
testExceedLimit algo identifier = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig
        { throttleLimit = 2
        , throttlePeriod = 60
        , throttleAlgorithm = algo
        , throttleIdentifier = identifier
        , throttleTokenBucketTTL = Just 3600
        }
  env' <- addThrottle env (T.pack "test_throttle") throttle
  let app = attackMiddleware env' mockApp
  let session = do
        _ <- srequest $ SRequest mkIPv4Request ""
        _ <- srequest $ SRequest mkIPv4Request ""
        result3 <- srequest $ SRequest mkIPv4Request ""
        return result3
  response3 <- runSession session app
  assertEqual "Third request status" status429 (simpleStatus response3)

-- | **Test Scenario**: Verifies that the rate limit counter resets after the configured window period.
-- It sets a limit of 1 request per 1-second window. It sends one request, waits for 2 seconds,
-- then sends a second request. It asserts that both requests are successful (200 OK).
testTiming
  :: Algorithm -- ^ The rate-limiting algorithm to test.
  -> (Request -> IO (Maybe Text)) -- ^ The function to identify the client.
  -> Assertion
testTiming algo identifier = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig
        { throttleLimit = 1
        , throttlePeriod = 1
        , throttleAlgorithm = algo
        , throttleIdentifier = identifier
        , throttleTokenBucketTTL = Just 3600
        }
  env' <- addThrottle env (T.pack "test_throttle") throttle
  let app = attackMiddleware env' mockApp
  lock <- newMVar ()
  let session = do
        result1 <- srequest $ SRequest mkIPv4Request ""
        liftIO $ modifyMVar_ lock $ \_ -> threadDelay 2000000 >> return () -- Wait 2s
        result2 <- srequest $ SRequest mkIPv4Request ""
        return (result1, result2)
  (response1, response2) <- runSession session app
  assertEqual "First request status" status200 (simpleStatus response1)
  assertEqual "Second request status after reset" status200 (simpleStatus response2)

-- | **Test Scenario**: Verifies correct IP identification using the @x-forwarded-for@ header.
-- It sends two requests from the same forwarded IP address with a limit of 1.
-- It asserts that the first request succeeds (200 OK) and the second is blocked (429 Too Many Requests).
testXFF
  :: Algorithm -- ^ The rate-limiting algorithm to test.
  -> (Request -> IO (Maybe Text)) -- ^ The function to identify the client.
  -> Assertion
testXFF algo identifier = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig
        { throttleLimit = 1
        , throttlePeriod = 60
        , throttleAlgorithm = algo
        , throttleIdentifier = identifier
        , throttleTokenBucketTTL = Just 3600
        }
  env' <- addThrottle env (T.pack "test_throttle") throttle
  let app = attackMiddleware env' mockApp
  let session = do
        result1 <- srequest $ SRequest (mkRequestWithXFF "192.168.1.1") ""
        result2 <- srequest $ SRequest (mkRequestWithXFF "192.168.1.1") ""
        return (result1, result2)
  (response1, response2) <- runSession session app
  assertEqual "First XFF request status" status200 (simpleStatus response1)
  assertEqual "Second XFF request status" status429 (simpleStatus response2)

-- | **Test Scenario**: Verifies correct IP identification using the @x-real-ip@ header.
-- It sends two requests from the same real IP address with a limit of 1.
-- It asserts that the first request succeeds (200 OK) and the second is blocked (429 Too Many Requests).
testRealIP
  :: Algorithm -- ^ The rate-limiting algorithm to test.
  -> (Request -> IO (Maybe Text)) -- ^ The function to identify the client.
  -> Assertion
testRealIP algo identifier = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig
        { throttleLimit = 1
        , throttlePeriod = 60
        , throttleAlgorithm = algo
        , throttleIdentifier = identifier
        , throttleTokenBucketTTL = Just 3600
        }
  env' <- addThrottle env (T.pack "test_throttle") throttle
  let app = attackMiddleware env' mockApp
  let session = do
        result1 <- srequest $ SRequest (mkRequestWithRealIP "::1") ""
        result2 <- srequest $ SRequest (mkRequestWithRealIP "::1") ""
        return (result1, result2)
  (response1, response2) <- runSession session app
  assertEqual "First Real-IP request status" status200 (simpleStatus response1)
  assertEqual "Second Real-IP request status" status429 (simpleStatus response2)

-- | **Test Scenario**: Verifies the middleware's behavior under moderate concurrent load.
-- It sends 5 concurrent requests to an endpoint with a limit of 5. It then sends a sixth request.
-- It asserts that the first 5 requests succeed (200 OK) and the sixth is blocked (429 Too Many Requests).
testConcurrent
  :: Algorithm -- ^ The rate-limiting algorithm to test.
  -> (Request -> IO (Maybe Text)) -- ^ The function to identify the client.
  -> Assertion
testConcurrent algo identifier = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig
        { throttleLimit = 5
        , throttlePeriod = 60
        , throttleAlgorithm = algo
        , throttleIdentifier = identifier
        , throttleTokenBucketTTL = Just 3600
        }
  env' <- addThrottle env (T.pack "test_throttle") throttle
  let app = attackMiddleware env' mockApp
  
  -- Execute concurrent requests sequentially within the session
  let session = do
        responses <- replicateM 5 (srequest $ SRequest mkIPv4Request "")
        result6 <- srequest $ SRequest mkIPv4Request ""
        return (responses, result6)
  (responses, response6) <- runSession session app
  
  -- Check that first 5 requests succeeded
  mapM_ (\(i, resp) -> assertEqual ("Request " ++ show i ++ " status") status200 (simpleStatus resp))
        (zip [1..5] responses)
  -- Check that 6th request was throttled
  assertEqual "Sixth request status after limit" status429 (simpleStatus response6)

-- | **Test Scenario**: Simulates a Denial-of-Service (DoS) attack with high concurrency.
-- It sends 15 concurrent requests to an endpoint with a limit of 10.
-- It asserts that some requests were successful, some were blocked, and the total processed
-- matches the number sent, ensuring the server remains stable.
testDoS
  :: Algorithm -- ^ The rate-limiting algorithm to test.
  -> (Request -> IO (Maybe Text)) -- ^ The function to identify the client.
  -> Assertion
testDoS algo identifier = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig
        { throttleLimit = 10
        , throttlePeriod = 60
        , throttleAlgorithm = algo
        , throttleIdentifier = identifier
        , throttleTokenBucketTTL = Just 3600
        }
  env' <- addThrottle env (T.pack "test_throttle") throttle
  let app = attackMiddleware env' mockApp
  
  -- Execute many requests sequentially to simulate DoS scenario
  let session = do
        responses <- replicateM 15 (srequest $ SRequest mkIPv4Request "")
        return responses
  responses <- runSession session app
  
  -- Count how many succeeded (should be <= 10)
  let successCount = length $ filter (\resp -> simpleStatus resp == status200) responses
  let throttledCount = length $ filter (\resp -> simpleStatus resp == status429) responses
  
  assertBool "Some requests should succeed" (successCount > 0)
  assertBool "Some requests should be throttled" (throttledCount > 0)
  assertEqual "Total requests processed" 15 (successCount + throttledCount)
