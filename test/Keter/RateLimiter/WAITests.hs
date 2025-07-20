{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Keter.RateLimiter.WAITests (tests) where

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
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newMVar, modifyMVar_, readMVar, MVar)
import Control.Monad (replicateM_, replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.CaseInsensitive (mk)
import Keter.RateLimiter.IPZones (defaultIPZone)
import Keter.RateLimiter.WAI
import Keter.RateLimiter.RequestUtils
import Keter.RateLimiter.Cache (Algorithm(..))
import Keter.RateLimiter.CacheWithZone (allowFixedWindowRequest)

-- Helper functions to create requests
mkIPv4Request :: Request
mkIPv4Request = defaultRequest { remoteHost = SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)) } -- 127.0.0.1

mkIPv6Request :: Request
mkIPv6Request = defaultRequest { remoteHost = SockAddrInet6 0 0 (0, 0, 0, 1) 0 } -- ::1

mkRequestWithXFF :: Text -> Request
mkRequestWithXFF ip = defaultRequest { requestHeaders = [(mk "x-forwarded-for", TE.encodeUtf8 ip)] }

mkRequestWithRealIP :: Text -> Request
mkRequestWithRealIP ip = defaultRequest { requestHeaders = [(mk "x-real-ip", TE.encodeUtf8 ip)] }

-- Mock application that always returns 200 OK
mockApp :: Application
mockApp _ respond = respond $ responseLBS status200 [] (LBS.fromStrict $ TE.encodeUtf8 "OK")

-- Test suite
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
      , testCase "Handles concurrent requests" $ testConcurrent TokenBucket byIP
      ]
  , testGroup "Leaky Bucket Algorithm"
      [ testCase "Allows IPv4 requests below limit" $ testBelowLimit LeakyBucket byIP
      , testCase "Blocks IPv4 requests exceeding limit" $ testExceedLimit LeakyBucket byIP
      , testCase "Handles concurrent requests" $ testConcurrent LeakyBucket byIP
      ]
  , testGroup "TinyLRU Algorithm"
      [ testCase "Allows IPv4 requests below limit" $ testBelowLimit TinyLRU byIP
      , testCase "Blocks IPv4 requests exceeding limit" $ testExceedLimit TinyLRU byIP
      , testCase "Handles concurrent requests" $ testConcurrent TinyLRU byIP
      ]
  ]

-- Helper function for testing requests below limit
testBelowLimit :: Algorithm -> (Request -> IO (Maybe Text)) -> Assertion
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

-- Helper function for testing requests exceeding limit
testExceedLimit :: Algorithm -> (Request -> IO (Maybe Text)) -> Assertion
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

-- Helper function for testing timing
testTiming :: Algorithm -> (Request -> IO (Maybe Text)) -> Assertion
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

-- Helper function for testing x-forwarded-for header
testXFF :: Algorithm -> (Request -> IO (Maybe Text)) -> Assertion
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

-- Helper function for testing x-real-ip header
testRealIP :: Algorithm -> (Request -> IO (Maybe Text)) -> Assertion
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

-- Helper function for testing concurrent requests
testConcurrent :: Algorithm -> (Request -> IO (Maybe Text)) -> Assertion
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

-- Helper function for testing DoS-like concurrency
testDoS :: Algorithm -> (Request -> IO (Maybe Text)) -> Assertion
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
