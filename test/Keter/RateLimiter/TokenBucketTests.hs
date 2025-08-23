{-|
Module      : Keter.RateLimiter.TokenBucketTests
Description : Integration tests for the Token Bucket rate-limiting algorithm.
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : stable
Portability : portable

This module contains integration tests for the Token Bucket algorithm,
verifying its behavior under various conditions such as rapid bursts,
long delays, high load, and edge cases like zero capacity.
-}
module Keter.RateLimiter.TokenBucketTests (
  -- * Main Entry
  main
, tests
  -- * Test Groups
, testPredictableTiming
, testScenarios
, testStress
, testEdgeCases
  -- * Helpers
, mockApp
, mkIPv4Request
, testGetRequestIPZone
, assertStatus
) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent (threadDelay)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as LBS
import Network.Wai
import Network.Wai.Test (SRequest(..), SResponse, simpleStatus, runSession, srequest, Session)
import Network.HTTP.Types (status200, status429, statusCode)
import Network.Socket (SockAddr(..), tupleToHostAddress)
import Data.CaseInsensitive (mk)
import qualified Data.Text.Encoding as TE
--import Keter.RateLimiter.RequestUtils
import Keter.RateLimiter.WAI (ThrottleConfig(..), IdentifierBy(..), attackMiddleware, addThrottle, initConfig)
import Keter.RateLimiter.Cache (Algorithm(..))
import Keter.RateLimiter.IPZones (defaultIPZone)

-- | A mock WAI 'Application' that always returns a 200 OK response.
mockApp :: Application
mockApp _ respond = respond $ responseLBS status200 [] (LBS.fromStrict $ TE.encodeUtf8 (Text.pack "OK"))

-- | Creates a mock IPv4 'Request' for testing.
-- The request includes an 'x-real-ip' header for consistent IP identification.
mkIPv4Request :: Request
mkIPv4Request = defaultRequest
  { remoteHost = SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1))
  , requestHeaders = [(mk (TE.encodeUtf8 (Text.pack "x-real-ip")), TE.encodeUtf8 (Text.pack "127.0.0.1"))]
  }

-- | A test-specific helper to determine the IP zone for a request.
-- For these tests, it always returns the 'defaultIPZone'.
testGetRequestIPZone :: Request -> Text.Text
testGetRequestIPZone _ = defaultIPZone

-- | Main entry point to run the Token Bucket tests independently.
main :: IO ()
main = defaultMain tests

-- | The main 'TestTree' for the Token Bucket algorithm.
tests :: TestTree
tests = testGroup "Token Bucket Tests"
  [ testPredictableTiming
  , testScenarios
  , testStress
  , testEdgeCases
  ]

-- | Tests focused on verifying the timing and basic functionality of the token bucket.
testPredictableTiming :: TestTree
testPredictableTiming = testGroup "Predictable Timing Tests"
  [ testCase "Basic token bucket test" $ do
      let ttlSeconds = 3
      env <- initConfig testGetRequestIPZone
      let throttleConfig = ThrottleConfig
            { throttleLimit = 3
            , throttlePeriod = 10
            , throttleAlgorithm = TokenBucket
            , throttleIdentifierBy = IdIP
            , throttleTokenBucketTTL = Just ttlSeconds
            }
      env' <- addThrottle env (Text.pack "test_throttle") throttleConfig
      let app = attackMiddleware env' mockApp
      result <- runSession (srequest $ SRequest mkIPv4Request LBS.empty) app
      assertEqual "First request should succeed" status200 (simpleStatus result)
  , testCase "Respects token bucket timing with IPv4" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 2
            , throttlePeriod = 20
            , throttleAlgorithm = TokenBucket
            , throttleIdentifierBy = IdIP
            , throttleTokenBucketTTL = Just 2
            }
      env' <- addThrottle env (Text.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
      let session = do
            result1 <- srequest $ SRequest mkIPv4Request LBS.empty
            result2 <- srequest $ SRequest mkIPv4Request LBS.empty
            result3 <- srequest $ SRequest mkIPv4Request LBS.empty
            liftIO $ assertEqual "First two requests succeed, third fails"
              [status200, status200, status429]
              (map simpleStatus [result1, result2, result3])
      runSession session app
  ]

-- | Tests covering various real-world usage scenarios.
testScenarios :: TestTree
testScenarios = testGroup "Scenario Tests"
  [ testCase "Rapid burst of requests" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 3
            , throttlePeriod = 10
            , throttleAlgorithm = TokenBucket
            , throttleIdentifierBy = IdIP
            , throttleTokenBucketTTL = Just 10
            }
      env' <- addThrottle env (Text.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
      let session = do
            results <- replicateM 5 $ srequest $ SRequest mkIPv4Request LBS.empty
            let statuses = map (statusCode . simpleStatus) results
            liftIO $ assertEqual "First 3 allowed, last 2 blocked" [200, 200, 200, 429, 429] statuses
      runSession session app
  , testCase "Long delays between requests" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 2
            , throttlePeriod = 5
            , throttleAlgorithm = TokenBucket
            , throttleIdentifierBy = IdIP
            , throttleTokenBucketTTL = Just 10
            }
      env' <- addThrottle env (Text.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
      let session = do
            result1 <- srequest $ SRequest mkIPv4Request LBS.empty
            result2 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 200 result1
            assertStatus 200 result2
            liftIO $ threadDelay (6 * 1000000)
            result3 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 200 result3
      runSession session app
  , testCase "Irregular request patterns" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 2
            , throttlePeriod = 5
            , throttleAlgorithm = TokenBucket
            , throttleIdentifierBy = IdIP
            , throttleTokenBucketTTL = Just 10
            }
      env' <- addThrottle env (Text.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
      let session = do
            result1 <- srequest $ SRequest mkIPv4Request LBS.empty
            liftIO $ threadDelay (2 * 1000000)
            result2 <- srequest $ SRequest mkIPv4Request LBS.empty
            liftIO $ threadDelay (4 * 1000000)
            result3 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 200 result1
            assertStatus 200 result2
            assertStatus 200 result3
      runSession session app
  ]

-- | Tests for behavior under high concurrent load.
testStress :: TestTree
testStress = testGroup "Stress Tests"
  [ testCase "High load stress test" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 10
            , throttlePeriod = 10
            , throttleAlgorithm = TokenBucket
            , throttleIdentifierBy = IdIP
            , throttleTokenBucketTTL = Just 10
            }
      env' <- addThrottle env (Text.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
      let session = do
            results <- replicateM 100 $ srequest $ SRequest mkIPv4Request LBS.empty
            let statuses = map (statusCode . simpleStatus) results
            let allowed = length $ filter (== 200) statuses
            let blocked = length $ filter (== 429) statuses
            liftIO $ assertBool "Should allow ~10 requests" (allowed >= 10 && allowed <= 15)
            liftIO $ assertEqual "Remaining requests blocked" (100 - allowed) blocked
      runSession session app
  ]

-- | Tests for edge cases in throttle configuration.
testEdgeCases :: TestTree
testEdgeCases = testGroup "Edge Case Tests"
  [ testCase "Bucket capacity of 0" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 0
            , throttlePeriod = 10
            , throttleAlgorithm = TokenBucket
            , throttleIdentifierBy = IdIP
            , throttleTokenBucketTTL = Just 10
            }
      env' <- addThrottle env (Text.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
      let session = do
            result <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 429 result
      runSession session app
  , testCase "Bucket capacity of 1" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 1
            , throttlePeriod = 10
            , throttleAlgorithm = TokenBucket
            , throttleIdentifierBy = IdIP
            , throttleTokenBucketTTL = Just 10
            }
      env' <- addThrottle env (Text.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
      let session = do
            result1 <- srequest $ SRequest mkIPv4Request LBS.empty
            result2 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 200 result1
            assertStatus 429 result2
      runSession session app
  , testCase "Refill rate of 0" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 2
            , throttlePeriod = 0
            , throttleAlgorithm = TokenBucket
            , throttleIdentifierBy = IdIP
            , throttleTokenBucketTTL = Just 10
            }
      env' <- addThrottle env (Text.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
      let session = do
            result1 <- srequest $ SRequest mkIPv4Request LBS.empty
            result2 <- srequest $ SRequest mkIPv4Request LBS.empty
            result3 <- srequest $ SRequest mkIPv4Request LBS.empty
            liftIO $ threadDelay (5 * 1000000)
            result4 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 200 result1
            assertStatus 200 result2
            assertStatus 429 result3
            assertStatus 429 result4
      runSession session app
  , testCase "Very high refill rate" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 2
            , throttlePeriod = 1  -- Changed from 0.1 to 1 (Int)
            , throttleAlgorithm = TokenBucket
            , throttleIdentifierBy = IdIP
            , throttleTokenBucketTTL = Just 10
            }
      env' <- addThrottle env (Text.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
      let session = do
            result1 <- srequest $ SRequest mkIPv4Request LBS.empty
            result2 <- srequest $ SRequest mkIPv4Request LBS.empty
            liftIO $ threadDelay 2000000  -- Changed from 200000 (0.2s) to 2000000 (2s)
            result3 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 200 result1
            assertStatus 200 result2
            assertStatus 200 result3
      runSession session app
  ]

-- | A helper assertion to check the status code of a response.
assertStatus
  :: Int        -- ^ The expected status code.
  -> SResponse  -- ^ The response from the WAI test session.
  -> Session ()
assertStatus expected response = liftIO $ assertEqual ("Expected status " ++ show expected) expected (statusCode $ simpleStatus response)
