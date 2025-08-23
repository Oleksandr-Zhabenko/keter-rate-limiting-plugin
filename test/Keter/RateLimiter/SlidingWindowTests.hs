{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Keter.RateLimiter.SlidingWindowTests
Description : Tests for the Sliding Window rate-limiting algorithm.
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : experimental
Portability : POSIX

This module provides a suite of tests for the Sliding Window rate-limiting algorithm,
focusing on its integration with the WAI middleware. The Sliding Window algorithm offers
a more accurate rate limit over time compared to a Fixed Window by considering a rolling time
window.

The tests cover the following key areas:
  * **Basic Functionality**: Verifies that requests are allowed when under the limit and blocked when the limit is exceeded for both IPv4 and IPv6 clients.
  * **Proxy Header Support**: Ensures that the client's IP is correctly identified when using headers like @x-forwarded-for@ and @x-real-ip@.
  * **Concurrency**: Tests the algorithm's robustness under both moderate and high levels of concurrent requests to prevent race conditions and ensure stability.
  * **Timing Accuracy**: Includes a deterministic test using a fake clock to precisely simulate the sliding window's behavior over time, ensuring that expired requests are correctly discarded from the window.
-}
module Keter.RateLimiter.SlidingWindowTests (
    -- * Test Suite
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import Control.Monad (replicateM_)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (newMVar, modifyMVar_, withMVar)
import Network.Wai (Request, Application, defaultRequest, requestHeaders, responseLBS, remoteHost)
import Network.Wai.Test (runSession, srequest, SRequest(..), assertStatus, simpleStatus)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Status (statusCode)
import Network.Socket (SockAddr(..), tupleToHostAddress)
import Keter.RateLimiter.Cache
import Keter.RateLimiter.WAI
import Keter.RateLimiter.SlidingWindow (allowRequest)
import Keter.RateLimiter.IPZones (defaultIPZone)
import Data.CaseInsensitive (mk)
import Data.IORef (newIORef, modifyIORef', readIORef)

-- * Test Helpers

-- | Creates a new in-memory store specifically for the 'SlidingWindow' algorithm.
newCacheStore :: IO (InMemoryStore 'SlidingWindow)
newCacheStore = createStore @'SlidingWindow

-- | Initializes a new 'Cache' instance with an in-memory store for testing purposes.
initTestCache :: IO (Cache (InMemoryStore 'SlidingWindow))
initTestCache = do
  store <- newCacheStore
  pure $ newCache SlidingWindow store

-- | Creates a mock 'Request' with a default IPv4 address (127.0.0.1).
mkIPv4Request :: Request
mkIPv4Request = defaultRequest { remoteHost = SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)) } -- 127.0.0.1

-- | Creates a mock 'Request' with a default IPv6 address (::1).
mkIPv6Request :: Request
mkIPv6Request = defaultRequest { remoteHost = SockAddrInet6 0 0 (0, 0, 0, 1) 0 } -- ::1

-- | Creates a mock 'Request' containing an @x-forwarded-for@ header.
mkRequestWithXFF :: Text -> Request
mkRequestWithXFF ip = defaultRequest { requestHeaders = [(mk "x-forwarded-for", TE.encodeUtf8 ip)] }

-- | Creates a mock 'Request' containing an @x-real-ip@ header.
mkRequestWithRealIP :: Text -> Request
mkRequestWithRealIP ip = defaultRequest { requestHeaders = [(mk "x-real-ip", TE.encodeUtf8 ip)] }

-- | A mock WAI 'Application' that always returns a 200 OK response.
mockApp :: Application
mockApp _ respond = respond $ responseLBS status200 [] (LBS.fromStrict $ TE.encodeUtf8 "OK")


-- * Test Suite Definition

-- | The main test tree for the Sliding Window algorithm.
-- This groups all the WAI integration tests for the algorithm.
tests :: TestTree
tests = testGroup "Sliding Window Tests"
  [ testCase "Allows IPv4 requests below limit" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 2
            , throttlePeriod = 60
            , throttleAlgorithm = SlidingWindow
            , throttleIdentifierBy = IdIP
            , throttleTokenBucketTTL = Nothing
            }
      env' <- addThrottle env (T.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
          session = do
            result1 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 200 result1
            result2 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 200 result2
      runSession session app

  , testCase "Blocks IPv4 requests exceeding limit" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 2
            , throttlePeriod = 60
            , throttleAlgorithm = SlidingWindow
            , throttleIdentifierBy = IdIP
            , throttleTokenBucketTTL = Nothing
            }
      env' <- addThrottle env (T.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
          session = do
            result1 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 200 result1
            result2 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 200 result2
            result3 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 429 result3
      runSession session app

  , testCase "Allows IPv6 requests below limit" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 2
            , throttlePeriod = 60
            , throttleAlgorithm = SlidingWindow
            , throttleIdentifierBy = IdIP
            , throttleTokenBucketTTL = Nothing
            }
      env' <- addThrottle env (T.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
          session = do
            result1 <- srequest $ SRequest mkIPv6Request LBS.empty
            assertStatus 200 result1
            result2 <- srequest $ SRequest mkIPv6Request LBS.empty
            assertStatus 200 result2
      runSession session app

  , testCase "Blocks IPv6 requests exceeding limit" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 2
            , throttlePeriod = 60
            , throttleAlgorithm = SlidingWindow
            , throttleIdentifierBy = IdIP
            , throttleTokenBucketTTL = Nothing
            }
      env' <- addThrottle env (T.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
          session = do
            result1 <- srequest $ SRequest mkIPv6Request LBS.empty
            assertStatus 200 result1
            result2 <- srequest $ SRequest mkIPv6Request LBS.empty
            assertStatus 200 result2
            result3 <- srequest $ SRequest mkIPv6Request LBS.empty
            assertStatus 429 result3
      runSession session app

  , testCase "Handles x-forwarded-for header for IPv4" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 2
            , throttlePeriod = 60
            , throttleAlgorithm = SlidingWindow
            , throttleIdentifierBy = IdIP
            , throttleTokenBucketTTL = Nothing
            }
      env' <- addThrottle env (T.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
          req = mkRequestWithXFF (T.pack "192.168.1.1")
          session = do
            result1 <- srequest $ SRequest req LBS.empty
            assertStatus 200 result1
            result2 <- srequest $ SRequest req LBS.empty
            assertStatus 200 result2
            result3 <- srequest $ SRequest req LBS.empty
            assertStatus 429 result3
      runSession session app

  , testCase "Handles x-real-ip header for IPv6" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 2
            , throttlePeriod = 60
            , throttleAlgorithm = SlidingWindow
            , throttleIdentifierBy = IdIP
            , throttleTokenBucketTTL = Nothing
            }
      env' <- addThrottle env (T.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
          req = mkRequestWithRealIP (T.pack "2001:db8::1")
          session = do
            result1 <- srequest $ SRequest req LBS.empty
            assertStatus 200 result1
            result2 <- srequest $ SRequest req LBS.empty
            assertStatus 200 result2
            result3 <- srequest $ SRequest req LBS.empty
            assertStatus 429 result3
      runSession session app

  , testCase "Handles concurrent requests" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 2
            , throttlePeriod = 60
            , throttleAlgorithm = SlidingWindow
            , throttleIdentifierBy = IdIP
            , throttleTokenBucketTTL = Nothing
            }
      env' <- addThrottle env (T.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
      mvar <- newMVar []
      let runRequest = do
            result <- srequest $ SRequest mkIPv4Request LBS.empty
            return $ statusCode $ simpleStatus result
      -- Fork three concurrent requests.
      replicateM_ 3 $ forkIO $ do
        status <- runSession runRequest app
        modifyMVar_ mvar $ \results -> return (status : results)
      threadDelay (1 * 1000000) -- Ensure threads complete.
      results <- withMVar mvar return
      let successes = length $ filter (== 200) results
          failures  = length $ filter (== 429) results
      assertEqual "Exactly two requests should succeed" 2 successes
      assertEqual "One request should fail" 1 failures

  , testCase "Handles DoS-like concurrency" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 10
            , throttlePeriod = 60
            , throttleAlgorithm = SlidingWindow
            , throttleIdentifierBy = IdIP
            , throttleTokenBucketTTL = Nothing
            }
      env' <- addThrottle env (T.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
      mvar <- newMVar []
      let runRequest = do
            result <- srequest $ SRequest mkIPv4Request LBS.empty
            return $ statusCode $ simpleStatus result
      -- Fork 100 concurrent requests.
      replicateM_ 100 $ forkIO $ do
        status <- runSession runRequest app
        modifyMVar_ mvar $ \results -> return (status : results)
      threadDelay (3 * 1000000)
      results <- withMVar mvar return
      let successes = length $ filter (== 200) results
          failures  = length $ filter (== 429) results
      -- With high concurrency, the exact number can fluctuate.
      -- We assert that the number of successes is at most the limit.
      assertBool "Number of successful requests should not exceed the limit" (successes <= 10)
      assertEqual "Total requests should be accounted for" 100 (successes + failures)

  , testCase "Respects sliding window timing with fake clock for IPv4" $ do
      -- Set up a fake time source using IORef for deterministic testing.
      fakeTimeRef <- newIORef 100.0  -- Start fake time at 100.0 seconds.
      let getFakeTime = readIORef fakeTimeRef
          advanceTime dt = modifyIORef' fakeTimeRef (+ dt)
          ipZone   = "ipv4"
          userKey  = "user-ipv4"
          window   = 2  -- Sliding window of 2 seconds.
          limit    = 2    -- Allow two requests within the window.

      cacheInstance <- initTestCache
      -- Extract the underlying STM store from the cache.
      let TimestampStore tvar = cacheStore cacheInstance

      -- First two calls should be allowed.
      allowed1 <- allowRequest getFakeTime tvar (T.pack "test_throttle") (T.pack ipZone) (T.pack userKey) window limit
      allowed2 <- allowRequest getFakeTime tvar (T.pack "test_throttle") (T.pack ipZone) (T.pack userKey) window limit
      -- Third call should be blocked as the window is full.
      allowed3 <- allowRequest getFakeTime tvar (T.pack "test_throttle") (T.pack ipZone) (T.pack userKey) window limit

      assertBool "First request should be allowed" allowed1
      assertBool "Second request should be allowed" allowed2
      assertBool "Third request should be blocked" (not allowed3)

      -- Advance fake time by 3 seconds, so the first two requests fall out of the window.
      advanceTime 3.0

      -- This new request should now be allowed.
      allowed4 <- allowRequest getFakeTime tvar (T.pack "test_throttle") (T.pack ipZone) (T.pack userKey) window limit
      assertBool "After advancing time, a new request should be allowed" allowed4
  ]
