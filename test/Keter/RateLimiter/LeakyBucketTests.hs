{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

{-|
Module      : Keter.RateLimiter.LeakyBucketTests
Description : Tests for the Leaky Bucket rate-limiting algorithm.
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : stable
Portability : portable

This module provides integration and unit tests for the Leaky Bucket algorithm.
The Leaky Bucket algorithm is useful for smoothing out bursts of requests and maintaining a steady outflow rate.

It verifies the correctness of the implementation across several dimensions:
  * **IP Version Handling**: Ensures both IPv4 and IPv6 are handled correctly.
  * **Request Headers**: Tests identification of clients via @x-forwarded-for@ and @x-real-ip@ headers.
  * **Concurrency**: Checks for correct behavior under concurrent access.
  * **Timing and Capacity**: Verifies that the bucket "leaks" at the correct rate and respects its capacity.
  * **Direct Function Calls**: Includes tests that call the core algorithm functions directly for unit testing.
-}
module Keter.RateLimiter.LeakyBucketTests (
  -- * Test Suite
  tests,
  
  -- * Test Helpers
  mockApp,
  mkIPv4Request,
  mkIPv6Request,
  mkRequestWithXFF,
  mkRequestWithRealIP
) where

import Test.Tasty
import Test.Tasty.HUnit
--import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
--import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (newMVar, modifyMVar_, withMVar)
import Network.Wai (Request, Application, defaultRequest, requestHeaders, responseLBS, remoteHost)
import Network.Wai.Test (runSession, srequest, SRequest(..), assertStatus, simpleStatus)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Status (statusCode)
import Network.Socket (SockAddr(..), tupleToHostAddress)
import Keter.RateLimiter.Cache
import Keter.RateLimiter.RequestUtils
import Keter.RateLimiter.WAI
import Keter.RateLimiter.IPZones
--import Keter.RateLimiter.Types (LeakyBucketState(..))
import Keter.RateLimiter.LeakyBucket (allowRequest)
import Control.Monad.IO.Class (liftIO)
import Data.CaseInsensitive (mk)
import qualified StmContainers.Map as StmMap
import Control.Concurrent.STM
--import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Function (fix)

-- * Mock Request Generation

-- | Creates a mock IPv4 'Request' for testing purposes.
-- The address is set to @127.0.0.1@.
mkIPv4Request :: Request
mkIPv4Request = defaultRequest { remoteHost = SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)) } -- 127.0.0.1

-- | Creates a mock IPv6 'Request' for testing purposes.
-- The address is set to @::1@.
mkIPv6Request :: Request
mkIPv6Request = defaultRequest { remoteHost = SockAddrInet6 0 0 (0, 0, 0, 1) 0 } -- ::1

-- | Creates a mock 'Request' with an @x-forwarded-for@ header.
-- This is essential for testing applications behind a reverse proxy.
--
-- ==== __Example__
-- > let req = mkRequestWithXFF "198.51.100.1"
mkRequestWithXFF
  :: Text -- ^ The IP address to use in the header.
  -> Request
mkRequestWithXFF ip = defaultRequest { requestHeaders = [(mk "x-forwarded-for", TE.encodeUtf8 ip)] }

-- | Creates a mock 'Request' with an @x-real-ip@ header.
-- Similar to @x-forwarded-for@, this is used for client IP identification.
--
-- ==== __Example__
-- > let req = mkRequestWithRealIP "2001:db8::a"
mkRequestWithRealIP
  :: Text -- ^ The IP address to use in the header.
  -> Request
mkRequestWithRealIP ip = defaultRequest { requestHeaders = [(mk "x-real-ip", TE.encodeUtf8 ip)] }

-- * Mock Application

-- | A mock WAI 'Application' that consistently returns a 200 OK response.
-- This allows tests to focus solely on the behavior of the rate-limiting middleware.
mockApp :: Application
mockApp _ respond = respond $ responseLBS status200 [] (LBS.fromStrict $ TE.encodeUtf8 "OK")

-- * Test Suite Definition

-- | The main test tree for the Leaky Bucket algorithm.
-- This 'TestTree' aggregates all the test cases defined in this module.
tests :: TestTree
tests = testGroup "Leaky Bucket Tests"
  [ testCase "Allows IPv4 requests below capacity" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 2
            , throttlePeriod = 60
            , throttleAlgorithm = LeakyBucket
            , throttleIdentifier = byIP
            , throttleTokenBucketTTL = Nothing
            }
      env' <- addThrottle env (T.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
      let session = do
            result1 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 200 result1
            result2 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 200 result2
      runSession session app

  , testCase "Blocks IPv4 requests exceeding capacity" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 2
            , throttlePeriod = 60
            , throttleAlgorithm = LeakyBucket
            , throttleIdentifier = byIP
            , throttleTokenBucketTTL = Nothing
            }
      env' <- addThrottle env (T.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
      let session = do
            result1 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 200 result1
            result2 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 200 result2
            result3 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 429 result3
      runSession session app

  , testCase "Allows IPv6 requests below capacity" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 2
            , throttlePeriod = 60
            , throttleAlgorithm = LeakyBucket
            , throttleIdentifier = byIP
            , throttleTokenBucketTTL = Nothing
            }
      env' <- addThrottle env (T.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
      let session = do
            result1 <- srequest $ SRequest mkIPv6Request LBS.empty
            assertStatus 200 result1
            result2 <- srequest $ SRequest mkIPv6Request LBS.empty
            assertStatus 200 result2
      runSession session app

  , testCase "Blocks IPv6 requests exceeding capacity" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 2
            , throttlePeriod = 60
            , throttleAlgorithm = LeakyBucket
            , throttleIdentifier = byIP
            , throttleTokenBucketTTL = Nothing
            }
      env' <- addThrottle env (T.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
      let session = do
            result1 <- srequest $ SRequest mkIPv6Request LBS.empty
            assertStatus 200 result1
            result2 <- srequest $ SRequest mkIPv6Request LBS.empty
            assertStatus 200 result2
            result3 <- srequest $ SRequest mkIPv6Request LBS.empty
            assertStatus 429 result3
      runSession session app

  , testCase "Respects leaky bucket timing with IPv4" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 2  -- capacity
            , throttlePeriod = 2 -- time to leak one request (leak rate is capacity / period)
            , throttleAlgorithm = LeakyBucket
            , throttleIdentifier = byIP
            , throttleTokenBucketTTL = Nothing
            }
      env' <- addThrottle env (T.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
      let session = do
            result1 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 200 result1
            result2 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 200 result2
            result3 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 429 result3
            -- Wait for the bucket to leak, allowing new requests
            liftIO $ threadDelay (3 * 1000000) -- Wait 3 seconds
            -- This request should now be allowed
            result4 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 200 result4
      runSession session app

  , testCase "Handles x-forwarded-for header for IPv4" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 2
            , throttlePeriod = 60
            , throttleAlgorithm = LeakyBucket
            , throttleIdentifier = byIP
            , throttleTokenBucketTTL = Nothing
            }
      env' <- addThrottle env (T.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
      let req = mkRequestWithXFF (T.pack "192.168.1.1")
      let session = do
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
            , throttleAlgorithm = LeakyBucket
            , throttleIdentifier = byIP
            , throttleTokenBucketTTL = Nothing
            }
      env' <- addThrottle env (T.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
      let req = mkRequestWithRealIP (T.pack "2001:db8::1")
      let session = do
            _ <- srequest $ SRequest req LBS.empty
            _ <- srequest $ SRequest req LBS.empty
            result3 <- srequest $ SRequest req LBS.empty
            assertStatus 429 result3
      runSession session app
  , testCase "Handles concurrent requests" $ do
      env <- initConfig (const defaultIPZone)
      cacheResetAll env -- Reset all caches
      let throttle = ThrottleConfig
            { throttleLimit = 2
            , throttlePeriod = 60
            , throttleAlgorithm = LeakyBucket
            , throttleIdentifier = byIP
            , throttleTokenBucketTTL = Nothing
            }
      env' <- addThrottle env (T.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
      mvar <- newMVar []
      barrier <- newMVar 0
      let runRequest = do
            result <- srequest $ SRequest mkIPv4Request LBS.empty
            liftIO $ modifyMVar_ barrier $ \c -> return (c + 1)
            return $ statusCode $ simpleStatus result
      -- Fork three concurrent requests
      _ <- forkIO $ do
        status <- runSession runRequest app
        modifyMVar_ mvar $ \results -> return (status : results)
      _ <- forkIO $ do
        status <- runSession runRequest app
        modifyMVar_ mvar $ \results -> return (status : results)
      _ <- forkIO $ do
        status <- runSession runRequest app
        modifyMVar_ mvar $ \results -> return (status : results)
      -- Wait for all threads to complete
      fix $ \loop -> do
        count <- withMVar barrier return
        if count == 3 then return () else threadDelay 100000 >> loop
      results <- withMVar mvar return
      let successes = length $ filter (== 200) results
          failures = length $ filter (== 429) results
      assertEqual "Exactly two requests should succeed" 2 successes
      assertEqual "One request should fail" 1 failures
  , testCase "Direct allowRequest allows request below capacity" $ do
      leakyBucketMap <- atomically StmMap.new
      -- Corrected: LeakyBucketStore expects a TVar, not directly the StmMap
      leakyBucketTVar <- newTVarIO leakyBucketMap
      let cache = newCache LeakyBucket (LeakyBucketStore leakyBucketTVar)
      -- Start a purge thread for the LeakyBucketStore
      -- Removed the extra predicate argument, as startCustomPurgeLeakyBucket doesn't take it.
      _ <- startCustomPurgeLeakyBucket leakyBucketMap 60 7200
      let ipZone = defaultIPZone
          userKey = "test_user"
          capacity = 2
          leakRate = 1
      allowed1 <- allowRequest cache ipZone userKey capacity leakRate
      assertBool "First request should be allowed" allowed1
      allowed2 <- allowRequest cache ipZone userKey capacity leakRate
      assertBool "Second request should be allowed" allowed2
  , testCase "Direct allowRequest denies request at capacity" $ do
      leakyBucketMap <- atomically StmMap.new
      -- Corrected: LeakyBucketStore expects a TVar, not directly the StmMap
      leakyBucketTVar <- newTVarIO leakyBucketMap
      let cache = newCache LeakyBucket (LeakyBucketStore leakyBucketTVar)
      -- Start a purge thread for the LeakyBucketStore
      -- Removed the extra predicate argument, as startCustomPurgeLeakyBucket doesn't take it.
      _ <- startCustomPurgeLeakyBucket leakyBucketMap 60 7200
      let ipZone = defaultIPZone
          userKey = "test_user"
          capacity = 2
          leakRate = 1
      allowed1 <- allowRequest cache ipZone userKey capacity leakRate
      assertBool "First request should be allowed" allowed1
      allowed2 <- allowRequest cache ipZone userKey capacity leakRate
      assertBool "Second request should be allowed" allowed2
      allowed3 <- allowRequest cache ipZone userKey capacity leakRate
      assertBool "Third request should be denied" (not allowed3)
  ]
