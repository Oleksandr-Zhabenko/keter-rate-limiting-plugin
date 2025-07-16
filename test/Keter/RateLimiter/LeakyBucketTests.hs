{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Keter.RateLimiter.LeakyBucketTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (newMVar, modifyMVar_, withMVar, MVar)
import Network.Wai (Request, Application, defaultRequest, requestHeaders, responseLBS, remoteHost)
import Network.Wai.Test (runSession, srequest, SRequest(..), SResponse, assertStatus, simpleStatus)
import Network.HTTP.Types (methodGet, status200, status429)
import Network.HTTP.Types.Status (statusCode)
import Network.Socket (SockAddr(..), tupleToHostAddress)
import Keter.RateLimiter.Cache
import Keter.RateLimiter.RequestUtils
import Keter.RateLimiter.WAI
import Keter.RateLimiter.LeakyBucketState (LeakyBucketState(..))
import Keter.RateLimiter.LeakyBucket (allowRequest)
import Control.Monad.IO.Class (liftIO)
import Data.CaseInsensitive (CI, mk)
import qualified StmContainers.Map as StmMap
import Control.Concurrent.STM
import Data.Time.Clock.POSIX (getPOSIXTime)

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
      let env' = addThrottle env (T.pack "test_throttle") throttle
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
      let env' = addThrottle env (T.pack "test_throttle") throttle
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
      let env' = addThrottle env (T.pack "test_throttle") throttle
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
      let env' = addThrottle env (T.pack "test_throttle") throttle
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
            { throttleLimit = 2
            , throttlePeriod = 2
            , throttleAlgorithm = LeakyBucket
            , throttleIdentifier = byIP
            , throttleTokenBucketTTL = Nothing
            }
      let env' = addThrottle env (T.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
      let session = do
            result1 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 200 result1
            result2 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 200 result2
            result3 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 429 result3
            liftIO $ threadDelay (3 * 1000000) -- Wait for bucket drain
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
      let env' = addThrottle env (T.pack "test_throttle") throttle
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
      let env' = addThrottle env (T.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
      let req = mkRequestWithRealIP (T.pack "2001:db8::1")
      let session = do
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
            , throttleAlgorithm = LeakyBucket
            , throttleIdentifier = byIP
            , throttleTokenBucketTTL = Nothing
            }
      let env' = addThrottle env (T.pack "test_throttle") throttle
      let app = attackMiddleware env' mockApp
      mvar <- newMVar []
      let runRequest = do
            result <- srequest $ SRequest mkIPv4Request LBS.empty
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
      threadDelay (3 * 1000000) -- Increased delay to ensure threads complete
      results <- withMVar mvar return
      let successes = length $ filter (== 200) results
          failures = length $ filter (== 429) results
      assertEqual "Exactly two requests should succeed" 2 successes
      assertEqual "One request should fail" 1 failures
  , testCase "Direct allowRequest allows request below capacity" $ do
      leakyBucketMap <- atomically StmMap.new
      let cache = newCache LeakyBucket (LeakyBucketCacheStore leakyBucketMap)
      -- Start a purge thread for the LeakyBucketCacheStore
      _ <- startCustomPurgeLeakyBucket leakyBucketMap 60 7200
            (\(LeakyBucketState _ lastTime) now -> now - lastTime <= 7200)
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
      let cache = newCache LeakyBucket (LeakyBucketCacheStore leakyBucketMap)
      -- Start a purge thread for the LeakyBucketCacheStore
      _ <- startCustomPurgeLeakyBucket leakyBucketMap 60 7200
            (\(LeakyBucketState _ lastTime) now -> now - lastTime <= 7200)
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
