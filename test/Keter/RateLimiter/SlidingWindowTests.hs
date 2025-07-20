{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Keter.RateLimiter.SlidingWindowTests (tests) where

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
import Keter.RateLimiter.RequestUtils
import Keter.RateLimiter.IPZones (defaultIPZone) -- Add this import
import Control.Monad.IO.Class (liftIO)
import Data.CaseInsensitive (mk)

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
tests = testGroup "Sliding Window Tests"
  [ testCase "Allows IPv4 requests below limit" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 2
            , throttlePeriod = 60
            , throttleAlgorithm = SlidingWindow
            , throttleIdentifier = byIP
            , throttleTokenBucketTTL = Nothing
            }
      env' <- addThrottle env (T.pack "test_throttle") throttle -- Fix: bind the IO action
      let app = attackMiddleware env' mockApp
      let session = do
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
            , throttleIdentifier = byIP
            , throttleTokenBucketTTL = Nothing
            }
      env' <- addThrottle env (T.pack "test_throttle") throttle -- Fix: bind the IO action
      let app = attackMiddleware env' mockApp
      let session = do
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
            , throttleIdentifier = byIP
            , throttleTokenBucketTTL = Nothing
            }
      env' <- addThrottle env (T.pack "test_throttle") throttle -- Fix: bind the IO action
      let app = attackMiddleware env' mockApp
      let session = do
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
            , throttleIdentifier = byIP
            , throttleTokenBucketTTL = Nothing
            }
      env' <- addThrottle env (T.pack "test_throttle") throttle -- Fix: bind the IO action
      let app = attackMiddleware env' mockApp
      let session = do
            result1 <- srequest $ SRequest mkIPv6Request LBS.empty
            assertStatus 200 result1
            result2 <- srequest $ SRequest mkIPv6Request LBS.empty
            assertStatus 200 result2
            result3 <- srequest $ SRequest mkIPv6Request LBS.empty
            assertStatus 429 result3
      runSession session app
  , testCase "Respects sliding window timing with IPv4" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 2
            , throttlePeriod = 2
            , throttleAlgorithm = SlidingWindow
            , throttleIdentifier = byIP
            , throttleTokenBucketTTL = Nothing
            }
      env' <- addThrottle env (T.pack "test_throttle") throttle -- Fix: bind the IO action
      let app = attackMiddleware env' mockApp
      let session = do
            result1 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 200 result1
            result2 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 200 result2
            result3 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 429 result3
            liftIO $ threadDelay (3 * 1000000) -- Wait for window reset
            result4 <- srequest $ SRequest mkIPv4Request LBS.empty
            assertStatus 200 result4
      runSession session app
  , testCase "Handles x-forwarded-for header for IPv4" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 2
            , throttlePeriod = 60
            , throttleAlgorithm = SlidingWindow
            , throttleIdentifier = byIP
            , throttleTokenBucketTTL = Nothing
            }
      env' <- addThrottle env (T.pack "test_throttle") throttle -- Fix: bind the IO action
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
            , throttleAlgorithm = SlidingWindow
            , throttleIdentifier = byIP
            , throttleTokenBucketTTL = Nothing
            }
      env' <- addThrottle env (T.pack "test_throttle") throttle -- Fix: bind the IO action
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
            , throttleAlgorithm = SlidingWindow
            , throttleIdentifier = byIP
            , throttleTokenBucketTTL = Nothing
            }
      env' <- addThrottle env (T.pack "test_throttle") throttle -- Fix: bind the IO action
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
  , testCase "Handles DoS-like concurrency" $ do
      env <- initConfig (const defaultIPZone)
      let throttle = ThrottleConfig
            { throttleLimit = 2
            , throttlePeriod = 60
            , throttleAlgorithm = SlidingWindow
            , throttleIdentifier = byIP
            , throttleTokenBucketTTL = Nothing
            }
      env' <- addThrottle env (T.pack "test_throttle") throttle -- Fix: bind the IO action
      let app = attackMiddleware env' mockApp
      mvar <- newMVar []
      let runRequest = do
            result <- srequest $ SRequest mkIPv4Request LBS.empty
            return $ statusCode $ simpleStatus result
      -- Fork 100 concurrent requests
      replicateM_ 100 $ forkIO $ do
        status <- runSession runRequest app
        modifyMVar_ mvar $ \results -> return (status : results)
      threadDelay (3 * 1000000) -- Wait for threads to complete
      results <- withMVar mvar return
      let successes = length $ filter (== 200) results
          failures = length $ filter (== 429) results
      assertEqual "Exactly two requests should succeed" 2 successes
      assertBool "Most requests should fail" (failures >= 98)
  ]
