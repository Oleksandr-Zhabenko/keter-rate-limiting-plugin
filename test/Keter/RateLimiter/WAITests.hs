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
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.CaseInsensitive (CI, mk)
import Keter.RateLimiter.WAI (initConfig, defaultIPZone, attackMiddleware, ThrottleConfig(..), addThrottle)
import Keter.RateLimiter.RequestUtils
import Keter.RateLimiter.Cache (Algorithm(..))

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
      [ testCase "Allows IPv4 requests below limit" $ do
          env <- initConfig (const defaultIPZone)
          let throttle = ThrottleConfig
                { throttleLimit = 2
                , throttlePeriod = 60
                , throttleAlgorithm = FixedWindow
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
      , testCase "Blocks IPv4 requests exceeding limit" $ do
          env <- initConfig (const defaultIPZone)
          let throttle = ThrottleConfig
                { throttleLimit = 2
                , throttlePeriod = 60
                , throttleAlgorithm = FixedWindow
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
      , testCase "Allows IPv6 requests below limit" $ do
          env <- initConfig (const defaultIPZone)
          let throttle = ThrottleConfig
                { throttleLimit = 2
                , throttlePeriod = 60
                , throttleAlgorithm = FixedWindow
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
      , testCase "Blocks IPv6 requests exceeding limit" $ do
          env <- initConfig (const defaultIPZone)
          let throttle = ThrottleConfig
                { throttleLimit = 2
                , throttlePeriod = 60
                , throttleAlgorithm = FixedWindow
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
      , testCase "Respects fixed window timing with IPv4" $ do
          env <- initConfig (const defaultIPZone)
          let throttle = ThrottleConfig
                { throttleLimit = 2
                , throttlePeriod = 2
                , throttleAlgorithm = FixedWindow
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
                liftIO $ threadDelay (3 * 1000000) -- Wait for window reset
                result4 <- srequest $ SRequest mkIPv4Request LBS.empty
                assertStatus 200 result4
          runSession session app
      , testCase "Handles x-forwarded-for header for IPv4" $ do
          env <- initConfig (const defaultIPZone)
          let throttle = ThrottleConfig
                { throttleLimit = 2
                , throttlePeriod = 60
                , throttleAlgorithm = FixedWindow
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
                , throttleAlgorithm = FixedWindow
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
      ]
  , testGroup "Sliding Window Algorithm"
      [ testCase "Allows IPv4 requests below limit" $ do
          env <- initConfig (const defaultIPZone)
          let throttle = ThrottleConfig
                { throttleLimit = 2
                , throttlePeriod = 60
                , throttleAlgorithm = SlidingWindow
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
      , testCase "Blocks IPv4 requests exceeding limit" $ do
          env <- initConfig (const defaultIPZone)
          let throttle = ThrottleConfig
                { throttleLimit = 2
                , throttlePeriod = 60
                , throttleAlgorithm = SlidingWindow
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
      , testCase "Allows IPv6 requests below limit" $ do
          env <- initConfig (const defaultIPZone)
          let throttle = ThrottleConfig
                { throttleLimit = 2
                , throttlePeriod = 60
                , throttleAlgorithm = SlidingWindow
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
      , testCase "Blocks IPv6 requests exceeding limit" $ do
          env <- initConfig (const defaultIPZone)
          let throttle = ThrottleConfig
                { throttleLimit = 2
                , throttlePeriod = 60
                , throttleAlgorithm = SlidingWindow
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
      , testCase "Respects sliding window timing with IPv4" $ do
          env <- initConfig (const defaultIPZone)
          let throttle = ThrottleConfig
                { throttleLimit = 2
                , throttlePeriod = 2
                , throttleAlgorithm = SlidingWindow
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
                , throttleAlgorithm = SlidingWindow
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
      ]
  , testGroup "Token Bucket Algorithm"
      [ testCase "Allows IPv4 requests below limit" $ do
          env <- initConfig (const defaultIPZone)
          let throttle = ThrottleConfig
                { throttleLimit = 2
                , throttlePeriod = 60
                , throttleAlgorithm = TokenBucket
                , throttleIdentifier = byIP
                , throttleTokenBucketTTL = Just 60
                }
          let env' = addThrottle env (T.pack "test_throttle") throttle
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
                , throttleAlgorithm = TokenBucket
                , throttleIdentifier = byIP
                , throttleTokenBucketTTL = Just 60
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
      , testCase "Allows IPv6 requests below limit" $ do
          env <- initConfig (const defaultIPZone)
          let throttle = ThrottleConfig
                { throttleLimit = 2
                , throttlePeriod = 60
                , throttleAlgorithm = TokenBucket
                , throttleIdentifier = byIP
                , throttleTokenBucketTTL = Just 60
                }
          let env' = addThrottle env (T.pack "test_throttle") throttle
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
                , throttleAlgorithm = TokenBucket
                , throttleIdentifier = byIP
                , throttleTokenBucketTTL = Just 60
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
      , testCase "Respects token bucket timing with IPv4" $ do
          env <- initConfig (const defaultIPZone)
          let throttle = ThrottleConfig
                { throttleLimit = 2
                , throttlePeriod = 2
                , throttleAlgorithm = TokenBucket
                , throttleIdentifier = byIP
                , throttleTokenBucketTTL = Just 2
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
                liftIO $ threadDelay (3 * 1000000) -- Wait for token replenishment
                result4 <- srequest $ SRequest mkIPv4Request LBS.empty
                assertStatus 200 result4
          runSession session app
      , testCase "Handles x-forwarded-for header for IPv4" $ do
          env <- initConfig (const defaultIPZone)
          let throttle = ThrottleConfig
                { throttleLimit = 2
                , throttlePeriod = 60
                , throttleAlgorithm = TokenBucket
                , throttleIdentifier = byIP
                , throttleTokenBucketTTL = Just 60
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
                , throttleAlgorithm = TokenBucket
                , throttleIdentifier = byIP
                , throttleTokenBucketTTL = Just 60
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
      ]
  , testGroup "Leaky Bucket Algorithm"
      [ testCase "Allows IPv4 requests below limit" $ do
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
      , testCase "Blocks IPv4 requests exceeding limit" $ do
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
      , testCase "Allows IPv6 requests below limit" $ do
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
      , testCase "Blocks IPv6 requests exceeding limit" $ do
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
      ]
  ]
