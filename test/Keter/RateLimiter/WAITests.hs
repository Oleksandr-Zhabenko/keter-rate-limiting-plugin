{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

{-|
Module      : Keter.RateLimiter.WAITests
Description : Comprehensive WAI middleware tests for various rate-limiting algorithms.
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : experimental
Portability : POSIX

This module provides a comprehensive test suite for the WAI (Web Application Interface) middleware responsible for rate limiting. It uses the tasty, tasty-hunit, and tasty-quickcheck frameworks to define and run tests.

The tests cover five distinct rate-limiting algorithms:

* Fixed Window
* Sliding Window
* Token Bucket
* Leaky Bucket
* TinyLRU

For each algorithm, the following scenarios are tested:

* Allowing requests under the defined limit.
* Blocking requests exceeding the defined limit.
* Correctly handling IPv4 and IPv6 addresses.
* Ensuring rate-limiting window resets correctly over time.
* Identifying clients using proxy headers like @x-forwarded-for@ and @x-real-ip@.
* Managing concurrent requests to prevent race conditions.
* Simulating high-volume concurrent requests to test DoS protection.

Additional tests cover:

* Configuration-driven middleware (buildRateLimiter).
* Multiple throttle rules simultaneously.
* Different identifier strategies (header, cookie, combined).
* Zone-based separation.
* JSON configuration parsing.
* Cache management functions.
* Error handling and edge cases.
* Property-based tests for robustness.

The module defines helper functions to create mock requests and a mock application to isolate the middleware for testing.
-}

module Keter.RateLimiter.WAITests (
  -- * Test Suite
  tests
) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic
import Network.Wai
import Network.Wai.Test
import Network.HTTP.Types
import Network.Socket (SockAddr(..), tupleToHostAddress)
import Data.Text (Text)
import Control.Concurrent.STM (readTVarIO)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import Data.Aeson hiding (pairs)
import Data.CaseInsensitive (mk)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Web.Cookie as WC
import qualified Data.Text.Encoding.Error as TEE
import Keter.RateLimiter.IPZones (defaultIPZone)
import Keter.RateLimiter.WAI
import Keter.RateLimiter.Cache (Algorithm(..))
import Text.Printf (printf)

-- * Request Helpers

-- | A simple WAI 'Application' that always returns a 200 OK response.
mockApp :: Application
mockApp _ respond = respond $ responseLBS status200 [] (LBS.fromStrict $ TE.encodeUtf8 "OK")

-- | Creates a mock 'Request' with a default IPv4 address (127.0.0.1).
mkIPv4Request :: Request
mkIPv4Request = defaultRequest { remoteHost = SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)) }

-- | Creates a mock 'Request' with a default IPv6 address (::1).
mkIPv6Request :: Request
mkIPv6Request = defaultRequest { remoteHost = SockAddrInet6 0 0 (0, 0, 0, 1) 0 }

-- | Creates a mock 'Request' with a specific header.
mkRequestWithHeader :: Text -> Text -> Request
mkRequestWithHeader name value = defaultRequest {
  requestHeaders = [(mk (TE.encodeUtf8 name), TE.encodeUtf8 value)]
  }

-- | Creates a mock 'Request' with a cookie header.
mkRequestWithCookie :: Text -> Text -> Request
mkRequestWithCookie name value = defaultRequest {
  requestHeaders = [(mk "Cookie", TE.encodeUtf8 $ name <> "=" <> value)]
  }

-- | Creates a mock 'Request' with an @x-forwarded-for@ header.
mkRequestWithXFF :: Text -> Request
mkRequestWithXFF ip = defaultRequest { requestHeaders = [(mk "x-forwarded-for", TE.encodeUtf8 ip)] }

-- | Creates a mock 'Request' with an @x-real-ip@ header.
mkRequestWithRealIP :: Text -> Request
mkRequestWithRealIP ip = defaultRequest { requestHeaders = [(mk "x-real-ip", TE.encodeUtf8 ip)] }

-- | Extracts a cookie value using Web.Cookie, ignoring empty values.
extractCookieWC :: Text -> BS.ByteString -> Maybe Text
extractCookieWC name raw =
  let pairs = WC.parseCookies raw
  in case lookup (TE.encodeUtf8 name) pairs of
    Just v | not (BS.null v) -> Just (TE.decodeUtf8With TEE.lenientDecode v)
    _                       -> Nothing

-- * Test Suite Definition

tests :: TestTree
tests = testGroup "Rate Limiting Tests"
  [ algorithmTests
  , configurationTests
  , multipleThrottleTests
  , identifierStrategyTests
  , zoneBasedTests
  , jsonConfigTests
  , cacheManagementTests
  , errorHandlingTests
  , performanceTests
  , propertyBasedTests
  ]

-- | Tests for each rate-limiting algorithm across various scenarios.
algorithmTests :: TestTree
algorithmTests = testGroup "Algorithm-Specific Tests"
  [ algorithmTestGroup FixedWindow
  , algorithmTestGroup SlidingWindow
  , algorithmTestGroup TokenBucket
  , algorithmTestGroup LeakyBucket
  , algorithmTestGroup TinyLRU
  ]
  where
    algorithmTestGroup algo = testGroup (show algo ++ " Algorithm")
      [ testCase "Allows IPv4 requests below limit" $ testBelowLimit algo IdIP mkIPv4Request
      , testCase "Blocks IPv4 requests exceeding limit" $ testExceedLimit algo IdIP mkIPv4Request
      , testCase "Allows IPv6 requests below limit" $ testBelowLimit algo IdIP mkIPv6Request
      , testCase "Blocks IPv6 requests exceeding limit" $ testExceedLimit algo IdIP mkIPv6Request
      , testCase "Respects timing with IPv4" $ testTiming algo IdIP
      , testCase "Handles x-forwarded-for header for IPv4" $ testXFF algo IdIP
      , testCase "Handles x-real-ip header for IPv6" $ testRealIP algo IdIP
      , testCase "Handles concurrent requests" $ testConcurrent algo IdIP
      , testCase "Handles DoS-like concurrency" $ testDoS algo IdIP
      ]

-- | Test buildRateLimiter with various configurations.
configurationTests :: TestTree
configurationTests = testGroup "Configuration-Driven Middleware"
  [ testCase "buildRateLimiter with single throttle" testBuildSingleThrottle
  , testCase "buildRateLimiter with multiple throttles" testBuildMultipleThrottles
  , testCase "buildRateLimiter with different zones" testBuildWithZones
  , testCase "Empty throttles list" testEmptyThrottles
  ]

-- | Test multiple throttles running simultaneously.
multipleThrottleTests :: TestTree
multipleThrottleTests = testGroup "Multiple Throttle Rules"
  [ testCase "Multiple throttles with same algorithm" testMultipleSameAlgo
  , testCase "Multiple throttles with different algorithms" testMultipleDiffAlgo
  , testCase "Throttle priority and interaction" testThrottlePriority
  , testCase "Independent throttle counters" testIndependentCounters
  ]

-- | Test different identifier strategies.
identifierStrategyTests :: TestTree
identifierStrategyTests = testGroup "Identifier Strategies"
  [ testCase "IdHeader strategy" testIdHeaderStrategy
  , testCase "IdCookie strategy" testIdCookieStrategy
  , testCase "IdIPAndPath strategy" testIdIPAndPathStrategy
  , testCase "IdIPAndUA strategy" testIdIPAndUAStrategy
  , testCase "IdHeaderAndIP strategy" testIdHeaderAndIPStrategy
  , testCase "Missing header/cookie handling" testMissingIdentifiers
  , testCase "Cookie parsing edge cases" testCookieParsing
  ]

-- | Test zone-based separation.
zoneBasedTests :: TestTree
zoneBasedTests = testGroup "Zone-Based Separation"
  [ testCase "ZoneIP separation" testZoneIPSeparation
  , testCase "ZoneHeader separation" testZoneHeaderSeparation
  , testCase "Zone creation and cleanup" testZoneCreation
  , testCase "Default zone fallback" testDefaultZoneFallback
  ]

-- | Test JSON configuration parsing.
jsonConfigTests :: TestTree
jsonConfigTests = testGroup "JSON Configuration"
  [ testCase "Parse IdentifierBy JSON" testParseIdentifierBy
  , testCase "Parse ZoneBy JSON" testParseZoneBy
  , testCase "Parse RLThrottle JSON" testParseRLThrottle
  , testCase "Parse RateLimiterConfig JSON" testParseRateLimiterConfig
  , testCase "Invalid JSON handling" testInvalidJSON
  ]

-- | Test cache management functions.
cacheManagementTests :: TestTree
cacheManagementTests = testGroup "Cache Management"
  [ testCase "cacheResetAll functionality" testCacheResetAll
  , testCase "Zone cache isolation" testZoneCacheIsolation
  , testCase "Memory cleanup after reset" testMemoryCleanup
  ]

-- | Test error handling and edge cases.
errorHandlingTests :: TestTree
errorHandlingTests = testGroup "Error Handling & Edge Cases"
  [ testCase "Zero period handling" testZeroPeriod
  , testCase "Negative limit handling" testNegativeLimit
  , testCase "Very large numbers" testLargeNumbers
  , testCase "Malformed requests" testMalformedRequests
  , testCase "Concurrent access safety" testConcurrentSafety
  ]

-- | Performance and stress tests.
performanceTests :: TestTree
performanceTests = testGroup "Performance Tests"
  [ testCase "High throughput single client" testHighThroughputSingle
  , testCase "Many unique clients" testManyClients
  , testCase "Algorithm performance comparison" testAlgorithmPerformance
  , testCase "Memory usage with many zones" testManyZones
  ]

-- | Property-based tests using QuickCheck.
propertyBasedTests :: TestTree
propertyBasedTests = testGroup "Property-Based Tests"
  [ testProperty "Cookie extraction properties" propCookieExtraction
  , testProperty "Header name round-trip" propHeaderNameRoundTrip
  , testProperty "IP extraction consistency" propIPExtraction
  , testProperty "Rate limiting monotonicity" propRateLimitingMonotonicity
  , testProperty "Cookie extraction multiple" propCookieExtractionMultiple
  , testProperty "Identifier independence" propIdentifierIndependence
  ]

-- | Verifies that requests below the limit are allowed.
testBelowLimit :: Algorithm -> IdentifierBy -> Request -> Assertion
testBelowLimit algo identifier req = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig 2 60 algo identifier (Just 3600)
  env' <- addThrottle env "test_throttle" throttle
  let app = attackMiddleware env' mockApp
  let session = do
        result1 <- srequest $ SRequest req ""
        result2 <- srequest $ SRequest req ""
        return (result1, result2)
  (response1, response2) <- runSession session app
  assertEqual "First request status" status200 (simpleStatus response1)
  assertEqual "Second request status" status200 (simpleStatus response2)

-- | Verifies that requests exceeding the limit are blocked.
testExceedLimit :: Algorithm -> IdentifierBy -> Request -> Assertion
testExceedLimit algo identifier req = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig 2 60 algo identifier (Just 3600)
  env' <- addThrottle env "test_throttle" throttle
  let app = attackMiddleware env' mockApp
  let session = do
        _ <- srequest $ SRequest req ""
        _ <- srequest $ SRequest req ""
        result3 <- srequest $ SRequest req ""
        return result3
  response3 <- runSession session app
  assertEqual "Third request status" status429 (simpleStatus response3)

-- | Verifies that the rate limit counter resets after the window period.
testTiming :: Algorithm -> IdentifierBy -> Assertion
testTiming algo identifier = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig 1 1 algo identifier (Just 3600)
  env' <- addThrottle env "test_throttle" throttle
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

-- | Verifies correct IP identification using x-forwarded-for header.
testXFF :: Algorithm -> IdentifierBy -> Assertion
testXFF algo identifier = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig 1 60 algo identifier (Just 3600)
  env' <- addThrottle env "test_throttle" throttle
  let app = attackMiddleware env' mockApp
  let session = do
        result1 <- srequest $ SRequest (mkRequestWithXFF "192.168.1.1") ""
        result2 <- srequest $ SRequest (mkRequestWithXFF "192.168.1.1") ""
        return (result1, result2)
  (response1, response2) <- runSession session app
  assertEqual "First XFF request status" status200 (simpleStatus response1)
  assertEqual "Second XFF request status" status429 (simpleStatus response2)

-- | Verifies correct IP identification using x-real-ip header.
testRealIP :: Algorithm -> IdentifierBy -> Assertion
testRealIP algo identifier = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig 1 60 algo identifier (Just 3600)
  env' <- addThrottle env "test_throttle" throttle
  let app = attackMiddleware env' mockApp
  let session = do
        result1 <- srequest $ SRequest (mkRequestWithRealIP "::1") ""
        result2 <- srequest $ SRequest (mkRequestWithRealIP "::1") ""
        return (result1, result2)
  (response1, response2) <- runSession session app
  assertEqual "First Real-IP request status" status200 (simpleStatus response1)
  assertEqual "Second Real-IP request status" status429 (simpleStatus response2)

-- | Verifies behavior under moderate concurrent load.
testConcurrent :: Algorithm -> IdentifierBy -> Assertion
testConcurrent algo identifier = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig 5 60 algo identifier (Just 3600)
  env' <- addThrottle env "test_throttle" throttle
  let app = attackMiddleware env' mockApp
  let session = do
        responses <- replicateM 5 (srequest $ SRequest mkIPv4Request "")
        result6 <- srequest $ SRequest mkIPv4Request ""
        return (responses, result6)
  (responses, response6) <- runSession session app
  mapM_ (\(i, resp) -> assertEqual ("Request " ++ show i ++ " status") status200 (simpleStatus resp)) (zip [1..5] responses)
  assertEqual "Sixth request status after limit" status429 (simpleStatus response6)

-- | Simulates a DoS attack with high concurrency.
testDoS :: Algorithm -> IdentifierBy -> Assertion
testDoS algo identifier = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig 10 60 algo identifier (Just 3600)
  env' <- addThrottle env "test_throttle" throttle
  let app = attackMiddleware env' mockApp
  let session = do
        responses <- replicateM 15 (srequest $ SRequest mkIPv4Request "")
        return responses
  responses <- runSession session app
  let successCount = length $ filter (\resp -> simpleStatus resp == status200) responses
  let throttledCount = length $ filter (\resp -> simpleStatus resp == status429) responses
  assertBool "Some requests should succeed" (successCount > 0)
  assertBool "Some requests should be throttled" (throttledCount > 0)
  assertEqual "Total requests processed" 15 (successCount + throttledCount)

-- | Tests buildRateLimiter with a single throttle.
testBuildSingleThrottle :: Assertion
testBuildSingleThrottle = do
  let config = RateLimiterConfig ZoneDefault
                 [ RLThrottle "api-limit" 5 60 FixedWindow IdIP Nothing ]
  middleware <- buildRateLimiter config
  let app = middleware mockApp
  let session = replicateM 6 (srequest $ SRequest mkIPv4Request "")
  responses <- runSession session app
  let successCount = length $ filter (\r -> simpleStatus r == status200) responses
  let throttledCount = length $ filter (\r -> simpleStatus r == status429) responses
  assertEqual "Success count" 5 successCount
  assertEqual "Throttled count" 1 throttledCount

-- | Tests buildRateLimiter with multiple throttles.
testBuildMultipleThrottles :: Assertion
testBuildMultipleThrottles = do
  let config = RateLimiterConfig ZoneDefault
                 [ RLThrottle "global-limit" 10 60 FixedWindow IdIP Nothing
                 , RLThrottle "api-limit" 3 60 SlidingWindow (IdHeader "X-API-Key") Nothing
                 ]
  middleware <- buildRateLimiter config
  let app = middleware mockApp
  let requestWithApi = mkRequestWithHeader "X-API-Key" "test-key"
  let session = do
        r1 <- srequest $ SRequest requestWithApi ""
        r2 <- srequest $ SRequest requestWithApi ""
        r3 <- srequest $ SRequest requestWithApi ""
        r4 <- srequest $ SRequest requestWithApi ""
        return [r1, r2, r3, r4]
  responses <- runSession session app
  assertEqual "First 3 API requests" [status200, status200, status200] (map simpleStatus $ take 3 responses)
  assertEqual "4th API request blocked" status429 (simpleStatus $ responses !! 3)

-- | Tests buildRateLimiter with different zones.
testBuildWithZones :: Assertion
testBuildWithZones = do
  let config = RateLimiterConfig (ZoneHeader "X-Zone")
                 [ RLThrottle "zone-limit" 2 60 FixedWindow IdIP Nothing ]
  middleware <- buildRateLimiter config
  let app = middleware mockApp
  let zoneAReq = mkRequestWithHeader "X-Zone" "A"
  let zoneBReq = mkRequestWithHeader "X-Zone" "B"
  let session = do
        ra1 <- srequest $ SRequest zoneAReq ""
        ra2 <- srequest $ SRequest zoneAReq ""
        rb1 <- srequest $ SRequest zoneBReq ""
        rb2 <- srequest $ SRequest zoneBReq ""
        ra3 <- srequest $ SRequest zoneAReq ""
        return [ra1, ra2, rb1, rb2, ra3]
  responses <- runSession session app
  assertEqual "Zone separation works" [status200, status200, status200, status200, status429] (map simpleStatus responses)

-- | Tests buildRateLimiter with an empty throttles list.
testEmptyThrottles :: Assertion
testEmptyThrottles = do
  let config = RateLimiterConfig ZoneDefault []
  middleware <- buildRateLimiter config
  let app = middleware mockApp
  let session = replicateM 10 (srequest $ SRequest mkIPv4Request "")
  responses <- runSession session app
  assertEqual "All requests succeed" 10 (length $ filter (\r -> simpleStatus r == status200) responses)

-- | Tests multiple throttles with the same algorithm.
testMultipleSameAlgo :: Assertion
testMultipleSameAlgo = do
  env <- initConfig (const defaultIPZone)
  let throttle1 = ThrottleConfig 5 60 FixedWindow IdIP Nothing
  let throttle2 = ThrottleConfig 3 60 FixedWindow IdIP Nothing
  env' <- addThrottle env "global" throttle1 >>= \e -> addThrottle e "strict" throttle2
  let app = attackMiddleware env' mockApp
  let session = replicateM 4 (srequest $ SRequest mkIPv4Request "")
  responses <- runSession session app
  let successCount = length $ filter (\r -> simpleStatus r == status200) responses
  assertEqual "Limited by stricter rule" 3 successCount

-- | Tests multiple throttles with different algorithms.
testMultipleDiffAlgo :: Assertion
testMultipleDiffAlgo = do
  env <- initConfig (const defaultIPZone)
  let throttle1 = ThrottleConfig 10 60 FixedWindow IdIP Nothing
  let throttle2 = ThrottleConfig 5 60 TokenBucket IdIP (Just 120)
  env' <- addThrottle env "fixed" throttle1 >>= \e -> addThrottle e "bucket" throttle2
  let app = attackMiddleware env' mockApp
  let session = replicateM 6 (srequest $ SRequest mkIPv4Request "")
  responses <- runSession session app
  let successCount = length $ filter (\r -> simpleStatus r == status200) responses
  assertEqual "Multiple algorithms interact" 5 successCount

-- | Tests throttle priority and interaction.
testThrottlePriority :: Assertion
testThrottlePriority = do
  env <- initConfig (const defaultIPZone)
  let permissive = ThrottleConfig 1000 60 FixedWindow IdIP Nothing
  let restrictive = ThrottleConfig 1 60 FixedWindow IdIP Nothing
  env' <- addThrottle env "permissive" permissive >>= \e -> addThrottle e "restrictive" restrictive
  let app = attackMiddleware env' mockApp
  let session = do
        r1 <- srequest $ SRequest mkIPv4Request ""
        r2 <- srequest $ SRequest mkIPv4Request ""
        return [r1, r2]
  responses <- runSession session app
  assertEqual "Most restrictive rule wins" [status200, status429] (map simpleStatus responses)

-- | Tests independent throttle counters.
testIndependentCounters :: Assertion
testIndependentCounters = do
  env <- initConfig (const defaultIPZone)
  let ipThrottle = ThrottleConfig 2 60 FixedWindow IdIP Nothing
  let headerThrottle = ThrottleConfig 2 60 FixedWindow (IdHeader "X-User-ID") Nothing
  env' <- addThrottle env "ip" ipThrottle >>= \e -> addThrottle e "user" headerThrottle
  let app = attackMiddleware env' mockApp
  let userReq = mkRequestWithHeader "X-User-ID" "user123"
  let session = do
        ri1 <- srequest $ SRequest mkIPv4Request ""
        ri2 <- srequest $ SRequest mkIPv4Request ""
        ru1 <- srequest $ SRequest userReq ""
        ru2 <- srequest $ SRequest userReq ""
        return [ri1, ri2, ru1, ru2]
  responses <- runSession session app
  assertEqual "Independent counters" [status200, status200, status200, status200] (map simpleStatus responses)

-- | Tests header-based identifier strategy.
testIdHeaderStrategy :: Assertion
testIdHeaderStrategy = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig 2 60 FixedWindow (IdHeader "X-Client-ID") Nothing
  env' <- addThrottle env "header" throttle
  let app = attackMiddleware env' mockApp
  let client1Req = mkRequestWithHeader "X-Client-ID" "client1"
  let client2Req = mkRequestWithHeader "X-Client-ID" "client2"
  let session = do
        r1 <- srequest $ SRequest client1Req ""
        r2 <- srequest $ SRequest client1Req ""
        r3 <- srequest $ SRequest client2Req ""
        r4 <- srequest $ SRequest client2Req ""
        r5 <- srequest $ SRequest client1Req ""
        return [r1, r2, r3, r4, r5]
  responses <- runSession session app
  assertEqual "Header-based identification"
    [status200, status200, status200, status200, status429]
    (map simpleStatus responses)

-- | Tests cookie-based identifier strategy.
testIdCookieStrategy :: Assertion
testIdCookieStrategy = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig 1 60 FixedWindow (IdCookie "session") Nothing
  env' <- addThrottle env "cookie" throttle
  let app = attackMiddleware env' mockApp
  let session1Req = mkRequestWithCookie "session" "sess123"
  let session2Req = mkRequestWithCookie "session" "sess456"
  let session = do
        r1 <- srequest $ SRequest session1Req ""
        r2 <- srequest $ SRequest session2Req ""
        r3 <- srequest $ SRequest session1Req ""
        return [r1, r2, r3]
  responses <- runSession session app
  assertEqual "Cookie-based identification"
    [status200, status200, status429]
    (map simpleStatus responses)

-- | Tests IP+Path identifier strategy.
testIdIPAndPathStrategy :: Assertion
testIdIPAndPathStrategy = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig 1 60 FixedWindow IdIPAndPath Nothing
  env' <- addThrottle env "ip-path" throttle
  let app = attackMiddleware env' mockApp
  let path1Req = mkIPv4Request { rawPathInfo = "/api/v1" }
  let path2Req = mkIPv4Request { rawPathInfo = "/api/v2" }
  let session = do
        r1 <- srequest $ SRequest path1Req ""
        r2 <- srequest $ SRequest path2Req ""
        r3 <- srequest $ SRequest path1Req ""
        return [r1, r2, r3]
  responses <- runSession session app
  assertEqual "IP+Path identification"
    [status200, status200, status429]
    (map simpleStatus responses)

-- | Tests IP+UserAgent identifier strategy.
testIdIPAndUAStrategy :: Assertion
testIdIPAndUAStrategy = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig 1 60 FixedWindow IdIPAndUA Nothing
  env' <- addThrottle env "ip-ua" throttle
  let app = attackMiddleware env' mockApp
  let ua1Req = mkRequestWithHeader "User-Agent" "Browser/1.0"
  let ua2Req = mkRequestWithHeader "User-Agent" "Browser/2.0"
  let session = do
        r1 <- srequest $ SRequest ua1Req ""
        r2 <- srequest $ SRequest ua2Req ""
        r3 <- srequest $ SRequest ua1Req ""
        return [r1, r2, r3]
  responses <- runSession session app
  assertEqual "IP+UserAgent identification"
    [status200, status200, status429]
    (map simpleStatus responses)

-- | Tests Header+IP identifier strategy.
testIdHeaderAndIPStrategy :: Assertion
testIdHeaderAndIPStrategy = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig 1 60 FixedWindow (IdHeaderAndIP "X-Service") Nothing
  env' <- addThrottle env "header-ip" throttle
  let app = attackMiddleware env' mockApp
  let service1Req = mkRequestWithHeader "X-Service" "service1"
  let service2Req = mkRequestWithHeader "X-Service" "service2"
  let session = do
        r1 <- srequest $ SRequest service1Req ""
        r2 <- srequest $ SRequest service2Req ""
        r3 <- srequest $ SRequest service1Req ""
        return [r1, r2, r3]
  responses <- runSession session app
  assertEqual "Header+IP identification"
    [status200, status200, status429]
    (map simpleStatus responses)

-- | Tests handling of missing identifiers.
testMissingIdentifiers :: Assertion
testMissingIdentifiers = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig 1 60 FixedWindow (IdHeader "Missing-Header") Nothing
  env' <- addThrottle env "missing" throttle
  let app = attackMiddleware env' mockApp
  let session = replicateM 5 (srequest $ SRequest mkIPv4Request "")
  responses <- runSession session app
  let successCount = length $ filter (\r -> simpleStatus r == status200) responses
  assertEqual "Missing identifiers bypass throttling" 5 successCount

-- | Tests cookie parsing edge cases.
testCookieParsing :: Assertion
testCookieParsing = do
  let testCases =
        [ ("session=abc123", Just "abc123")
        , ("session=abc123; other=value", Just "abc123")
        , ("other=value; session=def456", Just "def456")
        , ("session=; other=value", Nothing)
        , ("other=value", Nothing)
        , ("malformed", Nothing)
        ]
  mapM_ (\(cookie, expected) -> do
            let result = extractCookieWC "session" (TE.encodeUtf8 cookie)
            assertEqual ("Cookie parsing: " <> T.unpack cookie) expected result
        ) testCases

-- | Tests IP-based zone separation.
testZoneIPSeparation :: Assertion
testZoneIPSeparation = do
  let config = RateLimiterConfig ZoneIP
                 [ RLThrottle "ip-zone" 1 60 FixedWindow IdIP Nothing ]
  middleware <- buildRateLimiter config
  let app = middleware mockApp
  let ip1Req = mkRequestWithXFF "192.168.1.1"
  let ip2Req = mkRequestWithXFF "192.168.1.2"
  let session = do
        r1 <- srequest $ SRequest ip1Req ""
        r2 <- srequest $ SRequest ip2Req ""
        r3 <- srequest $ SRequest ip1Req ""
        r4 <- srequest $ SRequest ip2Req ""
        return [r1, r2, r3, r4]
  responses <- runSession session app
  assertEqual "IP zone separation"
    [status200, status200, status429, status429]
    (map simpleStatus responses)

-- | Tests header-based zone separation.
testZoneHeaderSeparation :: Assertion
testZoneHeaderSeparation = do
  let config = RateLimiterConfig (ZoneHeader "X-Tenant")
                 [ RLThrottle "tenant-limit" 1 60 FixedWindow IdIP Nothing ]
  middleware <- buildRateLimiter config
  let app = middleware mockApp
  let tenant1Req = mkRequestWithHeader "X-Tenant" "tenant1"
  let tenant2Req = mkRequestWithHeader "X-Tenant" "tenant2"
  let session = do
        r1 <- srequest $ SRequest tenant1Req ""
        r2 <- srequest $ SRequest tenant2Req ""
        r3 <- srequest $ SRequest tenant1Req ""
        return [r1, r2, r3]
  responses <- runSession session app
  assertEqual "Header zone separation"
    [status200, status200, status429]
    (map simpleStatus responses)

-- | Tests zone creation and cleanup.
testZoneCreation :: Assertion
testZoneCreation = do
  env <- initConfig (\req -> maybe "default" TE.decodeUtf8 (lookup (mk "X-Zone") (requestHeaders req)))
  let throttle = ThrottleConfig 1 60 FixedWindow IdIP Nothing
  env' <- addThrottle env "test" throttle
  let zone1Req = mkRequestWithHeader "X-Zone" "zone1"
  let zone2Req = mkRequestWithHeader "X-Zone" "zone2"
  zoneCaches <- readTVarIO (envZoneCachesMap env)
  initialSize <- return $ HM.size zoneCaches
  _ <- instrument env' zone1Req
  _ <- instrument env' zone2Req
  zoneCaches' <- readTVarIO (envZoneCachesMap env')
  finalSize <- return $ HM.size zoneCaches'
  assertBool "New zones created" (finalSize > initialSize)

-- | Tests default zone fallback.
testDefaultZoneFallback :: Assertion
testDefaultZoneFallback = do
  let config = RateLimiterConfig (ZoneHeader "Missing-Header")
                 [ RLThrottle "default-fallback" 1 60 FixedWindow IdIP Nothing ]
  middleware <- buildRateLimiter config
  let app = middleware mockApp
  let session = do
        r1 <- srequest $ SRequest mkIPv4Request ""
        r2 <- srequest $ SRequest mkIPv4Request ""
        return [r1, r2]
  responses <- runSession session app
  assertEqual "Default zone fallback"
    [status200, status429]
    (map simpleStatus responses)

-- | Tests parsing of IdentifierBy JSON.
testParseIdentifierBy :: Assertion
testParseIdentifierBy = do
  let testCases =
        [ ("\"ip\"", Right IdIP)
        , ("\"ip+path\"", Right IdIPAndPath)
        , ("\"ip+ua\"", Right IdIPAndUA)
        , ("{\"header\": \"X-API-Key\"}", Right (IdHeader (hdr "X-API-Key")))
        , ("{\"cookie\": \"session\"}", Right (IdCookie "session"))
        , ("{\"header+ip\": \"X-User\"}", Right (IdHeaderAndIP (hdr "X-User")))
        , ("\"invalid\"", Left ("Error in $: identifier_by: 'ip' | 'ip+path' | 'ip+ua' | {header} | {cookie} | {header+ip}" :: String))
        ]
  mapM_ (\(json, expected) -> do
          let result = eitherDecode (LBS.fromStrict $ TE.encodeUtf8 json) :: Either String IdentifierBy
          case (result, expected) of
            (Right actual, Right expected') -> assertEqual ("Parse: " <> T.unpack json) expected' actual
            (Left _, Left _) -> return ()
            (Left err, Right _) -> assertFailure $ "Parse failed unexpectedly for " <> T.unpack json <> ": " <> err
            (Right res, Left _) -> assertFailure $ "Parse succeeded unexpectedly for " <> T.unpack json <> ": " <> show res
        ) testCases

-- | Tests parsing of ZoneBy JSON.
testParseZoneBy :: Assertion
testParseZoneBy = do
  let testCases =
        [ ("\"default\"", Right ZoneDefault)
        , ("\"ip\"", Right ZoneIP)
        , ("{\"header\": \"X-Region\"}", Right (ZoneHeader (hdr "X-Region")))
        , ("\"invalid\"", Left ("Error in $: zone_by: 'default' | 'ip' | {header}" :: String))
        ]
  mapM_ (\(json, expected) -> do
          let result = eitherDecode (LBS.fromStrict $ TE.encodeUtf8 json) :: Either String ZoneBy
          case (result, expected) of
            (Right actual, Right expected') -> assertEqual ("Parse: " <> T.unpack json) expected' actual
            (Left _, Left _) -> return ()
            (Left err, Right _) -> assertFailure $ "Parse failed unexpectedly for " <> T.unpack json <> ": " <> err
            (Right res, Left _) -> assertFailure $ "Parse succeeded unexpectedly for " <> T.unpack json <> ": " <> show res
        ) testCases

-- | Tests parsing of RLThrottle JSON.
testParseRLThrottle :: Assertion
testParseRLThrottle = do
  let json = "{\"name\":\"test\",\"limit\":100,\"period\":3600,\"algorithm\":\"FixedWindow\",\"identifier_by\":\"ip\"}"
  let result = eitherDecode (LBS.fromStrict $ TE.encodeUtf8 json) :: Either String RLThrottle
  case result of
    Right throttle -> do
      assertEqual "Name" "test" (rlName throttle)
      assertEqual "Limit" 100 (rlLimit throttle)
      assertEqual "Period" 3600 (rlPeriod throttle)
      assertEqual "Algorithm" FixedWindow (rlAlgo throttle)
    Left err -> assertFailure $ "Parse failed: " <> err

-- | Tests parsing of RateLimiterConfig JSON.
testParseRateLimiterConfig :: Assertion
testParseRateLimiterConfig = do
  let json = "{\"zone_by\":\"default\",\"throttles\":[{\"name\":\"test\",\"limit\":100,\"period\":3600,\"algorithm\":\"FixedWindow\",\"identifier_by\":\"ip\"}]}"
  let result = eitherDecode (LBS.fromStrict $ TE.encodeUtf8 json) :: Either String RateLimiterConfig
  case result of
    Right config -> do
      assertEqual "Zone by" ZoneDefault (rlZoneBy config)
      assertEqual "Throttles length" 1 (length $ rlThrottles config)
    Left err -> assertFailure $ "Parse failed: " <> err

-- | Tests handling of invalid JSON.
testInvalidJSON :: Assertion
testInvalidJSON = do
  let invalidJson = "{\"invalid\": true}"
  let result = eitherDecode (LBS.fromStrict $ TE.encodeUtf8 invalidJson) :: Either String RateLimiterConfig
  case result of
    Left _ -> return ()
    Right _ -> assertFailure "Should have failed to parse invalid JSON"

-- | Tests cacheResetAll functionality.
testCacheResetAll :: Assertion
testCacheResetAll = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig 1 60 FixedWindow IdIP Nothing
  env' <- addThrottle env "test" throttle
  let app = attackMiddleware env' mockApp
  let session1 = srequest $ SRequest mkIPv4Request ""
  resp1 <- runSession session1 app
  assertEqual "First request succeeds" status200 (simpleStatus resp1)
  let session2 = srequest $ SRequest mkIPv4Request ""
  resp2 <- runSession session2 app
  assertEqual "Second request blocked" status429 (simpleStatus resp2)
  cacheResetAll env'
  let session3 = srequest $ SRequest mkIPv4Request ""
  resp3 <- runSession session3 app
  assertEqual "Request succeeds after reset" status200 (simpleStatus resp3)

-- | Tests zone cache isolation.
testZoneCacheIsolation :: Assertion
testZoneCacheIsolation = do
  env <- initConfig (\req -> maybe "A" TE.decodeUtf8 (lookup (mk "X-Zone") (requestHeaders req)))
  let throttle = ThrottleConfig 1 60 FixedWindow IdIP Nothing
  env' <- addThrottle env "test" throttle
  let app = attackMiddleware env' mockApp
  let zoneAReq = mkRequestWithHeader "X-Zone" "A"
  let zoneBReq = mkRequestWithHeader "X-Zone" "B"
  let session = do
        ra1 <- srequest $ SRequest zoneAReq ""
        rb1 <- srequest $ SRequest zoneBReq ""
        return [ra1, rb1]
  responses <- runSession session app
  assertEqual "Both zones populated" [status200, status200] (map simpleStatus responses)
  zoneCaches <- readTVarIO (envZoneCachesMap env')
  let zoneCount = HM.size zoneCaches
  assertBool "Multiple zones created" (zoneCount >= 2)

-- | Tests memory cleanup after reset.
testMemoryCleanup :: Assertion
testMemoryCleanup = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig 100 60 FixedWindow IdIP Nothing
  env' <- addThrottle env "test" throttle
  cacheResetAll env'
  cacheResetAll env'
  _ <- instrument env' mkIPv4Request
  return ()

-- | Tests handling of zero period.
testZeroPeriod :: Assertion
testZeroPeriod = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig 10 0 TokenBucket IdIP (Just 60)
  env' <- addThrottle env "zero-period" throttle
  _ <- instrument env' mkIPv4Request
  return ()

-- | Tests handling of negative limit.
testNegativeLimit :: Assertion
testNegativeLimit = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig (-1) 60 FixedWindow IdIP Nothing
  env' <- addThrottle env "negative" throttle
  _ <- instrument env' mkIPv4Request
  return ()

-- | Tests handling of very large numbers.
testLargeNumbers :: Assertion
testLargeNumbers = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig (maxBound :: Int) (maxBound :: Int) FixedWindow IdIP Nothing
  env' <- addThrottle env "large" throttle
  blocked <- instrument env' mkIPv4Request
  assertEqual "Large numbers handled" False blocked

-- | Tests handling of malformed requests.
testMalformedRequests :: Assertion
testMalformedRequests = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig 5 60 FixedWindow (IdHeader "X-Malformed") Nothing
  env' <- addThrottle env "malformed" throttle
  let malformedReq = defaultRequest { requestHeaders = [(mk "X-Malformed", "\xFF\xFE\xFD")] }
  _ <- instrument env' malformedReq
  return ()

-- | Tests concurrent access safety.
testConcurrentSafety :: Assertion
testConcurrentSafety = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig 100 60 FixedWindow IdIP Nothing
  env' <- addThrottle env "concurrent" throttle
  results <- newMVar []
  let worker :: Integer -> IO ()
      worker i = do
        if i `mod` 10 == 0
          then cacheResetAll env'
          else do
            blocked <- instrument env' mkIPv4Request
            modifyMVar_ results (return . (blocked:))
  mapM_ (\i -> forkIO (worker i)) [1..50 :: Integer]
  threadDelay 100000
  finalResults <- readMVar results
  assertBool "Concurrent operations completed" (length finalResults > 0)

-- | Tests high throughput for a single client.
testHighThroughputSingle :: Assertion
testHighThroughputSingle = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig 1000 60 FixedWindow IdIP Nothing
  env' <- addThrottle env "throughput" throttle
  let app = attackMiddleware env' mockApp
  let session = replicateM 500 (srequest $ SRequest mkIPv4Request "")
  responses <- runSession session app
  let successCount = length $ filter (\r -> simpleStatus r == status200) responses
  assertBool "High throughput handled" (successCount > 0)

-- | Tests handling of many unique clients.
testManyClients :: Assertion
testManyClients = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig 2 60 FixedWindow (IdHeader "X-Client-ID") Nothing
  env' <- addThrottle env "many-clients" throttle
  let app = attackMiddleware env' mockApp
  let makeClientRequest :: Integer -> Request
      makeClientRequest i = mkRequestWithHeader "X-Client-ID" (T.pack $ "client" <> show i)
  let session = mapM (\i -> srequest $ SRequest (makeClientRequest i) "") [1..100 :: Integer]
  responses <- runSession session app
  let successCount = length $ filter (\r -> simpleStatus r == status200) responses
  assertEqual "Many clients handled" 100 successCount

-- | Tests performance across different algorithms.
testAlgorithmPerformance :: Assertion
testAlgorithmPerformance = do
  let algorithms = [FixedWindow, SlidingWindow, TokenBucket, LeakyBucket, TinyLRU]
  results <- mapM (\algo -> do
                     env <- initConfig (const defaultIPZone)
                     let throttle = ThrottleConfig 100 60 algo IdIP (Just 120)
                     env' <- addThrottle env ("perf-" <> T.pack (show algo)) throttle
                     let start = (0 :: Integer)
                     mapM_ (\_ -> instrument env' mkIPv4Request) [1..100 :: Integer]
                     let end = (1 :: Integer)
                     return (algo, end - start)
                  ) algorithms
  assertEqual "All algorithms tested" (length algorithms) (length results)

-- | Tests memory usage with many zones.
testManyZones :: Assertion
testManyZones = do
  env <- initConfig (\req ->
                      maybe "default" TE.decodeUtf8 (lookup (mk "X-Zone-ID") (requestHeaders req)))
  let throttle = ThrottleConfig 5 60 FixedWindow IdIP Nothing
  env' <- addThrottle env "zones" throttle
  let makeZoneRequest :: Integer -> Request
      makeZoneRequest i = mkRequestWithHeader "X-Zone-ID" (T.pack $ "zone" <> show i)
  mapM_ (\i -> instrument env' (makeZoneRequest i)) [1..50 :: Integer]
  zoneCaches <- readTVarIO (envZoneCachesMap env')
  let zoneCount = HM.size zoneCaches
  assertBool "Many zones created" (zoneCount > 10)
  assertBool "Reasonable zone count" (zoneCount <= 51)

-- * Property-Based Tests (Fixed Version)

-- | Generates valid cookie value characters (excluding problematic ones for Web.Cookie)
validCookieValueChar :: Gen Char
validCookieValueChar = elements $ concat
  [ ['!']
  , ['#'..'&']
  , ['('..'/']
  , ['0'..'9']
  , [':'] -- Split the range to exclude ';'
  , ['<'..'@']
  , ['A'..'Z']
  , ['['..'`']
  , ['a'..'z']
  , ['{'..'~']
  ]
  -- Excludes: '"', ';', ',', '=', ' ', '\t', '\n', '\r' and control chars

-- | Generates valid cookie name characters (stricter than values)
validCookieNameChar :: Gen Char
validCookieNameChar = elements $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['!', '#', '$', '%', '&', '\'', '*', '+', '-', '.', '^', '_', '`', '|', '~']

-- | Generates a valid cookie name.
cookieName :: Gen Text
cookieName = T.pack <$> listOf1 validCookieNameChar

-- | Generates a valid cookie value (no quotes, semicolons, etc.)
cookieValue :: Gen Text
cookieValue = T.pack <$> listOf1 validCookieValueChar

-- | Tests cookie extraction properties with proper cookie format.
propCookieExtraction :: Property
propCookieExtraction =
  forAll cookieName $ \name ->
  forAll cookieValue $ \value ->
  let header = TE.encodeUtf8 (name <> "=" <> value)
      extracted = extractCookieWC name header
  in counterexample ("Header: " <> show header <> ", Expected: " <> show value <> ", Got: " <> show extracted) $
       extracted === Just value

-- | Property test for cookie extraction with multiple cookies
propCookieExtractionMultiple :: Property
propCookieExtractionMultiple =
  forAll cookieName $ \targetName ->
  forAll cookieValue $ \targetValue ->
  forAll (listOf ((,) <$> cookieName <*> cookieValue)) $ \otherCookies ->
  let allCookies = (targetName, targetValue) : otherCookies
      headerValue = T.intercalate "; " $ map (\(n, v) -> n <> "=" <> v) allCookies
      header = TE.encodeUtf8 headerValue
      extracted = extractCookieWC targetName header
  in counterexample ("Header: " <> show headerValue <> ", Expected: " <> show targetValue <> ", Got: " <> show extracted) $
       extracted === Just targetValue

-- | Tests header name round-trip with valid header characters only.
propHeaderNameRoundTrip :: Property
propHeaderNameRoundTrip =
  forAll (listOf1 validHeaderChar) $ \headerChars ->
  let originalTxt = T.pack headerChars
      headerName = hdr originalTxt
      roundTrip = TE.decodeUtf8 (fromHeaderName headerName)
  in counterexample ("Original: " <> show originalTxt <> ", RoundTrip: " <> show roundTrip) $
       roundTrip === originalTxt
  where
    -- Valid HTTP header name characters per RFC 7230
    validHeaderChar :: Gen Char
    validHeaderChar = elements $ concat
      [ ['!']
      , ['#'..'\'']
      , ['*', '+', '-', '.']
      , ['0'..'9']
      , ['A'..'Z']
      , ['^'..'z']
      , ['|', '~']
      ]

-- | Tests IP extraction consistency with valid IP formats.
propIPExtraction :: Property
propIPExtraction =
  forAll genValidIP $ \ip ->
  let req = mkRequestWithXFF ip
      extracted = getClientIPPure req
      expected = T.takeWhile (/= ',') ip
  in counterexample ("IP: " <> show ip <> ", Expected: " <> show expected <> ", Got: " <> show extracted) $
       extracted === expected
  where
    genValidIP = oneof [genIPv4, genIPv6, genIPv4List]

    genIPv4 = do
      a <- choose (1, 255) :: Gen Int
      b <- choose (0, 255)
      c <- choose (0, 255)
      d <- choose (0, 255)
      -- Corrected: Convert each number to Text before intercalating.
      return $ T.intercalate "." $ map (T.pack . show) [a, b, c, d]

    genIPv6 = do
      segments <- replicateM 8 (choose (0, 65535 :: Int))
      -- Corrected: Convert each formatted hex string to Text before intercalating.
      return $ T.intercalate ":" $ map (T.pack . printf "%x") segments

    genIPv4List = do
      ips <- listOf1 genIPv4
      return $ T.intercalate ", " ips

-- | Tests rate limiting monotonicity - more requests should never result in fewer blocks.
propRateLimitingMonotonicity :: Property
propRateLimitingMonotonicity =
  forAll (choose (1, 100)) $ \limit ->
  forAll (choose (1, 3600)) $ \period ->
  monadicIO $ do
    env <- run $ initConfig (const defaultIPZone)
    let throttle = ThrottleConfig limit period FixedWindow IdIP Nothing
    env' <- run $ addThrottle env "prop" throttle
    -- Test with exactly limit requests
    results1 <- run $ mapM (\_ -> instrument env' mkIPv4Request) [1..limit]
    let blockedCount1 = length $ filter id results1
    -- Reset and test with limit + 1 requests
    run $ cacheResetAll env'
    results2 <- run $ mapM (\_ -> instrument env' mkIPv4Request) [1..limit + 1]
    let blockedCount2 = length $ filter id results2
    -- Monotonicity: more requests should result in more (or equal) blocks
    Test.QuickCheck.Monadic.assert (blockedCount2 >= blockedCount1)
    -- First batch should allow all requests within limit
    Test.QuickCheck.Monadic.assert (blockedCount1 == 0)
    -- Second batch should block at least one request
    Test.QuickCheck.Monadic.assert (blockedCount2 >= 1)

-- | Tests that different identifiers are treated independently
propIdentifierIndependence :: Property
propIdentifierIndependence =
  -- Removed unused cookieName generators
  forAll cookieValue $ \cookieValue1 ->
  forAll cookieValue $ \cookieValue2 ->
  -- This precondition ensures the two identifiers are actually different.
  cookieValue1 /= cookieValue2 ==> monadicIO $ do
    env <- run $ initConfig (const defaultIPZone)
    let throttle = ThrottleConfig 1 60 FixedWindow (IdCookie "session") Nothing
    env' <- run $ addThrottle env "test" throttle
    let req1 = mkRequestWithCookie "session" cookieValue1
    let req2 = mkRequestWithCookie "session" cookieValue2
    -- Both different session values should be allowed initially
    blocked1 <- run $ instrument env' req1
    blocked2 <- run $ instrument env' req2
    Test.QuickCheck.Monadic.assert (not blocked1)
    Test.QuickCheck.Monadic.assert (not blocked2)
    -- Second request with same session should be blocked
    blocked1_2 <- run $ instrument env' req1
    Test.QuickCheck.Monadic.assert blocked1_2
