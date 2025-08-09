{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

{-|
Module      : Keter.RateLimiter.WAI
Description : WAI-compatible rate limiting middleware with IP zone support
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : stable
Portability : portable

This file is a ported to Haskell language code with some simplifications of rack-attack 
https://github.com/rack/rack-attack/blob/main/lib/rack/attack.rb
and is based on the structure of the original code of 
rack-attack, Copyright (c) 2016 by Kickstarter, PBC, under the MIT License.
Oleksandr Zhabenko added several implementations of the window algorithm: tinyLRU, sliding window, token bucket window, leaky bucket window alongside with the initial count algorithm using AI chatbots.
IP Zone functionality added to allow separate caches per IP zone.

This implementation is released under the MIT License.

This module provides WAI middleware that implements multiple IP-zone-specific
rate limiting strategies including:

- Fixed Window
- Sliding Window
- Token Bucket
- Leaky Bucket
- TinyLRU

It supports multiple throttles applied simultaneously (e.g., per endpoint or
user group), each with its own algorithm, rate, and identifier logic.

Rate limiting state is tracked separately per IP zone using the
'ZoneSpecificCaches' abstraction with efficient HashMap-based lookups.

Use `attackMiddleware` to enforce throttling in your WAI application.

-}

module Keter.RateLimiter.WAI
  ( -- * Environment & Configuration
    Env
  , ThrottleConfig(..)
  , IdentifierBy(..)
  , ZoneBy(..)
  , RLThrottle(..)
  , RateLimiterConfig(..)
  , initConfig
  , addThrottle
    -- * Middleware
  , attackMiddleware
    -- * Manual Control & Inspection
  , instrument
  , cacheResetAll
    -- * Accessors
  , envZoneCachesMap
    -- * Functions for Middleware
  , buildRateLimiter
  , registerThrottle
  , mkIdentifier
  , extractCookie
  , mkZoneFn
  , getClientIPPure
  , hdr
  , fromHeaderName
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as Tx
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import Data.Foldable (asum)
import Network.Wai
import Network.HTTP.Types (status429, HeaderName, hCookie)
import Network.Socket (SockAddr(..))
import Data.CaseInsensitive (mk, original)
import GHC.Generics
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Control.Concurrent.STM
import Keter.RateLimiter.Cache
import Keter.RateLimiter.IPZones (IPZoneIdentifier, defaultIPZone, ZoneSpecificCaches(..), createZoneCaches)
import qualified Keter.RateLimiter.SlidingWindow as SlidingWindow
import qualified Keter.RateLimiter.TokenBucket as TokenBucket
import qualified Keter.RateLimiter.LeakyBucket as LeakyBucket
import Keter.RateLimiter.CacheWithZone (allowFixedWindowRequest)
import qualified Keter.RateLimiter.RequestUtils as RU
import Data.TinyLRU (allowRequestTinyLRU)
import System.Clock (Clock(Monotonic), getTime)
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)

--------------------------------------------------------------------------------
-- | Configuration for a single throttle rule.
data ThrottleConfig = ThrottleConfig
  { throttleLimit :: Int
    -- ^ Maximum number of requests allowed per period.
  , throttlePeriod :: Int
    -- ^ Period over which requests are counted (in seconds).
  , throttleAlgorithm :: Algorithm
    -- ^ Algorithm used for throttling (e.g., 'FixedWindow', 'LeakyBucket').
  , throttleIdentifier :: Request -> IO (Maybe Text)
    -- ^ Function to extract an identifier (e.g., user or IP) from the request.
    -- Returns 'Nothing' if throttling should be skipped.
  , throttleTokenBucketTTL :: Maybe Int
    -- ^ Optional TTL (seconds) for TokenBucket entries. If not provided, defaults to 2.
  }

-- | Global environment for throttling, including zone-specific cache HashMap and throttle configurations.
--
-- The environment uses efficient HashMap-based lookups for both IP zone caches and
-- throttle configurations, providing O(1) average-case performance for cache and
-- throttle rule retrieval.
--
-- ==== HashMap Usage
--
-- * __envZoneCachesMap__: HashMap from IP zones to their corresponding rate limiter caches
-- * __envThrottles__: HashMap of throttle rules indexed by their unique names
--
-- ==== Concurrency Model
--
-- Both HashMaps are wrapped in 'IORef' for thread-safe atomic updates while
-- maintaining efficient read access patterns typical in web middleware.
data Env = Env
  { envZoneCachesMap :: IORef (HM.HashMap IPZoneIdentifier ZoneSpecificCaches)
    -- ^ HashMap from IP zones to rate limiter caches for O(1) zone cache lookup.
  , envThrottles :: IORef (HM.HashMap Text ThrottleConfig)
    -- ^ HashMap of registered throttle rules by name for O(1) throttle rule retrieval.
  , envGetRequestIPZone :: Request -> IPZoneIdentifier
    -- ^ Function to derive an IP zone from the incoming WAI request.
  }

--------------------------------------------------------------------------------

-- | Initialize the rate-limiter environment with a default zone and no throttles.
--
-- Creates a fresh environment with:
-- * Empty HashMap for zone-specific caches (initialized with default zone only)
-- * Empty HashMap for throttle configurations
-- * Custom zone derivation function
--
-- ==== Performance Characteristics
--
-- * Initial HashMap creation: O(1)
-- * Default zone cache creation: O(1) 
-- * Thread-safe IORef initialization: O(1)
initConfig
  :: (Request -> IPZoneIdentifier)
  -- ^ Function to extract an IP zone label from the request.
  -> IO Env
initConfig getIPZone = do
  defaultCaches <- createZoneCaches
  zoneCachesMap <- newIORef $ HM.singleton defaultIPZone defaultCaches
  throttles <- newIORef HM.empty
  return $ Env zoneCachesMap throttles getIPZone

-- | Register a new named throttle rule in the environment.
--
-- Adds a throttle configuration to the environment's HashMap-based registry.
-- If a throttle with the same name already exists, it will be replaced.
--
-- ==== HashMap Update
--
-- * Average case: O(1) insertion into throttles HashMap
-- * Atomic update via 'modifyIORef'' for thread safety
-- * Returns updated environment for method chaining
addThrottle
  :: Env
  -> Text             -- ^ Throttle rule name (must be unique).
  -> ThrottleConfig   -- ^ Throttle configuration.
  -> IO Env
addThrottle env name config = do
  modifyIORef' (envThrottles env) $ HM.insert name config
  return env

-- | WAI middleware that applies registered throttles from the environment.
--
-- If any throttle blocks the request, a 429 response is returned.
-- Otherwise, the request is forwarded to the application.
attackMiddleware
  :: Env
  -> Application
  -> Application
attackMiddleware env app req respond = do
  blocked <- instrument env req
  if blocked
    then respond $ responseLBS status429 [] (LBS.fromStrict $ TE.encodeUtf8 "Too Many Requests")
    else app req respond

-- | Inspect all throttle rules and apply them to the incoming request.
--
-- Returns 'True' if any rule blocks the request.
--
-- ==== HashMap Processing
--
-- * O(1) throttles HashMap lookup via 'readIORef'
-- * O(n) processing of all throttle rules where n = number of throttles  
-- * O(1) average case zone cache lookup in IP zone HashMap
instrument
  :: Env
  -> Request
  -> IO Bool
instrument env req = do
  throttles <- readIORef (envThrottles env)
  let zone = envGetRequestIPZone env req
  zoneCaches <- getOrCreateZoneCaches env zone
  anyBlocked <- or <$> mapM (checkThrottle zoneCaches zone req) (HM.elems throttles)
  return anyBlocked
  where
    checkThrottle :: ZoneSpecificCaches -> Text -> Request -> ThrottleConfig -> IO Bool
    checkThrottle caches zone req config = do
      mIdentifier <- throttleIdentifier config req
      case mIdentifier of
        Nothing -> return False
        Just identifier -> case throttleAlgorithm config of
          FixedWindow ->
            not <$> allowFixedWindowRequest
              (zscCounterCache caches) zone identifier
              (throttleLimit config) (throttlePeriod config)

          SlidingWindow -> case zscTimestampCache caches of
            Cache { cacheStore = TimestampStore tvar } ->
              not <$> SlidingWindow.allowRequest
                      (realToFrac <$> getPOSIXTime)
                      tvar zone identifier
                      (throttlePeriod config) (throttleLimit config)

          TokenBucket -> do
            let period = throttlePeriod config
                limit = throttleLimit config
                refillRate = if period > 0 then fromIntegral limit / fromIntegral period else 0.0
                ttl = fromMaybe 2 (throttleTokenBucketTTL config)
            not <$> TokenBucket.allowRequest
                      (zscTokenBucketCache caches)
                      zone identifier
                      limit refillRate ttl

          LeakyBucket -> do
            let period = throttlePeriod config
                limit = throttleLimit config
                leakRate = if period > 0 then fromIntegral limit / fromIntegral period else 0.0
            not <$> LeakyBucket.allowRequest
                      (zscLeakyBucketCache caches)
                      zone identifier
                      limit leakRate

          TinyLRU -> do
            now <- getTime Monotonic
            case cacheStore (zscTinyLRUCache caches) of
              TinyLRUStore tvar -> do
                cache <- readTVarIO tvar
                not <$> atomically (allowRequestTinyLRU now cache identifier (throttleLimit config) (throttlePeriod config))

-- | Reset all caches for all IP zones tracked in the environment.
--
-- Iterates through the HashMap of zone caches and resets each zone's
-- rate limiting state across all algorithms.
--
-- ==== HashMap Processing
--
-- * O(1) HashMap read via 'readIORef'
-- * O(n) iteration over all zones where n = number of active zones
-- * Each zone reset involves clearing multiple algorithm-specific caches
cacheResetAll :: Env -> IO ()
cacheResetAll env = do
  zoneCachesMap <- readIORef (envZoneCachesMap env)
  mapM_ (resetZoneCaches . snd) (HM.toList zoneCachesMap)
  where
    resetZoneCaches :: ZoneSpecificCaches -> IO ()
    resetZoneCaches caches = do
      cacheReset (zscCounterCache caches)
      cacheReset (zscTimestampCache caches)
      cacheReset (zscTokenBucketCache caches)
      cacheReset (zscLeakyBucketCache caches)
      cacheReset (zscTinyLRUCache caches)

-- | Retrieve or create the set of caches for a specific IP zone.
--
-- Uses HashMap-based zone cache lookup with atomic fallback creation
-- for new zones. Ensures thread-safe zone cache initialization.
--
-- ==== HashMap Operations
--
-- * O(1) average case lookup in zone caches HashMap
-- * O(1) average case insertion for new zones via 'insertWith'
-- * Atomic 'modifyIORef'' prevents race conditions during zone creation
-- * Uses 'insertWith' with preference to existing caches (first writer wins)
getOrCreateZoneCaches
  :: Env
  -> IPZoneIdentifier
  -> IO ZoneSpecificCaches
getOrCreateZoneCaches env zone = do
  readIORef (envZoneCachesMap env) >>= \m ->
    case HM.lookup zone m of
      Just caches -> return caches
      Nothing -> do
        newCaches <- createZoneCaches
        atomicModifyIORef' (envZoneCachesMap env) $ \currentMap ->
          let updatedMap = HM.insertWith (\_ old -> old) zone newCaches currentMap
          in (updatedMap, updatedMap HM.! zone)


--------------------------------------------------------------------------------
-- Rate limiter config types

--------------------------------------------------------------------------------
-- * Configuration Data Types

-- | Specification for how to extract rate limiting identifiers from requests.
--
-- This data type provides a declarative way to specify how requests should
-- be grouped for rate limiting purposes. Each constructor represents a
-- different strategy for extracting unique identifiers from WAI requests.
--
-- ==== Constructor Overview
--
-- @
-- IdIP                  → Group by client IP address
-- IdHeader HeaderName   → Group by specific header value
-- IdCookie Text         → Group by cookie value
-- IdIPAndPath           → Group by IP + request path combination
-- IdIPAndUA             → Group by IP + User-Agent combination  
-- IdHeaderAndIP         → Group by header value + IP combination
-- @
--
-- ==== JSON Representation
--
-- The type supports flexible JSON serialization for configuration files:
--
-- @
-- "ip"                           → IdIP
-- "ip+path"                      → IdIPAndPath
-- "ip+ua"                        → IdIPAndUA
-- {"header": "X-API-Key"}        → IdHeader "X-API-Key"
-- {"cookie": "session_id"}       → IdCookie "session_id"
-- {"header+ip": "X-User-ID"}     → IdHeaderAndIP "X-User-ID"
-- @
--
-- ==== Use Cases
--
-- * __IP-based limiting__: Prevent abuse from specific addresses
-- * __API key limiting__: Rate limit per authenticated client
-- * __User session limiting__: Limit per logged-in user via cookies
-- * __Endpoint-specific limiting__: Different limits per API endpoint
-- * __Combined limiting__: Multi-dimensional rate limiting strategies
--
-- ==== Examples
--
-- @
-- -- Basic IP rate limiting
-- ipThrottle = RLThrottle "api-ip" 100 3600 FixedWindow IdIP Nothing
--
-- -- API key rate limiting
-- apiThrottle = RLThrottle "api-key" 1000 3600 TokenBucket 
--                (IdHeader "X-API-Key") (Just 300)
--
-- -- User session limiting
-- userThrottle = RLThrottle "user-session" 50 300 SlidingWindow
--                 (IdCookie "user_session") Nothing
--
-- -- Combined IP + endpoint limiting
-- endpointThrottle = RLThrottle "ip-endpoint" 10 60 LeakyBucket
--                     IdIPAndPath Nothing
-- @
--
-- @since 0.1.1.0
data IdentifierBy
  = IdIP                          -- ^ Group by client IP address
  | IdHeader !HeaderName          -- ^ Group by specific HTTP header value
  | IdCookie !Text                -- ^ Group by cookie value from Cookie header
  | IdIPAndPath                   -- ^ Group by IP address + request path
  | IdIPAndUA                     -- ^ Group by IP address + User-Agent header
  | IdHeaderAndIP !HeaderName     -- ^ Group by header value + IP address
  deriving (Show, Eq, Generic)

-- | Specification for how to derive IP zones from requests.
--
-- IP zones allow rate limiting caches to be separated by logical or
-- geographic boundaries. Each zone maintains independent rate limiting
-- state using separate HashMap entries, enabling different policies for 
-- different request sources.
--
-- ==== Constructor Overview
--
-- @
-- ZoneDefault         → All requests use the same default zone
-- ZoneIP              → Each unique IP gets its own zone
-- ZoneHeader          → Zone determined by header value
-- @
--
-- ==== JSON Representation
--
-- @
-- "default"                    → ZoneDefault
-- "ip"                         → ZoneIP  
-- {"header": "X-Service-Tier"} → ZoneHeader "X-Service-Tier"
-- @
--
-- ==== Zone Isolation Benefits
--
-- * __Geographic separation__: Different limits for different regions
-- * __Service tier isolation__: Premium vs basic user separation
-- * __Tenant isolation__: Multi-tenant application separation
-- * __Environment separation__: Staging vs production isolation
--
-- ==== Performance Considerations
--
-- * Each zone maintains separate cache structures in memory
-- * More zones = higher memory usage but better isolation
-- * Zone derivation happens on every request (keep it fast)
-- * HashMap-based zone lookup provides O(1) average access time
--
-- ==== Examples
--
-- @
-- -- Single zone for simple applications
-- config = RateLimiterConfig ZoneDefault throttles
--
-- -- IP-based zones for geographic separation
-- config = RateLimiterConfig ZoneIP throttles  
--
-- -- Service tier zones
-- config = RateLimiterConfig (ZoneHeader "X-Service-Tier") throttles
-- @
--
-- @since 0.1.1.0
data ZoneBy
  = ZoneDefault                   -- ^ All requests use default zone
  | ZoneIP                        -- ^ Derive zone from client IP
  | ZoneHeader !HeaderName        -- ^ Derive zone from header value
  deriving (Show, Eq, Generic)

-- | Complete specification for a single rate limiting rule.
--
-- This data type represents a complete throttle configuration that can be
-- applied to incoming requests. Multiple throttles can be active simultaneously,
-- each with independent limits and algorithms, stored efficiently in the 
-- environment's HashMap-based throttle registry.
--
-- ==== Field Descriptions
--
-- * __rlName__: Unique identifier for this throttle rule (HashMap key)
-- * __rlLimit__: Maximum requests allowed per period  
-- * __rlPeriod__: Time window in seconds
-- * __rlAlgo__: Rate limiting algorithm to use
-- * __rlIdBy__: How to extract identifiers from requests
-- * __rlTokenBucketTTL__: TTL for TokenBucket entries (optional)
--
-- ==== Algorithm-Specific Behavior
--
-- * __FixedWindow__: Resets count every period
-- * __SlidingWindow__: Continuous window tracking
-- * __TokenBucket__: Burst capacity with gradual refill
-- * __LeakyBucket__: Smooth request rate enforcement
-- * __TinyLRU__: Memory-efficient with LRU eviction
--
-- ==== JSON Configuration Example
--
-- @
-- {
--   "name": "api-rate-limit",
--   "limit": 1000,
--   "period": 3600,
--   "algorithm": "TokenBucket",
--   "identifier_by": {"header": "X-API-Key"},
--   "token_bucket_ttl": 300
-- }
-- @
--
-- ==== Multiple Throttle Strategy
--
-- @
-- -- Combine different throttling strategies
-- throttles = 
--   [ RLThrottle "global-ip" 10000 3600 FixedWindow IdIP Nothing
--   , RLThrottle "api-key" 1000 3600 TokenBucket (IdHeader "X-API-Key") (Just 300)
--   , RLThrottle "user-burst" 50 60 LeakyBucket (IdCookie "user_id") Nothing
--   ]
-- @
--
-- @since 0.1.1.0
data RLThrottle = RLThrottle
  { rlName   :: !Text             -- ^ Unique throttle identifier
  , rlLimit  :: !Int              -- ^ Maximum requests per period
  , rlPeriod :: !Int              -- ^ Time period in seconds
  , rlAlgo   :: !Algorithm        -- ^ Rate limiting algorithm
  , rlIdBy   :: !IdentifierBy     -- ^ Request identifier strategy
  , rlTokenBucketTTL :: !(Maybe Int) -- ^ TTL for TokenBucket (seconds)
  } deriving (Show, Eq, Generic)

-- | Complete rate limiter configuration combining zone strategy and throttles.
--
-- This is the top-level configuration type that specifies both how to
-- derive IP zones and what throttling rules to apply. It represents
-- everything needed to configure a complete rate limiting middleware
-- with efficient HashMap-based lookups.
--
-- ==== Configuration Structure
--
-- * __rlZoneBy__: Strategy for deriving IP zones from requests
-- * __rlThrottles__: List of throttle rules to apply (stored as HashMap internally)
--
-- ==== Processing Model
--
-- 1. Extract zone identifier from request using 'rlZoneBy'
-- 2. Apply all throttles in 'rlThrottles' list (O(n) where n = number of throttles)
-- 3. Block request if any throttle rule is violated
-- 4. Allow request if all throttle rules pass
-- 5. HashMap-based caches provide O(1) zone and throttle lookups
--
-- ==== JSON Configuration Example
--
-- @
-- {
--   "zone_by": {"header": "X-Service-Region"},
--   "throttles": [
--     {
--       "name": "api-global",
--       "limit": 10000,
--       "period": 3600, 
--       "algorithm": "FixedWindow",
--       "identifier_by": "ip"
--     },
--     {
--       "name": "api-authenticated", 
--       "limit": 5000,
--       "period": 3600,
--       "algorithm": "TokenBucket",
--       "identifier_by": {"header": "X-API-Key"},
--       "token_bucket_ttl": 300
--     }
--   ]
-- }
-- @
--
-- ==== Configuration Best Practices
--
-- * Start with simple IP-based limiting
-- * Add more specific rules for authenticated users
-- * Use appropriate algorithms for your traffic patterns
-- * Monitor memory usage with many zones/identifiers
-- * Test configuration changes in staging first
--
-- ==== Loading from Files
--
-- @
-- -- YAML configuration
-- config <- Data.Yaml.decodeFileThrow "rate-limiter.yaml"
-- middleware <- buildRateLimiter config
--
-- -- JSON configuration  
-- config <- Data.Aeson.eitherDecodeFileStrict "rate-limiter.json"
-- case config of
--   Right cfg -> buildRateLimiter cfg
--   Left err -> error $ "Config parse error: " ++ err
-- @
--
-- @since 0.1.1.0
data RateLimiterConfig = RateLimiterConfig
  { rlZoneBy    :: !ZoneBy        -- ^ Zone derivation strategy
  , rlThrottles :: ![RLThrottle]  -- ^ List of throttle rules
  } deriving (Show, Eq, Generic)

instance FromJSON IdentifierBy where
  parseJSON (String "ip")        = pure IdIP
  parseJSON (String "ip+path")   = pure IdIPAndPath
  parseJSON (String "ip+ua")     = pure IdIPAndUA
  parseJSON (Object o) =
    asum [ IdHeader      . hdr <$> o .: "header"
         , IdCookie            <$> o .: "cookie"
         , IdHeaderAndIP . hdr <$> o .: "header+ip"
         ]
  parseJSON _ = fail "identifier_by must be one of: 'ip' | 'ip+path' | 'ip+ua' | {header: ...} | {cookie: ...} | {header+ip: ...}"

instance ToJSON IdentifierBy where
  toJSON IdIP              = String "ip"
  toJSON IdIPAndPath       = String "ip+path"
  toJSON IdIPAndUA         = String "ip+ua"
  toJSON (IdHeader h)      = object ["header"    .= TE.decodeUtf8 (fromHeaderName h)]
  toJSON (IdCookie c)      = object ["cookie"    .= c]
  toJSON (IdHeaderAndIP h) = object ["header+ip" .= TE.decodeUtf8 (fromHeaderName h)]

instance FromJSON ZoneBy where
  parseJSON (String "default") = pure ZoneDefault
  parseJSON (String "ip")      = pure ZoneIP
  parseJSON (Object o)         = ZoneHeader . hdr <$> o .: "header"
  parseJSON _ = fail "zone_by must be 'default' | 'ip' | {header: ...}"

instance ToJSON ZoneBy where
  toJSON ZoneDefault     = String "default"
  toJSON ZoneIP          = String "ip"
  toJSON (ZoneHeader h)  = object ["header" .= TE.decodeUtf8 (fromHeaderName h)]

instance FromJSON RLThrottle where
  parseJSON = withObject "throttle" $ \o -> do
    n   <- o .:  "name"
    l   <- o .:  "limit"
    p   <- o .:  "period"
    at  <- o .:  "algorithm" >>= parseAlgoText
    idb <- o .:  "identifier_by"
    ttl <- o .:? "token_bucket_ttl"
    pure (RLThrottle n l p at idb ttl)

instance ToJSON RLThrottle where
  toJSON (RLThrottle n l p a idb ttl) =
    object [ "name" .= n, "limit" .= l, "period" .= p
           , "algorithm" .= algoToText a, "identifier_by" .= idb
           , "token_bucket_ttl" .= ttl
           ]

instance FromJSON RateLimiterConfig where
  parseJSON = withObject "rate-limiter" $ \o ->
    RateLimiterConfig
      <$> o .:? "zone_by" .!= ZoneDefault
      <*> o .:  "throttles"

instance ToJSON RateLimiterConfig where
  toJSON (RateLimiterConfig zb ths) =
    object [ "zone_by" .= zb, "throttles" .= ths ]

--------------------------------------------------------------------------------
-- * Functions for Middleware


-- | Convert Text header name to WAI HeaderName.
--
-- Utility function that creates case-insensitive HTTP header names from
-- Text values. Uses UTF-8 encoding and the Data.CaseInsensitive wrapper
-- for proper HTTP header handling.
--
-- ==== Examples
--
-- @
-- hdr "Content-Type"    -- HeaderName for "content-type"
-- hdr "X-API-Key"       -- HeaderName for "x-api-key"
-- hdr "Authorization"   -- HeaderName for "authorization"
-- @
--
-- ==== Case Insensitivity
--
-- The resulting HeaderName follows HTTP standards where header names
-- are case-insensitive. "Content-Type" and "content-type" will match
-- the same header in requests.
--
-- ==== Thread Safety
--
-- Pure function, completely thread-safe.
--
-- ==== Performance
--
-- * Text to ByteString encoding: O(n) where n is text length
-- * Case-insensitive wrapper: O(1)
--
-- @since 0.1.1.0
hdr :: Text -> HeaderName
hdr = mk . TE.encodeUtf8

-- | Extract the original ByteString from a case-insensitive HeaderName.
--
-- Utility function that retrieves the underlying ByteString representation
-- from a WAI HeaderName. Useful for serialization, logging, or when you
-- need the raw header name bytes.
--
-- ==== Examples
--
-- @
-- let headerName = hdr "Content-Type"
-- fromHeaderName headerName  -- "Content-Type" (original casing preserved)
--
-- -- Round-trip conversion
-- original = "X-Custom-Header"
-- headerName = hdr original
-- restored = fromHeaderName headerName  -- "X-Custom-Header"
-- @
--
-- ==== Casing Behavior
--
-- Returns the original byte representation that was used to create the
-- HeaderName. The casing matches what was originally provided to the
-- case-insensitive wrapper.
--
-- ==== Thread Safety
--
-- Pure function, completely thread-safe.
--
-- ==== Performance
--
-- O(1) - simple field access from the case-insensitive wrapper.
--
-- @since 0.1.1.0
fromHeaderName :: HeaderName -> S.ByteString
fromHeaderName = original

--------------------------------------------------------------------------------
-- | Build a complete WAI middleware from a rate limiter configuration.
--
-- This is the primary function for creating rate limiting middleware from
-- declarative configuration. It handles the complete setup process including
-- environment initialization, throttle registration, and middleware creation
-- using efficient HashMap-based data structures.
--
-- ==== Usage Pattern
--
-- @
-- -- Configuration-driven setup
-- config <- decodeFileThrow "rate-limiter.yaml" :: IO RateLimiterConfig
-- middleware <- buildRateLimiter config
-- 
-- -- Apply to WAI application
-- app' = middleware app
-- @
--
-- ==== Configuration Structure
--
-- The function expects a 'RateLimiterConfig' containing:
-- * Zone derivation strategy ('ZoneBy')
-- * List of throttle rules ('RLThrottle')
--
-- ==== HashMap Initialization
--
-- * Creates empty HashMap for zone caches
-- * Creates empty HashMap for throttle configurations  
-- * Registers all throttles into HashMap with O(1) average insertion
--
-- ==== Error Handling
--
-- This function performs IO operations for environment setup but does not
-- validate configuration semantics. Invalid throttle parameters (e.g., 
-- negative limits) will cause runtime errors during request processing.
--
-- ==== Thread Safety
--
-- The returned middleware is fully thread-safe and can be used in concurrent
-- WAI applications. All internal caches use STM for atomic operations and
-- HashMap operations are protected by IORef atomic updates.
--
-- @since 0.1.1.0
buildRateLimiter :: RateLimiterConfig -> IO Middleware
buildRateLimiter (RateLimiterConfig zb ths) = do
  let zoneFn = mkZoneFn zb
  env <- initConfig zoneFn
  mapM_ (registerThrottle env) ths
  pure (attackMiddleware env)

-- | Register a single throttle rule in an existing environment.
--
-- This function converts a declarative 'RLThrottle' configuration into
-- an imperative 'ThrottleConfig' and adds it to the environment's throttle
-- HashMap. Multiple throttles can be registered and will be applied
-- simultaneously during request processing.
--
-- ==== Parameters
--
-- * __env__: Target environment for throttle registration
-- * __throttle__: Throttle configuration to register
--
-- ==== HashMap Operations
--
-- * O(1) average case insertion into throttles HashMap
-- * Atomic update via 'addThrottle' function
-- * Thread-safe registration for concurrent environments
--
-- ==== Behavior
--
-- The function converts the declarative throttle specification into:
-- * A throttle identifier function based on 'rlIdBy'
-- * Algorithm-specific configuration parameters
-- * Optional token bucket TTL settings
--
-- ==== Name Uniqueness
--
-- Throttle names should be unique within an environment. Registering
-- a throttle with an existing name will overwrite the previous configuration
-- in the HashMap.
--
-- @
-- -- Register multiple throttles
-- registerThrottle env apiThrottle
-- registerThrottle env userThrottle
-- registerThrottle env ipThrottle
-- @
--
-- @since 0.1.1.0
registerThrottle :: Env -> RLThrottle -> IO ()
registerThrottle env (RLThrottle name l p algo idBy ttl) = do
  let cfg = ThrottleConfig
              { throttleLimit = l
              , throttlePeriod = p
              , throttleAlgorithm = algo
              , throttleIdentifier = mkIdentifier idBy
              , throttleTokenBucketTTL = ttl
              }
  _ <- addThrottle env name cfg
  pure ()

-- | Create an identifier extraction function from declarative specification.
--
-- This function converts 'IdentifierBy' values into executable functions
-- that extract rate limiting keys from WAI requests. The identifier
-- determines how requests are grouped for rate limiting purposes.
--
-- ==== Supported Identifier Types
--
-- @
-- IdIP              → Client IP address
-- IdHeader h        → Value of header 'h'
-- IdCookie name     → Cookie value for 'name'
-- IdIPAndPath       → IP + request path
-- IdIPAndUA         → IP + User-Agent header
-- IdHeaderAndIP h   → Header 'h' + IP address
-- @
--
-- ==== Return Values
--
-- * __Just identifier__: Rate limiting should apply using this key
-- * __Nothing__: Skip rate limiting for this request
--
-- ==== Examples
--
-- @
-- -- IP-based rate limiting
-- ipIdentifier <- mkIdentifier IdIP
-- result <- ipIdentifier request  -- Just "192.168.1.1"
--
-- -- Cookie-based (user sessions)
-- userIdentifier <- mkIdentifier (IdCookie "session_id")
-- result <- userIdentifier request  -- Just "abc123" or Nothing
--
-- -- API key from header
-- apiIdentifier <- mkIdentifier (IdHeader "X-API-Key")
-- result <- apiIdentifier request  -- Just "key123" or Nothing
-- @
--
-- ==== Error Handling
--
-- This function handles missing headers/cookies gracefully by returning
-- 'Nothing'. Cookie parsing is basic and may not handle all edge cases.
--
-- @since 0.1.1.0
mkIdentifier :: IdentifierBy -> Request -> IO (Maybe Text)
mkIdentifier IdIP              = RU.byIP
mkIdentifier IdIPAndPath       = RU.byIPAndPath
mkIdentifier IdIPAndUA         = RU.byIPAndUserAgent
mkIdentifier (IdHeader h)      = pure . fmap TE.decodeUtf8 . lookup h . requestHeaders
mkIdentifier (IdCookie name)   = pure . (>>= extractCookie name) . lookup hCookie . requestHeaders
mkIdentifier (IdHeaderAndIP h) = RU.byHeaderAndIP h


-- | Extract a specific cookie value from the Cookie header.
--
-- Simple cookie parser that extracts values from the standard HTTP Cookie
-- header format. Handles basic "name=value" pairs separated by semicolons.
--
-- ==== Cookie Format Support
--
-- @
-- "session=abc123; theme=dark; lang=en"  →  extractCookie "session" → Just "abc123"
-- "user=; other=value"                   →  extractCookie "user"    → Nothing
-- "malformed cookie data"                →  extractCookie "any"     → Nothing
-- @
--
-- ==== Parameters
--
-- * __name__: Cookie name to extract
-- * __raw__: Raw Cookie header value as ByteString
--
-- ==== Limitations
--
-- * Does not handle quoted cookie values
-- * No support for cookie attributes (path, domain, etc.)
-- * Basic whitespace handling around semicolons
-- * No URL decoding of cookie values
--
-- ==== Thread Safety
--
-- Pure function, completely thread-safe.
--
-- @
-- -- Usage in request processing
-- case lookup hCookie (requestHeaders req) of
--   Just cookieHeader -> extractCookie "user_id" cookieHeader
--   Nothing -> Nothing
-- @
--
-- @since 0.1.1.0
extractCookie :: Text -> S.ByteString -> Maybe Text
extractCookie n raw =
  let t   = TE.decodeUtf8 raw
      kvs = map (Tx.breakOn "=" . Tx.strip) (Tx.splitOn ";" t)
  in lookup n [ (k, Tx.drop 1 v) | (k,v) <- kvs, not (Tx.null v) ]

-- | Create a zone derivation function from declarative specification.
--
-- Converts 'ZoneBy' configuration into an executable function that
-- determines which IP zone cache to use for each request. Zone separation
-- allows independent rate limiting across different network segments or
-- logical partitions using HashMap-based zone cache storage.
--
-- ==== Zone Derivation Strategies
--
-- @
-- ZoneDefault     → All requests use default zone
-- ZoneIP          → Zone determined by client IP
-- ZoneHeader h    → Zone from header 'h' value
-- @
--
-- ==== HashMap Integration
--
-- The returned function produces zone identifiers that serve as keys in
-- the environment's zone caches HashMap, enabling O(1) average case
-- lookup of zone-specific rate limiting state.
--
-- ==== Examples
--
-- @
-- -- Single zone for all requests
-- zoneFn <- mkZoneFn ZoneDefault
-- zone <- zoneFn request  -- Always "default"
--
-- -- IP-based zones (geographic separation)
-- zoneFn <- mkZoneFn ZoneIP
-- zone <- zoneFn request  -- "192.168.1.1" or "2001:db8::1"
--
-- -- Header-based zones (service tiers)
-- zoneFn <- mkZoneFn (ZoneHeader "X-Service-Tier")
-- zone <- zoneFn request  -- "premium", "basic", etc.
-- @
--
-- ==== Default Fallback
--
-- When header-based zone derivation fails (missing header), the function
-- falls back to the default zone to ensure requests are always processed
-- and can be found in the zone caches HashMap.
--
-- ==== Use Cases
--
-- * Geographic rate limiting separation
-- * Service tier differentiation
-- * Tenant isolation in multi-tenant applications
-- * A/B testing with different rate limits
--
-- @since 0.1.1.0
mkZoneFn :: ZoneBy -> (Request -> IPZoneIdentifier)
mkZoneFn ZoneDefault    = const defaultIPZone
mkZoneFn ZoneIP         = getClientIPPure
mkZoneFn (ZoneHeader h) = \req ->
  maybe defaultIPZone TE.decodeUtf8 (lookup h (requestHeaders req))

-- | Extract client IP address from WAI request with header precedence.
--
-- Pure function that determines client IP using the same precedence logic
-- as the RequestUtils module but without IO dependencies. Checks forwarded
-- headers before falling back to socket address.
--
-- ==== IP Resolution Precedence
--
-- 1. __X-Forwarded-For__: First IP from comma-separated list
-- 2. __X-Real-IP__: Direct proxy header
-- 3. __Remote Socket__: Direct connection address
-- 4. __Unix Socket__: Socket path for local connections
--
-- ==== Header Processing
--
-- * X-Forwarded-For: Takes only the first IP (client, not proxies)
-- * Handles both IPv4 and IPv6 address formats
-- * No validation of IP format correctness
--
-- ==== Examples
--
-- @
-- -- Behind load balancer
-- request with headers: [("x-forwarded-for", "203.0.113.1, 198.51.100.1")]
-- getClientIPPure request  -- "203.0.113.1"
--
-- -- Direct connection
-- request with remoteHost: SockAddrInet _ addr
-- getClientIPPure request  -- "192.168.1.100"
--
-- -- IPv6 connection
-- request with remoteHost: SockAddrInet6 _ _ addr _
-- getClientIPPure request  -- "2001:db8::1"
-- @
--
-- ==== Thread Safety
--
-- Pure function with no side effects, completely thread-safe.
--
-- ==== Performance
--
-- * Header lookup: O(n) where n is number of headers
-- * String processing: O(m) where m is header value length
-- * Socket address conversion: O(1)
--
-- @since 0.1.1.0
getClientIPPure :: Request -> IPZoneIdentifier
getClientIPPure req =
  case lookup (mk "x-forwarded-for") (requestHeaders req) of
    Just xff -> Tx.takeWhile (/= ',') $ TE.decodeUtf8 xff
    Nothing  ->
      case lookup (mk "x-real-ip") (requestHeaders req) of
        Just rip -> TE.decodeUtf8 rip
        Nothing  ->
          case remoteHost req of
            SockAddrInet  _ addr     -> RU.ipv4ToString addr
            SockAddrInet6 _ _ addr _ -> RU.ipv6ToString addr
            SockAddrUnix   path      -> Tx.pack path
