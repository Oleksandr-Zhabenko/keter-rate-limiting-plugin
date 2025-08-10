{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

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
    Env(..)
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
    -- * Functions for Middleware
  , buildRateLimiter
  , registerThrottle
  , mkIdentifier
  , mkZoneFn
  , getClientIPPure
  , hdr
  , fromHeaderName
  ) where

import Data.Aeson hiding (pairs)
import Data.Text (Text)
import qualified Data.Text as Tx
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
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
import qualified Web.Cookie as WC

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
  -- Check all throttles and collect their block/allow decisions, passing throttle names
  anyBlocked <- or <$> mapM (\(name, config) -> checkThrottle zoneCaches zone req name config) (HM.toList throttles)
  return anyBlocked

-- Updated checkThrottle function that needs to be called with throttle name
-- This should be called from instrument function which has access to throttle names
checkThrottle :: ZoneSpecificCaches -> Text -> Request -> Text -> ThrottleConfig -> IO Bool
checkThrottle caches zone req throttleName config = do
  mIdentifier <- throttleIdentifier config req
  case mIdentifier of
    Nothing -> return False
    Just identifier -> case throttleAlgorithm config of
      FixedWindow ->
        -- allowFixedWindowRequest cache throttleName ipZone userKey limit period
        not <$> allowFixedWindowRequest
          (zscCounterCache caches)
          throttleName
          zone
          identifier
          (throttleLimit config)
          (throttlePeriod config)

      SlidingWindow -> case zscTimestampCache caches of
        Cache { cacheStore = TimestampStore tvar } ->
          -- SlidingWindow.allowRequest getTimeNow stmMapTVar throttleName ipZone userKey windowSize limit
          not <$> SlidingWindow.allowRequest
                  (realToFrac <$> getPOSIXTime)
                  tvar
                  throttleName
                  zone
                  identifier
                  (throttlePeriod config)
                  (throttleLimit config)

      TokenBucket -> do
        let period = throttlePeriod config
            limit = throttleLimit config
            refillRate = if period > 0 then fromIntegral limit / fromIntegral period else 0.0
            ttl = fromMaybe 2 (throttleTokenBucketTTL config)
        -- TokenBucket.allowRequest cache throttleName ipZone userKey capacity refillRate expiresIn
        not <$> TokenBucket.allowRequest
                  (zscTokenBucketCache caches)
                  throttleName
                  zone
                  identifier
                  (fromIntegral limit)
                  refillRate
                  (fromIntegral ttl)

      LeakyBucket -> do
        let period = throttlePeriod config
            limit = throttleLimit config
            leakRate = if period > 0 then fromIntegral limit / fromIntegral period else 0.0
        -- LeakyBucket.allowRequest cache throttleName ipZone userKey capacity leakRate
        not <$> LeakyBucket.allowRequest
                  (zscLeakyBucketCache caches)
                  throttleName
                  zone
                  identifier
                  (fromIntegral limit)
                  leakRate

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
-- @since 0.1.1.0
data ZoneBy
  = ZoneDefault                   -- ^ All requests use default zone
  | ZoneIP                        -- ^ Derive zone from client IP
  | ZoneHeader !HeaderName        -- ^ Derive zone from header value
  deriving (Show, Eq, Generic)

-- | Complete specification for a single rate limiting rule.
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
-- @since 0.1.1.0
hdr :: Text -> HeaderName
hdr = mk . TE.encodeUtf8

-- | Extract the original ByteString from a case-insensitive HeaderName.
--
-- @since 0.1.1.0
fromHeaderName :: HeaderName -> S.ByteString
fromHeaderName = original

--------------------------------------------------------------------------------
-- | Build a complete WAI middleware from a rate limiter configuration.
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
-- @since 0.1.1.0
mkIdentifier :: IdentifierBy -> Request -> IO (Maybe Text)
mkIdentifier IdIP              = RU.byIP
mkIdentifier IdIPAndPath       = RU.byIPAndPath
mkIdentifier IdIPAndUA         = RU.byIPAndUserAgent
mkIdentifier (IdHeader h)      = \req -> pure $ fmap (TE.decodeUtf8With TEE.lenientDecode) . lookup h . requestHeaders $ req
mkIdentifier (IdCookie name)   = \req -> pure $ cookieLookupText name req
mkIdentifier (IdHeaderAndIP h) = RU.byHeaderAndIP h

-- Internal helper: cookie lookup via Web.Cookie with empty-value rejection (matches previous semantics).
cookieLookupText :: Text -> Request -> Maybe Text
cookieLookupText n req = do
  raw <- lookup hCookie (requestHeaders req)
  let pairs = WC.parseCookies raw
  v <- lookup (TE.encodeUtf8 n) pairs
  if S.null v
     then Nothing
     else Just (TE.decodeUtf8With TEE.lenientDecode v)

-- | Create a zone derivation function from declarative specification.
--
-- @since 0.1.1.0
mkZoneFn :: ZoneBy -> (Request -> IPZoneIdentifier)
mkZoneFn ZoneDefault    = const defaultIPZone
mkZoneFn ZoneIP         = getClientIPPure
mkZoneFn (ZoneHeader h) = \req ->
  maybe defaultIPZone (TE.decodeUtf8With TEE.lenientDecode) (lookup h (requestHeaders req))

-- | Extract client IP address from WAI request with header precedence.
--
-- @since 0.1.1.0
getClientIPPure :: Request -> IPZoneIdentifier
getClientIPPure req =
  let safeDecode = TE.decodeUtf8With TEE.lenientDecode
  in case lookup (mk "x-forwarded-for") (requestHeaders req) of
    Just xff -> Tx.takeWhile (/= ',') $ safeDecode xff
    Nothing  ->
      case lookup (mk "x-real-ip") (requestHeaders req) of
        Just rip -> safeDecode rip
        Nothing  ->
          case remoteHost req of
            SockAddrInet  _ addr     -> RU.ipv4ToString addr
            SockAddrInet6 _ _ addr _ -> RU.ipv6ToString addr
            SockAddrUnix   path      -> Tx.pack path
