{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Keter.RateLimiter.WAI
Description : WAI-compatible, plugin-friendly rate limiting middleware with IP-zone support
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Copyright   : (c) 2025 Oleksandr Zhabenko
Stability   : stable
Portability : portable

This file is a ported to Haskell language code with some simplifications of rack-attack
<https://github.com/rack/rack-attack/blob/main/lib/rack/attack.rb>
and is based on the structure of the original code of
rack-attack, Copyright (c) 2016 by Kickstarter, PBC, under the MIT License.
Oleksandr Zhabenko added several implementations of the window algorithm: tinyLRU, sliding window, token bucket window, leaky bucket window alongside with the initial count algorithm using AI chatbots.
IP Zone functionality added to allow separate caches per IP zone.

Overview
========

This module provides WAI middleware for declarative, IP-zone-aware rate limiting with
multiple algorithms:

- Fixed Window
- Sliding Window
- Token Bucket
- Leaky Bucket
- TinyLRU

Key points
----------

- Plugin-friendly construction: build an environment once (Env) from 'RateLimiterConfig'
  and produce a pure WAI 'Middleware'. This matches common WAI patterns and avoids
  per-request setup or global mutable state.

- Concurrency model: all shared structures inside 'Env' use STM 'TVar', not 'IORef'.
  This ensures thread-safe updates under GHC's lightweight (green) threads.

- Zone-specific caches: per-IP-zone caches are stored in a HashMap keyed by zone
  identifiers. Zones are derived from a configurable strategy ('ZoneBy'), with a default.

- No global caches in Keter: you can build one Env per compiled middleware chain
  and cache that chain externally (e.g., per-vhost + middleware-list), preserving
  counters/windows across requests.

Quick start
-----------

1) Declarative configuration (e.g., parsed from JSON/YAML):

@
let cfg = RateLimiterConfig
      { rlZoneBy = ZoneDefault
      , rlThrottles =
          [ RLThrottle "api"   1000 3600 FixedWindow IdIP Nothing
          , RLThrottle "login" 5    300  TokenBucket IdIP (Just 600)
          ]
      }
@

2) Build Env once and obtain a pure Middleware:

@
env <- buildEnvFromConfig cfg
let mw = buildRateLimiterWithEnv env
app = mw baseApplication
@

Alternatively:

@
mw <- buildRateLimiter cfg  -- convenience: Env creation + Middleware
app = mw baseApplication
@

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
  , attackMiddleware         -- low-level: apply throttling with an existing Env
  , buildRateLimiter         -- convenience: build Env from config, return Middleware
  , buildRateLimiterWithEnv  -- preferred: pure Middleware from a pre-built Env
  , buildEnvFromConfig       -- build Env once from RateLimiterConfig

    -- * Manual Control & Inspection
  , instrument
  , cacheResetAll

    -- * Helpers for configuration
  , registerThrottle
  , mkIdentifier
  , mkZoneFn
  , getClientIPPure
  , hdr
  , fromHeaderName
  ) where

import Control.Concurrent.STM
import Data.Aeson hiding (pairs)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (mk, original)
import Data.Foldable (asum)
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Tx
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import GHC.Generics
import Network.HTTP.Types (HeaderName, hCookie, status429)
import Network.Socket (SockAddr (..))
import Network.Wai
import qualified Web.Cookie as WC

-- SOLUTION: Import Cache with hiding Algorithm to avoid conflict, then import Algorithm explicitly
import Keter.RateLimiter.Cache hiding (Algorithm)
import Keter.RateLimiter.Cache (Algorithm(..))
import Keter.RateLimiter.CacheWithZone (allowFixedWindowRequest)
import Keter.RateLimiter.IPZones
  ( IPZoneIdentifier
  , ZoneSpecificCaches(..)
  , createZoneCaches
  , defaultIPZone
  )
import qualified Keter.RateLimiter.LeakyBucket as LeakyBucket
import qualified Keter.RateLimiter.RequestUtils as RU
import qualified Keter.RateLimiter.SlidingWindow as SlidingWindow
import qualified Keter.RateLimiter.TokenBucket as TokenBucket
import Data.TinyLRU (allowRequestTinyLRU)
import System.Clock (Clock (Monotonic), getTime)
import Data.Time.Clock.POSIX (getPOSIXTime)

--------------------------------------------------------------------------------
-- Configuration and Environment

-- SOLUTION: Use the Algorithm from Cache module, don't redefine it
-- type Algorithm = Cache.Algorithm  -- Remove this line since we import directly

-- | Runtime throttle parameters assembled from declarative configuration.
--
-- See 'RLThrottle' for the declarative counterpart.
data ThrottleConfig = ThrottleConfig
  { throttleLimit :: !Int
    -- ^ Maximum allowed requests per period.
  , throttlePeriod :: !Int
    -- ^ Period length in seconds.
  , throttleAlgorithm :: !Algorithm
    -- ^ Which throttling algorithm to use.
  , throttleIdentifierBy :: !IdentifierBy
    -- ^ Declarative spec for extracting an identifier (e.g., IP, header, cookie).
    -- At runtime we derive the extractor using 'mkIdentifier' and compute it
    -- at most once per request per IdentifierBy group. If extraction yields
    -- Nothing, this throttle does not apply to the request.
  , throttleTokenBucketTTL :: !(Maybe Int)
    -- ^ Optional TTL (seconds) for TokenBucket entries.
  } deriving (Show, Eq, Generic)

-- | Thread-safe, shared state for rate limiting.
--
-- Concurrency model
-- -----------------
-- - Uses 'TVar' from STM for in-memory HashMaps.
-- - Safe for green-threaded request handlers.
-- - No global variables: construct 'Env' in your wiring/bootstrap and reuse it.
data Env = Env
  { envZoneCachesMap    :: TVar (HM.HashMap IPZoneIdentifier ZoneSpecificCaches)
    -- ^ Per-zone caches for all algorithms.
  , envThrottles        :: TVar (HM.HashMap Text ThrottleConfig)
    -- ^ Named throttle configurations.
  , envGetRequestIPZone :: Request -> IPZoneIdentifier
    -- ^ Function deriving the IP zone for a given request.
  }

-- | Initialize an empty environment with a zone-derivation function.
--
-- Populates the default zone lazily as needed; a default cache is allocated
-- immediately for the default zone to keep fast-path lookups cheap.
initConfig
  :: (Request -> IPZoneIdentifier)  -- ^ Request -> zone label
  -> IO Env
initConfig getIPZone = do
  defaultCaches <- createZoneCaches
  zoneCachesMap <- newTVarIO $ HM.singleton defaultIPZone defaultCaches
  throttles     <- newTVarIO HM.empty
  pure $ Env zoneCachesMap throttles getIPZone

-- | Add or replace a named throttle configuration.
--
-- STM-backed insertion for concurrency safety.
addThrottle
  :: Env
  -> Text
  -> ThrottleConfig
  -> IO Env
addThrottle env name config = do
  atomically $ modifyTVar' (envThrottles env) $ HM.insert name config
  pure env

--------------------------------------------------------------------------------
-- Middleware (application of throttles)

-- | Low-level middleware: apply throttling using an existing 'Env'.
--
-- If any throttle denies the request, a 429 response is returned.
-- Otherwise, 'app' is invoked.
attackMiddleware
  :: Env
  -> Application
  -> Application
attackMiddleware env app req respond = do
  blocked <- instrument env req
  if blocked
    then respond $ responseLBS status429 [("Content-Type","text/plain; charset=utf-8")]
                      (LBS.fromStrict $ TE.encodeUtf8 "Too Many Requests")
    else app req respond

-- | Inspect all active throttles in 'Env' for the given request.
--
-- Returns True if the request should be blocked under any rule.
instrument :: Env -> Request -> IO Bool
instrument env req = do
  throttles <- readTVarIO (envThrottles env)
  if HM.null throttles
    then pure False
    else do
      let zone = envGetRequestIPZone env req
      caches <- getOrCreateZoneCaches env zone
      let buckets = groupByIdentifier throttles
      anyMHashMap
        (\idBy group ->
           case group of
             [] -> pure False
             ((_name0, _cfg0):_) -> do
               -- Compute identifier once per IdentifierBy group
               mIdent <- mkIdentifier idBy req
               case mIdent of
                 Nothing    -> pure False
                 Just ident ->
                   anyMList
                     (\(name, cfg) ->
                        checkThrottleWithIdent caches zone req name cfg (Just ident)
                     )
                     group
        )
        buckets

-- | Check an individual throttle with a precomputed identifier.
--
-- True = block, False = allow.
checkThrottleWithIdent
  :: ZoneSpecificCaches
  -> Text                 -- ^ zone
  -> Request
  -> Text                 -- ^ throttle name
  -> ThrottleConfig
  -> Maybe Text           -- ^ precomputed identifier
  -> IO Bool
checkThrottleWithIdent caches zone _req throttleName cfg mIdentifier =
  case mIdentifier of
    Nothing    -> pure False
    Just ident ->
      case throttleAlgorithm cfg of
        -- SOLUTION: Use unqualified Algorithm constructors since we imported them explicitly
        FixedWindow ->
          -- allowFixedWindowRequest cache throttleName zone ident limit period
          not <$> allowFixedWindowRequest
                    (zscCounterCache caches)
                    throttleName
                    zone
                    ident
                    (throttleLimit cfg)
                    (throttlePeriod cfg)

        SlidingWindow -> case zscTimestampCache caches of
          Cache { cacheStore = TimestampStore tvar } ->
            -- SlidingWindow.allowRequest timeNow tvar throttleName zone ident window limit
            not <$> SlidingWindow.allowRequest
                      (realToFrac <$> getPOSIXTime)
                      tvar
                      throttleName
                      zone
                      ident
                      (throttlePeriod cfg)
                      (throttleLimit cfg)

        TokenBucket -> do
          let period     = throttlePeriod cfg
              limit      = throttleLimit cfg
              refillRate = if period > 0 then fromIntegral limit / fromIntegral period else 0.0
              ttl        = fromMaybe 2 (throttleTokenBucketTTL cfg)
          -- TokenBucket.allowRequest cache throttleName zone ident capacity refill expires
          not <$> TokenBucket.allowRequest
                    (zscTokenBucketCache caches)
                    throttleName
                    zone
                    ident
                    (fromIntegral limit)
                    refillRate
                    (fromIntegral ttl)

        LeakyBucket -> do
          let period   = throttlePeriod cfg
              limit    = throttleLimit cfg
              leakRate = if period > 0 then fromIntegral limit / fromIntegral period else 0.0
          -- LeakyBucket.allowRequest cache throttleName zone ident capacity leakRate
          not <$> LeakyBucket.allowRequest
                    (zscLeakyBucketCache caches)
                    throttleName
                    zone
                    ident
                    (fromIntegral limit)
                    leakRate

        TinyLRU -> do
          now <- getTime Monotonic
          case cacheStore (zscTinyLRUCache caches) of
            TinyLRUStore tvar -> do
              cache <- readTVarIO tvar
              -- allowRequestTinyLRU now cache ident capacity periodSecs
              not <$> atomically (allowRequestTinyLRU now cache ident (throttleLimit cfg) (throttlePeriod cfg))

-- | Backward-compatible entry that derives the identifier and delegates
-- to the precomputed path, ensuring no duplicate computation.
checkThrottle
  :: ZoneSpecificCaches -> Text -> Request -> Text -> ThrottleConfig -> IO Bool
checkThrottle caches zone req throttleName cfg = do
  mIdentifier <- mkIdentifier (throttleIdentifierBy cfg) req
  checkThrottleWithIdent caches zone req throttleName cfg mIdentifier

-- | Reset all caches across all known zones.
--
-- Useful in tests or administrative endpoints.
cacheResetAll :: Env -> IO ()
cacheResetAll env = do
  zoneCachesMap <- readTVarIO (envZoneCachesMap env)
  mapM_ (resetZone . snd) (HM.toList zoneCachesMap)
  where
    resetZone :: ZoneSpecificCaches -> IO ()
    resetZone caches = do
      cacheReset (zscCounterCache caches)
      cacheReset (zscTimestampCache caches)
      cacheReset (zscTokenBucketCache caches)
      cacheReset (zscLeakyBucketCache caches)
      cacheReset (zscTinyLRUCache caches)

-- | Retrieve or create caches for a given IP zone.
--
-- Ensures a single writer initializes a new zone; readers see either the
-- existing or newly-inserted caches.
getOrCreateZoneCaches
  :: Env
  -> IPZoneIdentifier
  -> IO ZoneSpecificCaches
getOrCreateZoneCaches env zone = do
  m <- readTVarIO (envZoneCachesMap env)
  case HM.lookup zone m of
    Just caches -> pure caches
    Nothing -> do
      newCaches <- createZoneCaches
      atomically $ do
        m0 <- readTVar (envZoneCachesMap env)
        case HM.lookup zone m0 of
          Just existing -> pure existing
          Nothing -> do
            let m1 = HM.insert zone newCaches m0
            writeTVar (envZoneCachesMap env) m1
            pure newCaches

--------------------------------------------------------------------------------
-- Declarative configuration types

-- | How to identify clients for throttling.
data IdentifierBy
  = IdIP
  | IdHeader !HeaderName
  | IdCookie !Text
  | IdIPAndPath
  | IdIPAndUA
  | IdHeaderAndIP !HeaderName
  deriving (Show, Eq, Generic)

-- Manual Hashable instance since HeaderName doesn't have one
instance Hashable IdentifierBy where
  hashWithSalt s IdIP = hashWithSalt s (0 :: Int)
  hashWithSalt s (IdHeader h) = hashWithSalt s (1 :: Int, original h)
  hashWithSalt s (IdCookie t) = hashWithSalt s (2 :: Int, t)
  hashWithSalt s IdIPAndPath = hashWithSalt s (3 :: Int)
  hashWithSalt s IdIPAndUA = hashWithSalt s (4 :: Int)
  hashWithSalt s (IdHeaderAndIP h) = hashWithSalt s (5 :: Int, original h)

-- | How to derive IP zones from requests.
data ZoneBy
  = ZoneDefault
  | ZoneIP
  | ZoneHeader !HeaderName
  deriving (Show, Eq, Generic)

-- | Declarative throttle rule (parsed from JSON/YAML).
data RLThrottle = RLThrottle
  { rlName   :: !Text
  , rlLimit  :: !Int
  , rlPeriod :: !Int
  , rlAlgo   :: !Algorithm
  , rlIdBy   :: !IdentifierBy
  , rlTokenBucketTTL :: !(Maybe Int)
  } deriving (Show, Eq, Generic)

-- | Top-level configuration: zone strategy and throttle rules.
data RateLimiterConfig = RateLimiterConfig
  { rlZoneBy    :: !ZoneBy
  , rlThrottles :: ![RLThrottle]
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
  parseJSON _ = fail "identifier_by: 'ip' | 'ip+path' | 'ip+ua' | {header} | {cookie} | {header+ip}"

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
  parseJSON _ = fail "zone_by: 'default' | 'ip' | {header}"

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
-- Public builders (preferred wiring API)

-- | Build 'Env' once from a declarative 'RateLimiterConfig'.
--
-- Use this at wiring time; the returned 'Env' is stable and reused across requests.
buildEnvFromConfig :: RateLimiterConfig -> IO Env
buildEnvFromConfig (RateLimiterConfig zb ths) = do
  let zoneFn = mkZoneFn zb
  env <- initConfig zoneFn
  mapM_ (registerThrottle env) ths
  pure env

-- | Produce a pure 'Middleware' from an existing 'Env'.
--
-- This is the recommended way to integrate with WAI/Keter: the middleware is
-- a pure function, while the state is already encapsulated in 'Env'.
buildRateLimiterWithEnv :: Env -> Middleware
buildRateLimiterWithEnv = attackMiddleware

-- | Convenience: build an 'Env' from config and return the 'Middleware'.
--
-- Suitable if you don't need to retain the 'Env' for administrative operations.
buildRateLimiter :: RateLimiterConfig -> IO Middleware
buildRateLimiter cfg = buildRateLimiterWithEnv <$> buildEnvFromConfig cfg

--------------------------------------------------------------------------------
-- Helper functions for configuration

-- | Register a single throttle rule into an 'Env'.
registerThrottle :: Env -> RLThrottle -> IO Env
registerThrottle env (RLThrottle name l p algo idBy ttl) =
  addThrottle env name ThrottleConfig
    { throttleLimit = l
    , throttlePeriod = p
    , throttleAlgorithm = algo
    , throttleIdentifierBy = idBy
    , throttleTokenBucketTTL = ttl
    }

-- | Build a request-identifier function from a declarative spec.
mkIdentifier :: IdentifierBy -> Request -> IO (Maybe Text)
mkIdentifier IdIP              = RU.byIP
mkIdentifier IdIPAndPath       = RU.byIPAndPath
mkIdentifier IdIPAndUA         = RU.byIPAndUserAgent
mkIdentifier (IdHeader h)      = \req -> pure $ fmap (TE.decodeUtf8With TEE.lenientDecode) . lookup h . requestHeaders $ req
mkIdentifier (IdCookie name)   = \req -> pure $ cookieLookupText name req
mkIdentifier (IdHeaderAndIP h) = RU.byHeaderAndIP h

-- | Cookie lookup via Web.Cookie; ignores empty values.
cookieLookupText :: Text -> Request -> Maybe Text
cookieLookupText n req = do
  raw <- lookup hCookie (requestHeaders req)
  let pairs = WC.parseCookies raw
  v <- lookup (TE.encodeUtf8 n) pairs
  if S.null v then Nothing else Just (TE.decodeUtf8With TEE.lenientDecode v)

-- | Derive IP zone function from a declarative spec.
mkZoneFn :: ZoneBy -> (Request -> IPZoneIdentifier)
mkZoneFn ZoneDefault    = const defaultIPZone
mkZoneFn ZoneIP         = getClientIPPure
mkZoneFn (ZoneHeader h) = \req ->
  maybe defaultIPZone (TE.decodeUtf8With TEE.lenientDecode) (lookup h (requestHeaders req))

-- | Extract client IP with header precedence: X-Forwarded-For, X-Real-IP, then socket.
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

-- | Construct a case-insensitive header name from Text.
hdr :: Text -> HeaderName
hdr = mk . TE.encodeUtf8

-- | Extract original bytes from a case-insensitive header name.
fromHeaderName :: HeaderName -> S.ByteString
fromHeaderName = original

--------------------------------------------------------------------------------
-- Internal helpers: grouping and traversal (to avoid duplicate work)

type ThrottleName = Text
type Grouped = HM.HashMap IdentifierBy [(ThrottleName, ThrottleConfig)]

-- | Group throttles by their IdentifierBy to compute the identifier once per group.
groupByIdentifier :: HM.HashMap ThrottleName ThrottleConfig -> Grouped
groupByIdentifier =
  HM.foldlWithKey' step HM.empty
  where
    step acc name cfg =
      HM.insertWith (++) (throttleIdentifierBy cfg) [(name, cfg)] acc

anyMList :: (a -> IO Bool) -> [a] -> IO Bool
anyMList _ []     = pure False
anyMList f (x:xs) = do
  b <- f x
  if b then pure True else anyMList f xs

anyMHashMap :: (k -> v -> IO Bool) -> HM.HashMap k v -> IO Bool
anyMHashMap f = anyMList (uncurry f) . HM.toList
