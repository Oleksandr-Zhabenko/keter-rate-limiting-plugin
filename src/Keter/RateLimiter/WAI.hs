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
'ZoneSpecificCaches' abstraction.

Use `attackMiddleware` to enforce throttling in your WAI application.

-}

module Keter.RateLimiter.WAI
  ( -- * Environment & Configuration
    Env
  , ThrottleConfig(..)
  , initConfig
  , addThrottle
    -- * Middleware
  , attackMiddleware
    -- * Manual Control & Inspection
  , instrument
  , cacheResetAll
    -- * Accessors
  , envZoneCachesMap
  ) where

import Data.Text (Text)
--import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Map.Strict as Map
import Network.Wai
import Network.HTTP.Types (status429)
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Control.Concurrent.STM
--import Control.Monad.IO.Class (liftIO)
import Keter.RateLimiter.Cache
import Keter.RateLimiter.IPZones (IPZoneIdentifier, defaultIPZone, ZoneSpecificCaches(..), createZoneCaches)
import qualified Keter.RateLimiter.SlidingWindow as SlidingWindow
import qualified Keter.RateLimiter.TokenBucket as TokenBucket
import qualified Keter.RateLimiter.LeakyBucket as LeakyBucket
--import Keter.RateLimiter.RequestUtils
import Keter.RateLimiter.CacheWithZone (allowFixedWindowRequest)
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

-- | Global environment for throttling, including zone-specific cache maps and throttle configurations.
data Env = Env
  { envZoneCachesMap :: IORef (Map.Map IPZoneIdentifier ZoneSpecificCaches)
    -- ^ Map from IP zones to rate limiter caches.
  , envThrottles :: IORef (Map.Map Text ThrottleConfig)
    -- ^ Registered throttle rules by name.
  , envGetRequestIPZone :: Request -> IPZoneIdentifier
    -- ^ Function to derive an IP zone from the incoming WAI request.
  }

--------------------------------------------------------------------------------

-- | Initialize the rate-limiter environment with a default zone and no throttles.
initConfig
  :: (Request -> IPZoneIdentifier)
  -- ^ Function to extract an IP zone label from the request.
  -> IO Env
initConfig getIPZone = do
  defaultCaches <- createZoneCaches
  zoneCachesMap <- newIORef $ Map.singleton defaultIPZone defaultCaches
  throttles <- newIORef Map.empty
  return $ Env zoneCachesMap throttles getIPZone

-- | Register a new named throttle rule in the environment.
addThrottle
  :: Env
  -> Text             -- ^ Throttle rule name (must be unique).
  -> ThrottleConfig   -- ^ Throttle configuration.
  -> IO Env
addThrottle env name config = do
  modifyIORef' (envThrottles env) $ Map.insert name config
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
instrument
  :: Env
  -> Request
  -> IO Bool
instrument env req = do
  throttles <- readIORef (envThrottles env)
  let zone = envGetRequestIPZone env req
  zoneCaches <- getOrCreateZoneCaches env zone
  anyBlocked <- or <$> mapM (checkThrottle zoneCaches zone req) (Map.elems throttles)
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
cacheResetAll :: Env -> IO ()
cacheResetAll env = do
  zoneCachesMap <- readIORef (envZoneCachesMap env)
  mapM_ (resetZoneCaches . snd) (Map.toList zoneCachesMap)
  where
    resetZoneCaches :: ZoneSpecificCaches -> IO ()
    resetZoneCaches caches = do
      cacheReset (zscCounterCache caches)
      cacheReset (zscTimestampCache caches)
      cacheReset (zscTokenBucketCache caches)
      cacheReset (zscLeakyBucketCache caches)
      cacheReset (zscTinyLRUCache caches)

-- | Retrieve or create the set of caches for a specific IP zone.
getOrCreateZoneCaches
  :: Env
  -> IPZoneIdentifier
  -> IO ZoneSpecificCaches
getOrCreateZoneCaches env zone = do
  readIORef (envZoneCachesMap env) >>= \m ->
    case Map.lookup zone m of
      Just caches -> return caches
      Nothing -> do
        newCaches <- createZoneCaches
        atomicModifyIORef' (envZoneCachesMap env) $ \currentMap ->
          let updatedMap = Map.insertWith (\_ old -> old) zone newCaches currentMap
          in (updatedMap, updatedMap Map.! zone)
