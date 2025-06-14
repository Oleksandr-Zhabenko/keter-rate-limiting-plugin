{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

{-|
Module      : Keter.RateLimiter
Description : Comprehensive rate limiting middleware with IP zone support and convenient/customisable key management
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : stable
Portability : portable

This file is a ported to Haskell language code with some simplifications of rack-attack 
https://github.com/rack/rack-attack/blob/main/lib/rack/attack.rb
and is based on the structure of the original code of 
rack-attack, Copyright (c) 2016 by Kickstarter, PBC, under the MIT License.
Oleksandr Zhabenko added several implementations of the window algorithm: sliding window, token bucket window, leaky bucket window alongside with the initial count algorithm using AI chatbots.
IP Zone functionality added to allow separate caches per IP zone.

This implementation is released under the MIT License.

This module provides a rack-attack inspired rate limiting middleware with IP zone support.
It allows convenient configuration with automatic key composition, but also supports full customisation
for advanced users who wish to control key structure or throttling logic.

== Features

* Middleware integration via 'attackMiddleware'
* Support for multiple algorithms selectable per throttle
* Dynamic throttle configuration with 'addThrottle'
* Per-IP-zone cache management
* Notifications on rate limit events
* Cache reset utilities
* Convenient key management wrappers (recommended for most use cases)
* Full customisability for advanced scenarios

== Usage

1. Initialise environment with 'initConfig', providing a function to extract IP zones from requests.
2. Add throttles with 'addThrottle'.
3. Use 'attackMiddleware' to enforce rate limiting.
4. Use 'cacheResetAll' to clear caches if needed.
5. For most use cases, use the convenient key wrappers; for advanced control, use the base functions and compose keys as you wish.

-}

module Keter.RateLimiter
  ( Env(..)
  , Response
  , App
  , Request(..)
  , attackMiddleware
  , instrument
  , Configuration(..)
  , defaultConfiguration
  , initConfig
  , addThrottle
  , ThrottleConfig(..)
  , Algorithm(..)
  , cacheResetAll
  , IPZoneIdentifier
  , defaultIPZone
  , ZoneSpecificCaches
  -- Wrappers for convenience (re-exported from Cache)
  , makeCacheKey
  , incStoreWithZone
  , readCacheWithZone
  , writeCacheWithZone
  , deleteCacheWithZone
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Keter.RateLimiter.Notifications as Notifications
import Keter.RateLimiter.IPZones (IPZoneIdentifier, defaultIPZone, ZoneSpecificCaches(..), newZoneSpecificCaches, resetSingleZoneCaches)
import Keter.RateLimiter.Cache
import qualified Keter.RateLimiter.SlidingWindow as SW
import qualified Keter.RateLimiter.TokenBucket as TB
import qualified Keter.RateLimiter.LeakyBucket as LB
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Control.Monad (when)

-- | Environment holding configuration and caches per IP zone.
data Env = Env
  { envConfig :: Configuration
  , envZoneCachesMap :: IORef (Map IPZoneIdentifier ZoneSpecificCaches)
  }

data Configuration = Configuration
  { configThrottles :: Map Text ThrottleConfig
  , configNotifier :: Notifications.Notifier
  , configGetRequestIPZone :: Request -> IPZoneIdentifier
  }

-- | For TokenBucket, use 'throttleTokenBucketTTL' to specify cache TTL (in seconds).
data ThrottleConfig = ThrottleConfig
  { throttleLimit :: Int
  , throttlePeriod :: Int
  , throttleAlgorithm :: Algorithm
  , throttleIdentifier :: Request -> Maybe Text
  , throttleTokenBucketTTL :: Maybe Int -- ^ Only used for TokenBucket, default: 7200 (2 hours)
  }

data Algorithm = FixedWindow | SlidingWindow | TokenBucket | LeakyBucket
  deriving (Show, Eq, Generic)

data Request = Request
  { requestMethod :: Text
  , requestPath :: Text
  , requestHost :: Text
  , requestIP :: Text
  , requestHeaders :: Map Text Text
  } deriving (Show)

type Response = Text
type App = Request -> IO Response

-- 1. Ініціалізація Env з IORef
initConfig :: (Request -> IPZoneIdentifier) -> IO Env
initConfig getIPZone = do
  let config = defaultConfiguration { configGetRequestIPZone = getIPZone }
  cachesRef <- newIORef Map.empty
  return Env { envConfig = config, envZoneCachesMap = cachesRef }

defaultConfiguration :: Configuration
defaultConfiguration = Configuration
  { configThrottles = Map.empty
  , configNotifier = Notifications.consoleNotifier
  , configGetRequestIPZone = const defaultIPZone
  }

addThrottle :: Env -> Text -> ThrottleConfig -> Env
addThrottle env name cfg =
  let newThrottles = Map.insert name cfg (configThrottles $ envConfig env)
  in env { envConfig = (envConfig env) { configThrottles = newThrottles } }

attackMiddleware :: Env -> App -> App
attackMiddleware env app req = do
  blocked <- instrument env req
  if blocked
    then return "Too Many Requests"
    else app req

instrument :: Env -> Request -> IO Bool
instrument initialEnv req = do
  let throttles = Map.toList (configThrottles $ envConfig initialEnv)
  let go :: Bool -> Env -> [(Text, ThrottleConfig)] -> IO (Bool, Env)
      go currentBlocked currentEnv [] = return (currentBlocked, currentEnv)
      go currentBlocked currentEnv ((name, throttleCfg):remainingThrottles) = do
        if currentBlocked
          then return (True, currentEnv)
          else do
            (blocked, updatedEnv) <- checkWithZone currentEnv name throttleCfg req
            go blocked updatedEnv remainingThrottles
  (isBlocked, _) <- go False initialEnv throttles
  return isBlocked

-- | Check a single throttle against a request, updating caches and notifying if blocked.
--   Uses convenient key composition by default, but can be replaced for advanced scenarios.
checkWithZone :: Env -> Text -> ThrottleConfig -> Request -> IO (Bool, Env)
checkWithZone env throttleName throttleCfg req =
  case throttleIdentifier throttleCfg req of
    Nothing -> return (False, env)
    Just userKey -> do
      let config = envConfig env
          ipZone = configGetRequestIPZone config req
      cachesMap <- readIORef (envZoneCachesMap env)
      zoneCaches <- case Map.lookup ipZone cachesMap of
        Just caches -> return caches
        Nothing -> do
          newCaches <- newZoneSpecificCaches
          modifyIORef' (envZoneCachesMap env) (Map.insert ipZone newCaches)
          return newCaches
      let fullKey = makeCacheKey throttleName ipZone userKey
      isBlocked <- case throttleAlgorithm throttleCfg of
        FixedWindow -> do
          currentCount <- incStore (zscCounterCache zoneCaches) fullKey (throttlePeriod throttleCfg)
          return (currentCount > throttleLimit throttleCfg)
        SlidingWindow -> do
          allowed <- SW.allowRequest
            (zscTimestampCache zoneCaches)
            fullKey
            (throttleLimit throttleCfg)
            (throttlePeriod throttleCfg)
          return (not allowed)
        TokenBucket -> do
          let refillRate = fromIntegral (throttleLimit throttleCfg) / fromIntegral (throttlePeriod throttleCfg)
              ttl = maybe 7200 id (throttleTokenBucketTTL throttleCfg) -- Default 2 hours if not set
          allowed <- TB.allowRequest
            (zscTokenBucketCache zoneCaches)
            fullKey
            (throttleLimit throttleCfg)
            refillRate
            ttl
          return (not allowed)
        LeakyBucket -> do
          let leakRate = fromIntegral (throttleLimit throttleCfg) / fromIntegral (throttlePeriod throttleCfg)
              ttl = throttlePeriod throttleCfg
          allowed <- LB.allowRequest
            (zscLeakyBucketCache zoneCaches)
            fullKey
            (throttleLimit throttleCfg)
            leakRate
            ttl
          return (not allowed)
      when isBlocked $
        Notifications.notify
          (configNotifier config)
          throttleName
          req
          (throttleLimit throttleCfg)
      return (isBlocked, env)

cacheResetAll :: Env -> IO ()
cacheResetAll env = do
  cachesMap <- readIORef (envZoneCachesMap env)
  mapM_ resetSingleZoneCaches (Map.elems cachesMap)
