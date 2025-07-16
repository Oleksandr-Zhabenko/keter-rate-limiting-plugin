{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Keter.RateLimiter.WAI
Description : Comprehensive rate limiting middleware with IP zone support and convenient/customisable key management
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : stable
Portability : portable
-}

module Keter.RateLimiter.WAI
  ( Env(..)
  , attackMiddleware
  , instrument
  , Configuration(..)
  , defaultConfiguration
  , initConfig
  , addThrottle
  , ThrottleConfig(..)
  , cacheResetAll
  , IPZoneIdentifier
  , defaultIPZone
  , ZoneSpecificCaches
  , simpleIPRateLimit
  , apiRateLimit
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.Wai (Request, Response, Middleware, ResponseReceived)
import qualified Network.Wai as WAI
import Network.HTTP.Types (Status, status429)
import Data.IP (fromHostAddress, fromHostAddress6)
import Debug.Trace (traceM)
import qualified Keter.RateLimiter.Notifications as Notifications
import Keter.RateLimiter.IPZones (IPZoneIdentifier, defaultIPZone, ZoneSpecificCaches(..), newZoneSpecificCaches, resetSingleZoneCaches)
import Keter.RateLimiter.Cache (Cache(..), Algorithm(..), makeCacheKey, incStoreWithZone)
import Keter.RateLimiter.SlidingWindow as SW (allowRequest)
import Keter.RateLimiter.TokenBucket as TB (allowRequest)
import Keter.RateLimiter.LeakyBucket as LB (allowRequest)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Control.Monad (when)
import Keter.RateLimiter.RequestUtils (getClientIP, getRequestPath, getRequestMethod, byIP, byIPAndPath, byIPAndUserAgent, byHeaderAndIP)

-- | Environment holding configuration and caches per IP zone.
data Env = Env
  { envConfig :: Configuration
  , envZoneCachesMap :: IORef (Map IPZoneIdentifier ZoneSpecificCaches)
  }

-- | Configuration for the rate limiter
data Configuration = Configuration
  { configThrottles :: Map Text ThrottleConfig
  , configNotifier :: Notifications.WAINotifier
  , configGetRequestIPZone :: Request -> IPZoneIdentifier
  , configRateLimitResponse :: Request -> Response
  }

-- | For TokenBucket, use 'throttleTokenBucketTTL' to specify cache TTL (in seconds).
data ThrottleConfig = ThrottleConfig
  { throttleLimit :: Int
  , throttlePeriod :: Int
  , throttleAlgorithm :: Algorithm
  , throttleIdentifier :: Request -> Maybe Text
  , throttleTokenBucketTTL :: Maybe Int
  }

-- | Default rate limit response (429 Too Many Requests)
defaultRateLimitResponse :: Request -> Response
defaultRateLimitResponse _ = WAI.responseLBS 
  status429 
  [("Content-Type", "text/plain")]
  "Too Many Requests"

defaultConfiguration :: Configuration
defaultConfiguration = Configuration
  { configThrottles = Map.empty
  , configNotifier = Notifications.consoleWAINotifier
  , configGetRequestIPZone = const defaultIPZone
  , configRateLimitResponse = defaultRateLimitResponse
  }

initConfig :: (Request -> IPZoneIdentifier) -> IO Env
initConfig getIPZone = do
  let config = defaultConfiguration { configGetRequestIPZone = getIPZone }
  cachesRef <- newIORef Map.empty
  return Env { envConfig = config, envZoneCachesMap = cachesRef }

addThrottle :: Env -> Text -> ThrottleConfig -> Env
addThrottle env name cfg =
  let newThrottles = Map.insert name cfg (configThrottles $ envConfig env)
  in env { envConfig = (envConfig env) { configThrottles = newThrottles } }

attackMiddleware :: Env -> Middleware
attackMiddleware env app req respond = do
  blocked <- instrument env req
  if blocked
    then respond (configRateLimitResponse (envConfig env) req)
    else app req respond

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

checkWithZone :: Env -> Text -> ThrottleConfig -> Request -> IO (Bool, Env)
checkWithZone env throttleName throttleCfg req = do
  case throttleIdentifier throttleCfg req of
    Nothing -> return (False, env)
    Just userKey -> do
      let config = envConfig env
      -- FIXED: Use the configured function to get the IP zone, not a hardcoded call.
      let ipZone = configGetRequestIPZone config req
      cachesMap <- readIORef (envZoneCachesMap env)
      zoneCaches <- case Map.lookup ipZone cachesMap of
        Just caches -> return caches
        Nothing -> do
          newCaches <- newZoneSpecificCaches
          modifyIORef' (envZoneCachesMap env) (Map.insert ipZone newCaches)
          return newCaches
      let fullKey = makeCacheKey (throttleAlgorithm throttleCfg) ipZone userKey
      traceM $ "Checking throttle: " ++ T.unpack throttleName ++ ", Key: " ++ T.unpack fullKey
      isBlocked <- case throttleAlgorithm throttleCfg of
        FixedWindow -> do
          currentCount <- incStoreWithZone (zscCounterCache zoneCaches) ipZone userKey (throttlePeriod throttleCfg)
          return (currentCount > throttleLimit throttleCfg)
        SlidingWindow -> do
          allowed <- SW.allowRequest
            (zscTimestampCache zoneCaches)
            ipZone
            userKey
            (throttlePeriod throttleCfg)
            (throttleLimit throttleCfg)
          return (not allowed)
        TokenBucket -> do
          let refillRate = fromIntegral (throttleLimit throttleCfg) / fromIntegral (throttlePeriod throttleCfg) :: Double
              ttl = maybe 7200 id (throttleTokenBucketTTL throttleCfg)
          allowed <- TB.allowRequest
            (zscTokenBucketCache zoneCaches)
            ipZone
            userKey
            (throttleLimit throttleCfg)
            refillRate
            ttl
          return (not allowed)
        LeakyBucket -> do
          let leakRate = ceiling (fromIntegral (throttleLimit throttleCfg) / fromIntegral (throttlePeriod throttleCfg) :: Double) :: Int
          allowed <- LB.allowRequest
            (zscLeakyBucketCache zoneCaches)
            ipZone
            userKey
            (throttleLimit throttleCfg)
            leakRate
          return (not allowed)
        TinyLRU -> do
          currentCount <- incStoreWithZone (zscTinyLRUCache zoneCaches) ipZone userKey (throttlePeriod throttleCfg)
          return (currentCount > throttleLimit throttleCfg)
      when isBlocked $
        Notifications.notifyWAI
          (configNotifier config)
          throttleName
          req
          (throttleLimit throttleCfg)
      return (isBlocked, env)

-- | Reset all caches in the environment
cacheResetAll :: Env -> IO ()
cacheResetAll env = do
  cachesMap <- readIORef (envZoneCachesMap env)
  mapM_ resetSingleZoneCaches (Map.elems cachesMap)

simpleIPRateLimit :: Int -> Int -> IO Env
simpleIPRateLimit limit period = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig
        { throttleLimit = limit
        , throttlePeriod = period
        , throttleAlgorithm = FixedWindow
        , throttleIdentifier = byIP
        , throttleTokenBucketTTL = Nothing
        }
  return $ addThrottle env "ip_limit" throttle

apiRateLimit :: Int -> Int -> IO Env
apiRateLimit limit period = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig
        { throttleLimit = limit
        , throttlePeriod = period
        , throttleAlgorithm = SlidingWindow
        , throttleIdentifier = byIPAndPath
        , throttleTokenBucketTTL = Nothing
        }
  return $ addThrottle env "api_limit" throttle
