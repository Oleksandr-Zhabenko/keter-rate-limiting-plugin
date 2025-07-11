{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

{-|
Module      : Keter.RateLimiter.WAI
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

WAI-compatible version of the rate limiter that works with standard WAI Request/Response types.

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
  -- For convenience (re-exported from Cache)
  , Algorithm(..)
  , algorithmPrefix
  , makeCacheKey
  , incStoreWithZone
  , readCacheWithZone
  , writeCacheWithZone
  , deleteCacheWithZone
  -- WAI-specific helpers
  , getClientIP
  , getRequestPath
  , getRequestMethod
  , byIP
  , byIPAndPath
  , byIPAndUserAgent
  , byHeaderAndIP
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import GHC.Generics (Generic)
import Network.Wai (Request, Response, Middleware, ResponseReceived)
import qualified Network.Wai as WAI
import Network.HTTP.Types (Status, status429)
import Network.HTTP.Types.Header (HeaderName)
import Network.Socket (SockAddr(..))
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

-- | Configuration for the rate limiter
data Configuration = Configuration
  { configThrottles :: Map Text ThrottleConfig
  , configNotifier :: Notifications.WAINotifier  -- Updated for WAI
  , configGetRequestIPZone :: Request -> IPZoneIdentifier
  , configRateLimitResponse :: Request -> Response  -- Customizable response
  }

-- | For TokenBucket, use 'throttleTokenBucketTTL' to specify cache TTL (in seconds).
data ThrottleConfig = ThrottleConfig
  { throttleLimit :: Int
  , throttlePeriod :: Int
  , throttleAlgorithm :: Algorithm
  , throttleIdentifier :: Request -> Maybe Text  -- Now takes WAI Request
  , throttleTokenBucketTTL :: Maybe Int -- ^ Only used for TokenBucket, default: 7200 (2 hours)
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

-- 1. Initialization of Env with IORef
initConfig :: (Request -> IPZoneIdentifier) -> IO Env
initConfig getIPZone = do
  let config = defaultConfiguration { configGetRequestIPZone = getIPZone }
  cachesRef <- newIORef Map.empty
  return Env { envConfig = config, envZoneCachesMap = cachesRef }

addThrottle :: Env -> Text -> ThrottleConfig -> Env
addThrottle env name cfg =
  let newThrottles = Map.insert name cfg (configThrottles $ envConfig env)
  in env { envConfig = (envConfig env) { configThrottles = newThrottles } }

-- | WAI Middleware that applies rate limiting
attackMiddleware :: Env -> Middleware
attackMiddleware env app req respond = do
  blocked <- instrument env req
  if blocked
    then respond (configRateLimitResponse (envConfig env) req)
    else app req respond

-- | Check if request should be blocked by rate limiting
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
      let fullKey = makeCacheKey (throttleAlgorithm throttleCfg) ipZone userKey
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
        TinyLRU -> do
          currentCount <- incStore (zscTinyLRUCache zoneCaches) fullKey (throttlePeriod throttleCfg)
          return (currentCount > throttleLimit throttleCfg)
      when isBlocked $
        Notifications.notifyWAI
          (configNotifier config)
          throttleName
          req
          (throttleLimit throttleCfg)
      return (isBlocked, env)

cacheResetAll :: Env -> IO ()
cacheResetAll env = do
  cachesMap <- readIORef (envZoneCachesMap env)
  mapM_ resetSingleZoneCaches (Map.elems cachesMap)

-- | Helper functions for extracting data from WAI Request

-- | Extract client IP from WAI Request
getClientIP :: Request -> Text
getClientIP req = 
  case lookup "x-forwarded-for" (WAI.requestHeaders req) of
    Just xff -> T.takeWhile (/= ',') $ TE.decodeUtf8 xff  -- Take first IP from X-Forwarded-For
    Nothing -> case lookup "x-real-ip" (WAI.requestHeaders req) of
      Just realIP -> TE.decodeUtf8 realIP
      Nothing -> case WAI.remoteHost req of
        SockAddrInet _ addr -> T.pack $ show addr
        SockAddrInet6 _ _ addr _ -> T.pack $ show addr  
        SockAddrUnix path -> T.pack path
        _ -> "unknown"

-- | Extract request path
getRequestPath :: Request -> Text
getRequestPath = TE.decodeUtf8 . WAI.rawPathInfo

-- | Extract request method  
getRequestMethod :: Request -> Text
getRequestMethod = TE.decodeUtf8 . WAI.requestMethod

-- | Extract host header
getRequestHost :: Request -> Maybe Text
getRequestHost req = TE.decodeUtf8 <$> WAI.requestHeaderHost req

-- | Extract user agent
getRequestUserAgent :: Request -> Maybe Text  
getRequestUserAgent req = TE.decodeUtf8 <$> WAI.requestHeaderUserAgent req

-- | Common throttle identifier functions

-- | Throttle by IP address
byIP :: Request -> Maybe Text
byIP = Just . getClientIP

-- | Throttle by IP + Path
byIPAndPath :: Request -> Maybe Text
byIPAndPath req = Just $ getClientIP req <> ":" <> getRequestPath req

-- | Throttle by IP + User Agent (useful for bot detection)
byIPAndUserAgent :: Request -> Maybe Text  
byIPAndUserAgent req = do
  ua <- getRequestUserAgent req
  return $ getClientIP req <> ":" <> ua

-- | Throttle by custom header value + IP
byHeaderAndIP :: HeaderName -> Request -> Maybe Text
byHeaderAndIP headerName req = do
  headerValue <- lookup headerName (WAI.requestHeaders req)
  return $ getClientIP req <> ":" <> TE.decodeUtf8 headerValue

-- | Example usage functions

-- | Create a simple IP-based rate limiter
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

-- | Create an API rate limiter (by IP + path)
apiRateLimit :: Int -> Int -> IO Env  
apiRateLimit limit period = do
  env <- initConfig (const defaultIPZone)
  let throttle = ThrottleConfig
        { throttleLimit = limit
        , throttlePeriod = period
        , throttleAlgorithm = SlidingWindow  -- More accurate for APIs
        , throttleIdentifier = byIPAndPath
        , throttleTokenBucketTTL = Nothing
        }
  return $ addThrottle env "api_limit" throttle
