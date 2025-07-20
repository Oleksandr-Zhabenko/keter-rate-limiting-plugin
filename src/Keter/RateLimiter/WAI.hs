{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Keter.RateLimiter.WAI
  ( Env
  , ThrottleConfig(..)
  , initConfig
  , addThrottle
  , attackMiddleware
  , instrument
  , cacheResetAll
  , envZoneCachesMap
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Map.Strict as Map
import Network.Wai
import Network.HTTP.Types (status429)
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Keter.RateLimiter.Cache
import Keter.RateLimiter.IPZones (IPZoneIdentifier, defaultIPZone, ZoneSpecificCaches(..), createZoneCaches)
import qualified Keter.RateLimiter.SlidingWindow as SlidingWindow
import qualified Keter.RateLimiter.TokenBucket as TokenBucket
import qualified Keter.RateLimiter.LeakyBucket as LeakyBucket
import Keter.RateLimiter.RequestUtils
import Keter.RateLimiter.CacheWithZone (allowFixedWindowRequest)
import Data.TinyLRU (allowRequestTinyLRU)
import System.Clock (TimeSpec, Clock(Monotonic), getTime)
import Data.Maybe (fromMaybe)

data ThrottleConfig = ThrottleConfig
  { throttleLimit :: Int
  , throttlePeriod :: Int
  , throttleAlgorithm :: Algorithm
  , throttleIdentifier :: Request -> IO (Maybe Text)
  , throttleTokenBucketTTL :: Maybe Int
  }

data Env = Env
  { envZoneCachesMap :: IORef (Map.Map IPZoneIdentifier ZoneSpecificCaches)
  , envThrottles :: IORef (Map.Map Text ThrottleConfig)
  , envGetRequestIPZone :: Request -> IPZoneIdentifier
  }

initConfig :: (Request -> IPZoneIdentifier) -> IO Env
initConfig getIPZone = do
  defaultCaches <- createZoneCaches
  zoneCachesMap <- newIORef $ Map.singleton defaultIPZone defaultCaches
  throttles <- newIORef Map.empty
  return $ Env zoneCachesMap throttles getIPZone

addThrottle :: Env -> Text -> ThrottleConfig -> IO Env
addThrottle env name config = do
  modifyIORef' (envThrottles env) $ Map.insert name config
  return env

attackMiddleware :: Env -> Application -> Application
attackMiddleware env app req respond = do
  blocked <- instrument env req
  if blocked
    then respond $ responseLBS status429 [] (LBS.fromStrict $ TE.encodeUtf8 "Too Many Requests")
    else app req respond

-- | Gets the cache for a given zone, creating it if it doesn't exist.
getOrCreateZoneCaches :: Env -> IPZoneIdentifier -> IO ZoneSpecificCaches
getOrCreateZoneCaches env zone = do
  readIORef (envZoneCachesMap env) >>= \m ->
    case Map.lookup zone m of
      Just caches -> return caches
      Nothing -> do
        newCaches <- createZoneCaches
        atomicModifyIORef' (envZoneCachesMap env) $ \currentMap ->
          let updatedMap = Map.insertWith (\_ old -> old) zone newCaches currentMap
          in (updatedMap, updatedMap Map.! zone)

instrument :: Env -> Request -> IO Bool
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
          FixedWindow -> not <$> allowFixedWindowRequest (zscCounterCache caches) zone identifier (throttleLimit config) (throttlePeriod config)
          SlidingWindow -> do
            let TimestampStore tvar = cacheStore (zscTimestampCache caches)
            not <$> SlidingWindow.allowRequest tvar zone identifier (throttlePeriod config) (throttleLimit config)
          TokenBucket -> do
            let period = throttlePeriod config
                limit = throttleLimit config
                refillRate = if period > 0 then fromIntegral limit / fromIntegral period else 0.0
            not <$> TokenBucket.allowRequest (zscTokenBucketCache caches) zone identifier limit refillRate (fromMaybe 2 (throttleTokenBucketTTL config))
          LeakyBucket -> do
            let period = throttlePeriod config
                limit = throttleLimit config
                leakRate = if period > 0 then fromIntegral limit / fromIntegral period else 0.0
            not <$> LeakyBucket.allowRequest (zscLeakyBucketCache caches) zone identifier limit leakRate
          TinyLRU -> do
            now <- getTime Monotonic
            case cacheStore (zscTinyLRUCache caches) of
              TinyLRUStore tvar -> do
                cache <- readTVarIO tvar
                not <$> atomically (allowRequestTinyLRU now cache identifier (throttleLimit config) (throttlePeriod config))

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
