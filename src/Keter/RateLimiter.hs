{-|
Copyright (c) 2025 Oleksandr Zhabenko
  
This file is a ported to Haskell language code with some simlifications of rack-attack 
https://github.com/rack/rack-attack/blob/main/lib/rack/attack.rb
and is based on the structure of the original code of 
rack-attack, Copyright (c) 2016 by Kickstarter, PBC, under the MIT License.
Oleksandr Zhabenko added several implementations of the window algorithm: sliding window, token bucket window, leaky bucket window alongside with the initial count algorithm using AI chatbots.
IP Zone functionality added to allow separate caches per IP zone.

This implementation is released under the MIT License.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

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
  , ZoneSpecificCaches(..)
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text (Text)

import Keter.RateLimiter.Cache
  ( Cache(..)
  , InMemoryStore
  , incStore
  )

-- Import IP Zone functionality
import Keter.RateLimiter.IPZones
  ( IPZoneIdentifier
  , defaultIPZone
  , ZoneSpecificCaches(..)
  , createZoneCaches
  , resetSingleZoneCaches
  )

import qualified Keter.RateLimiter.SlidingWindow as SW
import qualified Keter.RateLimiter.TokenBucket as TB
import qualified Keter.RateLimiter.LeakyBucket as LB
import qualified Keter.RateLimiter.Notifications as Notifications

-- | Application environment with configuration and zone-specific caches
data Env = Env
  { envConfig        :: Configuration
  -- | A map from IP zone identifiers to their specific caches
  , envZoneCachesMap :: Map IPZoneIdentifier ZoneSpecificCaches
  -- | Function to determine the IP zone for a request
  , envGetIPZone     :: Request -> IPZoneIdentifier
  }

type Response = Text
type App = IO

data Request = Request
  { requestMethod  :: Text
  , requestPath    :: Text
  , requestHost    :: Text
  , requestIP      :: Text
  , requestHeaders :: Map Text Text
  } deriving (Show, Eq)

data Algorithm = FixedWindow | SlidingWindow | TokenBucket | LeakyBucket deriving (Show, Eq)

data Configuration = Configuration
  { configThrottles :: Map Text ThrottleConfig
  , configNotifier  :: Notifications.Notifier
  -- | List of IP Zone Identifiers to initialize caches for.
  -- The 'defaultIPZone' is always created.
  , configIPZones   :: [IPZoneIdentifier]
  }

data ThrottleConfig = ThrottleConfig
  { throttleLimit     :: Int
  , throttlePeriod    :: Int
  , throttleCondition :: Request -> Bool
  , throttleKeyFn     :: Request -> Text
  , throttleAlgorithm :: Algorithm
  }

defaultConfiguration :: Configuration
defaultConfiguration = Configuration
  { configThrottles = Map.empty
  , configNotifier = Notifications.noopNotifier
  , configIPZones = [] -- No extra zones by default
  }

-- | Initialize configuration with IP zone support
-- User-defined function to get the IP zone for a request.
-- This needs to be implemented by the user of the library and passed to 'initConfig'.
-- Example structure:
--
-- > myGetRequestIPZone :: Request -> IPZoneIdentifier
-- > myGetRequestIPZone req =
-- >   if requestIP req `elem` ["127.0.0.1", "::1"]
-- >   then "localhost_zone"
-- >   else if T.isPrefixOf "192.168." (requestIP req)
-- >   then "internal_zone"
-- >   else defaultIPZone
initConfig :: Configuration
           -> (Request -> IPZoneIdentifier) -- ^ Function to determine IP zone
           -> IO Env
initConfig config getIPZoneFn = do
  -- Create caches for the default zone
  defaultCaches <- createZoneCaches
  let initialMap = Map.singleton defaultIPZone defaultCaches

  -- Create caches for other specified zones
  additionalZoneCachesList <- mapM (\zoneId -> (,) zoneId <$> createZoneCaches) (configIPZones config)
  let allZoneCachesMap = Map.union (Map.fromList additionalZoneCachesList) initialMap

  return $ Env
    { envConfig = config
    , envZoneCachesMap = allZoneCachesMap
    , envGetIPZone = getIPZoneFn
    }

addThrottle :: Text -> ThrottleConfig -> Configuration -> Configuration
addThrottle name cfg conf =
  conf { configThrottles = Map.insert name cfg (configThrottles conf) }

-- | Helper to get the caches for a given request
getCachesForRequest :: Env -> Request -> ZoneSpecificCaches
getCachesForRequest env req =
  let zoneId = envGetIPZone env req
      zoneMap = envZoneCachesMap env
  -- Prefer the specific zone's caches, fall back to default if the zoneId isn't in the map
  in fromMaybe (zoneMap Map.! defaultIPZone) (Map.lookup zoneId zoneMap)

attackMiddleware :: Env -> Request -> App Response -> App Response
attackMiddleware env req app = do
  let zoneCaches = getCachesForRequest env req -- Get caches specific to the request's IP zone
  blocked <- checkThrottles (envConfig env) zoneCaches req
  if blocked
    then return "Rate limit exceeded"
    else instrument env req app

instrument :: Env -> Request -> App Response -> App Response
instrument _env _req app = app  -- Placeholder for actual instrumentation logic

checkThrottles :: Configuration -> ZoneSpecificCaches -> Request -> App Bool
checkThrottles config zoneCaches req = do
  let throttles = configThrottles config
  results <- mapM (checkThrottle config zoneCaches req) (Map.toList throttles)
  return $ or results

checkThrottle :: Configuration -> ZoneSpecificCaches -> Request -> (Text, ThrottleConfig) -> App Bool
checkThrottle config zoneCaches req (name, throttleCfg) =
  if not (throttleCondition throttleCfg req)
    then return False
    else do
      let key = throttleKeyFn throttleCfg req
          fullKey = name <> ":" <> key
      isBlocked <- case throttleAlgorithm throttleCfg of
        FixedWindow -> do
          -- Use incStore directly with the throttle's period
          -- This ensures the cache entry for the counter respects the rule's specific period for expiration.
          currentCount <- incStore (zscCounterCache zoneCaches) fullKey (throttlePeriod throttleCfg)
          return (currentCount > fromIntegral (throttleLimit throttleCfg))

        SlidingWindow -> do
          allowed <- SW.allowRequest
            (zscTimestampCache zoneCaches)
            fullKey
            (throttleLimit throttleCfg)
            (throttlePeriod throttleCfg)
          return (not allowed)

        TokenBucket -> do
          let refillRate = fromIntegral (throttleLimit throttleCfg) / fromIntegral (throttlePeriod throttleCfg)
          allowed <- TB.allowRequest
            (zscTokenBucketCache zoneCaches)
            fullKey
            (throttleLimit throttleCfg)
            refillRate
          return (not allowed)

        LeakyBucket -> do
          let leakRate = fromIntegral (throttleLimit throttleCfg) / fromIntegral (throttlePeriod throttleCfg)
          allowed <- LB.allowRequest
            (zscLeakyBucketCache zoneCaches)
            fullKey
            (throttleLimit throttleCfg)
            leakRate
          return (not allowed)

      when isBlocked $
        Notifications.notify
          (configNotifier config)
          name
          req
          (throttleLimit throttleCfg)

      return isBlocked

cacheResetAll :: Env -> IO ()
cacheResetAll env = do
  -- Reset caches for all configured zones by iterating through the map values
  mapM_ resetSingleZoneCaches (Map.elems $ envZoneCachesMap env)

-- | Helper
when :: Monad m => Bool -> m () -> m ()
when p action = if p then action else return ()
