{-|
Copyright (c) 2025 Oleksandr Zhabenko
  
This file is a ported to Haskell language code with some simlifications of rack-attack 
https://github.com/rack/rack-attack/blob/main/lib/rack/attack.rb
and is based on the structure of the original code of 
rack-attack, Copyright (c) 2016 by Kickstarter, PBC, under the MIT License.
Oleksandr Zhabenko added several implementations of the window algorithm: sliding window, token bucket window, leaky bucket window alongside with the initial count algorithm using AI chatbots.

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
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text (Text)

import Keter.RateLimiter.Cache
  ( Cache(..)
  , InMemoryStore
  , count
  , reset
  , newCache
  , createInMemoryStore
  )

import qualified Keter.RateLimiter.SlidingWindow as SW
import qualified Keter.RateLimiter.TokenBucket as TB
import qualified Keter.RateLimiter.LeakyBucket as LB
import qualified Keter.RateLimiter.Notifications as Notifications

-- | Application environment with configuration and all required caches
data Env = Env
  { envConfig           :: Configuration
  , envCounterCache     :: Cache (InMemoryStore "counter")
  , envTimestampCache   :: Cache (InMemoryStore "timestamps")
  , envTokenBucketCache :: Cache (InMemoryStore "token_bucket")
  , envLeakyBucketCache :: Cache (InMemoryStore "leaky_bucket")   -- Added
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

-- Add LeakyBucket to Algorithm
data Algorithm = FixedWindow | SlidingWindow | TokenBucket | LeakyBucket deriving (Show, Eq)

data Configuration = Configuration
  { configThrottles :: Map Text ThrottleConfig
  , configNotifier  :: Notifications.Notifier
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
  }

initConfig :: Configuration -> IO Env
initConfig config = do
  counterStore <- createInMemoryStore
  timestampStore <- createInMemoryStore
  tokenBucketStore <- createInMemoryStore
  leakyBucketStore <- createInMemoryStore       -- Added
  let counterCache     = newCache "rate_limiter" counterStore
      timestampCache   = newCache "timestamps" timestampStore
      tokenBucketCache = newCache "token_bucket" tokenBucketStore
      leakyBucketCache = newCache "leaky_bucket" leakyBucketStore   -- Added
  return $ Env
    { envConfig = config
    , envCounterCache = counterCache
    , envTimestampCache = timestampCache
    , envTokenBucketCache = tokenBucketCache
    , envLeakyBucketCache = leakyBucketCache         -- Added
    }

addThrottle :: Text -> ThrottleConfig -> Configuration -> Configuration
addThrottle name cfg conf =
  conf { configThrottles = Map.insert name cfg (configThrottles conf) }

attackMiddleware :: Env -> Request -> App Response -> App Response
attackMiddleware env req app = do
  blocked <- checkThrottles env req
  if blocked
    then return "Rate limit exceeded"
    else instrument env req app

instrument :: Env -> Request -> App Response -> App Response
instrument _env _req app = app  -- Placeholder

checkThrottles :: Env -> Request -> App Bool
checkThrottles env req = do
  let throttles = configThrottles (envConfig env)
  results <- mapM (checkThrottle env req) (Map.toList throttles)
  return $ or results

checkThrottle :: Env -> Request -> (Text, ThrottleConfig) -> App Bool
checkThrottle env req (name, config) =
  if not (throttleCondition config req)
    then return False
    else do
      let key = throttleKeyFn config req
          fullKey = name <> ":" <> key
      isBlocked <- case throttleAlgorithm config of
        FixedWindow -> do
          currentCount <- count (envCounterCache env) fullKey
          return (currentCount > fromIntegral (throttleLimit config))

        SlidingWindow -> do
          allowed <- SW.allowRequest
            (envTimestampCache env)
            fullKey
            (throttleLimit config)
            (throttlePeriod config)
          return (not allowed)

        TokenBucket -> do
          let refillRate = fromIntegral (throttleLimit config) / fromIntegral (throttlePeriod config)
          allowed <- TB.allowRequest
            (envTokenBucketCache env)
            fullKey
            (throttleLimit config)
            refillRate
          return (not allowed)

        LeakyBucket -> do
          let leakRate = fromIntegral (throttleLimit config) / fromIntegral (throttlePeriod config)
          allowed <- LB.allowRequest
            (envLeakyBucketCache env)
            fullKey
            (throttleLimit config)
            leakRate
          return (not allowed)

      when isBlocked $
        Notifications.notify
          (configNotifier (envConfig env))
          name
          req
          (throttleLimit config)

      return isBlocked

-- | Helper
when :: Monad m => Bool -> m () -> m ()
when p action = if p then action else return ()
