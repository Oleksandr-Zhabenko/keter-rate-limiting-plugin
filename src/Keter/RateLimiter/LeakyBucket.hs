{-
Oleksandr Zhabenko added several implementations of the window algorithm: here in the file there is a leaky bucket window implementation using AI chatbots.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Keter.RateLimiter.LeakyBucket
  ( allowRequest
  ) where

import Keter.RateLimiter.Cache (Cache(..), CacheStore(..), InMemoryStore)
import Keter.RateLimiter.LeakyBucketState (LeakyBucketState(..))
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Text (Text)

-- | Leaky Bucket rate limiter
allowRequest
  :: Cache (InMemoryStore "leaky_bucket")
  -> Text   -- ^ Key
  -> Int    -- ^ Capacity
  -> Double -- ^ Leak rate (units per second)
  -> IO Bool
allowRequest cache unprefixedKey capacity leakRate = do
  now <- floor <$> getPOSIXTime
  let key = cachePrefix cache <> ":" <> unprefixedKey
  mstate <- readStore (cacheStore cache) (cachePrefix cache) key

  let state = case mstate of
        Nothing -> LeakyBucketState 0 now
        Just s  -> leak s now

  if level state < fromIntegral capacity
    then do
      let newState = state { level = level state + 1 }
      writeStore (cacheStore cache) (cachePrefix cache) key newState 3600
      return True
    else return False
  where
    leak (LeakyBucketState oldLevel lastTime) nowTime =
      let delta = fromIntegral (nowTime - lastTime) :: Double
          leaked = delta * leakRate
          newLevel = max 0 (oldLevel - leaked)
      in LeakyBucketState newLevel nowTime
