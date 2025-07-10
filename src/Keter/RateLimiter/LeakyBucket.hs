{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Keter.RateLimiter.LeakyBucket
Description : Leaky bucket rate limiting algorithm implementation
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : stable
Portability : portable

This module implements the leaky bucket rate limiting algorithm using a cache-backed state.

== Overview

The leaky bucket algorithm controls the rate of requests by simulating a bucket that leaks at a constant rate.
Requests add to the bucket's level, and if the level exceeds the capacity, further requests are denied until the bucket leaks enough.

This implementation stores the bucket state in a cache, allowing distributed or persistent rate limiting.

== Key function

- 'allowRequest' attempts to allow a request identified by a key.
  It updates the bucket state accordingly and returns 'True' if the request is allowed, or 'False' otherwise.

== Parameters of 'allowRequest'

* Cache — the cache storing the leaky bucket states, tagged with the LeakyBucket algorithm.
* Key — identifier for the bucket (e.g., user or IP).
* Capacity — maximum bucket level (number of requests allowed before blocking).
* Leak rate — the rate at which the bucket leaks (units per second).
* TTL — time-to-live for the cache entry in seconds.

== Behaviour

When a request arrives, the bucket leaks according to the elapsed time since the last update.
If the bucket level after leaking is below capacity, the request is allowed and the level increments.
Otherwise, the request is denied, but the state is still updated in the cache.

-}

module Keter.RateLimiter.LeakyBucket
  ( allowRequest
  ) where

import Keter.RateLimiter.Cache
import Keter.RateLimiter.LeakyBucketState (LeakyBucketState(..))
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Text (Text)

-- | Attempt to allow a request under the leaky bucket rate limiting algorithm.
allowRequest
  :: Cache (InMemoryStore 'LeakyBucket)
  -> Text
  -> Int
  -> Double
  -> Int
  -> IO Bool
allowRequest cache unprefixedKey capacity leakRate ttl = do
  now <- floor <$> getPOSIXTime
  let key = makeCacheKey (cacheAlgorithm cache) "" unprefixedKey
  mstate <- readStore (cacheStore cache) (algorithmPrefix $ cacheAlgorithm cache) key

  let state = case mstate of
        Nothing -> LeakyBucketState 0 now
        Just s  -> leak s now

  if level state < fromIntegral capacity
    then do
      let newState = state { level = level state + 1 }
      writeStore (cacheStore cache) (algorithmPrefix $ cacheAlgorithm cache) key newState ttl
      return True
    else do
      writeStore (cacheStore cache) (algorithmPrefix $ cacheAlgorithm cache) key state ttl
      return False
  where
    leak :: LeakyBucketState -> Int -> LeakyBucketState
    leak (LeakyBucketState oldLevel lastTime) nowTime =
      let delta = fromIntegral (nowTime - lastTime) :: Double
          leaked = delta * leakRate
          newLevel = max 0 (oldLevel - leaked)
      in LeakyBucketState newLevel nowTime
