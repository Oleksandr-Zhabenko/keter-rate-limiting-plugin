{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Keter.RateLimiter.SlidingWindow
Description : Sliding window rate limiting algorithm implementation
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : stable
Portability : portable

This module implements the sliding window rate limiting algorithm using a timestamp log stored in a cache.

== Overview

The sliding window algorithm limits the number of requests within a rolling time window by keeping track of
timestamps of recent requests. Requests older than the window period are discarded.

This implementation stores timestamps as a list of integers (POSIX seconds) in a cache keyed by user or client identifier.

== Key function

- 'allowRequest' checks if a new request is allowed under the limit and period,
  updates the timestamp log accordingly, and returns 'True' or 'False'.

== Parameters of 'allowRequest'

* Cache — the cache storing timestamp logs, tagged with the SlidingWindow algorithm.
* User key — identifier for the client or user.
* Limit — maximum allowed requests within the period.
* Period — sliding window duration in seconds.

== Behaviour

When a request arrives, timestamps older than the sliding window are removed.
If the count of recent requests is below the limit, the request is allowed and the timestamp is recorded.
Otherwise, the request is denied.

-}

module Keter.RateLimiter.SlidingWindow
  ( allowRequest
  ) where

import Keter.RateLimiter.Cache
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Text (Text)

-- | Attempt to allow a request under the sliding window rate limiting algorithm.
allowRequest
  :: Cache (InMemoryStore 'SlidingWindow)
  -> Text
  -> Int
  -> Int
  -> IO Bool
allowRequest cache unprefixedKey limit period = do
  now <- floor <$> getPOSIXTime
  let key = makeCacheKey (cacheAlgorithm cache) "" unprefixedKey
  maybeTimestamps <- readStore (cacheStore cache) (algorithmPrefix $ cacheAlgorithm cache) key
  let timestamps = maybe [] id maybeTimestamps
      recent = filter (\ts -> now - ts < period) timestamps
      newTimestamps = recent ++ [now]
  if length recent >= limit
    then return False
    else do
      writeStore (cacheStore cache) (algorithmPrefix $ cacheAlgorithm cache) key newTimestamps period
      return True
