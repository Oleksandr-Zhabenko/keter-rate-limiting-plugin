{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Keter.RateLimiter.SlidingWindow
Description : Sliding window rate-limiting algorithm implementation
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : stable
Portability : portable
-}

module Keter.RateLimiter.SlidingWindow
  ( allowRequest
  ) where

import Keter.RateLimiter.Cache
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Concurrent.STM
import Data.List (filter)
import Data.Maybe (fromMaybe)

-- | Check if a request is allowed based on the sliding window algorithm, considering IP zones.
allowRequest
  :: Cache (InMemoryStore 'SlidingWindow)
  -> Text -- ^ IP zone
  -> Text -- ^ User key
  -> Int -- ^ Window size (in seconds)
  -> Int -- ^ Request limit
  -> IO Bool
allowRequest cache ipZone userKey windowSize limit = do
  now <- floor <$> getPOSIXTime
  let key = makeCacheKey (cacheAlgorithm cache) ipZone userKey
  mTimestamps <- readCacheWithZone cache ipZone userKey
  let timestamps = fromMaybe [] mTimestamps
      validTimestamps = filter (\t -> now - t <= windowSize) timestamps
  if length validTimestamps >= limit
    then return False
    else do
      let newTimestamps = now : validTimestamps
      writeCacheWithZone cache ipZone userKey newTimestamps windowSize
      return True
