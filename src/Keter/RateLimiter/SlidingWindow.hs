{-
Oleksandr Zhabenko added several implementations of the window algorithm: here in the file there is a sliding window implementation using AI chatbots.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Keter.RateLimiter.SlidingWindow
  ( allowRequest
  ) where

import Keter.RateLimiter.Cache (Cache(..), CacheStore(..), InMemoryStore)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Text (Text)
import qualified Data.Text as T

-- | Rate limiter using sliding window log storing timestamps as [Int]
allowRequest
  :: Cache (InMemoryStore "timestamps")
  -> Text     -- ^ User key
  -> Int      -- ^ Limit
  -> Int      -- ^ Period (seconds)
  -> IO Bool
allowRequest cache unprefixedKey limit period = do
  now <- floor <$> getPOSIXTime
  let key = cachePrefix cache <> ":" <> unprefixedKey
  maybeTimestamps <- readStore (cacheStore cache) (cachePrefix cache) key
  let timestamps = maybe [] id maybeTimestamps
      recent = filter (\ts -> now - ts < period) timestamps
      newTimestamps = recent ++ [now]
  if length recent >= limit
    then return False
    else do
      writeStore (cacheStore cache) (cachePrefix cache) key newTimestamps period
      return True
