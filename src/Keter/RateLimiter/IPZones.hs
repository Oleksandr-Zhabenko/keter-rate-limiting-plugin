{-|
Copyright (c) 2025 Oleksandr Zhabenko

This module defines data structures and helpers for managing
IP zone-specific caches for rate limiting.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Keter.RateLimiter.IPZones
  ( IPZoneIdentifier
  , defaultIPZone
  , ZoneSpecificCaches(..)
  , createZoneCaches
  , resetSingleZoneCaches
  , newZoneSpecificCaches
  ) where

import Data.Text (Text)
import Keter.RateLimiter.Cache
  ( Cache(..)
  , InMemoryStore
  , newCache
  , createInMemoryStore
  , cacheReset
  )

-- | Type alias for IP Zone Identifier
type IPZoneIdentifier = Text

-- | Default IP Zone identifier
defaultIPZone :: IPZoneIdentifier
defaultIPZone = "default"

-- | A structure to hold all caches for a specific zone
data ZoneSpecificCaches = ZoneSpecificCaches
  { zscCounterCache     :: Cache (InMemoryStore "counter")
  , zscTimestampCache   :: Cache (InMemoryStore "timestamps")
  , zscTokenBucketCache :: Cache (InMemoryStore "token_bucket")
  , zscLeakyBucketCache :: Cache (InMemoryStore "leaky_bucket")
  }

-- | Helper to create a full set of caches for one zone
createZoneCaches :: IO ZoneSpecificCaches
createZoneCaches = do
  counterStore <- createInMemoryStore
  timestampStore <- createInMemoryStore
  tokenBucketStore <- createInMemoryStore
  leakyBucketStore <- createInMemoryStore
  return ZoneSpecificCaches
    { zscCounterCache     = newCache "rate_limiter" counterStore
    , zscTimestampCache   = newCache "timestamps" timestampStore
    , zscTokenBucketCache = newCache "token_bucket" tokenBucketStore
    , zscLeakyBucketCache = newCache "leaky_bucket" leakyBucketStore
    }

-- | Reset all caches in a specific ZoneSpecificCaches structure
resetSingleZoneCaches :: ZoneSpecificCaches -> IO ()
resetSingleZoneCaches zsc = do
  cacheReset (zscCounterCache zsc)
  cacheReset (zscTimestampCache zsc)
  cacheReset (zscTokenBucketCache zsc)
  cacheReset (zscLeakyBucketCache zsc)

-- | Alias for createZoneCaches.
newZoneSpecificCaches :: IO ZoneSpecificCaches
newZoneSpecificCaches = createZoneCaches
