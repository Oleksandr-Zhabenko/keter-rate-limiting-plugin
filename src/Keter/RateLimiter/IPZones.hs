{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

{-|
Module      : Keter.RateLimiter.IPZones
Description : Management of caches specific to IP zones for rate limiting
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : stable
Portability : portable

This module provides functionality to manage and organise caches that are specific to different IP zones within the Keter rate limiting system.

== Overview

In rate limiting systems, it is often useful to segment traffic by IP zones to apply different limits or policies. This module defines a structure to hold multiple caches per IP zone, each cache corresponding to a different rate limiting strategy or data type.

It offers utilities to create, reset, and manage these zone-specific caches efficiently.

== Key Concepts

- 'IPZoneIdentifier' is a type alias for a textual identifier of an IP zone.
- 'defaultIPZone' is the default identifier used when no specific zone is assigned.
- 'ZoneSpecificCaches' groups together four distinct caches used for different rate limiting algorithms or data:
  - Counter cache (FixedWindow Algorithm)
  - Timestamp cache (SlidingWindow Algorithm)
  - Token bucket cache (TokenBucket Algorithm)
  - Leaky bucket cache (LeakyBucket Algorithm)

== Functions

- 'createZoneCaches' creates a new set of caches for a single IP zone.
- 'resetSingleZoneCaches' clears all caches within a given 'ZoneSpecificCaches' instance.
- 'newZoneSpecificCaches' is an alias for 'createZoneCaches' for convenience.

== Usage Example

@
do
  zoneCaches <- createZoneCaches
  -- Use zoneCaches for rate limiting operations in this IP zone
  -- When needed, reset caches:
  resetSingleZoneCaches zoneCaches
@

-}

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

-- | Type alias representing an identifier for an IP zone.
type IPZoneIdentifier = Text

-- | The default IP zone identifier used when no specific zone is assigned.
defaultIPZone :: IPZoneIdentifier
defaultIPZone = "default"

-- | A collection of caches dedicated to a specific IP zone.
--
-- This data structure holds separate caches for different rate limiting mechanisms:
--
-- * 'zscCounterCache' — for counting occurrences
-- * 'zscTimestampCache' — for storing timestamps of events
-- * 'zscTokenBucketCache' — for token bucket algorithm data
-- * 'zscLeakyBucketCache' — for leaky bucket algorithm data
data ZoneSpecificCaches = ZoneSpecificCaches
  { zscCounterCache     :: Cache (InMemoryStore "counter")
  , zscTimestampCache   :: Cache (InMemoryStore "timestamps")
  , zscTokenBucketCache :: Cache (InMemoryStore "token_bucket")
  , zscLeakyBucketCache :: Cache (InMemoryStore "leaky_bucket")
  }

-- | Create a new set of caches for a single IP zone.
--
-- Each cache is backed by a fresh in-memory store.
--
-- ==== __Example__
--
-- > zoneCaches <- createZoneCaches
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

-- | Reset all caches within the given 'ZoneSpecificCaches'.
--
-- This clears all stored data, effectively resetting the rate limiting state for the zone.
--
-- ==== __Example__
--
-- > resetSingleZoneCaches zoneCaches
resetSingleZoneCaches :: ZoneSpecificCaches -> IO ()
resetSingleZoneCaches zsc = do
  cacheReset (zscCounterCache zsc)
  cacheReset (zscTimestampCache zsc)
  cacheReset (zscTokenBucketCache zsc)
  cacheReset (zscLeakyBucketCache zsc)

-- | Alias for 'createZoneCaches'.
--
-- Provided for semantic clarity or convenience.
newZoneSpecificCaches :: IO ZoneSpecificCaches
newZoneSpecificCaches = createZoneCaches
