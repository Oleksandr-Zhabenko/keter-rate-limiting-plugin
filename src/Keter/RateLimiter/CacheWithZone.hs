{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

{-|
Module      : Keter.RateLimiter.CacheWithZone
Description : Helpers for cache access with zone-aware composite keys
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : stable
Portability : portable

This module provides utility functions for interacting with 'Cache' instances
in a zone-aware manner. Each key is built from an algorithm-specific prefix,
an IP zone label, and a user-specific identifier. This allows different
logical groups (IP zones) to maintain independent rate limiting state even if
they share the same user identifiers.

These helpers wrap common operations — incrementing counters, reading or
writing values, or deleting entries — by automatically composing the correct
cache keys.

A convenience function, 'allowFixedWindowRequest', is provided to evaluate
requests using the Fixed Window rate-limiting strategy.

-}

module Keter.RateLimiter.CacheWithZone
  ( -- * General Cache Helpers
    incStoreWithZone
  , readCacheWithZone
  , writeCacheWithZone
  , deleteCacheWithZone
    -- * Fixed Window Rate Limiting
  , allowFixedWindowRequest
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Keter.RateLimiter.Cache

--------------------------------------------------------------------------------

-- | Attempt to allow a request under a Fixed Window rate-limiting strategy.
--
-- This increments the counter for the composed key built from the given
-- zone and user key. The request is allowed if the resulting count
-- does not exceed the limit for the given period.
--
-- The function takes a FixedWindow cache, throttle name, IP zone, user key,
-- request limit, and time period. It returns 'True' if the request should
-- be allowed, 'False' otherwise.
--
-- ==== __Examples__
--
-- @
-- allowed <- allowFixedWindowRequest myCache \"throttle1\" \"zone1\" \"userA\" 10 60
-- if allowed
--   then putStrLn \"Request allowed\"
--   else putStrLn \"Request denied - rate limit exceeded\"
-- @
allowFixedWindowRequest
  :: Cache (InMemoryStore 'FixedWindow)
  -- ^ FixedWindow-based cache handle
  -> Text
  -- ^ Throttle name
  -> Text
  -- ^ IP zone or tenant key
  -> Text
  -- ^ User key
  -> Int
  -- ^ Request limit
  -> Int
  -- ^ Period in seconds
  -> IO Bool
  -- ^ Whether the request is allowed
allowFixedWindowRequest cache throttleName ipZone userKey limit period = do
  count <- incStoreWithZone cache throttleName ipZone userKey period
  return $ count <= limit

--------------------------------------------------------------------------------

-- | Increment a cache entry using a composed key derived from the zone and user key.
--
-- This is typically used in fixed-window rate limiters to track the number
-- of requests within a time period. The function creates a composite cache key
-- from the throttle name, algorithm type, IP zone, and user key, then increments
-- the stored counter value.
--
-- The stored type must be JSON-serializable and support numeric operations.
--
-- ==== __Examples__
--
-- @
-- newCount <- incStoreWithZone cache \"api-throttle\" \"zone-a\" \"user123\" 3600
-- print newCount  -- Prints the incremented counter value
-- @
incStoreWithZone
  :: (CacheStore store v IO, FromJSON v, ToJSON v, Num v, Ord v)
  => Cache store
  -- ^ Cache with algorithm-specific store
  -> Text
  -- ^ Throttle name
  -> Text
  -- ^ IP zone
  -> Text
  -- ^ User key
  -> Int
  -- ^ TTL (seconds)
  -> IO v
  -- ^ New counter value
incStoreWithZone cache throttleName ipZone userKey expiresIn = do
  let key = makeCacheKey throttleName (cacheAlgorithm cache) ipZone userKey
      prefix = algorithmPrefix (cacheAlgorithm cache)
  incStore (cacheStore cache) prefix key expiresIn

-- | Read a cache entry using a composed key.
--
-- Useful for debugging or non-mutating cache inspection. The function
-- constructs the appropriate cache key from the provided components and
-- retrieves the stored value if it exists.
--
-- ==== __Examples__
--
-- @
-- maybeValue <- readCacheWithZone cache \"throttle1\" \"zone1\" \"user456\"
-- case maybeValue of
--   Just val -> print val
--   Nothing  -> putStrLn \"No entry found\"
-- @
readCacheWithZone
  :: (CacheStore store v IO)
  => Cache store
  -- ^ Cache handle
  -> Text
  -- ^ Throttle name
  -> Text
  -- ^ IP zone
  -> Text
  -- ^ User key
  -> IO (Maybe v)
  -- ^ Optional stored value
readCacheWithZone cache throttleName ipZone userKey = do
  let key = makeCacheKey throttleName (cacheAlgorithm cache) ipZone userKey
      prefix = algorithmPrefix (cacheAlgorithm cache)
  readStore (cacheStore cache) prefix key

-- | Write a value into the cache using a zone-aware key.
--
-- Typically used for manual updates, testing, or initializing state.
-- The function creates a composite key and stores the provided value
-- with the specified expiration time.
--
-- ==== __Examples__
--
-- @
-- writeCacheWithZone cache \"throttle1\" \"zone1\" \"user789\" (42 :: Int) 1800
-- putStrLn \"Value written to cache\"
-- @
writeCacheWithZone
  :: (CacheStore store v IO)
  => Cache store
  -- ^ Cache handle
  -> Text
  -- ^ Throttle name
  -> Text
  -- ^ IP zone
  -> Text
  -- ^ User key
  -> v
  -- ^ Value to store
  -> Int
  -- ^ Expiration in seconds
  -> IO ()
writeCacheWithZone cache throttleName ipZone userKey val expiresIn = do
  let key = makeCacheKey throttleName (cacheAlgorithm cache) ipZone userKey
      prefix = algorithmPrefix (cacheAlgorithm cache)
  writeStore (cacheStore cache) prefix key val expiresIn

-- | Delete an entry from the cache using a zone-aware key.
--
-- Can be used to forcibly reset rate limiter state or clean up expired
-- entries manually. The function constructs the appropriate cache key
-- and removes the corresponding entry.
--
-- ==== __Examples__
--
-- @
-- deleteCacheWithZone cache \"throttle1\" \"zone1\" \"user123\"
-- putStrLn \"Cache entry deleted\"
-- @
deleteCacheWithZone
  :: (CacheStore store v IO)
  => Cache store
  -- ^ Cache handle
  -> Text
  -- ^ Throttle name
  -> Text
  -- ^ IP zone
  -> Text
  -- ^ User key
  -> IO ()
deleteCacheWithZone cache throttleName ipZone userKey = do
  let key = makeCacheKey throttleName (cacheAlgorithm cache) ipZone userKey
      prefix = algorithmPrefix (cacheAlgorithm cache)
  deleteStore (cacheStore cache) prefix key