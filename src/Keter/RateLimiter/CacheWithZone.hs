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

This module provides utility functions for interacting with `Cache` instances
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
-- == Parameters
--
-- [@cache@] The FixedWindow cache to use.
-- [@ipZone@] IP zone or logical tenant label.
-- [@userKey@] Unique per-client identifier (e.g., IP or token).
-- [@limit@] Maximum number of allowed requests in the given period.
-- [@period@] Time window in seconds.
--
-- == Returns
--
-- A boolean indicating whether the request is allowed.
--
-- == Example
--
-- > allowed <- allowFixedWindowRequest myCache "zone1" "userA" 10 60
allowFixedWindowRequest
  :: Cache (InMemoryStore 'FixedWindow)
  -- ^ FixedWindow-based cache handle
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
allowFixedWindowRequest cache ipZone userKey limit period = do
  count <- incStoreWithZone cache ipZone userKey period
  return $ count <= limit

--------------------------------------------------------------------------------

-- | Increment a cache entry using a composed key derived from the zone and user key.
--
-- This is typically used in fixed-window rate limiters to track the number
-- of requests within a time period.
--
-- == Requirements
--
-- The stored type @v@ must be JSON-serializable and support numeric operations.
incStoreWithZone
  :: (CacheStore store v IO, FromJSON v, ToJSON v, Num v, Ord v)
  => Cache store
  -- ^ Cache with algorithm-specific store
  -> Text
  -- ^ IP zone
  -> Text
  -- ^ User key
  -> Int
  -- ^ TTL (seconds)
  -> IO v
  -- ^ New counter value
incStoreWithZone cache ipZone userKey expiresIn = do
  let key = makeCacheKey (cacheAlgorithm cache) ipZone userKey
      prefix = algorithmPrefix (cacheAlgorithm cache)
  incStore (cacheStore cache) prefix key expiresIn

-- | Read a cache entry using a composed key.
--
-- Useful for debugging or non-mutating cache inspection.
readCacheWithZone
  :: (CacheStore store v IO)
  => Cache store
  -- ^ Cache handle
  -> Text
  -- ^ IP zone
  -> Text
  -- ^ User key
  -> IO (Maybe v)
  -- ^ Optional stored value
readCacheWithZone cache ipZone userKey = do
  let key = makeCacheKey (cacheAlgorithm cache) ipZone userKey
      prefix = algorithmPrefix (cacheAlgorithm cache)
  readStore (cacheStore cache) prefix key

-- | Write a value into the cache using a zone-aware key.
--
-- Typically used for manual updates, testing, or initializing state.
writeCacheWithZone
  :: (CacheStore store v IO)
  => Cache store
  -- ^ Cache handle
  -> Text
  -- ^ IP zone
  -> Text
  -- ^ User key
  -> v
  -- ^ Value to store
  -> Int
  -- ^ Expiration in seconds
  -> IO ()
writeCacheWithZone cache ipZone userKey val expiresIn = do
  let key = makeCacheKey (cacheAlgorithm cache) ipZone userKey
      prefix = algorithmPrefix (cacheAlgorithm cache)
  writeStore (cacheStore cache) prefix key val expiresIn

-- | Delete an entry from the cache using a zone-aware key.
--
-- Can be used to forcibly reset rate limiter state or clean up.
deleteCacheWithZone
  :: (CacheStore store v IO)
  => Cache store
  -- ^ Cache handle
  -> Text
  -- ^ IP zone
  -> Text
  -- ^ User key
  -> IO ()
deleteCacheWithZone cache ipZone userKey = do
  let key = makeCacheKey (cacheAlgorithm cache) ipZone userKey
      prefix = algorithmPrefix (cacheAlgorithm cache)
  deleteStore (cacheStore cache) prefix key
