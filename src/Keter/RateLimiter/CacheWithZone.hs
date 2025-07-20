{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Keter.RateLimiter.CacheWithZone
  ( incStoreWithZone
  , readCacheWithZone
  , writeCacheWithZone
  , deleteCacheWithZone
  , allowFixedWindowRequest
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Keter.RateLimiter.Cache

-- | Check if a request is allowed under the FixedWindow algorithm.
-- Increments the counter for the given key and checks if it's within the limit.
allowFixedWindowRequest :: Cache (InMemoryStore 'FixedWindow) -> Text -> Text -> Int -> Int -> IO Bool
allowFixedWindowRequest cache ipZone userKey limit period = do
  count <- incStoreWithZone cache ipZone userKey period
  return $ count <= limit

-- | Increment a numeric cache value with a composed key.
incStoreWithZone
  :: (CacheStore store v IO, FromJSON v, ToJSON v, Num v, Ord v)
  => Cache store -> Text -> Text -> Int -> IO v
incStoreWithZone cache ipZone userKey expiresIn = do
  let key = makeCacheKey (cacheAlgorithm cache) ipZone userKey
      prefix = algorithmPrefix (cacheAlgorithm cache)
  -- Use the atomic incStore from the typeclass
  incStore (cacheStore cache) prefix key expiresIn

-- | Read from cache with a composed key.
readCacheWithZone
  :: (CacheStore store v IO)
  => Cache store -> Text -> Text -> IO (Maybe v)
readCacheWithZone cache ipZone userKey = do
  let key = makeCacheKey (cacheAlgorithm cache) ipZone userKey
      prefix = algorithmPrefix (cacheAlgorithm cache)
  readStore (cacheStore cache) prefix key

-- | Write to cache with a composed key.
writeCacheWithZone
  :: (CacheStore store v IO)
  => Cache store -> Text -> Text -> v -> Int -> IO ()
writeCacheWithZone cache ipZone userKey val expiresIn = do
  let key = makeCacheKey (cacheAlgorithm cache) ipZone userKey
      prefix = algorithmPrefix (cacheAlgorithm cache)
  writeStore (cacheStore cache) prefix key val expiresIn

-- | Delete from cache with a composed key.
deleteCacheWithZone
  :: (CacheStore store v IO)
  => Cache store -> Text -> Text -> IO ()
deleteCacheWithZone cache ipZone userKey = do
  let key = makeCacheKey (cacheAlgorithm cache) ipZone userKey
      prefix = algorithmPrefix (cacheAlgorithm cache)
  deleteStore (cacheStore cache) prefix key
