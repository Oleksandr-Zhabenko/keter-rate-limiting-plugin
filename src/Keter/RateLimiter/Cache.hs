{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

{-|
Module      : Keter.RateLimiter.Cache
Description : Cache abstraction and in-memory store for rate limiting with convenient and customisable key management
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : stable
Portability : portable

This module provides an abstraction for cache storage with an in-memory implementation.
It offers both convenient wrappers for typical use (automatic key composition from throttle name, IP zone, and user key)
and full customisability for advanced users who wish to control key structure themselves.

== Features

* Cache abstraction with key prefixing
* In-memory cache store with expiry support
* Support for storing various data types, including counters, timestamps, and algorithm states
* Resettable store interface for clearing caches
* Convenient key wrappers (recommended for most use cases)
* Full customisability for advanced scenarios

== Usage

- For convenience, use functions like 'incStoreWithZone', 'readCacheWithZone', etc., passing throttle name, IP zone, and user key as separate arguments.
- For full control, use 'incStore', 'readCache', etc., and compose the cache key as you wish.

-}

module Keter.RateLimiter.Cache
  ( Cache(..)
  , CacheStore(..)
  , InMemoryStore
  , ResettableStore
  , newInMemoryStore
  , readCache
  , writeCache
  , deleteCache
  , incStore
  , newCache
  , createInMemoryStore
  , clearInMemoryStore
  , cacheReset
  -- Convenient wrappers
  , makeCacheKey
  , incStoreWithZone
  , readCacheWithZone
  , writeCacheWithZone
  , deleteCacheWithZone
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (decodeStrict, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import qualified Data.Cache as C
import Data.IORef (IORef, newIORef, readIORef)
import GHC.TypeLits (Symbol)
import Keter.RateLimiter.LeakyBucketState
import System.IO.Unsafe (unsafePerformIO)
import System.Clock (TimeSpec(..))

-- | Cache wrapper combining a key prefix and a storage backend.
data Cache store = Cache
  { cachePrefix :: Text
  , cacheStore :: store
  }

-- | Typeclass abstracting cache storage backends.
class MonadIO m => CacheStore store v m | store -> v where
  readStore :: store -> Text -> Text -> m (Maybe v)
  writeStore :: store -> Text -> Text -> v -> Int -> m ()
  deleteStore :: store -> Text -> Text -> m ()

-- | Create a new cache with a given prefix and store.
newCache :: Text -> store -> Cache store
newCache prefix store = Cache
  { cachePrefix = prefix
  , cacheStore = store
  }

-- | Create a new in-memory store using 'Data.Cache' wrapped in 'IORef'.
createInMemoryStore :: IO (InMemoryStore a)
createInMemoryStore = do
  cacheRef <- C.newCache Nothing >>= newIORef
  return $ InMemoryStore cacheRef

-- | In-memory store type tagged by a type-level symbol.
newtype InMemoryStore (a :: Symbol) = InMemoryStore (IORef (C.Cache Text Text))

-- | Unsafe global in-memory store instance (use with caution).
newInMemoryStore :: InMemoryStore a
newInMemoryStore = unsafePerformIO $ C.newCache Nothing >>= fmap InMemoryStore . newIORef

-- | Read from cache using the prefixed key.
readCache :: (CacheStore store v IO) => Cache store -> Text -> IO (Maybe v)
readCache cache unprefixedKey =
  readStore (cacheStore cache) (cachePrefix cache) (cachePrefix cache <> ":" <> unprefixedKey)

-- | Write to cache using the prefixed key.
writeCache :: (CacheStore store v IO) => Cache store -> Text -> v -> Int -> IO ()
writeCache cache unprefixedKey val expiresIn =
  writeStore (cacheStore cache) (cachePrefix cache) (cachePrefix cache <> ":" <> unprefixedKey) val expiresIn

-- | Delete a key from cache using the prefixed key.
deleteCache :: (CacheStore store v IO) => Cache store -> Text -> IO ()
deleteCache cache unprefixedKey =
  deleteStore (cacheStore cache) (cachePrefix cache) (cachePrefix cache <> ":" <> unprefixedKey)

-- | Increment a numeric cache value or initialise it if missing.
incStore :: (CacheStore store v IO, Num v) => Cache store -> Text -> Int -> IO v
incStore cache unprefixedKey expiresIn = do
  let fullKey = cachePrefix cache <> ":" <> unprefixedKey
  mval <- readStore (cacheStore cache) (cachePrefix cache) fullKey
  let newVal = maybe 1 (+1) mval
  writeStore (cacheStore cache) (cachePrefix cache) fullKey newVal expiresIn
  return newVal

-- | Typeclass for stores that can be reset.
class ResettableStore store where
  resetStore :: store -> IO ()

instance ResettableStore (InMemoryStore a) where
  resetStore (InMemoryStore ref) = do
    cache <- readIORef ref
    C.purge cache

-- | Clear all entries in an in-memory store.
clearInMemoryStore :: InMemoryStore a -> IO ()
clearInMemoryStore = resetStore

-- | Reset all entries in a cache.
cacheReset :: ResettableStore store => Cache store -> IO ()
cacheReset (Cache _ store) = resetStore store

-- | Instance for storing Int lists (timestamps)
instance CacheStore (InMemoryStore "timestamps") [Int] IO where
  readStore (InMemoryStore ref) _prefix key = do
    cache <- readIORef ref
    mval <- C.lookup cache key
    case mval of
      Nothing -> return Nothing
      Just txt -> case decodeStrict (encodeUtf8 txt) of
        Nothing -> return Nothing
        Just val -> return (Just val)
  writeStore (InMemoryStore ref) _prefix key val expiresIn = do
    cache <- readIORef ref
    let bs = encode val
        strictBs = LBS.toStrict bs
        jsonTxt = case decodeUtf8' strictBs of
                    Left _ -> ""
                    Right txt -> txt
        expiration = Just (TimeSpec 0 (fromIntegral expiresIn * 1000000000))
    C.insert' cache expiration key jsonTxt
  deleteStore (InMemoryStore ref) _prefix key = do
    cache <- readIORef ref
    C.delete cache key

-- | Instance for storing Int counters
instance CacheStore (InMemoryStore "counter") Int IO where
  readStore (InMemoryStore ref) _prefix key = do
    cache <- readIORef ref
    mval <- C.lookup cache key
    case mval of
      Nothing -> return Nothing
      Just txt -> case decodeStrict (encodeUtf8 txt) of
        Nothing -> return Nothing
        Just val -> return (Just val)
  writeStore (InMemoryStore ref) _prefix key val expiresIn = do
    cache <- readIORef ref
    let bs = encode val
        strictBs = LBS.toStrict bs
        jsonTxt = case decodeUtf8' strictBs of
                    Left _ -> ""
                    Right txt -> txt
        expiration = Just (TimeSpec 0 (fromIntegral expiresIn * 1000000000))
    C.insert' cache expiration key jsonTxt
  deleteStore (InMemoryStore ref) _prefix key = do
    cache <- readIORef ref
    C.delete cache key

-- | Instance for storing Text values (for token bucket)
instance CacheStore (InMemoryStore "token_bucket") Text IO where
  readStore (InMemoryStore ref) _prefix key = do
    cache <- readIORef ref
    C.lookup cache key
  writeStore (InMemoryStore ref) _prefix key val expiresIn = do
    cache <- readIORef ref
    let expiration = Just (TimeSpec 0 (fromIntegral expiresIn * 1000000000))
    C.insert' cache expiration key val
  deleteStore (InMemoryStore ref) _prefix key = do
    cache <- readIORef ref
    C.delete cache key

-- | Instance for storing LeakyBucketState
instance CacheStore (InMemoryStore "leaky_bucket") LeakyBucketState IO where
  readStore (InMemoryStore ref) _prefix key = do
    cache <- readIORef ref
    mval <- C.lookup cache key
    case mval of
      Nothing -> return Nothing
      Just txt -> case decodeStrict (encodeUtf8 txt) of
        Nothing -> return Nothing
        Just val -> return (Just val)
  writeStore (InMemoryStore ref) _prefix key val expiresIn = do
    cache <- readIORef ref
    let bs = encode val
        strictBs = LBS.toStrict bs
        jsonTxt = case decodeUtf8' strictBs of
                    Left _ -> ""
                    Right txt -> txt
        expiration = Just (TimeSpec 0 (fromIntegral expiresIn * 1000000000))
    C.insert' cache expiration key jsonTxt
  deleteStore (InMemoryStore ref) _prefix key = do
    cache <- readIORef ref
    C.delete cache key

----------------------------------------------------------------------
-- CONVENIENT WRAPPERS (recommended for most users)
----------------------------------------------------------------------

{-|
  Compose a unique cache key from throttle name, IP zone, and user key.

  This function is used internally by the convenient wrappers, but can also be used directly
  if you wish to customise key structure further.
-}
makeCacheKey :: Text -> Text -> Text -> Text
makeCacheKey throttleName ipZone userKey = throttleName <> ":" <> ipZone <> ":" <> userKey

{-|
  Increment a numeric cache value, automatically composing the key from throttle name, IP zone, and user key.

  This is the recommended way for most users. For advanced scenarios, use 'incStore' and compose the key manually.
-}
incStoreWithZone
  :: (CacheStore store v IO, Num v)
  => Cache store
  -> Text               -- ^ Throttle name
  -> Text               -- ^ IP zone identifier
  -> Text               -- ^ User key (e.g. IP or login)
  -> Int                -- ^ Expiry in seconds
  -> IO v
incStoreWithZone cache throttleName ipZone userKey expiresIn =
  incStore cache (makeCacheKey throttleName ipZone userKey) expiresIn

{-|
  Read from cache using throttle name, IP zone, and user key.

  This is convenient for most scenarios. For full control, use 'readCache' directly.
-}
readCacheWithZone
  :: (CacheStore store v IO)
  => Cache store
  -> Text               -- ^ Throttle name
  -> Text               -- ^ IP zone identifier
  -> Text               -- ^ User key
  -> IO (Maybe v)
readCacheWithZone cache throttleName ipZone userKey =
  readCache cache (makeCacheKey throttleName ipZone userKey)

{-|
  Write to cache using throttle name, IP zone, and user key.

  Use this for convenience, or use 'writeCache' for full customisation.
-}
writeCacheWithZone
  :: (CacheStore store v IO)
  => Cache store
  -> Text               -- ^ Throttle name
  -> Text               -- ^ IP zone identifier
  -> Text               -- ^ User key
  -> v
  -> Int                -- ^ Expiry in seconds
  -> IO ()
writeCacheWithZone cache throttleName ipZone userKey val expiresIn =
  writeCache cache (makeCacheKey throttleName ipZone userKey) val expiresIn

{-|
  Delete from cache using throttle name, IP zone, and user key.

  Use this for convenience, or use 'deleteCache' for full customisation.
-}
deleteCacheWithZone
  :: (CacheStore store v IO)
  => Cache store
  -> Text               -- ^ Throttle name
  -> Text               -- ^ IP zone identifier
  -> Text               -- ^ User key
  -> IO ()
deleteCacheWithZone cache throttleName ipZone userKey =
  deleteCache cache (makeCacheKey throttleName ipZone userKey)
