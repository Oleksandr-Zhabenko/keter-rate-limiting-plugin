{-|
Copyright (c) 2025 Oleksandr Zhabenko

This file is a ported to Haskell language code with some simplifications of rack-attack
https://github.com/rack/rack-attack/blob/main/lib/rack/attack/cache.rb
and is based on the structure of the original code of
rack-attack, Copyright (c) 2016 by Kickstarter, PBC, under the MIT License.

Oleksandr Zhabenko added several implementations of the window algorithm: sliding window, token bucket window, leaky bucket window alongside with the initial count algorithm using AI chatbots.

This implementation is released under the MIT License.
 -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Keter.RateLimiter.Cache
  ( Cache(..)
  , CacheStore(..)
  , InMemoryStore
  , newInMemoryStore
  , readCache
  , writeCache
  , deleteCache
  , incStore
  , count
  , reset
  , newCache
  , createInMemoryStore
  , clearInMemoryStore
  , cacheReset
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON, decodeStrict, encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import qualified Data.Cache as C
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.Generics (Generic)
import System.Clock (TimeSpec(..))
import Data.Maybe (fromMaybe)
import GHC.TypeLits (Symbol)
import Data.Proxy (Proxy(..))
import Keter.RateLimiter.LeakyBucketState

-- Import to support unsafePerformIO
import System.IO.Unsafe (unsafePerformIO)

-- | Cache wrapper around the store and a key prefix.
data Cache store = Cache
  { cachePrefix :: Text
  , cacheStore  :: store
  }

-- | CacheStore class for abstracting storage backends.
class MonadIO m => CacheStore store v m | store -> v where
  readStore   :: store -> Text -> Text -> m (Maybe v)
  writeStore  :: store -> Text -> Text -> v -> Int -> m ()
  deleteStore :: store -> Text -> Text -> m ()

-- | Create new cache with prefix and store
newCache :: Text -> store -> Cache store
newCache prefix store = Cache
  { cachePrefix = prefix
  , cacheStore = store
  }

-- | Create new in-memory store
createInMemoryStore :: IO (InMemoryStore a)
createInMemoryStore = do
  cacheRef <- C.newCache Nothing >>= newIORef
  return $ InMemoryStore cacheRef

-- | In-memory store using IORef-wrapped Data.Cache with type-level tag
-- to differentiate instances for different value types
data InMemoryStore (a :: Symbol) = InMemoryStore (IORef (C.Cache Text Text))

-- | Constructor for in-memory store with explicit type tag
newInMemoryStore :: InMemoryStore a
newInMemoryStore = unsafePerformIO $ C.newCache Nothing >>= fmap InMemoryStore . newIORef

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
        txt = decodeUtf8' strictBs
    case txt of
      Left _ -> return ()
      Right vtxt -> do
        let expiration = Just (TimeSpec 0 (fromIntegral expiresIn * 1000000000))
        C.insert' cache expiration key vtxt

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
        txt = decodeUtf8' strictBs
    case txt of
      Left _ -> return ()
      Right vtxt -> do
        let expiration = Just (TimeSpec 0 (fromIntegral expiresIn * 1000000000))
        C.insert' cache expiration key vtxt

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
        txt = decodeUtf8' strictBs
    case txt of
      Left _ -> return ()
      Right vtxt -> do
        let expiration = Just (TimeSpec 0 (fromIntegral expiresIn * 1000000000))
        C.insert' cache expiration key vtxt

  deleteStore (InMemoryStore ref) _prefix key = do
    cache <- readIORef ref
    C.delete cache key

-- | Read from cache with prefixed key.
readCache :: (CacheStore store v IO) => Cache store -> Text -> IO (Maybe v)
readCache cache unprefixedKey =
  readStore (cacheStore cache) (cachePrefix cache) (cachePrefix cache <> ":" <> unprefixedKey)

-- | Write to cache with prefixed key.
writeCache :: (CacheStore store v IO) => Cache store -> Text -> v -> Int -> IO ()
writeCache cache unprefixedKey val expiresIn =
  writeStore (cacheStore cache) (cachePrefix cache) (cachePrefix cache <> ":" <> unprefixedKey) val expiresIn

-- | Delete a key from cache with prefix.
deleteCache :: (CacheStore store v IO) => Cache store -> Text -> IO ()
deleteCache cache unprefixedKey =
  deleteStore (cacheStore cache) (cachePrefix cache) (cachePrefix cache <> ":" <> unprefixedKey)

-- | Increment a numeric cache value, or initialize if missing.
incStore :: (CacheStore store v IO, Num v) => Cache store -> Text -> Int -> IO v
incStore cache unprefixedKey expiresIn = do
  let fullKey = cachePrefix cache <> ":" <> unprefixedKey
  mval <- readStore (cacheStore cache) (cachePrefix cache) fullKey
  let newVal = maybe 1 (+1) mval
  writeStore (cacheStore cache) (cachePrefix cache) fullKey newVal expiresIn
  return newVal

-- | Count operation - alias for incStore with a specific timeout
count :: (CacheStore store v IO, Num v) => Cache store -> Text -> IO v
count cache key = incStore cache key 3600  -- Default expiry of 1 hour

-- | Reset a cache key
reset :: (CacheStore store v IO) => Cache store -> Text -> IO ()
reset cache key = deleteCache cache key

-- | Clear all entries from an in-memory store
clearInMemoryStore :: InMemoryStore a -> IO ()
clearInMemoryStore (InMemoryStore ref) = do
  cache <- readIORef ref
  C.purge cache

-- | Reset all entries in a cache
cacheReset :: Cache (InMemoryStore a) -> IO ()
cacheReset = clearInMemoryStore . cacheStore
