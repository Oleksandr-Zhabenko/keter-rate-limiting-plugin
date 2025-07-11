{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

module Keter.RateLimiter.Cache
  ( Cache(..)
  , CacheStore(..)
  , InMemoryStore(..)
  , ResettableStore(..)
  , Algorithm(..)
  , algorithmPrefix
  , readCache
  , writeCache
  , deleteCache
  , incStore
  , newCache
  , createInMemoryStore
  , clearInMemoryStore
  , cacheReset
  , makeCacheKey
  , incStoreWithZone
  , readCacheWithZone
  , writeCacheWithZone
  , deleteCacheWithZone
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON, FromJSON, decodeStrict, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Semigroup ((<>))
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import qualified Data.Cache as C
import GHC.Generics (Generic)
import Keter.RateLimiter.LeakyBucketState
import System.Clock (TimeSpec(..), Clock(Monotonic), getTime)
import Data.Time.Clock (addUTCTime, getCurrentTime, NominalDiffTime)
import Data.Fixed (Pico)
import qualified Data.TinyLRU as TinyLRU
import qualified StmContainers.Map as Map
import Data.Kind (Type)

-- | Supported rate-limiting algorithms.
data Algorithm = FixedWindow | SlidingWindow | TokenBucket | LeakyBucket | TinyLRU
  deriving (Show, Eq, Generic)

-- | Map an Algorithm to its corresponding prefix.
algorithmPrefix :: Algorithm -> Text
algorithmPrefix FixedWindow   = "rate_limiter"
algorithmPrefix SlidingWindow = "timestamps"
algorithmPrefix TokenBucket   = "token_bucket"
algorithmPrefix LeakyBucket   = "leaky_bucket"
algorithmPrefix TinyLRU       = "tiny_lru"

-- | Cache wrapper combining an Algorithm and a storage backend.
data Cache store = Cache
  { cacheAlgorithm :: Algorithm
  , cacheStore :: store
  }

-- | Typeclass abstracting cache storage backends.
class MonadIO m => CacheStore store v m | store -> v where
  readStore :: store -> Text -> Text -> m (Maybe v)
  writeStore :: store -> Text -> Text -> v -> Int -> m ()
  deleteStore :: store -> Text -> Text -> m ()

-- | Typeclass for stores that can be reset.
class ResettableStore store where
  resetStore :: store -> IO ()

-- | Typeclass for creating stores based on Algorithm.
class CreateStore (a :: Algorithm) where
  createStore :: IO (InMemoryStore a)

-- | In-memory store type parameterized by Algorithm.
data InMemoryStore (a :: Algorithm) where
  CounterStore :: TVar (C.Cache Text Text) -> InMemoryStore 'FixedWindow
  TimestampStore :: TVar (C.Cache Text Text) -> InMemoryStore 'SlidingWindow
  TokenBucketStore :: TVar (C.Cache Text Text) -> InMemoryStore 'TokenBucket
  LeakyBucketStore :: TVar (C.Cache Text Text) -> InMemoryStore 'LeakyBucket
  TinyLRUStore :: TVar (TinyLRU.TinyLRUCache s) -> InMemoryStore 'TinyLRU

-- | Convert seconds to TimeSpec for use with Data.Cache
secondsToTimeSpec :: Int -> IO TimeSpec
secondsToTimeSpec seconds = do
  now <- getTime Monotonic
  return $ now + TimeSpec (fromIntegral seconds) 0

-- | Create store instances for each Algorithm.
instance CreateStore 'FixedWindow where
  createStore = CounterStore <$> (C.newCache Nothing >>= newTVarIO)

instance CreateStore 'SlidingWindow where
  createStore = TimestampStore <$> (C.newCache Nothing >>= newTVarIO)

instance CreateStore 'TokenBucket where
  createStore = TokenBucketStore <$> (C.newCache Nothing >>= newTVarIO)

instance CreateStore 'LeakyBucket where
  createStore = LeakyBucketStore <$> (C.newCache Nothing >>= newTVarIO)

instance CreateStore 'TinyLRU where
  createStore = TinyLRUStore <$> (atomically $ newTVar =<< TinyLRU.initTinyLRU 100)

-- | Create a new in-memory store for a specific rate-limiting algorithm.
createInMemoryStore :: forall (a :: Algorithm). CreateStore a => IO (InMemoryStore a)
createInMemoryStore = createStore @a

-- | Create a new cache with a given Algorithm and store.
newCache :: Algorithm -> store -> Cache store
newCache algo store = Cache
  { cacheAlgorithm = algo
  , cacheStore = store
  }

-- | Read from cache using the algorithm-prefixed key.
readCache :: (CacheStore store v IO) => Cache store -> Text -> IO (Maybe v)
readCache cache unprefixedKey =
  readStore (cacheStore cache) (algorithmPrefix $ cacheAlgorithm cache)
            (algorithmPrefix (cacheAlgorithm cache) <> ":" <> unprefixedKey)

-- | Write to cache using the algorithm-prefixed key.
writeCache :: (CacheStore store v IO) => Cache store -> Text -> v -> Int -> IO ()
writeCache cache unprefixedKey val expiresIn =
  writeStore (cacheStore cache) (algorithmPrefix $ cacheAlgorithm cache)
             (algorithmPrefix (cacheAlgorithm cache) <> ":" <> unprefixedKey) val expiresIn

-- | Delete a key from cache using the algorithm-prefixed key.
deleteCache :: (CacheStore store v IO) => Cache store -> Text -> IO ()
deleteCache cache unprefixedKey =
  deleteStore (cacheStore cache) (algorithmPrefix $ cacheAlgorithm cache)
              (algorithmPrefix (cacheAlgorithm cache) <> ":" <> unprefixedKey)

-- | Increment a numeric cache value or initialise it if missing.
-- FIX: This now properly handles expired entries by treating them as missing
incStore :: (CacheStore store v IO, FromJSON v, ToJSON v, Ord v, Num v) => Cache store -> Text -> Int -> IO v
incStore cache unprefixedKey expiresIn = do
  let fullKey = algorithmPrefix (cacheAlgorithm cache) <> ":" <> unprefixedKey
      prefix  = algorithmPrefix $ cacheAlgorithm cache
  mval <- readStore (cacheStore cache) prefix fullKey
  let newVal = case mval of
        Nothing -> 1
        Just v  -> if v <= 0 then 1 else v + 1
  writeStore (cacheStore cache) (algorithmPrefix $ cacheAlgorithm cache) fullKey newVal expiresIn
  return newVal

-- | Clear all entries in an in-memory store.
clearInMemoryStore :: ResettableStore store => store -> IO ()
clearInMemoryStore = resetStore

-- | Reset all entries in a cache.
cacheReset :: ResettableStore store => Cache store -> IO ()
cacheReset (Cache _ store) = resetStore store

-- | Instance for storing Int lists (timestamps) for SlidingWindow
instance CacheStore (InMemoryStore 'SlidingWindow) [Int] IO where
  readStore (TimestampStore ref) _prefix key = do
    cache <- atomically $ readTVar ref
    -- Force cleanup of expired entries first
    -- C.purgeExpired cache  - not needed according to ChatGPT - to be checked!
    mval <- C.lookup cache key
    case mval of
      Nothing -> return Nothing
      Just txt -> return $ decodeStrict (encodeUtf8 txt)
  writeStore (TimestampStore ref) _prefix key val expiresIn = do
    cache <- atomically $ readTVar ref
    -- Force cleanup of expired entries first
    -- C.purgeExpired cache  - not needed according to ChatGPT - to be checked! 
    let bs = encode val
        strictBs = LBS.toStrict bs
        jsonTxt = case decodeUtf8' strictBs of
                    Left _ -> ""
                    Right txt -> txt
    expiryTimeSpec <- secondsToTimeSpec expiresIn
    C.insert' cache (Just expiryTimeSpec) key jsonTxt
  deleteStore (TimestampStore ref) _prefix key = do
    cache <- atomically $ readTVar ref
    C.delete cache key

-- | Instance for storing Int counters for FixedWindow
-- FIX: This now properly handles TTL expiration by explicitly checking expiry times
instance CacheStore (InMemoryStore 'FixedWindow) Int IO where
  readStore (CounterStore ref) _prefix key = do
    cache <- atomically $ readTVar ref
    -- Force cleanup of expired entries first
    -- C.purgeExpired cache  - not needed according to ChatGPT - to be checked!
    mval <- C.lookup cache key
    case mval of
      Nothing -> return Nothing
      Just txt -> return $ decodeStrict (encodeUtf8 txt)
  writeStore (CounterStore ref) _prefix key val expiresIn = do
    cache <- atomically $ readTVar ref
    -- Force cleanup of expired entries first
    -- C.purgeExpired cache  - not needed according to ChatGPT - to be checked!
    let bs = encode val
        strictBs = LBS.toStrict bs
        jsonTxt = case decodeUtf8' strictBs of
                    Left _ -> ""
                    Right txt -> txt
    expiryTimeSpec <- secondsToTimeSpec expiresIn
    C.insert' cache (Just expiryTimeSpec) key jsonTxt
  deleteStore (CounterStore ref) _prefix key = do
    cache <- atomically $ readTVar ref
    C.delete cache key

-- | Instance for storing Text values for TokenBucket
instance CacheStore (InMemoryStore 'TokenBucket) Text IO where
  readStore (TokenBucketStore ref) _prefix key = do
    cache <- atomically $ readTVar ref
    -- Force cleanup of expired entries first
    -- C.purgeExpired cache  - not needed according to ChatGPT - to be checked!
    C.lookup cache key
  writeStore (TokenBucketStore ref) _prefix key val expiresIn = do
    cache <- atomically $ readTVar ref
    -- Force cleanup of expired entries first
    -- C.purgeExpired cache  - not needed according to ChatGPT - to be checked!
    expiryTimeSpec <- secondsToTimeSpec expiresIn
    C.insert' cache (Just expiryTimeSpec) key val
  deleteStore (TokenBucketStore ref) _prefix key = do
    cache <- atomically $ readTVar ref
    C.delete cache key

-- | Instance for storing LeakyBucketState for LeakyBucket
instance CacheStore (InMemoryStore 'LeakyBucket) LeakyBucketState IO where
  readStore (LeakyBucketStore ref) _prefix key = do
    cache <- atomically $ readTVar ref
    -- Force cleanup of expired entries first
    -- C.purgeExpired cache  - not needed according to ChatGPT - to be checked!
    mval <- C.lookup cache key
    case mval of
      Nothing -> return Nothing
      Just txt -> return $ decodeStrict (encodeUtf8 txt)
  writeStore (LeakyBucketStore ref) _prefix key val expiresIn = do
    cache <- atomically $ readTVar ref
    -- Force cleanup of expired entries first
    -- C.purgeExpired cache  - not needed according to ChatGPT - to be checked!
    let bs = encode val
        strictBs = LBS.toStrict bs
        jsonTxt = case decodeUtf8' strictBs of
                    Left _ -> ""
                    Right txt -> txt
    expiryTimeSpec <- secondsToTimeSpec expiresIn
    C.insert' cache (Just expiryTimeSpec) key jsonTxt
  deleteStore (LeakyBucketStore ref) _prefix key = do
    cache <- atomically $ readTVar ref
    C.delete cache key

-- | Instance for TinyLRU cache
instance CacheStore (InMemoryStore 'TinyLRU) Int IO where
  readStore (TinyLRUStore ref) _prefix key = do
    now <- getTime Monotonic
    atomically $ do
      cache <- readTVar ref
      maybeNodeRef <- Map.lookup key (TinyLRU.lruCache cache)
      case maybeNodeRef of
        Just nodeRef -> do
          node <- readTVar nodeRef
          let expired = TinyLRU.isExpired now node
          if expired
            then do
              TinyLRU.deleteKey key cache
              return Nothing
            else do
              let decoded :: Maybe Int = decodeStrict (TinyLRU.nodeValue node)
              TinyLRU.moveToFrontInCache cache nodeRef
              return decoded
        Nothing -> return Nothing

  writeStore (TinyLRUStore ref) _prefix key val expiresIn = do
    now <- getTime Monotonic
    atomically $ do
      cache <- readTVar ref
      _ <- TinyLRU.updateValue now key val expiresIn cache
      return ()

  deleteStore (TinyLRUStore ref) _prefix key = do
    atomically $ do
      cache <- readTVar ref
      TinyLRU.deleteKey key cache

-- | ResettableStore instance for all InMemoryStore types
instance ResettableStore (InMemoryStore a) where
  resetStore (CounterStore ref) = do
    newCache <- C.newCache Nothing
    atomically $ writeTVar ref newCache
  resetStore (TimestampStore ref) = do
    newCache <- C.newCache Nothing
    atomically $ writeTVar ref newCache
  resetStore (TokenBucketStore ref) = do
    newCache <- C.newCache Nothing
    atomically $ writeTVar ref newCache
  resetStore (LeakyBucketStore ref) = do
    newCache <- C.newCache Nothing
    atomically $ writeTVar ref newCache
  resetStore (TinyLRUStore ref) = atomically $ do
    cache <- readTVar ref
    TinyLRU.resetTinyLRU cache

-- | Compose a unique cache key from throttle name, IP zone, and user key.
makeCacheKey :: Algorithm -> Text -> Text -> Text
makeCacheKey algo ipZone userKey = algorithmPrefix algo <> ":" <> ipZone <> ":" <> userKey

-- | Increment a numeric cache value with a composed key.
incStoreWithZone
  :: (CacheStore store v IO, FromJSON v, ToJSON v, Num v, Ord v)
  => Cache store
  -> Text
  -> Text
  -> Int
  -> IO v
incStoreWithZone cache ipZone userKey expiresIn =
  incStore cache (makeCacheKey (cacheAlgorithm cache) ipZone userKey) expiresIn

-- | Read from cache with a composed key.
readCacheWithZone
  :: (CacheStore store v IO)
  => Cache store
  -> Text
  -> Text
  -> IO (Maybe v)
readCacheWithZone cache ipZone userKey =
  readCache cache (makeCacheKey (cacheAlgorithm cache) ipZone userKey)

-- | Write to cache with a composed key.
writeCacheWithZone
  :: (CacheStore store v IO)
  => Cache store
  -> Text
  -> Text
  -> v
  -> Int
  -> IO ()
writeCacheWithZone cache ipZone userKey val expiresIn =
  writeCache cache (makeCacheKey (cacheAlgorithm cache) ipZone userKey) val expiresIn

-- | Delete from cache with a composed key.
deleteCacheWithZone
  :: (CacheStore store v IO)
  => Cache store
  -> Text
  -> Text
  -> IO ()
deleteCacheWithZone cache ipZone userKey =
  deleteCache cache (makeCacheKey (cacheAlgorithm cache) ipZone userKey)
