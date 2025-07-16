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
  , CreateStore(..)
  , algorithmPrefix
  , readCache
  , writeCache
  , deleteCache
  , incStore
  , newCache
  , createInMemoryStore
  , clearInMemoryStore
  , cacheReset
  , startAutoPurge
  , secondsToTimeSpec
  , makeCacheKey
  , incStoreWithZone
  , readCacheWithZone
  , writeCacheWithZone
  , deleteCacheWithZone
  , LeakyBucketCacheStore(..)
  , SlidingWindowStore(..)
  , TokenBucketState(..)
  , startCustomPurge
  , startCustomPurgeLeakyBucket
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (void, forever, filterM)
import Control.Concurrent (forkIO, threadDelay, killThread, ThreadId)
import Control.Concurrent.MVar (MVar, newEmptyMVar, tryPutMVar, tryTakeMVar)
import Control.Exception (throwIO, AsyncException(ThreadKilled))
import Data.Hashable (Hashable(..))
import Data.Aeson (ToJSON, FromJSON, decodeStrict, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Semigroup ((<>))
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import qualified Data.Cache as C
import GHC.Generics (Generic)
import Keter.RateLimiter.LeakyBucketState (LeakyBucketState(..))
import System.Clock (TimeSpec(..), Clock(Monotonic), getTime)
import Data.Time.Clock (addUTCTime, getCurrentTime, NominalDiffTime)
import Data.Fixed (Pico)
import qualified Data.TinyLRU as TinyLRU
import qualified StmContainers.Map as StmMap
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as StrictMap
import Data.Time.Clock.POSIX (getPOSIXTime)
import ListT (ListT, toList)

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

data SlidingWindowStore = SlidingWindowStore (TVar (Map Text (TVar [Int], TQueue ())))

data LeakyBucketCacheStore = LeakyBucketCacheStore (StmMap.Map Text (TVar LeakyBucketState, TQueue (MVar Bool)))

-- | Represents the state of a token bucket.
data TokenBucketState = TokenBucketState
  { tokens     :: Int
  , lastUpdate :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON TokenBucketState
instance FromJSON TokenBucketState

-- | Convert seconds to TimeSpec for use with Data.Cache
secondsToTimeSpec :: Int -> IO TimeSpec
secondsToTimeSpec seconds = do
  now <- getTime Monotonic
  return $ now + TimeSpec (fromIntegral seconds) 0

-- | Start a background purge thread that terminates when signaled via MVar.
startAutoPurge :: (Hashable k) => C.Cache k v -> Int -> IO ()
startAutoPurge cache intervalSeconds = do
  stopSignal <- newEmptyMVar
  void $ forkIO $ forever $ do
    threadDelay (intervalSeconds * 1000000)
    stop <- tryTakeMVar stopSignal
    case stop of
      Just () -> throwIO ThreadKilled
      Nothing -> C.purgeExpired cache

-- | Start a background purge thread for SlidingWindowStore or similar structures.
startCustomPurge :: TVar (Map Text (TVar v, TQueue ())) -> Int -> Int -> (v -> Int -> Bool) -> IO ThreadId
startCustomPurge tvar intervalSeconds ttlSeconds isValid = do
  stopSignal <- newEmptyMVar
  forkIO $ forever $ do
    threadDelay (intervalSeconds * 1000000)
    stop <- tryTakeMVar stopSignal
    case stop of
      Just () -> throwIO ThreadKilled
      Nothing -> do
        now <- floor <$> getPOSIXTime
        atomically $ do
          m <- readTVar tvar
          filtered <- StrictMap.fromList <$> filterM
            (\(k, (tv, q)) -> do
                val <- readTVar tv
                return $ isValid val now)
            (StrictMap.toList m)
          writeTVar tvar filtered

-- | Start a background purge thread for LeakyBucketCacheStore.
startCustomPurgeLeakyBucket
  :: StmMap.Map Text (TVar LeakyBucketState, TQueue (MVar Bool))
  -> Int
  -> Int
  -> (LeakyBucketState -> Int -> Bool)
  -> IO ThreadId
startCustomPurgeLeakyBucket stmMap intervalSeconds ttlSeconds isValid = do
  stopSignal <- newEmptyMVar
  forkIO $ forever $ do
    threadDelay (intervalSeconds * 1000000)
    stop <- tryTakeMVar stopSignal
    case stop of
      Just () -> throwIO ThreadKilled
      Nothing -> do
        now <- floor <$> getPOSIXTime
        atomically $ do
          keys <- toList $ StmMap.listT stmMap
          expiredKeys <- filterM
            (\(_k, (tv, _)) -> do
                val <- readTVar tv
                return $ not (isValid val now))
            keys
          mapM_ (\(k, _) -> StmMap.delete k stmMap) expiredKeys

-- | General function to create a store with a purge thread.
--   FIXED: Removed `bracket` which was incorrectly terminating the purge thread
--   immediately after creation. The thread is now forked to run for the
--   application's lifetime, which is the intended behavior for a background service.
createStoreWith :: (TVar (C.Cache Text Text) -> InMemoryStore a) -> IO (InMemoryStore a)
createStoreWith mkStore = do
  rawCache <- C.newCache Nothing
  -- Fork a thread to purge expired items every 60 seconds.
  void $ forkIO $ forever $ do
    threadDelay (60 * 1000000)
    C.purgeExpired rawCache
  tvar <- newTVarIO rawCache
  return $ mkStore tvar

-- | Create store instances for each Algorithm.
instance CreateStore 'FixedWindow where
  createStore = createStoreWith CounterStore

instance CreateStore 'SlidingWindow where
  createStore = createStoreWith TimestampStore

instance CreateStore 'TokenBucket where
  createStore = createStoreWith TokenBucketStore

instance CreateStore 'LeakyBucket where
  createStore = createStoreWith LeakyBucketStore

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
  readStore (TimestampStore tvar) _prefix key = do
    cache <- readTVarIO tvar
    mval <- C.lookup cache key
    case mval of
      Nothing -> return Nothing
      Just txt -> return $ decodeStrict (encodeUtf8 txt)
  writeStore (TimestampStore tvar) _prefix key val expiresIn = do
    let bs = encode val
        strictBs = LBS.toStrict bs
        jsonTxt = case decodeUtf8' strictBs of
                    Left _ -> ""
                    Right txt -> txt
    expiryTimeSpec <- secondsToTimeSpec expiresIn
    atomically $ do
      cache <- readTVar tvar
      C.insertSTM key jsonTxt cache (Just expiryTimeSpec)
  deleteStore (TimestampStore tvar) _prefix key = do
    cache <- readTVarIO tvar
    C.delete cache key

-- | Instance for storing Int counters for FixedWindow
instance CacheStore (InMemoryStore 'FixedWindow) Int IO where
  readStore (CounterStore tvar) _prefix key = do
    cache <- readTVarIO tvar
    mval <- C.lookup cache key
    case mval of
      Nothing -> return Nothing
      Just txt -> return $ decodeStrict (encodeUtf8 txt)
  writeStore (CounterStore tvar) _prefix key val expiresIn = do
    let bs = encode val
        strictBs = LBS.toStrict bs
        jsonTxt = case decodeUtf8' strictBs of
                    Left _ -> ""
                    Right txt -> txt
    expiryTimeSpec <- secondsToTimeSpec expiresIn
    atomically $ do
      cache <- readTVar tvar
      C.insertSTM key jsonTxt cache (Just expiryTimeSpec)
  deleteStore (CounterStore tvar) _prefix key = do
    cache <- readTVarIO tvar
    C.delete cache key

-- | Instance for storing TokenBucketState for TokenBucket
instance CacheStore (InMemoryStore 'TokenBucket) TokenBucketState IO where
  readStore (TokenBucketStore tvar) _prefix key = do
    cache <- readTVarIO tvar
    mval <- C.lookup cache key
    case mval of
      Nothing -> return Nothing
      Just txt -> return $ decodeStrict (encodeUtf8 txt)

  writeStore (TokenBucketStore tvar) _prefix key val expiresIn = do
    let bs = encode val
        strictBs = LBS.toStrict bs
        jsonTxt = case decodeUtf8' strictBs of
                    Left _ -> ""
                    Right txt -> txt
    expiryTimeSpec <- secondsToTimeSpec expiresIn
    atomically $ do
      cache <- readTVar tvar
      C.insertSTM key jsonTxt cache (Just expiryTimeSpec)

  deleteStore (TokenBucketStore tvar) _prefix key = do
    cache <- readTVarIO tvar
    C.delete cache key

-- | Instance for storing LeakyBucketState for LeakyBucket
instance CacheStore (InMemoryStore 'LeakyBucket) LeakyBucketState IO where
  readStore (LeakyBucketStore tvar) _prefix key = do
    cache <- readTVarIO tvar
    mval <- C.lookup cache key
    case mval of
      Nothing -> return Nothing
      Just txt -> return $ decodeStrict (encodeUtf8 txt)
  writeStore (LeakyBucketStore tvar) _prefix key val expiresIn = do
    let bs = encode val
        strictBs = LBS.toStrict bs
        jsonTxt = case decodeUtf8' strictBs of
                    Left _ -> ""
                    Right txt -> txt
    expiryTimeSpec <- secondsToTimeSpec expiresIn
    atomically $ do
      cache <- readTVar tvar
      C.insertSTM key jsonTxt cache (Just expiryTimeSpec)
  deleteStore (LeakyBucketStore tvar) _prefix key = do
    cache <- readTVarIO tvar
    C.delete cache key

-- | Instance for TinyLRU cache
instance CacheStore (InMemoryStore 'TinyLRU) Int IO where
  readStore (TinyLRUStore ref) _prefix key = do
    now <- getTime Monotonic
    atomically $ do
      cache <- readTVar ref
      maybeNodeRef <- StmMap.lookup key (TinyLRU.lruCache cache)
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

-- | Instance for SlidingWindowStore
instance CacheStore SlidingWindowStore [Int] IO where
  readStore (SlidingWindowStore tvar) _prefix key = do
    m <- readTVarIO tvar
    case StrictMap.lookup key m of
      Nothing -> return Nothing
      Just (tv, _) -> Just <$> readTVarIO tv
  writeStore (SlidingWindowStore tvar) _prefix key val _expiresIn = do
    atomically $ do
      m <- readTVar tvar
      case StrictMap.lookup key m of
        Nothing -> do
          tv <- newTVar val
          q <- newTQueue
          writeTVar tvar $ StrictMap.insert key (tv, q) m
        Just (tv, _) -> writeTVar tv val
  deleteStore (SlidingWindowStore tvar) _prefix key = do
    atomically $ do
      m <- readTVar tvar
      writeTVar tvar $ StrictMap.delete key m

-- | Instance for LeakyBucketCacheStore
instance CacheStore LeakyBucketCacheStore LeakyBucketState IO where
  readStore (LeakyBucketCacheStore stmMap) _prefix key = atomically $ do
    m <- StmMap.lookup key stmMap
    case m of
      Nothing -> return Nothing
      Just (tv, _) -> Just <$> readTVar tv

  writeStore (LeakyBucketCacheStore stmMap) _prefix key val _expiresIn = do
    atomically $ do
      m <- StmMap.lookup key stmMap
      case m of
        Just (tv, _) -> writeTVar tv val
        Nothing -> do
          tv <- newTVar val
          q <- newTQueue
          StmMap.insert (tv, q) key stmMap

  deleteStore (LeakyBucketCacheStore stmMap) _prefix key =
    atomically $ StmMap.delete key stmMap

-- | Helper function to reset a TVar-based store with a new cache.
resetStoreWith :: TVar (C.Cache Text Text) -> IO ()
resetStoreWith tvar = do
  newCache <- C.newCache Nothing
  atomically $ writeTVar tvar newCache

-- | ResettableStore instance for all InMemoryStore types
instance ResettableStore (InMemoryStore a) where
  resetStore (CounterStore tvar) = resetStoreWith tvar
  resetStore (TimestampStore tvar) = resetStoreWith tvar
  resetStore (TokenBucketStore tvar) = resetStoreWith tvar
  resetStore (LeakyBucketStore tvar) = resetStoreWith tvar
  resetStore (TinyLRUStore ref) = atomically $ do
    cache <- readTVar ref
    TinyLRU.resetTinyLRU cache

-- | ResettableStore instance for SlidingWindowStore
instance ResettableStore SlidingWindowStore where
  resetStore (SlidingWindowStore tvar) = atomically $ writeTVar tvar StrictMap.empty

-- | ResettableStore instance for LeakyBucketCacheStore
instance ResettableStore LeakyBucketCacheStore where
  resetStore (LeakyBucketCacheStore stmMap) = atomically $ StmMap.reset stmMap

-- | Compose a unique cache key from throttle name, IP zone, and user key.
makeCacheKey :: Algorithm -> Text -> Text -> Text
makeCacheKey algo ipZone userKey = algorithmPrefix algo <> ":" <> ipZone <> ":" <> userKey

-- | Increment a numeric cache value with a composed key.
incStoreWithZone
  :: (CacheStore store v IO, FromJSON v, ToJSON v, Num v, Ord v)
  => Cache store -> Text -> Text -> Int -> IO v
incStoreWithZone cache ipZone userKey expiresIn = do
  let key = makeCacheKey (cacheAlgorithm cache) ipZone userKey
      prefix = algorithmPrefix (cacheAlgorithm cache)
  mval <- readStore (cacheStore cache) prefix key
  let newVal = case mval of
        Nothing -> 1
        Just v  -> if v <= 0 then 1 else v + 1
  writeStore (cacheStore cache) prefix key newVal expiresIn
  return newVal

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
