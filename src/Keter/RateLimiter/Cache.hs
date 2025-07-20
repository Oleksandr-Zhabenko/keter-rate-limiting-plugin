{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
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
  , incrementCache
  , newCache
  , createInMemoryStore
  , clearInMemoryStore
  , cacheReset
  , startAutoPurge
  , secondsToTimeSpec
  , makeCacheKey
  , startTokenBucketWorker
  , startLeakyBucketWorker
  , startCustomPurgeTokenBucket
  , startCustomPurgeLeakyBucket
  , createTokenBucketEntry
  , createLeakyBucketEntry
  ) where

import Control.Concurrent.STM
import Control.Concurrent.MVar (putMVar, takeMVar, newMVar, readMVar, MVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (void, forever)
import Control.Concurrent (forkIO, threadDelay)
import Data.Aeson (ToJSON, FromJSON, decodeStrict, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.TinyLRU as TinyLRU
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import qualified Data.Cache as C
import System.Clock (TimeSpec(..), Clock(Monotonic), getTime, toNanoSecs)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified StmContainers.Map as StmMap
import qualified Control.Concurrent.STM.TQueue as TQueue
import qualified Focus
import Keter.RateLimiter.Types (TokenBucketState(..), LeakyBucketState(..))
import Keter.RateLimiter.AutoPurge
import Keter.RateLimiter.TokenBucketWorker (startTokenBucketWorker)
import Data.Maybe (fromMaybe)

-- | Supported rate-limiting algorithms.
data Algorithm = FixedWindow | SlidingWindow | TokenBucket | LeakyBucket | TinyLRU
  deriving (Show, Eq)

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
  incStore :: (FromJSON v, ToJSON v, Ord v, Num v) => store -> Text -> Text -> Int -> m v
  incStore store prefix key expiresIn = do -- Default implementation
      mval <- readStore store prefix key
      let newVal = case mval of
            Nothing -> 1
            Just v -> if v <= 0 then 1 else v + 1
      writeStore store prefix key newVal expiresIn
      return newVal

-- | Typeclass for stores that can be reset.
class ResettableStore store where
  resetStore :: store -> IO ()

-- | In-memory store type parameterized by Algorithm.
data InMemoryStore (a :: Algorithm) where
  CounterStore :: TVar (C.Cache Text Text) -> InMemoryStore 'FixedWindow
  TimestampStore :: TVar (C.Cache Text Text) -> InMemoryStore 'SlidingWindow
  TokenBucketStore :: TVar (StmMap.Map Text TokenBucketEntry) -> InMemoryStore 'TokenBucket
  LeakyBucketStore :: TVar (StmMap.Map Text LeakyBucketEntry) -> InMemoryStore 'LeakyBucket
  TinyLRUStore :: TVar (TinyLRU.TinyLRUCache s) -> InMemoryStore 'TinyLRU

-- | Typeclass for creating stores based on Algorithm.
class CreateStore (a :: Algorithm) where
  createStore :: IO (InMemoryStore a)

-- | Convert seconds to TimeSpec for use with Data.Cache
secondsToTimeSpec :: Int -> IO TimeSpec
secondsToTimeSpec seconds = do
  now <- getTime Monotonic
  return $ now + TimeSpec (fromIntegral seconds) 0

-- | Create store instances for each Algorithm.
instance CreateStore 'FixedWindow where
  createStore = createStoreWith CounterStore

instance CreateStore 'SlidingWindow where
  createStore = createStoreWith TimestampStore

instance CreateStore 'TokenBucket where
  createStore = do
    emptyMap <- atomically (StmMap.new :: STM (StmMap.Map Text TokenBucketEntry))
    tvar <- newTVarIO emptyMap
    void $ startCustomPurgeTokenBucket emptyMap (60 :: Integer) (3600 :: Integer)
    pure $ TokenBucketStore tvar

instance CreateStore 'LeakyBucket where
  createStore = do
    emptyMap <- atomically (StmMap.new :: STM (StmMap.Map Text LeakyBucketEntry))
    tvar <- newTVarIO emptyMap
    void $ startCustomPurgeLeakyBucket emptyMap (60 :: Integer) (3600 :: Integer)
    pure $ LeakyBucketStore tvar

instance CreateStore 'TinyLRU where
  createStore = TinyLRUStore <$> (atomically $ newTVar =<< TinyLRU.initTinyLRU 100)

createStoreWith :: (TVar (C.Cache Text Text) -> InMemoryStore a) -> IO (InMemoryStore a)
createStoreWith mkStore = do
  rawCache <- C.newCache Nothing
  purgeInterval <- newMVar (60 :: Integer) -- Purge every 60 seconds
  purgeSignal <- newMVar ()   -- Signal to trigger purge
  void $ forkIO $ forever $ do
    takeMVar purgeSignal
    interval <- readMVar purgeInterval
    startTime <- getTime Monotonic
    C.purgeExpired rawCache
    endTime <- getTime Monotonic
    let elapsedMicros = (toNanoSecs endTime - toNanoSecs startTime) `div` 1000
        remainingMicros = max (0 :: Integer) (interval * 1000000 - elapsedMicros)
    waitUntilNextPurge startTime remainingMicros purgeSignal
  tvar <- newTVarIO rawCache
  return $ mkStore tvar

-- | Wait until the next purge interval deterministically
waitUntilNextPurge :: TimeSpec -> Integer -> MVar () -> IO ()
waitUntilNextPurge startTime remainingMicros purgeSignal = do
  currentTime <- getTime Monotonic
  let elapsedMicros = (toNanoSecs currentTime - toNanoSecs startTime) `div` 1000
  if elapsedMicros >= remainingMicros
    then putMVar purgeSignal () -- Signal the next purge
    else do
      let sleepMicros = fromIntegral (min remainingMicros (toInteger (maxBound :: Int))) :: Int
      threadDelay sleepMicros -- Use threadDelay for the remaining time
      putMVar purgeSignal ()  -- Signal after waiting

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
incrementCache :: (CacheStore store v IO, FromJSON v, ToJSON v, Ord v, Num v) => Cache store -> Text -> Int -> IO v
incrementCache cache unprefixedKey expiresIn = do
  let fullKey = algorithmPrefix (cacheAlgorithm cache) <> ":" <> unprefixedKey
      prefix = algorithmPrefix (cacheAlgorithm cache)
  incStore (cacheStore cache) prefix fullKey expiresIn

-- | Clear all entries in an in-memory store.
clearInMemoryStore :: ResettableStore store => store -> IO ()
clearInMemoryStore = resetStore

-- | Reset all entries in a cache.
cacheReset :: ResettableStore store => Cache store -> IO ()
cacheReset (Cache _ store) = resetStore store

-- | Helper function to create a TokenBucketEntry with proper TMVar initialization
createTokenBucketEntry :: TokenBucketState -> IO TokenBucketEntry
createTokenBucketEntry state = do
  stateVar <- newTVarIO state
  queue <- atomically TQueue.newTQueue
  workerLock <- atomically newEmptyTMVar
  return $ TokenBucketEntry stateVar queue workerLock

-- | Helper function to create a LeakyBucketEntry with proper TMVar initialization
createLeakyBucketEntry :: LeakyBucketState -> IO LeakyBucketEntry
createLeakyBucketEntry state = do
  stateVar <- newTVarIO state
  queue <- atomically TQueue.newTQueue
  workerLock <- atomically newEmptyTMVar
  return $ LeakyBucketEntry stateVar queue workerLock

-- | CacheStore instances
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
  incStore (CounterStore tvar) _prefix key expiresIn = do
    now <- getTime Monotonic
    expiryTimeSpec <- secondsToTimeSpec expiresIn
    atomically $ do
        cache <- readTVar tvar
        mval <- C.lookupSTM False key cache now
        let currentVal = case mval of
                           Nothing -> 0
                           Just txt -> fromMaybe 0 (decodeStrict (encodeUtf8 txt))
        let newVal = currentVal + 1
        let bs = encode newVal
            strictBs = LBS.toStrict bs
            jsonTxt = case decodeUtf8' strictBs of
                        Left _ -> ""
                        Right txt -> txt
        C.insertSTM key jsonTxt cache (Just expiryTimeSpec)
        return newVal

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

instance CacheStore (InMemoryStore 'TokenBucket) TokenBucketState IO where
  readStore (TokenBucketStore tvar) _prefix key = atomically $ do
    stmMap <- readTVar tvar
    mval <- StmMap.lookup key stmMap
    case mval of
      Nothing -> return Nothing
      Just entry -> Just <$> readTVar (tbeState entry)
  writeStore (TokenBucketStore tvar) _prefix key val _expiresIn = do
    entry <- createTokenBucketEntry val
    atomically $ do
      stmMap <- readTVar tvar
      StmMap.focus (focusInsertOrUpdate tbeState entry val) key stmMap
  deleteStore (TokenBucketStore tvar) _prefix key = atomically $ do
    stmMap <- readTVar tvar
    StmMap.delete key stmMap

instance CacheStore (InMemoryStore 'LeakyBucket) LeakyBucketState IO where
  readStore (LeakyBucketStore tvar) _prefix key = atomically $ do
    stmMap <- readTVar tvar
    mval <- StmMap.lookup key stmMap
    case mval of
      Nothing -> return Nothing
      Just entry -> Just <$> readTVar (lbeState entry)
  writeStore (LeakyBucketStore tvar) _prefix key val _expiresIn = do
    entry <- createLeakyBucketEntry val
    atomically $ do
      stmMap <- readTVar tvar
      StmMap.focus (focusInsertOrUpdate lbeState entry val) key stmMap
  deleteStore (LeakyBucketStore tvar) _prefix key = atomically $ do
    stmMap <- readTVar tvar
    StmMap.delete key stmMap

-- Generic helper for Focus insert-or-update
focusInsertOrUpdate
  :: (entry -> TVar v)
  -> entry
  -> v
  -> Focus.Focus entry STM ()
focusInsertOrUpdate getState entry newVal = Focus.Focus
  (pure ((), Focus.Set entry))
  (\existing -> do
     writeTVar (getState existing) newVal
     pure ((), Focus.Leave))

startLeakyBucketWorker
  :: TVar LeakyBucketState
  -> TQueue.TQueue (MVar Bool)
  -> Int       -- ^ Capacity
  -> Double    -- ^ Leak rate (requests drained per second)
  -> Text      -- ^ Key for logging
  -> IO ()
startLeakyBucketWorker stateVar queue capacity leakRate fullKey = void . forkIO $ forever $ do
  replyVar <- atomically $ readTQueue queue
  now <- liftIO $ floor <$> getPOSIXTime
  result <- atomically $ do
    LeakyBucketState{level, lastTime} <- readTVar stateVar
    let elapsed     = fromIntegral (now - lastTime) :: Double
        leakedLevel = max 0 (level - elapsed * leakRate)
        nextLevel   = leakedLevel + 1
        allowed = nextLevel <= fromIntegral capacity
        finalLevel = if allowed then nextLevel else leakedLevel
        -- Only update lastTime on a successful request. This correctly reflects the
        -- algorithm's logic and avoids unreliable floating-point comparisons.
        updatedLastTime = if allowed then now else lastTime
        newState = LeakyBucketState
          { level = finalLevel
          , lastTime = updatedLastTime
          }
    writeTVar stateVar newState
    pure allowed
  liftIO $ putStrLn $ "LeakyBucket Worker: Key=" ++ T.unpack fullKey ++ ", Allowed=" ++ show result
  liftIO $ putMVar replyVar result

instance CacheStore (InMemoryStore 'TinyLRU) Int IO where
  readStore (TinyLRUStore ref) _prefix key = do
    now <- liftIO $ getTime Monotonic
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
    now <- liftIO $ getTime Monotonic
    atomically $ do
      cache <- readTVar ref
      _ <- TinyLRU.updateValue now key val expiresIn cache
      return ()
  deleteStore (TinyLRUStore ref) _prefix key = do
    atomically $ do
      cache <- readTVar ref
      TinyLRU.deleteKey key cache

-- | ResettableStore instances
instance ResettableStore (InMemoryStore a) where
  resetStore (CounterStore tvar) = resetStoreWith tvar
  resetStore (TimestampStore tvar) = resetStoreWith tvar
  resetStore (TokenBucketStore tvar) = atomically $ do
    stmMap <- readTVar tvar
    StmMap.reset stmMap
  resetStore (LeakyBucketStore tvar) = atomically $ do
    stmMap <- readTVar tvar
    StmMap.reset stmMap
  resetStore (TinyLRUStore ref) = atomically $ do
    cache <- readTVar ref
    TinyLRU.resetTinyLRU cache

resetStoreWith :: TVar (C.Cache Text Text) -> IO ()
resetStoreWith tvar = do
  newCache <- C.newCache Nothing
  atomically $ writeTVar tvar newCache

-- | Compose a unique cache key from throttle name, IP zone, and user key.
makeCacheKey :: Algorithm -> Text -> Text -> Text
makeCacheKey algo ipZone userKey = algorithmPrefix algo <> ":" <> ipZone <> ":" <> userKey
