{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Keter.RateLimiter.AutoPurge
  ( TokenBucketEntry(..)
  , LeakyBucketEntry(..)
  , startAutoPurge
  , startCustomPurge
  , startCustomPurgeTokenBucket
  , startCustomPurgeLeakyBucket
  ) where

import Control.Concurrent (forkIO, threadDelay, ThreadId)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Exception (throwIO, AsyncException (ThreadKilled))
import Control.Monad (forever, filterM, void)
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.TQueue as TQueue
import qualified StmContainers.Map as StmMap
import qualified ListT
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Cache as C
import Keter.RateLimiter.Types (TokenBucketState(..), LeakyBucketState(..))
import Control.Monad.IO.Class (liftIO)
import System.Clock (TimeSpec(..), Clock(Monotonic), getTime, toNanoSecs)

-- | Token bucket entry with worker synchronization
data TokenBucketEntry = TokenBucketEntry
  { tbeState :: TVar TokenBucketState
  , tbeQueue :: TQueue.TQueue (MVar Bool)
  , tbeWorkerLock :: TMVar ()  -- Empty = no worker, Full = worker exists
  }

-- | Leaky bucket entry with worker synchronization
data LeakyBucketEntry = LeakyBucketEntry
  { lbeState :: TVar LeakyBucketState
  , lbeQueue :: TQueue.TQueue (MVar Bool)
  , lbeWorkerLock :: TMVar ()  -- Empty = no worker, Full = worker exists
  }

-- | Start a background purge thread that terminates when signaled via MVar.
startAutoPurge :: C.Cache Text v -> Integer -> IO ()
startAutoPurge cache intervalSeconds = do
  purgeSignal <- newMVar ()
  void $ forkIO $ forever $ do
    takeMVar purgeSignal
    startTime <- getTime Monotonic
    C.purgeExpired cache
    endTime <- getTime Monotonic
    let elapsedMicros = (toNanoSecs endTime - toNanoSecs startTime) `div` 1000
        remainingMicros = max (0 :: Integer) (intervalSeconds * 1000000 - elapsedMicros)
    waitUntilNextPurge startTime remainingMicros purgeSignal
  where
    waitUntilNextPurge :: TimeSpec -> Integer -> MVar () -> IO ()
    waitUntilNextPurge startTime remainingMicros purgeSignal = do
      currentTime <- getTime Monotonic
      let elapsedMicros = (toNanoSecs currentTime - toNanoSecs startTime) `div` 1000
      if elapsedMicros >= remainingMicros
        then putMVar purgeSignal ()
        else do
          let sleepMicros = fromIntegral (min remainingMicros (toInteger (maxBound :: Int))) :: Int
          threadDelay sleepMicros
          putMVar purgeSignal ()

-- | Start a background purge thread for a Token-bucket STM map.
startCustomPurgeTokenBucket
  :: StmMap.Map Text TokenBucketEntry
  -> Integer                      -- ^ intervalSeconds
  -> Integer                      -- ^ ttlSeconds
  -> IO ThreadId
startCustomPurgeTokenBucket stmMap intervalSeconds ttlSeconds = startCustomPurge
  (\entry -> do
      TokenBucketState _ lastT <- readTVar (tbeState entry)
      pure lastT)
  (\key entry -> do
      void $ tryTakeTMVar (tbeWorkerLock entry)
      StmMap.delete key stmMap)
  stmMap
  intervalSeconds
  ttlSeconds

-- | Start a background purge thread for a Leaky-bucket STM map.
startCustomPurgeLeakyBucket
  :: StmMap.Map Text LeakyBucketEntry
  -> Integer                      -- ^ intervalSeconds
  -> Integer                      -- ^ ttlSeconds
  -> IO ThreadId
startCustomPurgeLeakyBucket stmMap intervalSeconds ttlSeconds = startCustomPurge
  (\entry -> do
      LeakyBucketState _ lastT <- readTVar (lbeState entry)
      pure lastT)
  (\key entry -> do
      void $ tryTakeTMVar (lbeWorkerLock entry)
      StmMap.delete key stmMap)
  stmMap
  intervalSeconds
  ttlSeconds

-- | Generic purge loop used by both helpers above.
startCustomPurge
  :: forall entry.
     (entry -> STM Int)          -- ^ extract last-activity timestamp
  -> (Text -> entry -> STM ())   -- ^ Custom delete action (key -> entry -> STM ())
  -> StmMap.Map Text entry
  -> Integer                      -- ^ intervalSeconds
  -> Integer                      -- ^ ttlSeconds
  -> IO ThreadId
startCustomPurge getTimestamp deleteAction stmMap intervalSeconds ttlSeconds = do
  purgeSignal <- newMVar ()
  forkIO $ forever $ do
    takeMVar purgeSignal
    startTime <- getTime Monotonic
    now <- floor <$> getPOSIXTime
    expiredKVs <- atomically $ do
      kvs <- ListT.toList (StmMap.listT stmMap)
      filterM
        (\(_, entry) -> do
            ts <- getTimestamp entry
            pure (toInteger now - toInteger ts >= ttlSeconds))
        kvs
    atomically $ mapM_ (\(k, v) -> deleteAction k v) expiredKVs
    endTime <- getTime Monotonic
    let elapsedMicros = (toNanoSecs endTime - toNanoSecs startTime) `div` 1000
        remainingMicros = max (0 :: Integer) (intervalSeconds * 1000000 - elapsedMicros)
    waitUntilNextPurge startTime remainingMicros purgeSignal
  where
    waitUntilNextPurge :: TimeSpec -> Integer -> MVar () -> IO ()
    waitUntilNextPurge startTime remainingMicros purgeSignal = do
      currentTime <- getTime Monotonic
      let elapsedMicros = (toNanoSecs currentTime - toNanoSecs startTime) `div` 1000
      if elapsedMicros >= remainingMicros
        then putMVar purgeSignal ()
        else do
          let sleepMicros = fromIntegral (min remainingMicros (toInteger (maxBound :: Int))) :: Int
          threadDelay sleepMicros
          putMVar purgeSignal ()
