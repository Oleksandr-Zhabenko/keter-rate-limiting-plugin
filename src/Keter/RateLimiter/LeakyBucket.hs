{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Keter.RateLimiter.LeakyBucket
  ( allowRequest
  ) where

import Keter.RateLimiter.LeakyBucketState
import Keter.RateLimiter.Cache
import Data.Text (Text)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import qualified Focus                     as F
import qualified StmContainers.Map         as StmMap
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Concurrent.MVar
import Control.Monad (void, when, forever)
import qualified Data.Text as T

type LeakyBucketMap = StmMap.Map Text (TVar LeakyBucketState, TQueue (MVar Bool))

-- | Spawn a background worker for a given user key to handle requests sequentially.
startWorker
  :: TVar LeakyBucketState
  -> TQueue (MVar Bool)
  -> Int       -- ^ Capacity
  -> Int       -- ^ Leak rate
  -> IO ()
startWorker stateVar queue capacity leakRate = void . forkIO $ forever $ do
  replyVar <- atomically $ readTQueue queue
  now <- floor <$> getPOSIXTime
  result <- atomically $ do
    LeakyBucketState{level, lastTime} <- readTVar stateVar
    let elapsed     = fromIntegral (now - lastTime) :: Double
        leakedLevel = max 0 (level - elapsed * fromIntegral leakRate)
        nextLevel   = leakedLevel + 1
        newState    = LeakyBucketState
          { level = if nextLevel > fromIntegral capacity then leakedLevel else nextLevel
          , lastTime = now
          }
        allowed = nextLevel <= fromIntegral capacity
    writeTVar stateVar newState
    return allowed
  putMVar replyVar result

-- | Request permission under leaky bucket, per IP zone and user key.
allowRequest
  :: Cache LeakyBucketCacheStore
  -> Text              -- ^ IP-zone
  -> Text              -- ^ user key
  -> Int               -- ^ capacity
  -> Int               -- ^ leak rate
  -> IO Bool
allowRequest (Cache _ (LeakyBucketCacheStore mapVar))
             ipZone userKey capacity leakRate = do
  let fullKey = makeCacheKey LeakyBucket ipZone userKey
  replyVar <- newEmptyMVar
  now      <- floor <$> getPOSIXTime

  -- Use Focus to get or create the bucket pair, tracking if it was newly created
  wasNew <- atomically $ 
    StmMap.focus (F.casesM
        -- Case: bucket doesn't exist, create new one
        (do
          stateVar <- newTVar $ LeakyBucketState 0 now
          queue    <- newTQueue
          writeTQueue queue replyVar
          pure (True, F.Set (stateVar, queue))
        )
        -- Case: bucket exists, use existing one
        (\(stateVar, queue) -> do
          writeTQueue queue replyVar
          pure (False, F.Leave)
        )
      ) fullKey mapVar

  -- If bucket was newly created, start worker
  when wasNew $ do
    -- Get the bucket pair from the map since we just created it
    maybeBucketPair <- atomically $ StmMap.lookup fullKey mapVar
    case maybeBucketPair of
      Just (stateVar, queue) -> startWorker stateVar queue capacity leakRate
      Nothing -> error "Race condition: bucket should exist after creation"

  takeMVar replyVar
