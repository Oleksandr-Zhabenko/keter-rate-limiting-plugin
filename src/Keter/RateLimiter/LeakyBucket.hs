-- file: Keter.RateLimiter.LeakyBucket.hs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Keter.RateLimiter.LeakyBucket
  ( allowRequest
  ) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue (TQueue, writeTQueue)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Keter.RateLimiter.Types (LeakyBucketState(..))
import Keter.RateLimiter.Cache
import Keter.RateLimiter.AutoPurge (LeakyBucketEntry(..))
import qualified Focus as F
import qualified StmContainers.Map as StmMap

allowRequest
  :: MonadIO m
  => Cache (InMemoryStore 'LeakyBucket)
  -> Text     -- ^ IP-zone
  -> Text     -- ^ User key
  -> Int      -- ^ Capacity
  -> Double   -- ^ Leak rate (requests per second)
  -> m Bool
allowRequest cache ipZone userKey capacity leakRate = liftIO $ do
  if capacity <= 0 then pure False else do
    now <- realToFrac <$> getPOSIXTime
    let fullKey = makeCacheKey LeakyBucket ipZone userKey
        LeakyBucketStore tvBuckets = cacheStore cache
    replyVar <- newEmptyTMVarIO

    -- Create a template for a new entry. Its state is neutral (level 0).
    newEntry <- createLeakyBucketEntry (LeakyBucketState 0 now)

    -- FIX: Use the correct atomic get-or-create pattern.
    entry <- atomically $ do
      buckets <- readTVar tvBuckets
      StmMap.focus
        -- If key is missing: return newEntry and Set it in the map.
        (F.cases (newEntry, F.Set newEntry)
                 -- If key exists: return the existing entry and Leave it alone.
                 (\existing -> (existing, F.Leave)))
        fullKey buckets

    -- All requests queue up to be processed by the worker.
    atomically $ writeTQueue (lbeQueue entry) replyVar

    -- The first request to arrive will start the worker; subsequent requests won't.
    started <- atomically $ tryPutTMVar (lbeWorkerLock entry) ()
    when started $
      startLeakyBucketWorker
        (lbeState entry)
        (lbeQueue entry)
        capacity
        leakRate
        fullKey

    -- All requests wait for the worker to provide a result.
    atomically $ takeTMVar replyVar
