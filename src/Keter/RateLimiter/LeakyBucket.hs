-- file: Keter.RateLimiter.LeakyBucket.hs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Keter.RateLimiter.LeakyBucket
Description : Leaky bucket rate limiting algorithm implementation
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : stable
Portability : portable

This module implements the core logic for the Leaky Bucket rate-limiting algorithm. The primary goal of this algorithm is to smooth out bursts of requests into a steady, predictable flow. It is conceptually similar to a bucket with a hole in the bottom.

Incoming requests are like water being added to the bucket. The bucket has a finite 'capacity'. If a request arrives when the bucket is full, it is rejected (it "spills over"). The hole in the bucket allows requests to be processed (or "leak out") at a constant 'leakRate'.

This implementation uses a dedicated worker thread for each bucket (e.g., for each unique user or IP address) to process requests from a queue. This ensures that processing is serialized and state is managed safely under concurrent access. The first request for a given key will spawn the worker, which then serves all subsequent requests for that same key.
-}
module Keter.RateLimiter.LeakyBucket
  ( -- * Algorithm Logic
    allowRequest
  ) where

import Control.Concurrent.STM
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Keter.RateLimiter.Types (LeakyBucketState(..))
import Keter.RateLimiter.Cache
import Keter.RateLimiter.AutoPurge (LeakyBucketEntry(..))
import qualified Focus as F
import qualified StmContainers.Map as StmMap

-- | Determines whether a request should be allowed based on the state of its corresponding leaky bucket.
--
-- This function is the primary entry point for the leaky bucket algorithm. When a request arrives,
-- this function is called to check if it should be processed or rejected. It operates by finding
-- or creating a bucket associated with the user's key. Every request is added to a queue for its bucket.
--
-- A dedicated worker thread is responsible for processing the queue for each bucket. This function ensures
-- that a worker is started for a new bucket but avoids starting duplicate workers. The final result
-- (`True` or `False`) is communicated back to the caller once the worker has processed the request.
--
-- ==== __Example__
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Keter.RateLimiter...
-- >   isAllowed <- allowRequest cache "us-east-1" "user-123" 10 1.0
-- >
-- >   if isAllowed
-- >     then putStrLn "Request is allowed."
-- >     else putStrLn "Request is blocked (429 Too Many Requests)."
--
allowRequest
  :: MonadIO m
  => Cache (InMemoryStore 'LeakyBucket)
  -> Text
  -> Text
  -> Text
  -> Int
  -> Double
  -> m Bool
allowRequest cache throttleName ipZone userKey capacity leakRate = liftIO $ do
  if capacity <= 0 then pure False else do
    now <- realToFrac <$> getPOSIXTime
    let fullKey = makeCacheKey throttleName LeakyBucket ipZone userKey
        LeakyBucketStore tvBuckets = cacheStore cache
    replyVar <- newEmptyTMVarIO
    newEntry <- createLeakyBucketEntry (LeakyBucketState 0 now)
    entry <- atomically $ do
      buckets <- readTVar tvBuckets
      StmMap.focus
        (F.cases (newEntry, F.Set newEntry)
                 (\existing -> (existing, F.Leave)))
        fullKey buckets
    atomically $ writeTQueue (lbeQueue entry) replyVar
    started <- atomically $ tryPutTMVar (lbeWorkerLock entry) ()
    when started $
      startLeakyBucketWorker
        (lbeState entry)
        (lbeQueue entry)
        capacity
        leakRate
    atomically $ takeTMVar replyVar
