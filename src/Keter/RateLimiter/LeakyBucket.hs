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
--import Control.Concurrent.STM.TQueue (TQueue, writeTQueue)
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
-- > import Keter.RateLimiter.LeakyBucket (allowRequest)
-- > import Keter.RateLimiter.Cache
-- >
-- > main :: IO ()
-- > main = do
-- >   -- Initialize the cache store for the LeakyBucket algorithm.
-- >   leakyStore <- createStore @'LeakyBucket
-- >   let cache = newCache LeakyBucket leakyStore
-- >
-- >   -- Simulate a request for a user from a specific IP zone.
-- >   -- Capacity: 10 requests (burst limit)
-- >   -- Leak Rate: 1 request per second
-- >   isAllowed <- allowRequest cache "us-east-1" "user-123" 10 1.0
-- >
-- >   if isAllowed
-- >     then putStrLn "Request is allowed."
-- >     else putStrLn "Request is blocked (429 Too Many Requests)."
--
allowRequest
  :: MonadIO m
  => Cache (InMemoryStore 'LeakyBucket)
  -- ^ The cache containing the state of all leaky buckets.
  -> Text
  -- ^ The IP-based zone for segmenting the cache (e.g., "eu-central-1").
  -> Text
  -- ^ A unique key identifying the user or client (e.g., an IP address or API key).
  -> Int
  -- ^ The bucket's capacity (the maximum burst size). If a request arrives when the bucket is full, it will be rejected.
  -> Double
  -- ^ The rate at which the bucket "leaks" (in requests per second). This determines the sustained throughput.
  -> m Bool
  -- ^ Returns 'True' if the request is allowed, 'False' otherwise.
allowRequest cache ipZone userKey capacity leakRate = liftIO $ do
  -- Immediately reject if capacity is non-positive, as it's an invalid configuration.
  if capacity <= 0 then pure False else do
    now <- realToFrac <$> getPOSIXTime
    let fullKey = makeCacheKey LeakyBucket ipZone userKey
        LeakyBucketStore tvBuckets = cacheStore cache
    replyVar <- newEmptyTMVarIO

    -- Create a template for a new bucket entry. Its state is neutral (level 0).
    newEntry <- createLeakyBucketEntry (LeakyBucketState 0 now)

    -- Atomically get the existing bucket for the key or create a new one.
    -- This 'focus' operation provides an atomic get-or-create pattern.
    entry <- atomically $ do
      buckets <- readTVar tvBuckets
      StmMap.focus
        -- If the key is missing, create and insert the newEntry.
        (F.cases (newEntry, F.Set newEntry)
                 -- If the key exists, just return the existing entry.
                 (\existing -> (existing, F.Leave)))
        fullKey buckets

    -- All requests, new or existing, queue up to be processed by the worker.
    -- This serializes access to the bucket's state.
    atomically $ writeTQueue (lbeQueue entry) replyVar

    -- The first request to arrive will acquire the lock and start the worker.
    -- Subsequent requests for the same bucket won't be able to acquire the lock
    -- and will simply wait for the running worker to process them.
    started <- atomically $ tryPutTMVar (lbeWorkerLock entry) ()
    when started $
      startLeakyBucketWorker
        (lbeState entry)
        (lbeQueue entry)
        capacity
        leakRate
        fullKey

    -- All callers block here, waiting for the worker to process their specific
    -- request and provide a boolean result in the reply variable.
    atomically $ takeTMVar replyVar
