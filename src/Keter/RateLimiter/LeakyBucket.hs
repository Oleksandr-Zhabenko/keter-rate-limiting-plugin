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

Incoming requests are like water being added to the bucket. The bucket has a finite capacity. If a request arrives when the bucket is full, it is rejected (it \"spills over\"). The hole in the bucket allows requests to be processed (or \"leak out\") at a constant leak rate.

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
-- ('True' or 'False') is communicated back to the caller once the worker has processed the request.
--
-- === Algorithm Flow
--
-- 1. __Validation__: Check if capacity is valid (> 0)
-- 2. __Key Construction__: Build composite cache key from throttle name, IP zone, and user key
-- 3. __Bucket Management__: Find existing bucket or create new one atomically
-- 4. __Request Queuing__: Add request to bucket's processing queue
-- 5. __Worker Management__: Start worker thread for new buckets (one-time operation)
-- 6. __Response__: Wait for worker to process request and return result
--
-- === Concurrency Model
--
-- * Each bucket has its own worker thread for serialized processing
-- * Multiple clients can safely call this function concurrently
-- * STM ensures atomic bucket creation and state management
-- * Workers are started lazily on first request per bucket
--
-- ==== __Examples__
--
-- @
-- -- Basic usage: 10 request capacity, 1 request per second leak rate
-- isAllowed <- allowRequest cache \"api-throttle\" \"us-east-1\" \"user-123\" 10 1.0
-- if isAllowed
--   then putStrLn \"Request is allowed.\"
--   else putStrLn \"Request is blocked (429 Too Many Requests).\"
-- @
--
-- @
-- -- High-throughput API with burst tolerance
-- let capacity = 100        -- Allow burst of up to 100 requests
--     leakRate = 10.0       -- Process 10 requests per second steadily
-- 
-- result <- allowRequest cache \"high-volume\" \"zone-premium\" \"client-456\" capacity leakRate
-- when result $ processApiRequest
-- @
--
-- @
-- -- Rate limiting for different service tiers
-- let (cap, rate) = case userTier of
--       Premium -> (50, 5.0)   -- 50 burst, 5\/sec sustained
--       Standard -> (20, 2.0)  -- 20 burst, 2\/sec sustained  
--       Basic -> (5, 0.5)      -- 5 burst, 1 per 2 seconds
-- 
-- allowed <- allowRequest cache \"tiered-api\" zone userId cap rate
-- @
--
-- /Thread Safety:/ This function is fully thread-safe and can be called
-- concurrently from multiple threads.
--
-- /Performance:/ New buckets incur a one-time worker thread creation cost.
-- Subsequent requests are queued with minimal overhead.
allowRequest
  :: MonadIO m
  => Cache (InMemoryStore 'LeakyBucket)
  -- ^ Leaky bucket cache instance
  -> Text
  -- ^ Throttle name (logical grouping identifier)
  -> Text
  -- ^ IP zone identifier for multi-tenant isolation
  -> Text
  -- ^ User key (unique client identifier)
  -> Int
  -- ^ Bucket capacity (maximum queued requests, must be > 0)
  -> Double
  -- ^ Leak rate in requests per second (must be positive)
  -> m Bool
  -- ^ 'True' if request is allowed, 'False' if bucket is full
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