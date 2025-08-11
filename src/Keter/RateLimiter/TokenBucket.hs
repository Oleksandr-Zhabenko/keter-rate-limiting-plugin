{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

-- |
-- Module      : Keter.RateLimiter.TokenBucket
-- Description : Token bucket rate limiting algorithm implementation
-- Copyright   : (c) 2025 Oleksandr Zhabenko
-- License     : MIT
-- Maintainer  : oleksandr.zhabenko@yahoo.com
-- Stability   : stable
-- Portability : portable
--
-- This module provides a rate limiter based on the /Token Bucket/ algorithm.
-- It integrates with the "Keter.RateLimiter.Cache" infrastructure and uses STM
-- and worker threads to manage refill and request allowance.
--
-- The token bucket algorithm allows for a configurable burst size (capacity)
-- and replenishes tokens over time at a fixed rate. If a request is made and
-- a token is available, the request is allowed and a token is consumed.
-- Otherwise, the request is denied.
--
-- == Example usage
--
-- @
-- import Keter.RateLimiter.TokenBucket (allowRequest)
-- 
-- allowed <- allowRequest cache \"zone1\" \"user123\" 10 2.5 60
-- when allowed $ doSomething
-- @
--
-- This call checks if a request by @\"user123\"@ in @\"zone1\"@ is allowed, given a
-- bucket with a capacity of 10, a refill rate of 2.5 tokens\/second, and a TTL of 60 seconds.

module Keter.RateLimiter.TokenBucket
  ( -- * Request Evaluation
    allowRequest
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Data.Text                (Text)
import Data.Time.Clock.POSIX    (getPOSIXTime)

import Keter.RateLimiter.Cache
import Keter.RateLimiter.Types          (TokenBucketState (..))
import Keter.RateLimiter.AutoPurge      (TokenBucketEntry (..))
import qualified Focus                  as F
import qualified StmContainers.Map      as StmMap

------------------------------------------------------------------------------

-- | Minimum TTL allowed for a token bucket.
-- 
-- Requests with TTLs less than this threshold are denied to avoid race conditions
-- or unbounded cleanup complexity.
minTTL :: Int
minTTL = 2

-- | Check whether a request may pass through the token-bucket limiter.
--
-- This function enforces rate-limiting per (IP zone, user key) combination.
-- Each request will either:
--
-- * Succeed immediately if the request belongs to a new bucket and capacity allows.
-- * Be enqueued and handled asynchronously if the bucket already exists.
-- * Be denied if no tokens are available or TTL is invalid.
--
-- The token bucket is defined by:
--
-- * /capacity/: maximum number of tokens in the bucket (i.e., max burst size).
-- * /refillRate/: tokens added per second (can be fractional).
-- * /expiresIn/: TTL in seconds; determines how long idle buckets live.
--
-- The function performs the following steps:
--
-- 1. Validates that TTL meets the minimum threshold
-- 2. Creates or retrieves the token bucket for the given key
-- 3. For new buckets: starts a worker thread and allows the first request
-- 4. For existing buckets: queues the request and waits for the worker's response
--
-- ==== __Examples__
--
-- @
-- -- Allow 100 requests per minute with burst capacity of 10
-- let capacity = 10
--     refillRate = 100.0 \/ 60.0  -- ~1.67 tokens per second
--     ttl = 300                  -- 5 minutes TTL
--
-- result <- allowRequest cache \"api-throttle\" \"192.168.1.1\" \"user456\" capacity refillRate ttl
-- if result
--   then putStrLn \"Request allowed\"
--   else putStrLn \"Request denied - rate limit exceeded\"
-- @
--
-- @
-- -- High-frequency API with small bursts
-- allowed <- allowRequest cache \"fast-api\" \"zone-premium\" \"client789\" 5 10.0 120
-- @
--
-- /Thread Safety:/ This function is thread-safe and can be called concurrently
-- from multiple threads for the same or different keys.
--
-- /Performance:/ For new buckets, there's a one-time setup cost of starting
-- a worker thread. Subsequent requests are processed asynchronously with
-- minimal blocking.
allowRequest
  :: MonadIO m
  => Cache (InMemoryStore 'TokenBucket)
  -- ^ Token bucket cache instance
  -> Text
  -- ^ Throttle name (logical grouping identifier)
  -> Text
  -- ^ IP zone identifier
  -> Text
  -- ^ User key (unique client identifier)
  -> Int
  -- ^ Bucket capacity (maximum tokens, must be positive)
  -> Double
  -- ^ Refill rate in tokens per second (must be positive, can be fractional)
  -> Int
  -- ^ TTL in seconds (must be >= 'minTTL')
  -> m Bool
  -- ^ 'True' if request is allowed, 'False' if denied
allowRequest cache throttleName ipZone userKey capacity refillRate expiresIn = liftIO $
  if expiresIn < minTTL
     then do
       pure False
     else do
       now <- floor <$> getPOSIXTime
       let key = makeCacheKey throttleName (cacheAlgorithm cache) ipZone userKey
           TokenBucketStore tvBuckets = cacheStore cache
       replyVar <- newEmptyMVar
       newEntryInitialState <- createTokenBucketEntry (TokenBucketState (capacity - 1) now)
       
       (wasNew, entry) <- atomically $ do
         buckets <- readTVar tvBuckets
         (wasNewEntry, ent) <-
           StmMap.focus
             (F.Focus
                (pure ((True, newEntryInitialState), F.Set newEntryInitialState))
                (\existingEnt -> do
                  workerLockEmpty <- isEmptyTMVar (tbeWorkerLock existingEnt)
                  if workerLockEmpty
                    then pure ((True, newEntryInitialState), F.Set newEntryInitialState)
                    else pure ((False, existingEnt), F.Leave)
                )
             )
             key buckets
         pure (wasNewEntry, ent)
       if wasNew
         then
           if capacity > 0
             then do
               workerReadyVar <- atomically newEmptyTMVar
               atomically $ putTMVar (tbeWorkerLock entry) ()
               startTokenBucketWorker (tbeState entry)
                                      (tbeQueue entry)
                                      capacity
                                      refillRate
                                      workerReadyVar
               atomically $ takeTMVar workerReadyVar
               pure True
             else
               pure False
         else do
           atomically $ writeTQueue (tbeQueue entry) replyVar
           result <- takeMVar replyVar
           pure result