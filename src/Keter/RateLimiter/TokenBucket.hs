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
-- It integrates with the `Keter.RateLimiter.Cache` infrastructure and uses STM
-- and worker threads to manage refill and request allowance.
--
-- The token bucket algorithm allows for a configurable burst size (`capacity`)
-- and replenishes tokens over time at a fixed rate. If a request is made and
-- a token is available, the request is allowed and a token is consumed.
-- Otherwise, the request is denied.
--
-- == Example usage
--
-- > import Keter.RateLimiter.TokenBucket (allowRequest)
-- > 
-- > allowed <- allowRequest cache "zone1" "user123" 10 2.5 60
-- > when allowed $ doSomething
--
-- This call checks if a request by "user123" in "zone1" is allowed, given a
-- bucket with a capacity of 10, a refill rate of 2.5 tokens/second, and a TTL of 60 seconds.

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
-- - @capacity@: maximum number of tokens in the bucket (i.e., max burst size).
-- - @refillRate@: tokens added per second (can be fractional).
-- - @expiresIn@: TTL in seconds; determines how long idle buckets live.
--
-- == Parameters
--
-- [@cache@] The configured cache for storing per-user token bucket state.
-- [@ipZone@] A label identifying the IP zone (e.g., region or tenant).
-- [@userKey@] A unique identifier for the user (e.g., IP address or token).
-- [@capacity@] Maximum tokens the bucket can hold.
-- [@refillRate@] Token refill rate (tokens per second).
-- [@expiresIn@] Time-to-live for idle entries in seconds.
--
-- == Returns
--
-- A boolean in an arbitrary `MonadIO` context indicating whether the request is allowed.
--
-- == Example
--
-- > allowed <- allowRequest cache "zoneA" "192.168.0.1" 5 1.0 60
-- > when allowed $ putStrLn "Proceeding with request..."
allowRequest
  :: MonadIO m
  => Cache (InMemoryStore 'TokenBucket)
  -- ^ Token bucket cache backend
  -> Text
  -- ^ IP zone (e.g. region or customer identifier)
  -> Text
  -- ^ User key (e.g. IP address or API token)
  -> Int
  -- ^ Bucket capacity (max number of tokens)
  -> Double
  -- ^ Refill rate (tokens per second)
  -> Int
  -- ^ TTL (seconds) for the bucket state
  -> m Bool
allowRequest cache ipZone userKey capacity refillRate expiresIn = liftIO $
  if expiresIn < minTTL
     then do
       putStrLn $
         "TokenBucket: Request denied due to invalid TTL: "
           ++ show expiresIn
       pure False
     else do
       now <- floor <$> getPOSIXTime
       let key                     = makeCacheKey (cacheAlgorithm cache) ipZone userKey
           TokenBucketStore tvBuckets = cacheStore cache
       replyVar <- newEmptyMVar

       ----------------------------------------------------------------------
       -- 1. Obtain (or create) bucket entry and enqueue the request.
       -- Create the entry in IO, then pass it into the STM transaction.
       newEntryInitialState <- createTokenBucketEntry (TokenBucketState (capacity - 1) now)
       
       (wasNew, entry) <- atomically $ do
         buckets <- readTVar tvBuckets
         -- Use F.Focus directly to allow STM actions in the handler, bypassing F.cases.
         (wasNewEntry, ent) <-
           StmMap.focus
             (F.Focus
                -- Handler for when the key is NOT found (the "Nothing" case)
                (pure ((True, newEntryInitialState), F.Set newEntryInitialState))
                -- Handler for when the key IS found (the "Just" case)
                (\existingEnt -> do
                  -- This handler can now perform STM actions.
                  workerLockEmpty <- isEmptyTMVar (tbeWorkerLock existingEnt)
                  if workerLockEmpty
                    then pure ((True, newEntryInitialState), F.Set newEntryInitialState)  -- Replace dead entry
                    else pure ((False, existingEnt), F.Leave)     -- Keep existing entry
                )
             )
             key buckets
         pure (wasNewEntry, ent)

       ----------------------------------------------------------------------
       -- 2. Spawn a worker once for a fresh bucket.
       if wasNew
         then
           -- For a new bucket, the first request is allowed only if there is capacity.
           if capacity > 0
             then do
               workerReadyVar <- atomically newEmptyTMVar
               atomically $ putTMVar (tbeWorkerLock entry) () -- Mark worker lock as taken
               -- Start the worker with ready synchronization
               startTokenBucketWorker (tbeState entry)
                                      (tbeQueue entry)
                                      capacity
                                      refillRate
                                      workerReadyVar
               -- Wait for the worker to signal it's ready before proceeding
               atomically $ takeTMVar workerReadyVar
               pure True
             else do
               -- If capacity is 0, no request can ever be allowed.
               pure False
         else do
           -- For existing buckets, enqueue the request and wait for response
           atomically $ writeTQueue (tbeQueue entry) replyVar
           result <- takeMVar replyVar
           pure result
