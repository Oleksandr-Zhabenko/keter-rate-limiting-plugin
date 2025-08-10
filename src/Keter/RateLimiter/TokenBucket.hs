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
allowRequest
  :: MonadIO m
  => Cache (InMemoryStore 'TokenBucket)
  -> Text
  -> Text
  -> Text
  -> Int
  -> Double
  -> Int
  -> m Bool
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
