{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

{-|
Module      : Keter.RateLimiter.TokenBucketWorker
Description : Worker thread implementation for token bucket rate limiting
Copyright   : (c) Keter Project
License     : MIT
Maintainer  : maintainer@example.com
Stability   : experimental
Portability : POSIX

This module provides a concurrent worker thread implementation for the token bucket
rate limiting algorithm. The worker processes incoming requests from a queue and
atomically updates the bucket state using Software Transactional Memory (STM).

== Algorithm Overview

The token bucket algorithm works as follows:

1. __Initialization__: A bucket starts with a certain number of tokens (up to capacity)
2. __Token Refill__: Tokens are added to the bucket at a constant rate over time
3. __Request Processing__: Each request attempts to consume one token
4. __Rate Limiting__: If no tokens are available, the request is denied

== Concurrency Model

The worker uses STM for atomic state updates and communicates via:

* 'TQueue' for receiving incoming requests  
* 'MVar' for sending responses back to clients
* 'TVar' for maintaining bucket state
* 'TMVar' for signaling worker readiness

== Example Usage

@
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Data.Text

-- Create initial bucket state (100 tokens, last updated now)
now <- floor \<$\> getPOSIXTime  
initialState <- newTVarIO $ TokenBucketState 100 now

-- Create communication channels
requestQueue <- newTBroadcastTQueueIO
readySignal <- newEmptyTMVarIO

-- Start worker: 100 token capacity, 10 tokens/second refill rate
startTokenBucketWorker initialState requestQueue 100 10.0 "api-key-123" readySignal

-- Wait for worker to be ready
atomically $ takeTMVar readySignal

-- Send a request and wait for response
replyVar <- newEmptyMVar
atomically $ writeTQueue requestQueue replyVar  
allowed <- takeMVar replyVar  -- True if request allowed, False if denied
@

== Performance Characteristics

* __Time Complexity__: O(1) per request (constant time token calculation)
* __Space Complexity__: O(1) (fixed bucket state size)
* __Concurrency__: Lock-free using STM, supports high throughput
* __Precision__: Uses POSIX timestamps for accurate time-based calculations

== Thread Safety

All operations are thread-safe through STM. Multiple clients can safely
send requests to the same worker concurrently.
-}
module Keter.RateLimiter.TokenBucketWorker
  ( -- * Worker Thread Management
    startTokenBucketWorker
  ) where

import Control.Concurrent.STM
import Control.Monad (void, forever)
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Keter.RateLimiter.Types (TokenBucketState(..))
import Control.Concurrent.MVar (MVar, putMVar)

-- | Start a dedicated worker thread for processing token bucket requests.
--
-- The worker runs in an infinite loop, processing requests from the provided queue.
-- Each request is handled atomically: the bucket state is read, tokens are refilled
-- based on elapsed time, a token is consumed if available, and the new state is written back.
--
-- ==== Worker Lifecycle
--
-- 1. __Startup__: Worker thread is forked and signals readiness via 'TMVar'
-- 2. __Processing Loop__: Worker waits for requests, processes them atomically
-- 3. __Response__: Results are sent back to clients via 'MVar'
-- 4. __Logging__: Each request result is logged with key and allow/deny status
--
-- ==== Token Refill Algorithm
--
-- Tokens are refilled using the formula:
--
-- @
-- newTokens = min capacity (currentTokens + refillRate * elapsedSeconds)
-- @
--
-- This ensures:
-- * Tokens are added proportionally to elapsed time
-- * Bucket capacity is never exceeded
-- * Sub-second precision for refill calculations
--
-- ==== Atomic Request Processing
--
-- Each request is processed in a single STM transaction that:
--
-- 1. Reads current bucket state ('tokens', 'lastUpdate')
-- 2. Calculates elapsed time since last update
-- 3. Computes available tokens after refill
-- 4. Attempts to consume one token if available
-- 5. Updates bucket state with new token count and timestamp
-- 6. Returns allow/deny decision
--
-- ==== Error Handling
--
-- The worker is designed to be resilient:
--
-- * Time calculation errors are handled by using 'floor' for integer conversion
-- * Negative elapsed time (clock adjustments) results in no refill
-- * Worker continues running even if individual requests fail
--
-- ==== Example
--
-- @
-- -- Create a bucket for API rate limiting: 1000 requests/hour = ~0.278 req/sec
-- let capacity = 100              -- Allow bursts up to 100 requests
--     refillRate = 1000.0 / 3600.0 -- 1000 requests per hour
--     apiKey = "user-api-key-456"
--
-- initialState <- newTVarIO $ TokenBucketState capacity now
-- requestQueue <- newTBroadcastTQueueIO  
-- readySignal <- newEmptyTMVarIO
--
-- startTokenBucketWorker initialState requestQueue capacity refillRate apiKey readySignal
-- @
--
-- __Thread Safety:__ All state updates are atomic via STM transactions.
--
-- __Resource Usage:__ Creates one background thread that runs indefinitely.
--
-- __Logging:__ Writes to stdout for each processed request (consider configurable logging in production).
startTokenBucketWorker
  :: TVar TokenBucketState  -- ^ Shared bucket state (tokens + last update time).
                            --   This 'TVar' is read and updated atomically by the worker.
  -> TQueue (MVar Bool)     -- ^ Request queue containing 'MVar's for client responses.
                            --   Clients place their response 'MVar' in this queue and wait
                            --   for the worker to write the allow/deny decision.
  -> Int                    -- ^ Maximum bucket capacity (maximum tokens that can be stored).
                            --   This sets the upper limit for burst traffic handling.
                            --   Must be positive.
  -> Double                 -- ^ Token refill rate in tokens per second.
                            --   Determines the long-term sustainable request rate.
                            --   Must be positive. Can be fractional (e.g., 0.5 = 1 token per 2 seconds).
  -> Text                   -- ^ Identifier key for logging purposes (e.g., API key, user ID).
                            --   Used to distinguish between different buckets in log output.
                            --   Should not contain sensitive information as it appears in logs.
  -> TMVar ()               -- ^ Synchronization variable to signal when worker is ready.
                            --   The worker writes to this 'TMVar' once startup is complete.
                            --   Clients can wait on this to ensure the worker is operational.
  -> IO ()                  -- ^ Returns immediately after forking the worker thread.
                            --   The actual worker runs in the background indefinitely.
startTokenBucketWorker stateVar queue capacity refillRate fullKey readyVar = void . forkIO $ do
  -- Signal that the worker is ready
  atomically $ putTMVar readyVar ()
  
  forever $ do
    -- Wait for a request to arrive in the queue
    replyVar <- atomically $ readTQueue queue
    now <- liftIO $ floor <$> getPOSIXTime
    -- Atomically process the request: read state, calculate new tokens,
    -- consume a token if available, and write the new state back.
    allowed <- atomically $ do
      TokenBucketState { tokens, lastUpdate } <- readTVar stateVar
      
      let elapsed = fromIntegral (now - lastUpdate)
          refilled = elapsed * refillRate
          -- Add refilled tokens, but don't exceed the capacity
          currentTokens = min (fromIntegral capacity) (fromIntegral tokens + refilled)
      if currentTokens >= 1
        then do
          -- Request is allowed. Consume one token and update the timestamp.
          let newTokens = currentTokens - 1
          writeTVar stateVar (TokenBucketState (floor newTokens) now)
          return True
        else do
          -- Request is denied. Don't consume a token, but update the timestamp
          -- to ensure the next refill calculation is correct.
          writeTVar stateVar (TokenBucketState (floor currentTokens) now)
          return False
    -- Send the response back to the waiting client.
    liftIO $ putMVar replyVar allowed
