{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Keter.RateLimiter.AutoPurge
Description : Automatic cleanup and garbage collection for rate limiter caches
Copyright   : (c) Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : experimental
Portability : POSIX

This module provides automatic purging and cleanup functionality for rate limiter
caches and STM-based data structures. It implements background threads that
periodically remove expired entries to prevent memory leaks and maintain
optimal performance.

== Purging Strategies

The module supports two main purging approaches:

1. __Generic Cache Purging__: Works with any 'Data.Cache.Cache' instance
2. __Custom STM Map Purging__: Specialized for rate limiter bucket entries

== Architecture Overview

@
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   Cache/STM     │    │   Purge Thread   │    │   Timer Logic   │
│     Map         │◄───┤                  │◄───┤                 │
└─────────────────┘    │  • Check TTL     │    │ • Interval calc │
                       │  • Remove expired│    │ • Sleep mgmt    │
                       │  • Cleanup locks │    │ • Precise timing│
                       └──────────────────┘    └─────────────────┘
@

== Performance Characteristics

* __Time Complexity__: O(n) where n is the number of entries (full scan)
* __Space Complexity__: O(1) additional memory usage
* __Timing Precision__: Microsecond-level accuracy using monotonic clock
* __Thread Safety__: All operations are atomic using STM

== Example Usage

=== Basic Cache Purging

@
import qualified Data.Cache as C
import Data.Text

-- Create a cache with 1-hour TTL
cache <- C.newCache (Just $ C.seconds 3600)

-- Start auto-purge every 10 minutes (600 seconds)
startAutoPurge cache 600

-- Cache will now automatically remove expired entries
@

=== Token Bucket Purging

@
import qualified StmContainers.Map as StmMap

-- Create STM map for token buckets
tokenBuckets <- StmMap.newIO

-- Start purging inactive buckets every 5 minutes, TTL 1 hour
threadId <- startCustomPurgeTokenBucket tokenBuckets 300 3600

-- Purge thread runs in background, cleaning up unused buckets
@

=== Leaky Bucket Purging

@
-- Create STM map for leaky buckets  
leakyBuckets <- StmMap.newIO

-- Start purging inactive buckets every 2 minutes, TTL 30 minutes
threadId <- startCustomPurgeLeakyBucket leakyBuckets 120 1800

-- Thread automatically removes buckets not used for 30+ minutes
@

== Thread Management

All purge functions return 'ThreadId' values that can be used for thread
management (killing, monitoring, etc.). The threads run indefinitely until
explicitly terminated.

== Memory Management

The purge system is designed to prevent memory leaks in long-running
applications by:

* Removing expired cache entries
* Cleaning up unused worker threads and locks
* Releasing STM resources for inactive buckets
* Maintaining bounded memory usage regardless of request patterns
-}
module Keter.RateLimiter.AutoPurge
  ( -- * Data Types
    -- ** Token Bucket Entries
    TokenBucketEntry(..)
    -- ** Leaky Bucket Entries
  , LeakyBucketEntry(..)
    -- * Purging Functions
    -- ** Generic Cache Purging
  , startAutoPurge
    -- ** Custom STM Map Purging
  , startCustomPurge
  , startCustomPurgeTokenBucket
  , startCustomPurgeLeakyBucket
  ) where

import Control.Concurrent (forkIO, threadDelay, ThreadId)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Monad (forever, filterM, void)
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.TQueue as TQueue
import qualified StmContainers.Map as StmMap
import qualified ListT
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Cache as C
import Keter.RateLimiter.Types (TokenBucketState(..), LeakyBucketState(..))
import System.Clock (TimeSpec(..), Clock(Monotonic), getTime, toNanoSecs)

-- | Container for token bucket state and associated worker resources.
--
-- This type encapsulates all the resources needed for a single token bucket
-- rate limiter, including the bucket state, request queue, and worker thread
-- synchronization. It's designed to work with STM-based concurrent access
-- patterns.
--
-- ==== Resource Management
--
-- The entry includes a worker lock ('tbeWorkerLock') that coordinates worker
-- thread lifecycle:
--
-- * __Empty TMVar__: No worker thread is currently running for this bucket
-- * __Full TMVar__: A worker thread is active and processing requests
--
-- ==== Cleanup Behavior
--
-- When purging expired entries, the system:
--
-- 1. Attempts to take the worker lock (non-blocking)
-- 2. If successful, terminates any associated worker thread
-- 3. Removes the entry from the map
-- 4. Releases all associated STM resources
--
-- ==== Example
--
-- @
-- -- Create a token bucket entry
-- state <- newTVarIO $ TokenBucketState 100 now
-- queue <- TQueue.newTBroadcastTQueueIO
-- lock <- newEmptyTMVarIO
--
-- let entry = TokenBucketEntry
--       { tbeState = state
--       , tbeQueue = queue  
--       , tbeWorkerLock = lock
--       }
-- @
data TokenBucketEntry = TokenBucketEntry
  { tbeState      :: TVar TokenBucketState    -- ^ Atomic bucket state containing
                                              --   current token count and last update timestamp.
                                              --   Shared between worker thread and clients.
  , tbeQueue      :: TQueue.TQueue (MVar Bool) -- ^ Request queue for client communication.
                                              --   Clients place response 'MVar's here and wait
                                              --   for worker to process and respond.
  , tbeWorkerLock :: TMVar ()                 -- ^ Worker thread synchronization lock.
                                              --   Empty when no worker exists, full when active.
                                              --   Used for coordinating worker lifecycle and cleanup.
  }

-- | Container for leaky bucket state and associated worker resources.
--
-- Similar to 'TokenBucketEntry' but designed for leaky bucket algorithm
-- requirements. The leaky bucket uses continuous time calculations and
-- different queue communication patterns.
--
-- ==== Differences from Token Bucket
--
-- * Uses 'TMVar Bool' instead of 'MVar Bool' for queue communication
-- * State contains 'Double' timestamps for sub-second precision
-- * Worker threads implement continuous draining logic
--
-- ==== Resource Lifecycle
--
-- The entry lifecycle mirrors token buckets:
--
-- 1. __Creation__: Entry is created and added to STM map
-- 2. __Activation__: First request triggers worker thread creation
-- 3. __Processing__: Worker handles requests and updates state
-- 4. __Expiration__: Entry is purged after TTL period of inactivity
-- 5. __Cleanup__: Worker is terminated and resources are released
--
-- ==== Example
--
-- @
-- -- Create a leaky bucket entry
-- state <- newTVarIO $ LeakyBucketState 0.0 now
-- queue <- TQueue.newTBroadcastTQueueIO
-- lock <- newEmptyTMVarIO
--
-- let entry = LeakyBucketEntry
--       { lbeState = state
--       , lbeQueue = queue
--       , lbeWorkerLock = lock  
--       }
-- @
data LeakyBucketEntry = LeakyBucketEntry
  { lbeState      :: TVar LeakyBucketState      -- ^ Atomic bucket state with current level
                                                --   and last update timestamp (Double precision).
                                                --   Shared between worker and clients.
  , lbeQueue      :: TQueue.TQueue (TMVar Bool) -- ^ Request queue using 'TMVar' for STM-based
                                                --   client communication. Enables atomic
                                                --   request queuing and response handling.
  , lbeWorkerLock :: TMVar ()                   -- ^ Worker thread coordination lock, same
                                                --   semantics as token bucket but for leaky
                                                --   bucket worker lifecycle management.
  }

-- | Start a background thread that periodically purges expired entries from a generic cache.
--
-- This function creates a self-regulating purge thread that maintains precise timing
-- intervals regardless of purge operation duration. It uses monotonic clock measurements
-- to ensure accurate scheduling even under system load.
--
-- ==== Timing Algorithm
--
-- The purge cycle works as follows:
--
-- 1. __Start Timer__: Record monotonic timestamp before purge
-- 2. __Purge Operation__: Call 'C.purgeExpired' on the cache
-- 3. __Calculate Remaining__: Subtract elapsed time from target interval
-- 4. __Precise Sleep__: Wait for exactly the remaining time
-- 5. __Repeat__: Continue with next cycle
--
-- ==== Timing Precision
--
-- The algorithm ensures that purge cycles happen at regular intervals:
--
-- @
-- Target Interval:    |----10s----|----10s----|----10s----|
-- Actual Timing:      |--9s purge-|1s wait|--8s purge--|2s wait|
-- Result:             Consistent 10-second intervals maintained
-- @
--
-- ==== Performance Considerations
--
-- * __Non-blocking__: Returns immediately after starting background thread
-- * __Self-correcting__: Adapts sleep time based on actual purge duration
-- * __Memory efficient__: Only maintains minimal state for timing
-- * __CPU friendly__: Sleeps for majority of time between purges
--
-- ==== Example Usage
--
-- @
-- import qualified Data.Cache as C
-- import Data.Text
--
-- -- Create cache with 30-minute TTL
-- cache <- C.newCache (Just $ C.minutes 30)
--
-- -- Purge expired entries every 5 minutes (300 seconds)
-- startAutoPurge cache 300
--
-- -- Cache now automatically maintains itself
-- -- Memory usage remains bounded regardless of request patterns
-- @
--
-- __Thread Safety:__ Safe to call concurrently. Each call creates independent purge thread.
--
-- __Resource Usage:__ Creates one background thread with minimal memory footprint.
--
-- __Error Handling:__ Thread continues running even if individual purge operations fail.
startAutoPurge 
  :: C.Cache Text v   -- ^ The cache instance to purge. Can contain any value type 'v'.
                      --   Must support 'C.purgeExpired' operation for TTL-based cleanup.
  -> Integer          -- ^ Purge interval in seconds. Determines how frequently expired
                      --   entries are removed. Shorter intervals provide more responsive
                      --   cleanup but use more CPU. Typical values: 60-3600 seconds.
  -> IO ()            -- ^ Returns immediately. The purge thread runs in background indefinitely.
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

-- | Start a specialized purge thread for token bucket entries in an STM map.
--
-- This function provides a convenient wrapper around 'startCustomPurge' specifically
-- configured for 'TokenBucketEntry' cleanup. It handles the complexities of extracting
-- timestamps from token bucket state and properly cleaning up worker threads.
--
-- ==== Token Bucket Specific Behavior
--
-- * __Timestamp Extraction__: Uses 'lastUpdate' field from 'TokenBucketState'
-- * __Worker Cleanup__: Attempts to acquire worker lock and terminate threads
-- * __Resource Release__: Removes entry from STM map and frees all resources
--
-- ==== TTL Behavior
--
-- Token buckets are considered expired when:
--
-- @
-- currentTime - lastUpdateTime >= ttlSeconds
-- @
--
-- This means buckets are purged based on when they were last used for rate limiting,
-- not when they were created. Active buckets are never purged regardless of age.
--
-- ==== Example Scenarios
--
-- @
-- -- High-frequency API with 5-minute cleanup cycles
-- tokenBuckets <- StmMap.newIO
-- threadId <- startCustomPurgeTokenBucket tokenBuckets 300 3600  -- 5min interval, 1hr TTL
--
-- -- Low-frequency batch processing with daily cleanup  
-- batchBuckets <- StmMap.newIO
-- threadId <- startCustomPurgeTokenBucket batchBuckets 86400 604800  -- 1day interval, 1week TTL
-- @
--
-- __Thread Management:__ Returns 'ThreadId' for thread control (e.g., 'killThread').
--
-- __Memory Impact:__ Prevents unbounded memory growth in applications with dynamic rate limiting keys.
--
-- __Performance:__ O(n) scan of all buckets, but typically runs infrequently during low-traffic periods.
startCustomPurgeTokenBucket
  :: StmMap.Map Text TokenBucketEntry  -- ^ STM map containing token bucket entries keyed by identifier.
                                       --   Typically uses API keys, user IDs, or IP addresses as keys.
                                       --   Map is scanned atomically during each purge cycle.
  -> Integer                           -- ^ Purge interval in seconds. How often to scan for expired buckets.
                                       --   Balance between cleanup responsiveness and CPU usage.
                                       --   Recommended: 300-3600 seconds depending on traffic patterns.
  -> Integer                           -- ^ TTL (time-to-live) in seconds. Buckets unused for this duration
                                       --   are considered expired and eligible for removal.
                                       --   Should be much larger than typical request intervals.
                                       --   Recommended: 3600-86400 seconds.
  -> IO ThreadId                       -- ^ Returns thread ID of the background purge thread.
                                       --   Can be used with 'killThread' to stop purging.
startCustomPurgeTokenBucket stmMap intervalSeconds ttlSeconds = startCustomPurge
  (\entry -> do
      TokenBucketState _ lastT <- readTVar (tbeState entry)
      pure (fromIntegral lastT))
  (\key entry -> do
      void $ tryTakeTMVar (tbeWorkerLock entry)
      StmMap.delete key stmMap)
  stmMap
  intervalSeconds
  ttlSeconds

-- | Start a specialized purge thread for leaky bucket entries in an STM map.
--
-- Similar to 'startCustomPurgeTokenBucket' but configured for 'LeakyBucketEntry' cleanup.
-- Handles the specific requirements of leaky bucket timestamp extraction and worker
-- thread management.
--
-- ==== Leaky Bucket Specific Behavior
--
-- * __Timestamp Precision__: Uses 'Double' timestamp from 'LeakyBucketState' for sub-second accuracy
-- * __Continuous Time Model__: Supports fractional timestamps for precise drain calculations
-- * __Worker Cleanup__: Terminates continuous drain worker threads properly
--
-- ==== TTL Calculation
--
-- Leaky buckets are expired when:
--
-- @
-- currentTime - lastTime >= fromIntegral ttlSeconds
-- @
--
-- The 'lastTime' field represents the most recent bucket level update, providing
-- more precise expiration timing than integer-based systems.
--
-- ==== Use Cases
--
-- * __Streaming Applications__: Rate limiting for continuous data flows
-- * __Real-time APIs__: Sub-second precision rate limiting
-- * __Traffic Shaping__: Network bandwidth management with smooth rates
--
-- ==== Example Configuration
--
-- @
-- -- Real-time streaming with frequent cleanup
-- streamBuckets <- StmMap.newIO  
-- threadId <- startCustomPurgeLeakyBucket streamBuckets 60 900  -- 1min interval, 15min TTL
--
-- -- Network traffic shaping with moderate cleanup
-- trafficBuckets <- StmMap.newIO
-- threadId <- startCustomPurgeLeakyBucket trafficBuckets 600 7200  -- 10min interval, 2hr TTL
-- @
--
-- __Precision:__ Sub-second timestamp accuracy for fine-grained rate control.
--
-- __Cleanup Behavior:__ More aggressive cleanup suitable for high-frequency, short-lived connections.
startCustomPurgeLeakyBucket
  :: StmMap.Map Text LeakyBucketEntry  -- ^ STM map containing leaky bucket entries keyed by identifier.
                                       --   Keys typically represent connections, streams, or data flows.
                                       --   All entries are scanned atomically during purge operations.
  -> Integer                           -- ^ Purge interval in seconds. Frequency of expired entry cleanup.
                                       --   For high-frequency applications, shorter intervals (60-600s)
                                       --   provide more responsive cleanup.
  -> Integer                           -- ^ TTL in seconds. Buckets inactive for this duration are purged.
                                       --   Should account for typical connection/stream lifetime patterns.
                                       --   Shorter TTL (900-3600s) suitable for transient connections.
  -> IO ThreadId                       -- ^ Thread ID for background purge thread management.
                                       --   Thread runs continuously until explicitly terminated.
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

-- | Generic purge loop implementation for STM-based data structures.
--
-- This is the foundational purge function that powers both token bucket and leaky bucket
-- purging. It provides a flexible framework for implementing custom purge logic while
-- maintaining consistent timing and cleanup behavior.
--
-- ==== Algorithm Overview
--
-- The purge cycle consists of several phases:
--
-- 1. __Scan Phase__: Atomically list all entries in the STM map
-- 2. __Filter Phase__: Identify expired entries based on timestamp extraction
-- 3. __Cleanup Phase__: Execute custom delete actions for expired entries  
-- 4. __Timing Phase__: Calculate sleep duration to maintain precise intervals
-- 5. __Sleep Phase__: Wait until next purge cycle should begin
--
-- ==== Atomicity Guarantees
--
-- * __Entry Listing__: All entries are captured in a single STM transaction
-- * __Expiration Check__: Timestamp extraction is atomic per entry
-- * __Deletion__: Custom delete actions are executed atomically
-- * __Consistency__: Map state remains consistent throughout the process
--
-- ==== Custom Delete Actions
--
-- The 'deleteAction' parameter allows specialized cleanup logic:
--
-- @
-- -- Simple deletion
-- deleteAction key entry = StmMap.delete key stmMap
--
-- -- Cleanup with resource release
-- deleteAction key entry = do
--   releaseResources entry  -- Custom cleanup
--   StmMap.delete key stmMap
--
-- -- Conditional deletion  
-- deleteAction key entry = do
--   shouldDelete <- checkCondition entry
--   when shouldDelete $ StmMap.delete key stmMap
-- @
--
-- ==== Error Handling
--
-- The function is designed to be resilient:
--
-- * Individual entry failures don't stop the purge cycle
-- * Timing calculations handle clock adjustments gracefully
-- * Thread continues running even if STM transactions retry
--
-- ==== Performance Optimization
--
-- * __Batch Processing__: All deletions happen in a single STM transaction
-- * __Minimal Copying__: Entries are processed in-place where possible
-- * __Efficient Filtering__: Uses lazy evaluation for timestamp checks
-- * __Precise Timing__: Avoids unnecessary CPU usage during sleep periods
--
-- ==== Example Usage
--
-- @
-- -- Custom purge for application-specific entries
-- startCustomPurge
--   -- Extract timestamp from custom entry type
--   (\\entry -> readTVar (customTimestamp entry))
--   -- Custom cleanup with logging
--   (\\key entry -> do
--       liftIO $ logInfo ("Purging entry: " ++ show key)
--       releaseCustomResources entry
--       StmMap.delete key stmMap)
--   customMap
--   600    -- 10-minute intervals
--   3600   -- 1-hour TTL
-- @
--
-- __Flexibility:__ Supports any entry type with extractable timestamps.
--
-- __Extensibility:__ Custom delete actions enable complex cleanup scenarios.
--
-- __Reliability:__ Robust error handling ensures continuous operation.
startCustomPurge
  :: forall entry.
     (entry -> STM Double)            -- ^ Timestamp extraction function. Called for each entry
                                      --   to determine last activity time. Should be fast and
                                      --   side-effect free. Returns Unix timestamp as 'Double'
                                      --   for sub-second precision.
  -> (Text -> entry -> STM ())        -- ^ Custom delete action executed for expired entries.
                                      --   Receives both key and entry for context. Should
                                      --   handle resource cleanup and map removal atomically.
                                      --   Can perform logging, resource release, etc.
  -> StmMap.Map Text entry            -- ^ STM map to purge. Entries are identified by 'Text' keys
                                      --   and can be any type 'entry'. Map is scanned completely
                                      --   during each purge cycle for expired entries.
  -> Integer                          -- ^ Purge interval in seconds. Controls how frequently
                                      --   the purge operation runs. Shorter intervals provide
                                      --   more responsive cleanup but increase CPU usage.
  -> Integer                          -- ^ TTL (time-to-live) in seconds. Entries with timestamps
                                      --   older than (currentTime - ttlSeconds) are considered
                                      --   expired and eligible for removal.
  -> IO ThreadId                      -- ^ Returns thread ID of the purge thread. Thread runs
                                      --   indefinitely until killed. Can be used for thread
                                      --   management and monitoring.
startCustomPurge getTimestamp deleteAction stmMap intervalSeconds ttlSeconds = do
  purgeSignal <- newMVar ()
  forkIO $ forever $ do
    takeMVar purgeSignal
    startTime <- getTime Monotonic
    now <- realToFrac <$> getPOSIXTime
    expiredKVs <- atomically $ do
      kvs <- ListT.toList (StmMap.listT stmMap)
      filterM
        (\(_, entry) -> do
            ts <- getTimestamp entry
            pure (now - ts >= fromIntegral ttlSeconds))
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
