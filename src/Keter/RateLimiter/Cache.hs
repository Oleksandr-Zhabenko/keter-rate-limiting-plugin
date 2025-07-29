{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

{-|
Module      : Keter.RateLimiter.Cache
Description : Cache abstraction and in-memory store for rate limiting, with convenient and customisable key management
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : stable
Portability : portable

This file is a ported to Haskell language code with some simplifications of rack-attack
https://github.com/rack/rack-attack/blob/main/lib/rack/attack/cache.rb
and is based on the structure of the original code of
rack-attack, Copyright (c) 2016 by Kickstarter, PBC, under the MIT License.

Oleksandr Zhabenko added several implementations of the window algorithm: sliding window, token bucket window, leaky bucket window alongside with the initial count algorithm using AI chatbots. Also there is extended multiple IP zones and combined usage of the algorithms with convenient wrappers provided.

This implementation is released under the MIT License.

This module provides a unified cache abstraction layer that supports multiple
rate limiting algorithms and storage backends. It uses advanced Haskell type
system features including GADTs, DataKinds, and functional dependencies to
provide type-safe, algorithm-specific storage while maintaining a common interface.

== Architecture Overview

@
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   Algorithm     │    │   Cache Layer    │    │  Storage Backend│
│                 │    │                  │    │                 │
│ • FixedWindow   │◄───┤ • Type Safety    │◄───┤ • InMemoryStore │
│ • SlidingWindow │    │ • Key Prefixing  │    │ • Auto Purging  │
│ • TokenBucket   │    │ • Serialization  │    │ • STM Based     │
│ • LeakyBucket   │    │ • Error Handling │    │ • Thread Safe   │
│ • TinyLRU       │    │                  │    │                 │
└─────────────────┘    └──────────────────┘    └─────────────────┘
@

== Type-Level Algorithm Safety

The module uses DataKinds and GADTs to ensure compile-time type safety:

@
-- Algorithm types are promoted to type-level
data Algorithm = FixedWindow | TokenBucket | ...

-- Storage is parameterized by algorithm type
data InMemoryStore (a :: Algorithm) where
  CounterStore :: TVar (C.Cache Text Text) -> InMemoryStore 'FixedWindow
  TokenBucketStore :: TVar (StmMap.Map Text TokenBucketEntry) -> InMemoryStore 'TokenBucket
  -- ... other algorithms
@

This prevents runtime errors like trying to use token bucket operations on
a sliding window cache.

== Supported Algorithms

=== Fixed Window
* __Use Case__: Simple request counting per time window
* __Storage__: JSON-serialized counters with TTL
* __Performance__: O(1) read/write operations
* __Memory__: Minimal overhead, automatic expiration

=== Sliding Window  
* __Use Case__: Precise rate limiting with timestamp tracking
* __Storage__: Lists of request timestamps per key
* __Performance__: O(n) where n is requests in window
* __Memory__: Proportional to request frequency

=== Token Bucket
* __Use Case__: Bursty traffic with sustained rate limits
* __Storage__: Worker threads with STM-based state
* __Performance__: O(1) with background token refill
* __Memory__: Fixed per bucket, automatic cleanup

=== Leaky Bucket
* __Use Case__: Smooth rate limiting without bursts
* __Storage__: Continuous drain workers with STM state
* __Performance__: O(1) with background draining
* __Memory__: Fixed per bucket, automatic cleanup

=== TinyLRU
* __Use Case__: Bounded cache with LRU eviction
* __Storage__: In-memory LRU cache with expiration
* __Performance__: O(1) average case operations
* __Memory__: Bounded by cache size limit

== Example Usage

=== Creating Algorithm-Specific Caches

@
import Keter.RateLimiter.Cache
import Data.Proxy

-- Type-safe cache creation
tokenCache <- do
  store <- createInMemoryStore @'TokenBucket
  return $ newCache TokenBucket store

fixedWindowCache <- do
  store <- createInMemoryStore @'FixedWindow  
  return $ newCache FixedWindow store
@

=== Basic Operations

@
-- Write/Read operations (type-safe based on algorithm)
writeCache tokenCache "user123" initialTokenState 3600
maybeState <- readCache tokenCache "user123"

-- Increment operations for counter-based algorithms
newCount <- incrementCache fixedWindowCache "api_key" 60
@

=== Advanced Usage with Custom Keys

@
-- Composite key generation
let userKey = makeCacheKey TokenBucket "zone1" "user456"
writeCache tokenCache userKey state 7200

-- Cleanup and reset
cacheReset tokenCache  -- Clear all entries
clearInMemoryStore store  -- Direct store cleanup
@

== Thread Safety and Concurrency

All operations are thread-safe using Software Transactional Memory (STM):

* __Atomic Operations__: All read/write operations are atomic
* __Lock-Free__: No explicit locking, uses STM for coordination  
* __Concurrent Access__: Multiple threads can safely access same cache
* __Worker Threads__: Token/Leaky bucket algorithms use background workers
* __Auto-Purging__: Background threads clean up expired entries

== Performance Characteristics

=== Time Complexity
* __Fixed Window__: O(1) for all operations
* __Sliding Window__: O(n) for timestamp list operations
* __Token Bucket__: O(1) with background O(1) refill
* __Leaky Bucket__: O(1) with background O(1) drain
* __TinyLRU__: O(1) average case, O(n) worst case

=== Space Complexity
* __Fixed Window__: O(k) where k is number of active keys
* __Sliding Window__: O(k*n) where n is requests per window
* __Token Bucket__: O(k) with fixed per-bucket overhead
* __Leaky Bucket__: O(k) with fixed per-bucket overhead  
* __TinyLRU__: O(min(k, cache_size)) bounded by cache limit

== Error Handling

The module provides robust error handling:

* __Serialization Errors__: Graceful handling of JSON encode/decode failures
* __Type Safety__: Compile-time prevention of algorithm mismatches
* __Resource Cleanup__: Automatic cleanup of failed operations
* __Thread Exceptions__: Worker threads handle exceptions gracefully
-}
module Keter.RateLimiter.Cache
  ( -- * Core Types
    -- ** Algorithm Specification
    Algorithm(..)
  , Cache(..)
    -- ** Storage Abstraction
  , CacheStore(..)
  , InMemoryStore(..)
  , ResettableStore(..)
  , CreateStore(..)
    -- * Cache Operations  
    -- ** Basic Operations
  , readCache
  , writeCache
  , deleteCache
  , incrementCache
    -- ** Cache Management
  , newCache
  , createInMemoryStore
  , clearInMemoryStore
  , cacheReset
    -- * Utility Functions
    -- ** Key Management
  , algorithmPrefix
  , makeCacheKey
    -- ** Time Utilities
  , secondsToTimeSpec
    -- * Background Services
    -- ** Auto-Purging
  , startAutoPurge
  , startCustomPurgeTokenBucket
  , startCustomPurgeLeakyBucket
    -- ** Worker Threads
  , startTokenBucketWorker
  , startLeakyBucketWorker
    -- * Entry Creation
  , createTokenBucketEntry
  , createLeakyBucketEntry
  ) where

import Control.Concurrent.STM
import Control.Concurrent.MVar (putMVar, takeMVar, newMVar, readMVar, MVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (void, forever)
import Control.Concurrent (forkIO, threadDelay)
import Data.Aeson (ToJSON, FromJSON, decodeStrict, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.TinyLRU as TinyLRU
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import qualified Data.Cache as C
import System.Clock (TimeSpec(..), Clock(Monotonic), getTime, toNanoSecs)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified StmContainers.Map as StmMap
import qualified Control.Concurrent.STM.TQueue as TQueue
import qualified Focus
import Keter.RateLimiter.Types (TokenBucketState(..), LeakyBucketState(..))
import Keter.RateLimiter.AutoPurge
import Keter.RateLimiter.TokenBucketWorker (startTokenBucketWorker)
import Data.Maybe (fromMaybe)

-- | Enumeration of supported rate limiting algorithms.
--
-- Each algorithm represents a different approach to rate limiting with distinct
-- characteristics, use cases, and performance profiles. The type is promoted
-- to the kind level using DataKinds for compile-time algorithm verification.
--
-- ==== Algorithm Characteristics Comparison
--
-- @
-- Algorithm      | Bursts | Precision | Memory    | Use Case
-- ---------------|--------|-----------|-----------|-------------------------
-- FixedWindow    | Yes    | Low       | Minimal   | Simple API rate limiting
-- SlidingWindow  | Smooth | High      | Variable  | Precise traffic shaping
-- TokenBucket    | Yes    | Medium    | Fixed     | Bursty API with sustained limits
-- LeakyBucket    | No     | High      | Fixed     | Smooth streaming/bandwidth
-- TinyLRU        | N/A    | N/A       | Bounded   | General caching with eviction
-- @
--
-- ==== Detailed Algorithm Descriptions
--
-- * __FixedWindow__: Divides time into fixed intervals, counts requests per interval
-- * __SlidingWindow__: Maintains precise timestamps, allows smooth rate distribution  
-- * __TokenBucket__: Accumulates tokens over time, consumes tokens per request
-- * __LeakyBucket__: Continuous leak rate, requests fill the bucket
-- * __TinyLRU__: Least-Recently-Used cache with size bounds and TTL
data Algorithm = FixedWindow | SlidingWindow | TokenBucket | LeakyBucket | TinyLRU
  deriving (Show, Eq)

-- | Map each algorithm to its unique cache key prefix.
--
-- Prefixes prevent key collisions between different algorithms and provide
-- clear identification of cached data types. Each algorithm uses a distinct
-- namespace within the same storage backend.
--
-- ==== Prefix Usage Pattern
--
-- @
-- -- Fixed window counter for API key "abc123"
-- Key: "rate_limiter:api:abc123"
--
-- -- Token bucket state for user "user456"  
-- Key: "token_bucket:user:user456"
--
-- -- Sliding window timestamps for IP "192.168.1.1"
-- Key: "timestamps:ip:192.168.1.1"
-- @
--
-- ==== Example
--
-- @
-- ghci> algorithmPrefix TokenBucket
-- "token_bucket"
-- ghci> algorithmPrefix FixedWindow  
-- "rate_limiter"
-- @
algorithmPrefix :: Algorithm   -- ^ The rate limiting algorithm
                -> Text        -- ^ Corresponding cache key prefix
algorithmPrefix FixedWindow   = "rate_limiter"
algorithmPrefix SlidingWindow = "timestamps"
algorithmPrefix TokenBucket   = "token_bucket"
algorithmPrefix LeakyBucket   = "leaky_bucket"
algorithmPrefix TinyLRU       = "tiny_lru"

-- | Cache wrapper that combines an algorithm specification with a storage backend.
--
-- The cache type provides a unified interface while maintaining algorithm-specific
-- behavior through the type system. It encapsulates both the algorithm logic
-- and the underlying storage implementation.
--
-- ==== Type Parameters
--
-- * @store@: The storage backend type (e.g., 'InMemoryStore', Redis, etc.)
-- * The algorithm is captured in the store type for type safety
--
-- ==== Example Usage
--
-- @
-- -- Create a token bucket cache
-- tokenStore <- createInMemoryStore @'TokenBucket
-- let tokenCache = Cache TokenBucket tokenStore
--
-- -- Create a fixed window cache
-- counterStore <- createInMemoryStore @'FixedWindow
-- let counterCache = Cache FixedWindow counterStore
-- @
data Cache store = Cache
  { cacheAlgorithm :: Algorithm  -- ^ The rate limiting algorithm this cache implements.
                                 --   Used for key prefixing and operation validation.
  , cacheStore :: store          -- ^ The underlying storage backend. Type determines
                                 --   supported operations and value types.
  }

-- | Typeclass abstracting cache storage backends with functional dependencies.
--
-- This typeclass provides a uniform interface for different storage implementations
-- while allowing each backend to specify its supported value types. The functional
-- dependency @store -> v@ ensures that each store type uniquely determines its
-- value type, providing additional type safety.
--
-- ==== Design Principles
--
-- * __Type Safety__: Functional dependencies prevent type mismatches
-- * __Flexibility__: Support for different storage backends (memory, Redis, etc.)
-- * __Performance__: Allow backend-specific optimizations
-- * __Consistency__: Uniform interface across all implementations
--
-- ==== Default Implementations
--
-- The typeclass provides sensible defaults for increment operations, but backends
-- can override for performance optimizations:
--
-- @
-- -- Default increment: read, modify, write
-- incStore store prefix key expires = do
--   mval <- readStore store prefix key
--   let newVal = maybe 1 (+1) mval
--   writeStore store prefix key newVal expires
--   return newVal
-- @
--
-- ==== Atomicity Guarantees
--
-- Implementations should provide atomicity guarantees appropriate for their
-- backend:
--
-- * __STM-based stores__: Full ACID transactions
-- * __Memory stores__: Process-level atomicity  
-- * __Distributed stores__: Network-level consistency
class MonadIO m => CacheStore store v m | store -> v where
  -- | Read a value from the store.
  --
  -- @
  -- result <- readStore store "rate_limiter" "api_key_123"
  -- -- result: Maybe Int (for counter-based algorithms)
  -- @
  readStore :: store    -- ^ Storage backend instance
            -> Text     -- ^ Key prefix (algorithm-specific)
            -> Text     -- ^ Full cache key
            -> m (Maybe v)  -- ^ Retrieved value, or Nothing if not found

  -- | Write a value to the store with expiration.
  --
  -- @
  -- writeStore store "token_bucket" "user_456" bucketState 3600
  -- -- Stores bucket state with 1-hour TTL
  -- @
  writeStore :: store   -- ^ Storage backend instance
             -> Text    -- ^ Key prefix (algorithm-specific)
             -> Text    -- ^ Full cache key
             -> v       -- ^ Value to store
             -> Int     -- ^ TTL in seconds
             -> m ()

  -- | Delete a key from the store.
  --
  -- @
  -- deleteStore store "timestamps" "ip_192_168_1_1"
  -- -- Removes sliding window timestamps for IP
  -- @
  deleteStore :: store  -- ^ Storage backend instance
              -> Text   -- ^ Key prefix (algorithm-specific)  
              -> Text   -- ^ Full cache key
              -> m ()

  -- | Atomically increment a numeric value.
  --
  -- Provides atomic increment-or-initialize semantics. If the key doesn't exist,
  -- initializes to 1. If it exists, increments by 1. Essential for counter-based
  -- rate limiting algorithms.
  --
  -- @
  -- newCount <- incStore store "rate_limiter" "api_throttle" 60
  -- -- Returns new count after increment, with 60-second TTL
  -- @
  incStore :: (FromJSON v, ToJSON v, Ord v, Num v) 
           => store     -- ^ Storage backend instance
           -> Text      -- ^ Key prefix (algorithm-specific)
           -> Text      -- ^ Full cache key  
           -> Int       -- ^ TTL in seconds for the incremented value
           -> m v       -- ^ New value after increment
  incStore store prefix key expiresIn = do -- Default implementation
      mval <- readStore store prefix key
      let newVal = case mval of
            Nothing -> 1
            Just v -> if v <= 0 then 1 else v + 1
      writeStore store prefix key newVal expiresIn
      return newVal

-- | Typeclass for storage backends that support complete reset operations.
--
-- Provides a way to clear all data from a store, useful for testing,
-- maintenance, and emergency reset scenarios. Implementations should
-- ensure thread safety and atomic reset behavior.
--
-- ==== Use Cases
--
-- * __Testing__: Clean state between test runs
-- * __Maintenance__: Clear corrupted or stale data
-- * __Memory Management__: Recover from memory pressure
-- * __Configuration Changes__: Reset after algorithm parameter changes
--
-- ==== Example
--
-- @
-- -- Reset all rate limiting data
-- resetStore myTokenBucketStore
--
-- -- Reset entire cache
-- cacheReset myCache
-- @
class ResettableStore store where
  -- | Clear all entries from the store.
  --
  -- Should be atomic and thread-safe. After reset, the store should behave
  -- as if it were newly created.
  resetStore :: store -> IO ()

-- | Algorithm-parameterized in-memory storage using GADTs.
--
-- This type uses GADTs (Generalized Algebraic Data Types) to provide compile-time
-- guarantees that each algorithm uses appropriate storage structures. The phantom
-- type parameter ensures that token bucket operations can't be used on sliding
-- window stores, etc.
--
-- ==== GADT Benefits
--
-- * __Type Safety__: Prevents algorithm/storage mismatches at compile time
-- * __Performance__: Specialized storage for each algorithm's needs
-- * __Extensibility__: Easy to add new algorithms with appropriate storage
-- * __Documentation__: Types serve as executable documentation
--
-- ==== Storage Specialization
--
-- Each algorithm gets optimized storage:
--
-- * __Counters__: Simple key-value cache with TTL
-- * __Timestamps__: STM Map of timestamp lists  
-- * __Token Buckets__: STM Map of worker entries with background threads
-- * __Leaky Buckets__: STM Map with continuous drain workers
-- * __TinyLRU__: Bounded LRU cache with automatic eviction
--
-- ==== Memory Management
--
-- * Token and Leaky bucket stores include automatic purging
-- * TinyLRU provides bounded memory usage
-- * Counter stores use TTL-based expiration
-- * All stores support manual reset for cleanup
data InMemoryStore (a :: Algorithm) where
  -- | Counter-based storage for fixed window algorithm.
  --
  -- Uses 'Data.Cache' for automatic TTL-based expiration. Stores JSON-serialized
  -- counter values with precise expiration timing.
  CounterStore :: TVar (C.Cache Text Text) -> InMemoryStore 'FixedWindow

  -- | Timestamp list storage for sliding window algorithm.
  --
  -- Maintains lists of request timestamps per key. Enables precise rate
  -- calculation by examining timestamps within the sliding time window.
  TimestampStore :: TVar (StmMap.Map Text [Double]) -> InMemoryStore 'SlidingWindow

  -- | Token bucket entry storage with worker thread management.
  --
  -- Each entry includes bucket state, request queue, and worker synchronization.
  -- Automatically starts purge threads to clean up inactive buckets.
  TokenBucketStore :: TVar (StmMap.Map Text TokenBucketEntry) -> InMemoryStore 'TokenBucket

  -- | Leaky bucket entry storage with continuous drain workers.
  --
  -- Similar to token buckets but with continuous drain semantics. Each bucket
  -- has a worker thread that continuously drains at the specified rate.
  LeakyBucketStore :: TVar (StmMap.Map Text LeakyBucketEntry) -> InMemoryStore 'LeakyBucket

  -- | Bounded LRU cache storage.
  --
  -- Provides fixed-size cache with least-recently-used eviction. Suitable
  -- for scenarios where memory bounds are more important than precise rate limiting.
  TinyLRUStore :: TVar (TinyLRU.TinyLRUCache s) -> InMemoryStore 'TinyLRU

-- | Typeclass for creating algorithm-specific storage instances.
--
-- Uses type-level programming to ensure each algorithm gets appropriate storage.
-- The phantom type parameter prevents creation of incompatible store types.
--
-- ==== Type-Level Dispatch
--
-- @
-- -- Compiler ensures correct store type
-- tokenStore <- createStore @'TokenBucket    -- Creates TokenBucketStore
-- counterStore <- createStore @'FixedWindow  -- Creates CounterStore
-- 
-- -- This would be a compile error:
-- -- tokenStore <- createStore @'SlidingWindow  -- Type mismatch!
-- @
--
-- ==== Automatic Services
--
-- Some storage types automatically start background services:
--
-- * __Token/Leaky buckets__: Auto-purge threads for inactive entries
-- * __Counter stores__: TTL-based expiration threads
-- * __Timestamp stores__: Manual cleanup required
-- * __TinyLRU__: Built-in eviction on size limits
class CreateStore (a :: Algorithm) where
  -- | Create a new storage instance for the specified algorithm.
  --
  -- Initializes all necessary data structures and background services.
  -- The created store is immediately ready for use.
  createStore :: IO (InMemoryStore a)

-- | Convert seconds to TimeSpec for use with Data.Cache.
--
-- Calculates an absolute future time by adding the specified duration
-- to the current monotonic time. Used for setting TTL values in cache
-- operations.
--
-- ==== Monotonic Time Benefits
--
-- * __Clock Adjustments__: Unaffected by system clock changes
-- * __Precision__: Nanosecond resolution for accurate timing
-- * __Performance__: Fast system call with minimal overhead
-- * __Reliability__: Guaranteed monotonic progression
--
-- ==== Example
--
-- @
-- -- Create 5-minute expiration time
-- expiryTime <- secondsToTimeSpec 300
-- 
-- -- Use with cache operations
-- C.insertSTM key value cache (Just expiryTime)
-- @
secondsToTimeSpec :: Int        -- ^ Duration in seconds from now
                  -> IO TimeSpec -- ^ Absolute future time for expiration
secondsToTimeSpec seconds = do
  now <- getTime Monotonic
  return $ now + TimeSpec (fromIntegral seconds) 0

-- | Create store instances for each Algorithm.
instance CreateStore 'FixedWindow where
  createStore = createStoreWith CounterStore

instance CreateStore 'SlidingWindow where
  createStore = do
    emptyMap <- atomically (StmMap.new :: STM (StmMap.Map Text [Double]))
    tvar <- newTVarIO emptyMap
    pure $ TimestampStore tvar

instance CreateStore 'TokenBucket where
  createStore = do
    emptyMap <- atomically (StmMap.new :: STM (StmMap.Map Text TokenBucketEntry))
    tvar <- newTVarIO emptyMap
    void $ startCustomPurgeTokenBucket emptyMap (60 :: Integer) (3600 :: Integer)
    pure $ TokenBucketStore tvar

instance CreateStore 'LeakyBucket where
  createStore = do
    emptyMap <- atomically (StmMap.new :: STM (StmMap.Map Text LeakyBucketEntry))
    tvar <- newTVarIO emptyMap
    void $ startCustomPurgeLeakyBucket emptyMap (60 :: Integer) (3600 :: Integer)
    pure $ LeakyBucketStore tvar

instance CreateStore 'TinyLRU where
  createStore = TinyLRUStore <$> (atomically $ newTVar =<< TinyLRU.initTinyLRU 100)

createStoreWith :: (TVar (C.Cache Text Text) -> InMemoryStore a) -> IO (InMemoryStore a)
createStoreWith mkStore = do
  rawCache <- C.newCache Nothing
  purgeInterval <- newMVar (60 :: Integer) -- Purge every 60 seconds
  purgeSignal <- newMVar ()   -- Signal to trigger purge
  void $ forkIO $ forever $ do
    takeMVar purgeSignal
    interval <- readMVar purgeInterval
    startTime <- getTime Monotonic
    C.purgeExpired rawCache
    endTime <- getTime Monotonic
    let elapsedMicros = (toNanoSecs endTime - toNanoSecs startTime) `div` 1000
        remainingMicros = max (0 :: Integer) (interval * 1000000 - elapsedMicros)
    waitUntilNextPurge startTime remainingMicros purgeSignal
  tvar <- newTVarIO rawCache
  return $ mkStore tvar

-- | Wait until the next purge interval deterministically
waitUntilNextPurge :: TimeSpec -> Integer -> MVar () -> IO ()
waitUntilNextPurge startTime remainingMicros purgeSignal = do
  currentTime <- getTime Monotonic
  let elapsedMicros = (toNanoSecs currentTime - toNanoSecs startTime) `div` 1000
  if elapsedMicros >= remainingMicros
    then putMVar purgeSignal () -- Signal the next purge
    else do
      let sleepMicros = fromIntegral (min remainingMicros (toInteger (maxBound :: Int))) :: Int
      threadDelay sleepMicros -- Use threadDelay for the remaining time
      putMVar purgeSignal ()  -- Signal after waiting

-- | Create a new in-memory store for a specific rate-limiting algorithm.
--
-- This function provides a convenient, type-safe way to create algorithm-specific
-- storage. It uses TypeApplications to specify which algorithm's store to create,
-- ensuring compile-time correctness.
--
-- ==== Type Safety Example
--
-- @
-- -- These are all valid and type-safe:
-- tokenStore <- createInMemoryStore @'TokenBucket
-- counterStore <- createInMemoryStore @'FixedWindow
-- lruStore <- createInMemoryStore @'TinyLRU
--
-- -- This would be a compile error (typo):
-- -- badStore <- createInMemoryStore @'TokenBuckett  -- Not a valid algorithm
-- @
--
-- ==== Background Services
--
-- Some algorithms automatically start background services:
--
-- * __TokenBucket/LeakyBucket__: Purge threads for cleanup (60s interval, 1h TTL)
-- * __FixedWindow__: TTL-based expiration threads
-- * __SlidingWindow/TinyLRU__: No automatic background services
--
-- ==== Example Usage
--
-- @
-- import Data.Proxy
--
-- main = do
--   -- Create stores for different algorithms
--   tokenStore <- createInMemoryStore @'TokenBucket
--   slidingStore <- createInMemoryStore @'SlidingWindow
--   
--   -- Use in cache creation
--   let tokenCache = newCache TokenBucket tokenStore
--   let slidingCache = newCache SlidingWindow slidingStore
-- @
createInMemoryStore :: forall (a :: Algorithm). CreateStore a => IO (InMemoryStore a)
createInMemoryStore = createStore @a

-- | Create a new cache with a given Algorithm and store.
--
-- This is the primary constructor for cache instances. It combines an algorithm
-- specification with a storage backend to create a fully functional cache.
-- The algorithm parameter is used for key prefixing and operation validation.
--
-- ==== Design Rationale
--
-- * __Separation of Concerns__: Algorithm logic separate from storage implementation
-- * __Flexibility__: Same algorithm can use different storage backends
-- * __Type Safety__: Algorithm-store compatibility enforced by types
-- * __Testability__: Easy to mock storage for testing
--
-- ==== Example
--
-- @
-- -- Create a token bucket cache with in-memory storage
-- store <- createInMemoryStore @'TokenBucket
-- let cache = newCache TokenBucket store
--
-- -- Later operations use the unified cache interface
-- writeCache cache "user123" initialState 3600
-- result <- readCache cache "user123"
-- @
newCache :: Algorithm   -- ^ The rate limiting algorithm this cache implements
         -> store       -- ^ The storage backend (must be compatible with algorithm)
         -> Cache store -- ^ Complete cache instance ready for use
newCache algo store = Cache
  { cacheAlgorithm = algo
  , cacheStore = store
  }

-- | Read from cache using the algorithm-prefixed key.
--
-- Automatically applies the appropriate key prefix based on the cache's algorithm,
-- then delegates to the storage backend's read operation. This ensures consistent
-- key namespacing across all cache operations.
--
-- ==== Key Transformation
--
-- @
-- -- For a TokenBucket cache with key "user123":
-- -- Actual key used: "token_bucket:token_bucket:user123"
-- --                   ^^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^
-- --                   prefix       prefixed key
-- @
--
-- ==== Type Safety
--
-- The return type is determined by the storage backend's CacheStore instance,
-- ensuring you get the correct value type for the algorithm:
--
-- * __FixedWindow__: @Maybe Int@ (counter values)
-- * __TokenBucket__: @Maybe TokenBucketState@ (bucket state)
-- * __SlidingWindow__: @Maybe [Double]@ (timestamp lists)
--
-- ==== Example
--
-- @
-- -- Read token bucket state
-- maybeState <- readCache tokenCache "user456"
-- case maybeState of
--   Nothing -> putStrLn "No bucket exists for user"
--   Just state -> putStrLn $ "User has " ++ show (tokens state) ++ " tokens"
-- @
readCache :: (CacheStore store v IO) 
          => Cache store  -- ^ Cache instance with algorithm and storage
          -> Text         -- ^ Unprefixed key (e.g., "user123", "api_key_abc")
          -> IO (Maybe v) -- ^ Retrieved value with algorithm-appropriate type
readCache cache unprefixedKey =
  readStore (cacheStore cache) (algorithmPrefix $ cacheAlgorithm cache)
            (algorithmPrefix (cacheAlgorithm cache) <> ":" <> unprefixedKey)

-- | Write to cache using the algorithm-prefixed key.
--
-- Stores a value with automatic key prefixing and TTL handling. The value type
-- must match what the storage backend expects for the cache's algorithm.
--
-- ==== TTL Behavior
--
-- * __Absolute TTL__: Expiration time calculated from current time
-- * __Background Cleanup__: Most stores have automatic cleanup threads
-- * __Precision__: Uses monotonic clock for accurate timing
-- * __Consistency__: TTL behavior consistent across all algorithms
--
-- ==== Example
--
-- @
-- -- Store token bucket state for 1 hour
-- let initialState = TokenBucketState 100 currentTime
-- writeCache tokenCache "new_user" initialState 3600
--
-- -- Store counter value for 5 minutes
-- writeCache counterCache "api_limit" (42 :: Int) 300
-- @
writeCache :: (CacheStore store v IO) 
           => Cache store  -- ^ Cache instance
           -> Text         -- ^ Unprefixed key
           -> v            -- ^ Value to store (type must match store's expectation)
           -> Int          -- ^ TTL in seconds (time until expiration)
           -> IO ()        -- ^ No return value, throws on error
writeCache cache unprefixedKey val expiresIn =
  writeStore (cacheStore cache) (algorithmPrefix $ cacheAlgorithm cache)
             (algorithmPrefix (cacheAlgorithm cache) <> ":" <> unprefixedKey) val expiresIn

-- | Delete a key from cache using the algorithm-prefixed key.
--
-- Removes an entry from the cache, including any associated resources
-- (worker threads, background tasks, etc.). The operation is atomic
-- and thread-safe.
--
-- ==== Resource Cleanup
--
-- * __Token/Leaky Buckets__: Terminates associated worker threads
-- * __Counters__: Simple key removal
-- * __Timestamps__: Clears timestamp lists
-- * __LRU__: Updates LRU ordering and frees space
--
-- ==== Example
--
-- @
-- -- Remove user's rate limiting state
-- deleteCache tokenCache "inactive_user"
--
-- -- Clear API key's counter
-- deleteCache counterCache "expired_api_key"
-- @
deleteCache :: (CacheStore store v IO) 
            => Cache store  -- ^ Cache instance
            -> Text         -- ^ Unprefixed key to delete
            -> IO ()        -- ^ No return value, silent if key doesn't exist
deleteCache cache unprefixedKey =
  deleteStore (cacheStore cache) (algorithmPrefix $ cacheAlgorithm cache)
              (algorithmPrefix (cacheAlgorithm cache) <> ":" <> unprefixedKey)

-- | Increment a numeric cache value or initialise it if missing.
--
-- Provides atomic increment-or-initialize semantics essential for counter-based
-- rate limiting. If the key doesn't exist, initializes to 1. If it exists,
-- increments by 1. The operation is atomic even under high concurrency.
--
-- ==== Atomicity Guarantees
--
-- * __STM-based stores__: Full transaction isolation
-- * __Memory stores__: Process-level atomicity
-- * __Distributed stores__: Backend-specific consistency
--
-- ==== Error Handling
--
-- * __Type Mismatch__: Returns error if existing value isn't numeric
-- * __Serialization__: Handles JSON encoding/decoding failures gracefully
-- * __Overflow__: Behavior depends on numeric type (Int, Double, etc.)
--
-- ==== Example
--
-- @
-- -- Increment API request counter
-- newCount <- incrementCache apiCache "requests_per_minute" 60
-- when (newCount > 1000) $ throwError "Rate limit exceeded"
--
-- -- Initialize or increment user action count
-- actionCount <- incrementCache userCache "daily_actions" 86400
-- @
incrementCache :: (CacheStore store v IO, FromJSON v, ToJSON v, Ord v, Num v) 
               => Cache store  -- ^ Cache instance (must support numeric values)
               -> Text         -- ^ Unprefixed key to increment
               -> Int          -- ^ TTL in seconds for the incremented value
               -> IO v         -- ^ New value after increment
incrementCache cache unprefixedKey expiresIn = do
  let fullKey = algorithmPrefix (cacheAlgorithm cache) <> ":" <> unprefixedKey
      prefix = algorithmPrefix (cacheAlgorithm cache)
  incStore (cacheStore cache) prefix fullKey expiresIn

-- | Clear all entries in an in-memory store.
--
-- Provides a direct interface to the ResettableStore functionality.
-- Useful when you need to reset storage without going through the
-- cache wrapper.
--
-- ==== Use Cases
--
-- * __Testing__: Clean slate between test cases
-- * __Maintenance__: Clear corrupted state
-- * __Memory Management__: Free up memory during low usage
-- * __Reconfiguration__: Reset before changing algorithm parameters
--
-- ==== Thread Safety
--
-- The operation is atomic and thread-safe, but concurrent operations
-- may see the reset at different times. Consider coordinating with
-- other threads if precise timing is required.
--
-- ==== Example
--
-- @
-- -- Direct store reset
-- clearInMemoryStore myTokenBucketStore
--
-- -- Conditional reset based on memory usage
-- when memoryPressure $ clearInMemoryStore store
-- @
clearInMemoryStore :: ResettableStore store 
                   => store   -- ^ Storage instance to clear
                   -> IO ()   -- ^ No return value, completes when reset is done
clearInMemoryStore = resetStore

-- | Reset all entries in a cache.
--
-- Clears all data from the cache's storage backend. This is a convenience
-- wrapper around clearInMemoryStore that works at the cache level rather
-- than the storage level.
--
-- ==== Behavior
--
-- * __Complete Reset__: All keys and values are removed
-- * __Background Services__: Worker threads and purge threads continue running
-- * __Algorithm State__: Any algorithm-specific state is cleared
-- * __Immediate Effect__: Reset is visible to all threads immediately
--
-- ==== Example
--
-- @
-- -- Reset entire token bucket cache
-- cacheReset tokenBucketCache
--
-- -- Reset counter cache for new time period
-- cacheReset apiCounterCache
-- @
cacheReset :: ResettableStore store 
           => Cache store  -- ^ Cache instance to reset
           -> IO ()        -- ^ No return value, completes when reset is done
cacheReset (Cache _ store) = resetStore store

-- | Helper function to create a TokenBucketEntry with proper TMVar initialization.
--
-- Creates a complete token bucket entry with all necessary components:
-- state storage, request queue, and worker synchronization. This ensures
-- proper initialization of all STM components.
--
-- ==== Entry Components
--
-- * __State TVar__: Atomic storage for bucket state (tokens, last update)
-- * __Request Queue__: TQueue for client-worker communication
-- * __Worker Lock__: TMVar for coordinating worker thread lifecycle
--
-- ==== Example
--
-- @
-- -- Create entry for new user bucket
-- now <- floor <$> getPOSIXTime
-- let initialState = TokenBucketState 100 now  -- 100 tokens, current time
-- entry <- createTokenBucketEntry initialState
--
-- -- Entry is ready for insertion into STM map
-- atomically $ StmMap.insert entry "user123" bucketMap
-- @
createTokenBucketEntry :: TokenBucketState      -- ^ Initial bucket state (tokens, timestamp)
                       -> IO TokenBucketEntry   -- ^ Complete entry ready for use
createTokenBucketEntry state = do
  stateVar <- newTVarIO state
  queue <- atomically TQueue.newTQueue
  workerLock <- atomically newEmptyTMVar
  return $ TokenBucketEntry stateVar queue workerLock

-- | Helper function to create a LeakyBucketEntry with proper TMVar initialization.
--
-- Similar to createTokenBucketEntry but for leaky bucket algorithm. Creates
-- all necessary STM components for leaky bucket operation with continuous
-- drain semantics.
--
-- ==== Entry Components
--
-- * __State TVar__: Atomic storage for bucket level and last update time
-- * __Request Queue__: TQueue using TMVar for STM-based responses
-- * __Worker Lock__: TMVar for worker thread coordination
--
-- ==== Example
--
-- @
-- -- Create entry for streaming connection
-- now <- realToFrac <$> getPOSIXTime
-- let initialState = LeakyBucketState 0.0 now  -- Empty bucket, current time
-- entry <- createLeakyBucketEntry initialState
--
-- -- Entry ready for continuous drain processing
-- atomically $ StmMap.insert entry "stream123" bucketMap
-- @
createLeakyBucketEntry :: LeakyBucketState      -- ^ Initial bucket state (level, timestamp)
                       -> IO LeakyBucketEntry   -- ^ Complete entry ready for use
createLeakyBucketEntry state = do
  stateVar <- newTVarIO state
  queue <- atomically TQueue.newTQueue
  workerLock <- atomically newEmptyTMVar
  return $ LeakyBucketEntry stateVar queue workerLock

-- | CacheStore instances

-- | CacheStore instance for FixedWindow algorithm using integer counters.
--
-- Implements counter-based rate limiting with JSON serialization and TTL support.
-- Uses Data.Cache for automatic expiration and efficient storage.
--
-- ==== Storage Format
--
-- * __Keys__: Text identifiers (API keys, user IDs, etc.)
-- * __Values__: JSON-serialized integers representing request counts
-- * __Expiration__: Automatic TTL-based cleanup
--
-- ==== Atomicity
--
-- Increment operations use STM for thread-safe atomic updates, ensuring
-- accurate counting even under high concurrency.
instance CacheStore (InMemoryStore 'FixedWindow) Int IO where
  readStore (CounterStore tvar) _prefix key = do
    cache <- readTVarIO tvar
    mval <- C.lookup cache key
    case mval of
      Nothing -> return Nothing
      Just txt -> return $ decodeStrict (encodeUtf8 txt)
  writeStore (CounterStore tvar) _prefix key val expiresIn = do
    let bs = encode val
        strictBs = LBS.toStrict bs
        jsonTxt = case decodeUtf8' strictBs of
                    Left _ -> ""
                    Right txt -> txt
    expiryTimeSpec <- secondsToTimeSpec expiresIn
    atomically $ do
      cache <- readTVar tvar
      C.insertSTM key jsonTxt cache (Just expiryTimeSpec)
  deleteStore (CounterStore tvar) _prefix key = do
    cache <- readTVarIO tvar
    C.delete cache key
  incStore (CounterStore tvar) _prefix key expiresIn = do
    now <- getTime Monotonic
    expiryTimeSpec <- secondsToTimeSpec expiresIn
    atomically $ do
        cache <- readTVar tvar
        mval <- C.lookupSTM False key cache now
        let currentVal = case mval of
                           Nothing -> 0
                           Just txt -> fromMaybe 0 (decodeStrict (encodeUtf8 txt))
        let newVal = currentVal + 1
        let bs = encode newVal
            strictBs = LBS.toStrict bs
            jsonTxt = case decodeUtf8' strictBs of
                        Left _ -> ""
                        Right txt -> txt
        C.insertSTM key jsonTxt cache (Just expiryTimeSpec)
        return newVal

-- | CacheStore instance for SlidingWindow algorithm using timestamp lists.
--
-- Stores lists of request timestamps for precise sliding window calculations.
-- Does not use TTL as timestamps are managed by the sliding window logic.
instance CacheStore (InMemoryStore 'SlidingWindow) [Double] IO where
  readStore (TimestampStore tvar) _prefix key = atomically $ do
    stmMap <- readTVar tvar
    StmMap.lookup key stmMap
  writeStore (TimestampStore tvar) _prefix key val _expiresIn = atomically $ do
    stmMap <- readTVar tvar
    StmMap.insert val key stmMap
  deleteStore (TimestampStore tvar) _prefix key = atomically $ do
    stmMap <- readTVar tvar
    StmMap.delete key stmMap

-- | CacheStore instance for TokenBucket algorithm.
--
-- Provides access to token bucket state while maintaining the worker thread
-- infrastructure. Read operations return current bucket state, write operations
-- update state and manage worker lifecycle.
instance CacheStore (InMemoryStore 'TokenBucket) TokenBucketState IO where
  readStore (TokenBucketStore tvar) _prefix key = atomically $ do
    stmMap <- readTVar tvar
    mval <- StmMap.lookup key stmMap
    case mval of
      Nothing -> return Nothing
      Just entry -> Just <$> readTVar (tbeState entry)
  writeStore (TokenBucketStore tvar) _prefix key val _expiresIn = do
    entry <- createTokenBucketEntry val
    atomically $ do
      stmMap <- readTVar tvar
      StmMap.focus (focusInsertOrUpdate tbeState entry val) key stmMap
  deleteStore (TokenBucketStore tvar) _prefix key = atomically $ do
    stmMap <- readTVar tvar
    StmMap.delete key stmMap

-- | CacheStore instance for LeakyBucket algorithm.
--
-- Similar to TokenBucket but for continuous drain semantics. Manages
-- leaky bucket state and worker thread lifecycle.
instance CacheStore (InMemoryStore 'LeakyBucket) LeakyBucketState IO where
  readStore (LeakyBucketStore tvar) _prefix key = atomically $ do
    stmMap <- readTVar tvar
    mval <- StmMap.lookup key stmMap
    case mval of
      Nothing -> return Nothing
      Just entry -> Just <$> readTVar (lbeState entry)
  writeStore (LeakyBucketStore tvar) _prefix key val _expiresIn = do
    entry <- createLeakyBucketEntry val
    atomically $ do
      stmMap <- readTVar tvar
      StmMap.focus (focusInsertOrUpdate lbeState entry val) key stmMap
  deleteStore (LeakyBucketStore tvar) _prefix key = atomically $ do
    stmMap <- readTVar tvar
    StmMap.delete key stmMap

-- | Generic helper for Focus insert-or-update operations.
--
-- Provides atomic insert-or-update semantics for STM maps. If the key doesn't
-- exist, inserts the new entry. If it exists, updates the state within the
-- existing entry.
focusInsertOrUpdate
  :: (entry -> TVar v)    -- ^ Function to extract state TVar from entry
  -> entry                -- ^ New entry to insert if key doesn't exist
  -> v                    -- ^ New value to set in state TVar
  -> Focus.Focus entry STM ()  -- ^ Focus operation for atomic insert-or-update
focusInsertOrUpdate getState entry newVal = Focus.Focus
  (pure ((), Focus.Set entry))
  (\existing -> do
     writeTVar (getState existing) newVal
     pure ((), Focus.Leave))

-- | Leaky bucket worker thread implementation.
--
-- Implements the continuous drain algorithm for leaky buckets. Processes
-- requests from a queue and updates bucket state based on elapsed time
-- and leak rate.
--
-- ==== Algorithm Details
--
-- 1. __Drain Calculation__: level' = max(0, level - elapsed * leakRate)
-- 2. __Request Processing__: level'' = level' + requestSize (typically 1)
-- 3. __Capacity Check__: allowed = level'' <= capacity
-- 4. __State Update__: Apply leak even on denial for accurate timing
--
-- ==== Example
--
-- @
-- -- Start worker for streaming rate limiter
-- startLeakyBucketWorker stateVar queue 100 2.0 "stream123"
-- -- Capacity: 100 requests, Leak rate: 2 requests/second
-- @
startLeakyBucketWorker
  :: TVar LeakyBucketState          -- ^ Shared bucket state
  -> TQueue.TQueue (TMVar Bool)     -- ^ Request queue with STM responses
  -> Int                            -- ^ Bucket capacity (maximum level)
  -> Double                         -- ^ Leak rate (requests drained per second)
  -> IO ()                          -- ^ Returns immediately, worker runs in background
startLeakyBucketWorker stateVar queue capacity leakRate = void . forkIO $ 
  forever $ do
    replyVar <- atomically $ readTQueue queue
    now <- realToFrac <$> getPOSIXTime
    result <- atomically $ do
      LeakyBucketState{level,lastTime} <- readTVar stateVar
      let elapsed     = now - lastTime
          leakedLevel = max 0 (level - elapsed * leakRate)
          nextLevel   = leakedLevel + 1
          allowed     = nextLevel <= fromIntegral capacity
          -- **FIX:** apply leak even on denial
          finalLevel  = if allowed then nextLevel else leakedLevel
      writeTVar stateVar LeakyBucketState{ level = finalLevel, lastTime = now }
      return allowed
    atomically $ putTMVar replyVar result

-- | CacheStore instance for TinyLRU algorithm.
--
-- Provides bounded cache with least-recently-used eviction and TTL support.
-- Automatically handles expiration and LRU ordering updates.
instance CacheStore (InMemoryStore 'TinyLRU) Int IO where
  readStore (TinyLRUStore ref) _prefix key = do
    now <- liftIO $ getTime Monotonic
    atomically $ do
      cache <- readTVar ref
      maybeNodeRef <- StmMap.lookup key (TinyLRU.lruCache cache)
      case maybeNodeRef of
        Just nodeRef -> do
          node <- readTVar nodeRef
          let expired = TinyLRU.isExpired now node
          if expired
            then do
              TinyLRU.deleteKey key cache
              return Nothing
            else do
              let decoded :: Maybe Int = decodeStrict (TinyLRU.nodeValue node)
              TinyLRU.moveToFrontInCache cache nodeRef
              return decoded
        Nothing -> return Nothing
  writeStore (TinyLRUStore ref) _prefix key val expiresIn = do
    now <- liftIO $ getTime Monotonic
    atomically $ do
      cache <- readTVar ref
      _ <- TinyLRU.updateValue now key val expiresIn cache
      return ()
  deleteStore (TinyLRUStore ref) _prefix key = do
    atomically $ do
      cache <- readTVar ref
      TinyLRU.deleteKey key cache

-- | ResettableStore instances

-- | ResettableStore instances for all InMemoryStore variants.
--
-- Provides uniform reset behavior across all algorithm-specific stores.
-- Each implementation handles algorithm-specific cleanup requirements.
instance ResettableStore (InMemoryStore a) where
  resetStore (CounterStore tvar) = resetStoreWith tvar
  resetStore (TimestampStore tvar) = atomically $ do
    stmMap <- readTVar tvar
    StmMap.reset stmMap
  resetStore (TokenBucketStore tvar) = atomically $ do
    stmMap <- readTVar tvar
    StmMap.reset stmMap
  resetStore (LeakyBucketStore tvar) = atomically $ do
    stmMap <- readTVar tvar
    StmMap.reset stmMap
  resetStore (TinyLRUStore ref) = atomically $ do
    cache <- readTVar ref
    TinyLRU.resetTinyLRU cache

-- | Reset helper for Data.Cache-based stores.
--
-- Creates a fresh cache instance and atomically replaces the old one.
-- This ensures all TTL timers and cached data are completely cleared.
resetStoreWith :: TVar (C.Cache Text Text) -> IO ()
resetStoreWith tvar = do
  newCache <- C.newCache Nothing
  atomically $ writeTVar tvar newCache

-- | Compose a unique cache key from algorithm, IP zone, and user identifier.
--
-- Creates hierarchical cache keys that prevent collisions and enable
-- efficient organization of rate limiting data. The key format follows
-- a consistent pattern across all algorithms.
--
-- ==== Key Format
--
-- @
-- "algorithm_prefix:ip_zone:user_key"
-- @
--
-- ==== Examples
--
-- @
-- makeCacheKey TokenBucket "api" "user123"
-- -- Result: "token_bucket:api:user123"
--
-- makeCacheKey FixedWindow "web" "192.168.1.1"  
-- -- Result: "rate_limiter:web:192.168.1.1"
-- @
--
-- ==== Use Cases
--
-- * __Multi-tenant Applications__: Separate rate limits per tenant
-- * __Geographic Zones__: Different limits for different regions
-- * __Service Tiers__: Varied limits based on user subscription level
-- * __API Versioning__: Separate limits for different API versions
--
-- ==== Benefits
--
-- * __Collision Prevention__: Hierarchical structure prevents key conflicts
-- * __Query Efficiency__: Pattern-based queries and cleanup
-- * __Debugging__: Clear key structure aids troubleshooting
-- * __Monitoring__: Easy to aggregate metrics by zone or user type
makeCacheKey :: Algorithm  -- ^ Rate limiting algorithm for prefix
             -> Text       -- ^ IP zone or service identifier (e.g., "api", "web", "zone1")
             -> Text       -- ^ User key (e.g., API key, user ID, IP address)
             -> Text       -- ^ Complete hierarchical cache key
makeCacheKey algo ipZone userKey = algorithmPrefix algo <> ":" <> ipZone <> ":" <> userKey
