{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Keter.RateLimiter.SlidingWindow
-- Description : Sliding window rate limiting algorithm implementation
-- Copyright   : (c) 2025 Oleksandr Zhabenko
-- License     : MIT
-- Maintainer  : oleksandr.zhabenko@yahoo.com
-- Stability   : stable
-- Portability : portable
--
-- This module provides an implementation of the /Sliding Window Counter/
-- algorithm for rate limiting. It is implemented using STM primitives and
-- integrates with the "Keter.RateLimiter.Cache" key structure.
--
-- A sliding window allows a fixed number of requests within a moving time
-- interval (window). It tracks individual request timestamps and filters them
-- to only keep those within the defined time window. This allows for
-- fine-grained request control and smooth rate-limiting behavior.
--
-- == Example usage
--
-- @
-- let getTimeNow = realToFrac \<$\> getPOSIXTime
-- result <- allowRequest getTimeNow myMap \"zone1\" \"user42\" 60 100
-- when result (putStrLn \"Request allowed\")
-- @
--
-- This example checks whether @\"user42\"@ from @\"zone1\"@ can make a request,
-- allowing up to 100 requests in a 60-second window.

module Keter.RateLimiter.SlidingWindow
  ( -- * Sliding Window Rate Limiting
    allowRequest
  ) where

import Keter.RateLimiter.Cache (makeCacheKey, Algorithm(SlidingWindow))
import Data.Text (Text)
import Control.Concurrent.STM
import qualified StmContainers.Map as StmMap
import qualified Focus

--------------------------------------------------------------------------------

-- | Check whether a request is allowed under the sliding window policy.
--
-- This function implements a time-based sliding window algorithm. Each client
-- is associated with a list of timestamps representing past allowed requests.
-- If the number of timestamps in the current time window exceeds the limit,
-- the request is denied.
--
-- This version is /thread-safe/ and uses STM to avoid race conditions under
-- concurrency. The timestamp list is updated atomically using 'StmMap.focus'.
--
-- === Algorithm Steps
--
-- 1. __Time Acquisition__: Get current timestamp using provided time function
-- 2. __Key Construction__: Build composite cache key from throttle name, IP zone, and user key
-- 3. __Atomic Update__: Use STM Focus to atomically read, filter, and update timestamp list
-- 4. __Window Filtering__: Remove timestamps older than the sliding window
-- 5. __Limit Check__: Allow request if filtered list length is below limit
-- 6. __State Update__: Add current timestamp to list if request is allowed
--
-- === Memory Management
--
-- The algorithm automatically cleans up old timestamps and removes empty entries
-- from the map, ensuring efficient memory usage for long-running applications.
--
-- ==== __Examples__
--
-- @
-- -- Basic API rate limiting: 100 requests per minute
-- getTime <- realToFrac \<$\> getPOSIXTime
-- allowed <- allowRequest (pure getTime) stmMapTVar \"api\" \"zone1\" \"user123\" 60 100
-- if allowed 
--   then processApiRequest
--   else sendRateLimitError
-- @
--
-- @
-- -- High-frequency trading API: 1000 requests per second
-- let getTimeIO = realToFrac \<$\> getPOSIXTime
-- result <- allowRequest getTimeIO storage \"hft\" \"premium\" \"trader456\" 1 1000
-- when result $ executeTrade
-- @
--
-- @
-- -- Multi-tier rate limiting with different windows
-- let (windowSecs, maxReqs) = case userTier of
--       Premium  -> (60, 1000)    -- 1000 requests per minute
--       Standard -> (60, 100)     -- 100 requests per minute  
--       Free     -> (3600, 50)    -- 50 requests per hour
--
-- allowed <- allowRequest getTime storage \"tiered\" zone userId windowSecs maxReqs
-- @
--
-- /Thread Safety:/ All operations are atomic via STM. Multiple threads can
-- safely call this function concurrently for the same or different keys.
--
-- /Performance:/ Time complexity is O(n) where n is the number of timestamps
-- within the sliding window. Space complexity is O(k*n) where k is the number
-- of active clients.
allowRequest
  :: IO Double
  -- ^ Action to get current time as fractional seconds since epoch
  -> TVar (StmMap.Map Text [Double])
  -- ^ STM map storing per-client timestamp lists
  -> Text
  -- ^ Throttle name (logical grouping identifier)
  -> Text
  -- ^ IP zone identifier for multi-tenant isolation
  -> Text
  -- ^ User key (unique client identifier)
  -> Int
  -- ^ Sliding window size in seconds (must be positive)
  -> Int
  -- ^ Maximum allowed requests within the time window (must be positive)
  -> IO Bool
  -- ^ 'True' if request is allowed, 'False' if rate-limited
allowRequest getTimeNow stmMapTVar throttleName ipZone userKey windowSize limit = do
  now <- getTimeNow
  let key = makeCacheKey throttleName SlidingWindow ipZone userKey
      windowSizeD = fromIntegral windowSize

  atomically $ do
    stmMap <- readTVar stmMapTVar
    StmMap.focus (updateTimestamps now windowSizeD limit) key stmMap

--------------------------------------------------------------------------------

-- | Internal helper that atomically updates the timestamp list using STM Focus.
--
-- This function removes timestamps that are older than the current window,
-- and determines if the current request is allowed. If so, the current time
-- is appended to the list. If the list becomes empty, the entry is removed
-- from the map.
--
-- This ensures memory-efficient cleanup of old clients and automatic garbage
-- collection of inactive entries.
--
-- === Behavior Details
--
-- * If the client has no prior entries, the request is allowed and a new list is created.
-- * If the client exists, the list is trimmed to remove stale timestamps.
-- * The request is allowed only if the filtered list has fewer than the specified limit.
-- * Empty timestamp lists are automatically removed from the map.
--
-- === Algorithm Complexity
--
-- * /Time/: O(n) where n is the number of timestamps in the current window
-- * /Space/: O(1) additional space per operation (filtering is done in-place conceptually)
updateTimestamps
  :: Double
  -- ^ Current time in fractional seconds
  -> Double  
  -- ^ Sliding window duration in seconds
  -> Int
  -- ^ Request limit within the window
  -> Focus.Focus [Double] STM Bool
  -- ^ STM Focus that returns whether the request is allowed
updateTimestamps now windowSize limit = Focus.Focus
  (do
    -- New client: allow first request and create timestamp list
    let newList = [now]
    pure (True, Focus.Set newList)
  )
  (\currentTimestamps -> do
    -- Existing client: filter old timestamps and check limit
    let freshTimestamps = filter (\t -> now - t <= windowSize) currentTimestamps
    let allowed = length freshTimestamps < limit
    let updatedTimestamps = if allowed then now : freshTimestamps else freshTimestamps
    
    -- Clean up empty entries to prevent memory leaks
    if null updatedTimestamps
      then pure (allowed, Focus.Remove)
      else pure (allowed, Focus.Set updatedTimestamps)
  )