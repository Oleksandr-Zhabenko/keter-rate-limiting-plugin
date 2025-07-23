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
-- integrates with the `Keter.RateLimiter.Cache` key structure.
--
-- A sliding window allows a fixed number of requests within a moving time
-- interval (window). It tracks individual request timestamps and filters them
-- to only keep those within the defined time window. This allows for
-- fine-grained request control and smooth rate-limiting behavior.
--
-- == Example usage
--
-- > let getTimeNow = realToFrac <$> getPOSIXTime
-- > result <- allowRequest getTimeNow myMap "zone1" "user42" 60 100
-- > when result (putStrLn "Request allowed")
--
-- This example checks whether "user42" from "zone1" can make a request,
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
-- concurrency. The timestamp list is updated atomically using `StmMap.focus`.
--
-- == Parameters
--
-- [@getTimeNow@] Function that returns the current time as a `Double` (in seconds).
-- [@stmMapTVar@] STM container storing per-client timestamp lists.
-- [@ipZone@] A textual label identifying the group or IP zone.
-- [@userKey@] Unique identifier for the client (IP, token, etc).
-- [@windowSize@] Sliding window size in seconds.
-- [@limit@] Maximum allowed requests within the time window.
--
-- == Returns
--
-- A `Bool` indicating whether the request is allowed (`True`) or rate-limited (`False`).
--
-- == Example
--
-- > getTime <- realToFrac <$> getPOSIXTime
-- > allowed <- allowRequest getTime myStore "zone" "user" 10 5
-- > if allowed then serve else reject
allowRequest
  :: IO Double                    -- ^ Time retrieval function (returns time in seconds)
  -> TVar (StmMap.Map Text [Double]) -- ^ STM Map container (wrapped in a TVar)
  -> Text                         -- ^ Zone identifier (or grouping key)
  -> Text                         -- ^ User (or client) key
  -> Int                          -- ^ Sliding window size (in seconds)
  -> Int                          -- ^ Request limit within the window
  -> IO Bool                      -- ^ Returns True if the request is allowed, False if rate-limited
allowRequest getTimeNow stmMapTVar ipZone userKey windowSize limit = do
  now <- getTimeNow
  let key = makeCacheKey SlidingWindow ipZone userKey
      windowSizeD = fromIntegral windowSize

  atomically $ do
    stmMap <- readTVar stmMapTVar
    -- Use StmMap.focus for an atomic read-modify-write operation on the map entry
    StmMap.focus (updateTimestamps now windowSizeD limit) key stmMap

--------------------------------------------------------------------------------

-- | Internal helper that atomically updates the timestamp list using STM Focus.
--
-- This function removes timestamps that are older than the current window,
-- and determines if the current request is allowed. If so, the current time
-- is appended to the list. If the list becomes empty, the entry is removed
-- from the map.
--
-- This ensures memory-efficient cleanup of old clients.
--
-- == Parameters
--
-- [@now@] The current time in fractional seconds.
-- [@windowSize@] Duration of the sliding window.
-- [@limit@] Maximum allowed request count in the window.
--
-- == Behavior
--
-- - If the client has no prior entries, the request is allowed and a new list is created.
-- - If the client exists, the list is trimmed, and the request is allowed only if
--   the resulting list has fewer than @limit@ entries.
updateTimestamps
  :: Double                      -- ^ Current time
  -> Double                      -- ^ Sliding window size (seconds)
  -> Int                         -- ^ Request limit
  -> Focus.Focus [Double] STM Bool
updateTimestamps now windowSize limit = Focus.Focus
  -- Case 1: The key does not exist.
  -- This is the first request. It's allowed. Insert a new list with the current timestamp.
  (do
    let newList = [now]
    pure (True, Focus.Set newList)
  )
  -- Case 2: The key exists.
  -- Update the list and check the limit.
  (\currentTimestamps -> do
    -- Filter out timestamps older than the window
    let freshTimestamps = filter (\t -> now - t <= windowSize) currentTimestamps
    -- Check if the request is allowed
    let allowed = length freshTimestamps < limit
    -- If allowed, add the new timestamp. Otherwise, keep the old list.
    let updatedTimestamps = if allowed then now : freshTimestamps else freshTimestamps
    
    -- If the list becomes empty after filtering, remove it from the map.
    if null updatedTimestamps
      then pure (allowed, Focus.Remove)
      else pure (allowed, Focus.Set updatedTimestamps)
  )
