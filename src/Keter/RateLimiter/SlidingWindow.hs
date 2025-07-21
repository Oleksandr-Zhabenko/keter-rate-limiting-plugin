{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Keter.RateLimiter.SlidingWindow
  ( allowRequest
  ) where

import Keter.RateLimiter.Cache (makeCacheKey, Algorithm(SlidingWindow))
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Control.Concurrent.STM
import qualified StmContainers.Map as StmMap
import qualified Focus

{-|
    The updated function now accepts a time retrieval function as its first argument.
    This injected function should return the current time as a Double (in seconds).

    This implementation is now fully transactional and thread-safe.
-}
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

-- | STM Focus operation to atomically update and check the timestamp list.
updateTimestamps
  :: Double
  -> Double
  -> Int
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
