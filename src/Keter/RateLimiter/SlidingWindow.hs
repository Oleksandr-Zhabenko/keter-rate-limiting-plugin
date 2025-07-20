{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Keter.RateLimiter.SlidingWindow
Description : Sliding window rate-limiting algorithm implementation
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : stable
Portability : portable
-}

module Keter.RateLimiter.SlidingWindow
  ( allowRequest
  ) where

import Keter.RateLimiter.Cache (makeCacheKey, Algorithm(SlidingWindow))
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Maybe (fromMaybe)
import Control.Concurrent.STM
import qualified Data.Cache as C
import Data.Aeson (encode, decodeStrict)
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import System.Clock (Clock(Monotonic), getTime) -- Import for getTime

-- | Check if a request is allowed based on the sliding window algorithm, considering IP zones.
-- This function now takes the raw TVar of the cache for atomic operations.
allowRequest
  :: TVar (C.Cache Text Text) -- ^ The TVar holding the Data.Cache for timestamps
  -> Text -- ^ IP zone
  -> Text -- ^ User key
  -> Int -- ^ Window size (in seconds)
  -> Int -- ^ Request limit
  -> IO Bool
allowRequest cacheTVar ipZone userKey windowSize limit = do
  nowSecs <- floor <$> getPOSIXTime
  nowTimeSpec <- getTime Monotonic -- Get current TimeSpec for C.lookupSTM
  let key = makeCacheKey SlidingWindow ipZone userKey
  
  atomically $ do
    cache <- readTVar cacheTVar
    mTimestampsTxt <- C.lookupSTM False key cache nowTimeSpec
    let timestamps = case mTimestampsTxt of
                       Just txt -> fromMaybe [] (decodeStrict (encodeUtf8 txt))
                       Nothing -> []
        -- Filter based on nowSecs (integer seconds) for the window logic
        validTimestamps = filter (\t -> nowSecs - t <= windowSize) timestamps
        
        allowed = length validTimestamps < limit
    
    -- Always update the cache with the pruned list.
    -- Add the new timestamp only if the request is allowed.
    let newTimestamps = if allowed
                        then nowSecs : validTimestamps
                        else validTimestamps
    
    let jsonTxt = case decodeUtf8' (LBS.toStrict (encode newTimestamps)) of
                    Left _ -> "" -- Should not happen with valid Int list
                    Right txt -> txt
    -- C.insertSTM takes key, value, cache, and optional expiration TimeSpec
    C.insertSTM key jsonTxt cache Nothing -- TTL is managed by the algorithm logic, not Data.Cache's expiration
    
    return allowed
