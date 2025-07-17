{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Keter.RateLimiter.TokenBucket
Description : Token bucket rate limiting algorithm implementation
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : stable
Portability : portable
-}

module Keter.RateLimiter.TokenBucket
  ( TokenBucketState(..)
  , allowRequest
  ) where

import Control.Concurrent.STM (atomically, readTVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON, encode, decodeStrict)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Cache as C
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Keter.RateLimiter.Cache
import Data.Maybe (fromMaybe)
import System.Clock (getTime, Clock(Monotonic))

-- | Minimum TTL in seconds to prevent abuse in web applications
-- Set to 2 seconds as this is appropriate for HTTP-based rate limiting:
-- - Prevents cache thrashing from very short TTLs
-- - Still allows reasonable rate limiting for web requests
-- - Most legitimate web usage patterns don't need sub-2-second windows
minTTL :: Int
minTTL = 2

-- | Attempt to allow a request based on the token bucket algorithm.
allowRequest
  :: MonadIO m
  => Cache (InMemoryStore 'TokenBucket)
  -> T.Text       -- ^ IP zone
  -> T.Text       -- ^ User key
  -> Int          -- ^ Capacity
  -> Double       -- ^ Refill rate (tokens per second)
  -> Int          -- ^ Expires in (seconds)
  -> m Bool
allowRequest cache ipZone userKey capacity refillRate expiresIn = liftIO $ do
  -- Validate TTL against minimum required value
  if expiresIn < minTTL
    then return False  -- Block request due to invalid TTL
    else do
      now <- floor <$> getPOSIXTime
      currentTime <- getTime Monotonic
      expiryTimeSpec <- secondsToTimeSpec expiresIn
      let key = makeCacheKey (cacheAlgorithm cache) ipZone userKey
      -- Perform read, refill, check, and write in a single STM transaction
      (result, newState) <- atomically $ do
        let TokenBucketStore tvar = cacheStore cache
        cache <- readTVar tvar
        mval <- C.lookupSTM True key cache currentTime
        let state = fromMaybe (TokenBucketState capacity now) (mval >>= decodeStrict . TE.encodeUtf8)
            refilledState = refill state now
        if tokens refilledState > 0
          then do
            let newState = refilledState { tokens = tokens refilledState - 1 }
            C.insertSTM key (TE.decodeUtf8 (LBS.toStrict $ encode newState)) cache (Just expiryTimeSpec)
            return (True, newState)
          else do
            C.insertSTM key (TE.decodeUtf8 (LBS.toStrict $ encode refilledState)) cache (Just expiryTimeSpec)
            return (False, refilledState)
      return result
  where
    refill :: TokenBucketState -> Int -> TokenBucketState
    refill (TokenBucketState oldTokens lastTime) nowTime =
      let elapsed = fromIntegral (nowTime - lastTime) :: Double
          addedTokens = floor $ elapsed * refillRate
          newTokens = min capacity (oldTokens + addedTokens)
      in TokenBucketState newTokens (if addedTokens > 0 then nowTime else lastTime)
