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

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import Keter.RateLimiter.Cache
import Data.Maybe (fromMaybe)

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
  now <- floor <$> getPOSIXTime
  mState <- readCacheWithZone cache ipZone userKey
  let state = fromMaybe (TokenBucketState capacity now) mState
      refilledState = refill state now
  if tokens refilledState > 0
    then do
      let newState = refilledState { tokens = tokens refilledState - 1 }
      writeCacheWithZone cache ipZone userKey newState expiresIn
      return True
    else do
      -- No tokens, but we still need to write back the refilled state
      writeCacheWithZone cache ipZone userKey refilledState expiresIn
      return False
  where
    refill :: TokenBucketState -> Int -> TokenBucketState
    refill (TokenBucketState oldTokens lastTime) nowTime =
      let elapsed = fromIntegral (nowTime - lastTime) :: Double
          addedTokens = floor $ elapsed * refillRate
          newTokens = min capacity (oldTokens + addedTokens)
      in TokenBucketState newTokens (if addedTokens > 0 then nowTime else lastTime)
