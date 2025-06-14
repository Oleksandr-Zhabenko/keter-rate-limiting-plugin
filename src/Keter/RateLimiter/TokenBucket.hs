{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Keter.RateLimiter.TokenBucket
Description : Token bucket rate limiting algorithm implementation
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : stable
Portability : portable

This module provides an implementation of the token bucket rate limiting algorithm,
using a cache to store the bucket state.

== Overview

The token bucket algorithm controls the rate of requests by maintaining a bucket of tokens.
Tokens are refilled at a constant rate up to a maximum capacity.
Each request consumes a token; if no tokens are available, the request is denied.

The bucket state is stored in a cache keyed by a user or client identifier.

== Key data type

- 'TokenBucketState' holds the current number of tokens and the timestamp of the last refill.

== Key function

- 'allowRequest' checks if a request can be allowed based on the token bucket state,
  refills tokens as needed, updates the state in the cache, and returns whether the request is allowed.

== Parameters of 'allowRequest'

* Cache — the cache storing token bucket states.
* Key — identifier for the bucket (e.g., user ID).
* Capacity — maximum number of tokens in the bucket.
* Refill rate — number of tokens added per second.

== Behaviour

When a request arrives, tokens are refilled according to elapsed time since last update.
If tokens are available, one is consumed and the request is allowed.
If no tokens remain, the request is denied.
The bucket state is always updated in the cache to reflect the current token count and timestamp.

-}

module Keter.RateLimiter.TokenBucket
  ( TokenBucketState(..)
  , allowRequest
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson (FromJSON, ToJSON, encode, decodeStrict)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           GHC.Generics (Generic)

import           Keter.RateLimiter.Cache

-- | Represents the state of a token bucket.
data TokenBucketState = TokenBucketState
  { tokens     :: Int    -- ^ Current number of tokens available.
  , lastUpdate :: Int    -- ^ Timestamp of last token refill (epoch seconds).
  } deriving (Show, Eq, Generic)

instance ToJSON TokenBucketState
instance FromJSON TokenBucketState

-- | Attempt to allow a request based on the token bucket algorithm.
--
-- Returns 'True' if the request is allowed (token consumed), 'False' otherwise.
--
-- Tokens are refilled based on elapsed time and the refill rate.
allowRequest
  :: MonadIO m
  => Cache (InMemoryStore "token_bucket")
  -> T.Text                                  -- ^ Key (e.g. user identifier)
  -> Int                                     -- ^ Capacity (max tokens)
  -> Double                                  -- ^ Refill rate (tokens per second)
  -> Int                                     -- ^ Expiry in seconds (cache TTL)
  -> m Bool                                  -- ^ Result: allowed or not
allowRequest cache unprefixedKey capacity refillRate expiresIn = liftIO $ do
  now <- floor <$> getPOSIXTime
  
  -- Read the token bucket state from cache
  mStateText <- readCache cache unprefixedKey
  
  -- Decode JSON to TokenBucketState or initialize if missing
  let mstate = case mStateText of
                Nothing -> Nothing
                Just stateText -> 
                  case decodeStrict (TE.encodeUtf8 stateText) of
                    Nothing -> Nothing
                    Just s -> Just s

  let state = case mstate of
        Nothing -> TokenBucketState capacity now
        Just s  -> refill s now

  if tokens state > 0
    then do
      -- Consume one token and write updated state back
      let newState = state { tokens = tokens state - 1 }
      writeTokenState unprefixedKey newState
      return True
    else do
      -- Important: update state even if request is denied (time may have passed)
      writeTokenState unprefixedKey state
      return False

  where
    refill :: TokenBucketState -> Int -> TokenBucketState
    refill (TokenBucketState oldTokens lastTime) nowTime =
      let elapsed = fromIntegral (nowTime - lastTime) :: Double
          addedTokens = floor $ elapsed * refillRate
          newTokens = min capacity (oldTokens + addedTokens)
      in TokenBucketState newTokens (if addedTokens > 0 then nowTime else lastTime)

    -- Write TokenBucketState encoded as JSON Text to cache with user-specified expiration
    writeTokenState :: T.Text -> TokenBucketState -> IO ()
    writeTokenState key val = do
      let bs = encode val
          txt = case TE.decodeUtf8' (LBS.toStrict bs) of
                  Left _ -> ""  -- Handle encoding error gracefully
                  Right decodedText -> decodedText
      writeCache cache key txt expiresIn
