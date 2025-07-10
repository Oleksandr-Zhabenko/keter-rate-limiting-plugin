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

* Cache — the cache storing token bucket states, tagged with the TokenBucket algorithm.
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

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON, encode, decodeStrict)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import Keter.RateLimiter.Cache

-- | Represents the state of a token bucket.
data TokenBucketState = TokenBucketState
  { tokens     :: Int
  , lastUpdate :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON TokenBucketState
instance FromJSON TokenBucketState

-- | Attempt to allow a request based on the token bucket algorithm.
allowRequest
  :: MonadIO m
  => Cache (InMemoryStore 'TokenBucket)
  -> T.Text
  -> Int
  -> Double
  -> Int
  -> m Bool
allowRequest cache unprefixedKey capacity refillRate expiresIn = liftIO $ do
  now <- floor <$> getPOSIXTime
  let key = makeCacheKey (cacheAlgorithm cache) "" unprefixedKey
  mStateText <- readStore (cacheStore cache) (algorithmPrefix $ cacheAlgorithm cache) key
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
      let newState = state { tokens = tokens state - 1 }
      writeTokenState key newState
      return True
    else do
      writeTokenState key state
      return False
  where
    refill :: TokenBucketState -> Int -> TokenBucketState
    refill (TokenBucketState oldTokens lastTime) nowTime =
      let elapsed = fromIntegral (nowTime - lastTime) :: Double
          addedTokens = floor $ elapsed * refillRate
          newTokens = min capacity (oldTokens + addedTokens)
      in TokenBucketState newTokens (if addedTokens > 0 then nowTime else lastTime)
    writeTokenState :: T.Text -> TokenBucketState -> IO ()
    writeTokenState key val = do
      let bs = encode val
          txt = case TE.decodeUtf8' (LBS.toStrict bs) of
                  Left _ -> ""
                  Right decodedText -> decodedText
      writeStore (cacheStore cache) (algorithmPrefix $ cacheAlgorithm cache) key txt expiresIn
