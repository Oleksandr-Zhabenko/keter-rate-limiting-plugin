{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Keter.RateLimiter.Types
Description : Core data types for rate limiting algorithms
Copyright   : (c) Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : experimental
Portability : POSIX

This module provides the fundamental data types used in Keter's rate limiting system.
It defines state representations for two common rate limiting algorithms:

* __Token Bucket__: A rate limiting algorithm that maintains a bucket of tokens,
  where each request consumes a token. Tokens are replenished at a fixed rate.
* __Leaky Bucket__: A rate limiting algorithm that models a bucket with a hole,
  where requests fill the bucket and it drains at a constant rate.

Both types support JSON serialization for persistence and configuration purposes,
with validation to ensure state consistency.

== Example Usage

@
-- Token bucket example
let tokenState = TokenBucketState { tokens = 100, lastUpdate = 1640995200 }
print tokenState  -- TokenBucketState {tokens = 100, lastUpdate = 1640995200}

-- Leaky bucket example  
let leakyState = LeakyBucketState { level = 0.5, lastTime = 1640995200.123 }
print leakyState  -- LeakyBucketState {level = 0.5, lastTime = 1640995200.123}
@
-}
module Keter.RateLimiter.Types
  ( -- * Token Bucket Algorithm
    TokenBucketState(..)
    -- * Leaky Bucket Algorithm  
  , LeakyBucketState(..)
  ) where

import Data.Aeson (ToJSON, FromJSON(..), withObject, (.:))
import Control.Monad (when)
import GHC.Generics (Generic)

-- | State representation for the Token Bucket rate limiting algorithm.
--
-- The token bucket algorithm maintains a bucket that holds tokens up to a maximum capacity.
-- Each incoming request consumes one or more tokens from the bucket. Tokens are replenished
-- at a fixed rate. If insufficient tokens are available, the request is either delayed or rejected.
--
-- ==== Token Bucket Properties
--
-- * __Bursty Traffic__: Allows bursts of traffic up to the bucket capacity
-- * __Rate Control__: Long-term average rate is controlled by token replenishment rate
-- * __Memory Efficient__: Only requires tracking token count and last update time
--
-- ==== Use Cases
--
-- * API rate limiting with burst allowance
-- * Network traffic shaping
-- * Resource allocation with temporary overages
--
-- ==== Example
--
-- @
-- -- Initial state with 50 tokens, last updated at Unix timestamp 1640995200
-- let initialState = TokenBucketState 
--       { tokens = 50
--       , lastUpdate = 1640995200 
--       }
--
-- -- After consuming 10 tokens
-- let afterConsumption = initialState { tokens = 40 }
-- @
data TokenBucketState = TokenBucketState
  { tokens     :: Int  -- ^ Current number of available tokens in the bucket.
                       --   Must be non-negative. Represents the instantaneous
                       --   capacity available for processing requests.
  , lastUpdate :: Int  -- ^ Unix timestamp (seconds since epoch) of the last
                       --   bucket state update. Used to calculate how many
                       --   tokens should be replenished based on elapsed time.
  } deriving (Show, Eq, Generic)

-- | 'ToJSON' instance for 'TokenBucketState'.
--
-- Serializes the token bucket state to JSON format for persistence or network transmission.
--
-- ==== JSON Format
--
-- @
-- {
--   "tokens": 42,
--   "lastUpdate": 1640995200
-- }
-- @
instance ToJSON TokenBucketState

-- | 'FromJSON' instance for 'TokenBucketState' with validation.
--
-- Deserializes JSON to 'TokenBucketState' with the following validation rules:
--
-- * @tokens@ field must be non-negative (>= 0)
-- * @lastUpdate@ field must be present and parseable as 'Int'
--
-- ==== Validation Examples
--
-- @
-- -- Valid JSON
-- decode "{\"tokens\": 10, \"lastUpdate\": 1640995200}" :: Maybe TokenBucketState
-- -- Just (TokenBucketState {tokens = 10, lastUpdate = 1640995200})
--
-- -- Invalid JSON (negative tokens)
-- decode "{\"tokens\": -5, \"lastUpdate\": 1640995200}" :: Maybe TokenBucketState  
-- -- Nothing
-- @
--
-- __Throws:__ Parse error if @tokens@ is negative or required fields are missing.
instance FromJSON TokenBucketState where
  parseJSON = withObject "TokenBucketState" $ \o -> do
    tokens <- o .: "tokens"
    when (tokens < 0) $ fail "tokens must be non-negative"
    lastUpdate <- o .: "lastUpdate"
    return TokenBucketState { tokens, lastUpdate }

-- | State representation for the Leaky Bucket rate limiting algorithm.
--
-- The leaky bucket algorithm models a bucket with a hole in the bottom that drains
-- at a constant rate. Incoming requests add water to the bucket, and if the bucket
-- overflows, requests are rejected. This provides smooth rate limiting without bursts.
--
-- ==== Leaky Bucket Properties
--
-- * __Smooth Rate__: Enforces a consistent output rate regardless of input bursts
-- * __No Bursts__: Unlike token bucket, doesn't allow temporary rate exceedance  
-- * __Queue Modeling__: Can model request queuing with bucket level representing queue depth
--
-- ==== Use Cases
--
-- * Smooth traffic shaping for network connections
-- * Audio/video streaming rate control
-- * Database connection throttling
-- * Prevention of thundering herd problems
--
-- ==== Mathematical Model
--
-- The bucket level changes according to:
--
-- @
-- newLevel = max(0, oldLevel + requestSize - drainRate * timeDelta)
-- @
--
-- Where:
-- * @requestSize@ is the size of the incoming request
-- * @drainRate@ is the constant drain rate (requests per second)
-- * @timeDelta@ is the elapsed time since last update
--
-- ==== Example
--
-- @
-- -- Initial state: half-full bucket at timestamp 1640995200.5
-- let initialState = LeakyBucketState 
--       { level = 0.5
--       , lastTime = 1640995200.5 
--       }
--
-- -- After 1 second with drain rate 0.1/sec and no new requests
-- let afterDrain = initialState 
--       { level = 0.4  -- 0.5 - 0.1*1.0
--       , lastTime = 1640995201.5 
--       }
-- @
data LeakyBucketState = LeakyBucketState
  { level      :: Double  -- ^ Current fill level of the bucket (0.0 to capacity).
                          --   Represents the amount of "water" (pending requests)
                          --   currently in the bucket. Higher values indicate
                          --   more backpressure or pending work.
  , lastTime   :: Double  -- ^ Timestamp of last bucket update as Unix time with
                          --   fractional seconds. Higher precision than 'TokenBucketState'
                          --   to support sub-second drain rate calculations.
  } deriving (Show, Eq, Generic)

-- | 'ToJSON' instance for 'LeakyBucketState'.
--
-- Serializes the leaky bucket state to JSON format with full double precision.
--
-- ==== JSON Format
--
-- @
-- {
--   "level": 123.456,
--   "lastTime": 1640995200.789
-- }
-- @
instance ToJSON LeakyBucketState

-- | 'FromJSON' instance for 'LeakyBucketState' with validation.
--
-- Deserializes JSON to 'LeakyBucketState' with the following validation rules:
--
-- * @level@ must be non-negative (>= 0.0)
-- * @level@ must not exceed 1,000,000 (practical upper bound)
-- * @lastTime@ must be present and parseable as 'Double'
--
-- The upper bound on @level@ prevents potential overflow issues and ensures
-- reasonable memory usage for bucket state tracking.
--
-- ==== Validation Examples
--
-- @
-- -- Valid JSON
-- decode "{\"level\": 42.5, \"lastTime\": 1640995200.123}" :: Maybe LeakyBucketState
-- -- Just (LeakyBucketState {level = 42.5, lastTime = 1640995200.123})
--
-- -- Invalid JSON (negative level)
-- decode "{\"level\": -1.0, \"lastTime\": 1640995200.0}" :: Maybe LeakyBucketState
-- -- Nothing
--
-- -- Invalid JSON (level too high) 
-- decode "{\"level\": 2000000.0, \"lastTime\": 1640995200.0}" :: Maybe LeakyBucketState
-- -- Nothing
-- @
--
-- __Throws:__ Parse error if @level@ is negative, exceeds 1,000,000, or required fields are missing.
instance FromJSON LeakyBucketState where
  parseJSON = withObject "LeakyBucketState" $ \o -> do
    level <- o .: "level"
    when (level < 0) $ fail "level must be non-negative"
    when (level > 1000000) $ fail "level must not exceed 1000000"
    lastTime <- o .: "lastTime"
    return LeakyBucketState { level, lastTime }
