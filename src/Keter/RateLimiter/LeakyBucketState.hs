{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

{-|
Module      : Keter.RateLimiter.LeakyBucketState
Description : Data representation of the leaky bucket algorithm state
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : stable
Portability : portable

This module defines the 'LeakyBucketState' data type used to represent the state of a leaky bucket
rate limiting algorithm implementation within the Keter system.

== Overview

The leaky bucket algorithm is a common rate limiting technique that controls the rate at which
requests or events are processed by simulating a bucket that leaks at a constant rate.

The 'LeakyBucketState' stores:

* The current water level in the bucket ('level'), representing the accumulated amount of requests.
* The timestamp of the last leak event ('lastTime'), stored as POSIX time (integer).

This state is essential for tracking and updating the bucket's status as requests arrive and time progresses.

== JSON Instances

The module provides automatic JSON serialization and deserialization via 'ToJSON' and 'FromJSON'
instances, facilitating storage and transmission of the state.
-}

module Keter.RateLimiter.LeakyBucketState
  ( LeakyBucketState(..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON(..), withObject, (.:))
import Control.Monad (when, fail)

-- | Represents the state of a leaky bucket in the rate limiter.
data LeakyBucketState = LeakyBucketState
  { level    :: Double  -- ^ Current water level in the bucket.
  , lastTime :: Int     -- ^ Timestamp of the last leak event (POSIX time).
  } deriving (Show, Eq, Generic)

instance ToJSON LeakyBucketState

instance FromJSON LeakyBucketState where
  parseJSON = withObject "LeakyBucketState" $ \o -> do
    level <- o .: "level"
    when (level < 0) $ fail "level must be non-negative"
    when (level > 1000000) $ fail "level must not exceed 1000000"
    lastTime <- o .: "lastTime"
    return LeakyBucketState { level, lastTime }
