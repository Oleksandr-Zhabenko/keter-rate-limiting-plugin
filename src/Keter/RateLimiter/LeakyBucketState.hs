{-
Oleksandr Zhabenko added several implementations of the window algorithm: here in the file there is a leaky bucket window data implementation using AI chatbots.
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Keter.RateLimiter.LeakyBucketState
  ( LeakyBucketState(..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data LeakyBucketState = LeakyBucketState
  { level    :: Double  -- current water level
  , lastTime :: Int     -- last leak timestamp (POSIX time)
  } deriving (Show, Eq, Generic)

instance ToJSON LeakyBucketState
instance FromJSON LeakyBucketState

