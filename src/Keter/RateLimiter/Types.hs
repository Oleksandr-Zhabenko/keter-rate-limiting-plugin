{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Keter.RateLimiter.Types
  ( TokenBucketState(..)
  , LeakyBucketState(..)
  ) where

import Data.Aeson (ToJSON, FromJSON(..), withObject, (.:))
import Control.Monad (when)
import GHC.Generics (Generic)

data TokenBucketState = TokenBucketState
  { tokens     :: Int
  , lastUpdate :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON TokenBucketState
instance FromJSON TokenBucketState where
  parseJSON = withObject "TokenBucketState" $ \o -> do
    tokens <- o .: "tokens"
    when (tokens < 0) $ fail "tokens must be non-negative"
    lastUpdate <- o .: "lastUpdate"
    return TokenBucketState { tokens, lastUpdate }

data LeakyBucketState = LeakyBucketState
  { level      :: Double
  , lastTime   :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON LeakyBucketState
instance FromJSON LeakyBucketState where
  parseJSON = withObject "LeakyBucketState" $ \o -> do
    level <- o .: "level"
    when (level < 0) $ fail "level must be non-negative"
    when (level > 1000000) $ fail "level must not exceed 1000000"
    lastTime <- o .: "lastTime"
    return LeakyBucketState { level, lastTime }
