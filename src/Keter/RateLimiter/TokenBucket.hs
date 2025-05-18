{-
Oleksandr Zhabenko added several implementations of the window algorithm: here in the file there is a token bucket window implementation using AI chatbots.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

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

-- | Token Bucket state
data TokenBucketState = TokenBucketState
  { tokens     :: Int    -- Current number of tokens available
  , lastUpdate :: Int    -- Timestamp of last token refill (epoch seconds)
  } deriving (Show, Eq, Generic)

instance ToJSON TokenBucketState
instance FromJSON TokenBucketState

-- | Allow or deny a request based on token bucket
allowRequest
  :: MonadIO m
  => Cache (InMemoryStore "token_bucket")
  -> T.Text                                  -- Key (e.g. user identifier)
  -> Int                                     -- Capacity (max tokens)
  -> Double                                  -- Refill rate (tokens per second)
  -> m Bool                                  -- Result: allowed or not
allowRequest cache unprefixedKey capacity refillRate = liftIO $ do
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
    else
      return False

  where
    refill (TokenBucketState oldTokens lastTime) nowTime =
      let elapsed = fromIntegral (nowTime - lastTime) :: Double
          addedTokens = floor $ elapsed * refillRate
          newTokens = min capacity (oldTokens + addedTokens)
      in TokenBucketState newTokens (if addedTokens > 0 then nowTime else lastTime)

    -- Write TokenBucketState encoded as JSON Text to cache with some expiration
    writeTokenState :: T.Text -> TokenBucketState -> IO ()
    writeTokenState key val = do
      let bs = encode val
          txt = case TE.decodeUtf8' (LBS.toStrict bs) of
                  Left _ -> ""  -- Handle encoding error
                  Right decodedText -> decodedText
          expiresIn = 3600  -- 1 hour expiration
      writeCache cache key txt expiresIn
