-- Updated: Keter.RateLimiter.SlidingWindow
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Keter.RateLimiter.SlidingWindow
  ( allowRequest
  ) where

import Keter.RateLimiter.Cache (makeCacheKey, Algorithm(SlidingWindow), secondsToTimeSpec)
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Maybe (fromMaybe)
import Control.Concurrent.STM
import qualified Data.Cache as C
import Data.Aeson (encode, decodeStrict)
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import System.Clock (getTime, Clock(Monotonic))

allowRequest
  :: TVar (C.Cache Text Text)
  -> Text -> Text -> Int -> Int -> IO Bool
allowRequest cacheTVar ipZone userKey windowSize limit = do
  nowSecs <- floor <$> getPOSIXTime
  nowTime <- getTime Monotonic
  expireSpec <- secondsToTimeSpec (windowSize + 5) -- buffer window
  let key = makeCacheKey SlidingWindow ipZone userKey

  atomically $ do
    cache <- readTVar cacheTVar
    mTxt <- C.lookupSTM False key cache nowTime
    let old = case mTxt of
                Just txt -> fromMaybe [] (decodeStrict $ encodeUtf8 txt)
                Nothing  -> []
        fresh = filter (\t -> nowSecs - t <= windowSize) old
        allowed = length fresh < limit
        updated = if allowed then nowSecs : fresh else fresh
        outTxt = case decodeUtf8' (LBS.toStrict $ encode updated) of
                   Right t -> t
                   Left _  -> "[]"

    if null updated
      then C.deleteSTM key cache >> return allowed
      else C.insertSTM key outTxt cache (Just expireSpec) >> return allowed
