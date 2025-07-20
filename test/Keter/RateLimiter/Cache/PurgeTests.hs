{-# LANGUAGE OverloadedStrings, NumericUnderscores #-}

module Keter.RateLimiter.Cache.PurgeTests where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Cache as C
import Control.Concurrent (threadDelay)
import Data.Text (Text)
import System.Clock (TimeSpec(..))
import Keter.RateLimiter.Cache (startAutoPurge) -- Import your module

testBackgroundPurge :: TestTree
testBackgroundPurge = testCase "Background purge removes expired keys" $ do
  -- Create cache with no default TTL 
  cache <- C.newCache Nothing :: IO (C.Cache Text Text)

  -- Create a proper TimeSpec for 2 seconds (2 seconds = 2 * 10^9 nanoseconds)
  let ttlSeconds = 2
  let ttlTimeSpec = TimeSpec ttlSeconds 0  -- 2 seconds, 0 nanoseconds
  
  -- Insert a key with proper TTL
  C.insert' cache (Just ttlTimeSpec) "expired-key" "value"

  -- Confirm it's there using lookup' (non-deleting version)
  mval <- C.lookup' cache "expired-key"
  case mval of
    Nothing -> assertFailure "Expected key to exist immediately after insert"
    Just v  -> v @?= "value"

  -- Start the auto-purge with 1-second interval
  startAutoPurge cache 1

  -- Wait for 3 seconds (TTL = 2s, purge interval = 1s, should be purged)
  threadDelay 3_000_000

  -- Now the key should be completely gone
  mval' <- C.lookup' cache "expired-key"
  mval' @?= Nothing
