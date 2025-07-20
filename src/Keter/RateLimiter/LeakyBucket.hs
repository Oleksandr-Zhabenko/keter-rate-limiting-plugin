-- Updated: Keter.RateLimiter.LeakyBucket
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Keter.RateLimiter.LeakyBucket
  ( allowRequest
  ) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue (TQueue, newTQueue, writeTQueue, readTQueue)
import Control.Monad (when, void, forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)

import Keter.RateLimiter.Types (LeakyBucketState(..))
import Keter.RateLimiter.Cache
import Keter.RateLimiter.AutoPurge (LeakyBucketEntry(..))
import qualified Focus as F
import qualified StmContainers.Map as StmMap

allowRequest
  :: MonadIO m
  => Cache (InMemoryStore 'LeakyBucket)
  -> Text     -- ^ IP-zone
  -> Text     -- ^ User key
  -> Int      -- ^ Capacity
  -> Double   -- ^ Leak rate (requests per second)
  -> m Bool
allowRequest cache ipZone userKey capacity leakRate = liftIO $ do
  if capacity <= 0 then pure False else do
    now <- floor <$> getPOSIXTime
    let fullKey = makeCacheKey LeakyBucket ipZone userKey
        LeakyBucketStore tvBuckets = cacheStore cache
    replyVar <- newEmptyTMVarIO

    newEntry <- createLeakyBucketEntry (LeakyBucketState 1 now)

    (wasNew, entry) <- atomically $ do
      buckets <- readTVar tvBuckets
      StmMap.focus
        (F.cases ((True, newEntry), F.Set newEntry)
                 (\existing -> ((False, existing), F.Leave)))
        fullKey buckets

    if wasNew
      then do
        let allowed = capacity > 0
        when allowed $ do
          started <- atomically $ tryPutTMVar (lbeWorkerLock entry) ()
          when started $ startLeakyBucketWorker (lbeState entry) (lbeQueue entry)
                                              capacity leakRate fullKey
        pure allowed
      else do
        atomically $ writeTQueue (lbeQueue entry) replyVar
        atomically $ takeTMVar replyVar
