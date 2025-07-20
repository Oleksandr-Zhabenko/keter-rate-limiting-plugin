-- Keter.RateLimiter.LeakyBucket.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Keter.RateLimiter.LeakyBucket
  ( allowRequest
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad              (when)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Data.Text                  (Text)
import Data.Time.Clock.POSIX      (getPOSIXTime)

import Keter.RateLimiter.Types          (LeakyBucketState (..))
import Keter.RateLimiter.Cache
import Keter.RateLimiter.AutoPurge      (LeakyBucketEntry (..))
import qualified Focus                  as F
import qualified StmContainers.Map      as StmMap

------------------------------------------------------------------------------

-- | Leaky-bucket limiter (capacity / leak-rate per key).
allowRequest
  :: MonadIO m
  => Cache (InMemoryStore 'LeakyBucket)
  -> Text     -- ^ IP-zone
  -> Text     -- ^ User key
  -> Int      -- ^ Capacity
  -> Double   -- ^ Leak rate (requests drained per second)
  -> m Bool
allowRequest cache ipZone userKey capacity leakRate = liftIO $ do
  now   <- floor <$> getPOSIXTime
  let fullKey                 = makeCacheKey LeakyBucket ipZone userKey
      LeakyBucketStore tvBuckets = cacheStore cache
  replyVar <- newEmptyMVar

  -- For a new bucket, the initial state after this first request will be level 1.
  newEntry <- createLeakyBucketEntry (LeakyBucketState 1 now)

  (wasNew, entry) <- atomically $ do
     buckets <- readTVar tvBuckets
     (isNew, ent) <-
       StmMap.focus
         (F.cases
           -- bucket not present, use the newly created entry
           ((True, newEntry), F.Set newEntry)
           -- bucket already exists
           (\existingEnt -> ((False, existingEnt), F.Leave))
         )
         fullKey buckets
     pure (isNew, ent)

  if wasNew
    then do
      -- For a new bucket, the first request is always allowed if capacity > 0.
      -- The state has already been set to level=1.
      let allowed = capacity > 0
      when allowed $ do
        -- Start the worker to handle SUBSEQUENT requests.
        started <- atomically $ tryPutTMVar (lbeWorkerLock entry) ()
        when started $
          startLeakyBucketWorker
            (lbeState entry)
            (lbeQueue entry)
            capacity
            leakRate
            fullKey
      pure allowed
    else do
      -- For an existing bucket, enqueue the request and wait for the worker's response.
      atomically $ writeTQueue (lbeQueue entry) replyVar
      takeMVar replyVar
