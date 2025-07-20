-- Keter.RateLimiter.TokenBucket.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Keter.RateLimiter.TokenBucket
  ( allowRequest
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad            (when, void)
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Data.Text                (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX    (getPOSIXTime)

import Keter.RateLimiter.Cache
import Keter.RateLimiter.Types          (TokenBucketState (..))
import Keter.RateLimiter.AutoPurge      (TokenBucketEntry (..))
import qualified Focus                  as F
import qualified StmContainers.Map      as StmMap
import Keter.RateLimiter.TokenBucketWorker (startTokenBucketWorker)

------------------------------------------------------------------------------

minTTL :: Int
minTTL = 2

-- | Check whether a request may pass the token-bucket limiter.
allowRequest
  :: MonadIO m
  => Cache (InMemoryStore 'TokenBucket)
  -> Text    -- ^ IP zone
  -> Text    -- ^ User key
  -> Int     -- ^ Capacity
  -> Double  -- ^ Refill rate (tokens / second)
  -> Int     -- ^ TTL (seconds)
  -> m Bool
allowRequest cache ipZone userKey capacity refillRate expiresIn = liftIO $
  if expiresIn < minTTL
     then do
       putStrLn $
         "TokenBucket: Request denied due to invalid TTL: "
           ++ show expiresIn
       pure False
     else do
       now <- floor <$> getPOSIXTime
       let key                     = makeCacheKey (cacheAlgorithm cache) ipZone userKey
           TokenBucketStore tvBuckets = cacheStore cache
       replyVar <- newEmptyMVar

       ----------------------------------------------------------------------
       -- 1. Obtain (or create) bucket entry and enqueue the request.
       -- Create the entry in IO, then pass it into the STM transaction.
       newEntryInitialState <- createTokenBucketEntry (TokenBucketState (capacity - 1) now)
       
       (wasNew, entry) <- atomically $ do
         buckets <- readTVar tvBuckets
         -- Use F.Focus directly to allow STM actions in the handler, bypassing F.cases.
         (wasNewEntry, ent) <-
           StmMap.focus
             (F.Focus
                -- Handler for when the key is NOT found (the "Nothing" case)
                (pure ((True, newEntryInitialState), F.Set newEntryInitialState))
                -- Handler for when the key IS found (the "Just" case)
                (\existingEnt -> do
                  -- This handler can now perform STM actions.
                  workerLockEmpty <- isEmptyTMVar (tbeWorkerLock existingEnt)
                  if workerLockEmpty
                    then pure ((True, newEntryInitialState), F.Set newEntryInitialState)  -- Replace dead entry
                    else pure ((False, existingEnt), F.Leave)     -- Keep existing entry
                )
             )
             key buckets
         pure (wasNewEntry, ent)

       ----------------------------------------------------------------------
       -- 2. Spawn a worker once for a fresh bucket.
       if wasNew
         then
           -- For a new bucket, the first request is allowed only if there is capacity.
           if capacity > 0
             then do
               workerReadyVar <- atomically newEmptyTMVar
               atomically $ putTMVar (tbeWorkerLock entry) () -- Mark worker lock as taken
               -- Start the worker with ready synchronization
               startTokenBucketWorker (tbeState entry)
                                      (tbeQueue entry)
                                      capacity
                                      refillRate
                                      key
                                      workerReadyVar
               -- Wait for the worker to signal it's ready before proceeding
               atomically $ takeTMVar workerReadyVar
               putStrLn $ "TokenBucket: Key=" ++ T.unpack key ++ ", Allowed=True (new bucket)"
               pure True
             else do
               -- If capacity is 0, no request can ever be allowed.
               putStrLn $ "TokenBucket: Key=" ++ T.unpack key ++ ", Allowed=False (capacity is 0)"
               pure False
         else do
           -- For existing buckets, enqueue the request and wait for response
           atomically $ writeTQueue (tbeQueue entry) replyVar
           result <- takeMVar replyVar
           putStrLn $ "TokenBucket: Key=" ++ T.unpack key ++ ", Allowed=" ++ show result
           pure result
