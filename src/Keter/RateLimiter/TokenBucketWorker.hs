-- src/Keter/RateLimiter/TokenBucketWorker.hs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Keter.RateLimiter.TokenBucketWorker (startTokenBucketWorker) where

import Control.Concurrent.STM
import Control.Monad (void, forever)
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Keter.RateLimiter.Types (TokenBucketState(..))
import Control.Concurrent.MVar (MVar, putMVar)

-- | The worker thread for a single token bucket.
-- It processes requests from a queue, updating the bucket's state atomically.
startTokenBucketWorker
  :: TVar TokenBucketState
  -> TQueue (MVar Bool)
  -> Int       -- ^ Capacity
  -> Double    -- ^ Refill rate (tokens per second)
  -> Text      -- ^ Key for logging
  -> TMVar ()  -- ^ Signal when worker is ready
  -> IO ()
startTokenBucketWorker stateVar queue capacity refillRate fullKey readyVar = void . forkIO $ do
  -- Signal that the worker is ready
  atomically $ putTMVar readyVar ()
  
  forever $ do
    -- Wait for a request to arrive in the queue
    replyVar <- atomically $ readTQueue queue
    now <- liftIO $ floor <$> getPOSIXTime

    -- Atomically process the request: read state, calculate new tokens,
    -- consume a token if available, and write the new state back.
    allowed <- atomically $ do
      TokenBucketState { tokens, lastUpdate } <- readTVar stateVar
      
      let elapsed = fromIntegral (now - lastUpdate)
          refilled = elapsed * refillRate
          -- Add refilled tokens, but don't exceed the capacity
          currentTokens = min (fromIntegral capacity) (fromIntegral tokens + refilled)

      if currentTokens >= 1
        then do
          -- Request is allowed. Consume one token and update the timestamp.
          let newTokens = currentTokens - 1
          writeTVar stateVar (TokenBucketState (floor newTokens) now)
          return True
        else do
          -- Request is denied. Don't consume a token, but update the timestamp
          -- to ensure the next refill calculation is correct.
          writeTVar stateVar (TokenBucketState (floor currentTokens) now)
          return False

    -- Log the result and send the response back to the waiting client.
    -- Note: The token count for logging is not available here without another read,
    -- but the 'Allowed' status is the critical piece of information.
    liftIO $ putStrLn $ "TokenBucket Worker: Key=" ++ T.unpack fullKey ++ ", Allowed=" ++ show allowed
    liftIO $ putMVar replyVar allowed
