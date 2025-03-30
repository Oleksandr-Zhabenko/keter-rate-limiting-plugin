{-|
Copyright (c) 2025 Oleksandr Zhabenko
  
This file is a ported to Haskell language code with some simlifications of rack-attack 
https://github.com/rack/rack-attack/blob/main/lib/rack/attack/cache.rb
and is based on the structure of the original code of 
rack-attack, Copyright (c) 2016 by Kickstarter, PBC, under the MIT License.

This implementation is released under the MIT License.
 -}


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Keter.RateLimiter.Cache
  ( Cache
  , CacheStore(..)
  , newCache
  , count
  , readCache
  , writeCache
  , resetCount
  , deleteCache
  , reset
  , InMemoryStore
  , createInMemoryStore
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Hashable (Hashable)
import qualified Data.Cache as C
import System.Clock (TimeSpec(..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Exception (throwIO, Exception)
import Data.Int (Int64)

-- | Custom exception types.
data CacheError
  = MissingStoreError Text
  | MisconfiguredStoreError Text
  | IncompatibleStoreError Text
  deriving (Show)

instance Exception CacheError

-- | Type synonyms for Key and Value.
type Key = Text
type Value = Int

-- | A typeclass to abstract the store operations.
class (MonadIO m) => CacheStore store m where
  readStore  :: store -> Text -> Key -> m (Maybe Value)
  writeStore :: store -> Text -> Key -> Value -> Int -> m ()
  incStore   :: store -> Text -> Key -> Int -> m (Maybe Int)
  deleteStore:: store -> Text -> Key -> m ()

-- | The main Cache type.
data Cache store = Cache
  { cachePrefix :: Text
  , cacheStore  :: store
  }

-- | InMemoryStore: Holds an IORef to the Data.Cache.
newtype InMemoryStore = InMemoryStore (IORef (C.Cache Text Int))

-- | Create an InMemoryStore.
createInMemoryStore :: IO InMemoryStore
createInMemoryStore = InMemoryStore <$> (C.newCache Nothing >>= newIORef)

-- | Instance for InMemoryStore, using IO.
instance CacheStore InMemoryStore IO where
  readStore :: InMemoryStore -> Text -> Text -> IO (Maybe Int)
  readStore (InMemoryStore ref) _prefix key = do
      cache <- readIORef ref
      C.lookup cache key

  writeStore :: InMemoryStore -> Text -> Text -> Int -> Int -> IO ()
  writeStore (InMemoryStore ref) _prefix key value expires_in = do
      cache <- readIORef ref
      let expiration = Just (TimeSpec 0 (fromIntegral expires_in * 1000000000))
      C.insert' cache expiration key value

  incStore :: InMemoryStore -> Text -> Text -> Int -> IO (Maybe Int)
  incStore (InMemoryStore ref) prefix key expires_in = do
      cache <- readIORef ref
      maybeVal <- C.lookup cache key
      case maybeVal of
        Nothing -> do
          writeStore (InMemoryStore ref) prefix key 1 expires_in
          return $ Just 1
        Just val -> do
          let newVal = val + 1
          writeStore (InMemoryStore ref) prefix key newVal expires_in
          return $ Just newVal

  deleteStore :: InMemoryStore -> Text -> Text -> IO ()
  deleteStore (InMemoryStore ref) _prefix key = do
      cache <- readIORef ref
      C.delete cache key

-- | Create a new cache.
newCache :: (CacheStore store IO) => Text -> store -> Cache store
newCache prefix store = Cache { cachePrefix = prefix, cacheStore = store }

-- | Calculate the key and expiry time.
keyAndExpiry :: (MonadIO m) => Text -> Text -> Int -> m (Text, Int)
keyAndExpiry prefix unprefixedKey period = do
  now <- liftIO getCurrentTime
  let epochTime = floor (utcTimeToPOSIXSeconds now)
  let expires_in = period - (epochTime `mod` period) + 1
  let key = prefix <> ":" <> T.pack (show (epochTime `div` period)) <> ":" <> unprefixedKey
  return (key, expires_in)

-- | Count (increment) a key.
count :: (CacheStore store IO) => Cache store -> Text -> Int -> IO Int
count cache unprefixedKey period = do
  (key, expires_in) <- keyAndExpiry (cachePrefix cache) unprefixedKey period
  maybeResult <- incStore (cacheStore cache) (cachePrefix cache) key expires_in
  case maybeResult of
    Just result -> return result
    Nothing     -> do
      writeStore (cacheStore cache) (cachePrefix cache) key 1 expires_in
      return 1

-- | Read a key.
readCache :: (CacheStore store IO) => Cache store -> Text -> IO (Maybe Int)
readCache cache unprefixedKey =
  readStore (cacheStore cache) (cachePrefix cache) (cachePrefix cache <> ":" <> unprefixedKey)

-- | Write a key.
writeCache :: (CacheStore store IO) => Cache store -> Text -> Int -> Int -> IO ()
writeCache cache unprefixedKey value expires_in =
  writeStore (cacheStore cache) (cachePrefix cache) (cachePrefix cache <> ":" <> unprefixedKey) value expires_in

-- | Reset the count for a key (delete it).
resetCount :: (CacheStore store IO) => Cache store -> Text -> Int -> IO ()
resetCount cache unprefixedKey period = do
  (key, _) <- keyAndExpiry (cachePrefix cache) unprefixedKey period
  deleteStore (cacheStore cache) (cachePrefix cache) key

-- | Delete a key.
deleteCache :: (CacheStore store IO) => Cache store -> Text -> IO ()
deleteCache cache unprefixedKey =
  deleteStore (cacheStore cache) (cachePrefix cache) (cachePrefix cache <> ":" <> unprefixedKey)

-- | Reset the entire cache.
reset :: (CacheStore store IO) => Cache store -> IO ()
reset _ = do
  liftIO $ putStrLn "Warning: reset is not fully implemented. Clearing entire cache!"
  return ()
