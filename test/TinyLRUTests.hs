{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, OverloadedStrings, RankNTypes, TypeApplications, ScopedTypeVariables #-}

module TinyLRUTests where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent.STM
import Control.Concurrent.Async (replicateConcurrently)
import Data.Text (Text, pack)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Aeson (encode, decodeStrict)
import System.Clock (TimeSpec(..), Clock(Monotonic), getTime)
import Data.TinyLRU
import Keter.RateLimiter.Cache (Algorithm(..), Cache, InMemoryStore(..), newCache, createInMemoryStore, readCache, writeCache, deleteCache, readCacheWithZone, writeCacheWithZone, deleteCacheWithZone, cacheStore)
-- import Keter.RateLimiter.TinyLRUAlgorithm (allowRequest)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import StmContainers.Map (Map, size)
import qualified StmContainers.Map as Map
import System.Random (randomRIO)
import Data.Maybe (isJust)

-- Helper to create a TinyLRUCache in IO
createTinyLRU :: Int -> IO (TinyLRUCache s)
createTinyLRU capacity = atomically $ initTinyLRU capacity

-- Helper to create InMemoryStore 'TinyLRU
createTinyLRUStore :: IO (InMemoryStore 'Keter.RateLimiter.Cache.TinyLRU)
createTinyLRUStore = TinyLRUStore <$> (atomically $ newTVar =<< initTinyLRU 3)

-- Helper to create a Cache for TinyLRU
newTinyLRUCache :: IO (Cache (InMemoryStore 'Keter.RateLimiter.Cache.TinyLRU))
newTinyLRUCache = do
  store <- createInMemoryStore @'Keter.RateLimiter.Cache.TinyLRU
  return $ newCache Keter.RateLimiter.Cache.TinyLRU store

-- Helper to work with InMemoryStore without unwrapping
-- Instead of unwrapping, we provide a function that works with the store directly
withTinyLRUStore :: InMemoryStore 'Keter.RateLimiter.Cache.TinyLRU -> (forall s. TVar (TinyLRUCache s) -> STM a) -> STM a
withTinyLRUStore (TinyLRUStore tvarCache) action = action tvarCache

-- Placeholder for allowRequest function - replace with actual implementation
allowRequest :: Cache (InMemoryStore 'Keter.RateLimiter.Cache.TinyLRU) -> String -> Int -> Int -> IO Bool
allowRequest cache key limit period = do
  now <- getTime Monotonic
  let textKey = pack key
      store = cacheStore cache
  -- Use withTinyLRUStore instead of unwrapTinyLRUStore
  atomically $ withTinyLRUStore store $ \tvarCache -> do
    actualCache <- readTVar tvarCache
    allowRequestTinyLRU now actualCache textKey limit period

-- Helper to advance time
advanceTime :: TimeSpec -> Int -> TimeSpec
advanceTime (TimeSpec secs nsecs) seconds =
  TimeSpec (secs + fromIntegral seconds) nsecs

-- Helper to check LRU list consistency
checkListConsistency :: TinyLRUCache s -> STM ()
checkListConsistency cache = do
  list <- readTVar (lruList cache)
  cacheSize <- Map.size (lruCache cache)
  let checkNodes Nothing count = return count
      checkNodes (Just nodeRef) count = do
        node <- readTVar nodeRef
        case nodePrev node of
          Just prevRef -> do
            prev <- readTVar prevRef
            unless (nodeNext prev == Just nodeRef) $
              error "Inconsistent nodePrev -> nodeNext link"
          Nothing -> unless (lruHead list == Just nodeRef) $
            error "Head node has no prev but is not lruHead"
        checkNodes (nodeNext node) (count + 1)
  count <- checkNodes (lruHead list) 0
  unless (count == cacheSize) $
    error $ "Cache size (" ++ show cacheSize ++ ") does not match list length (" ++ show count ++ ")"
  case lruTail list of
    Just tailRef -> do
      tail <- readTVar tailRef
      unless (nodeNext tail == Nothing) $
        error "Tail node has a next node"
    Nothing -> unless (cacheSize == 0) $
      error "Empty tail with non-empty cache"

-- Helper to check consistency for wrapped cache
checkWrappedListConsistency :: Cache (InMemoryStore 'Keter.RateLimiter.Cache.TinyLRU) -> STM ()
checkWrappedListConsistency cache = do
  let store = cacheStore cache
  -- Use withTinyLRUStore instead of unwrapTinyLRUStore
  withTinyLRUStore store $ \tvarCache -> do
    actualCache <- readTVar tvarCache
    checkListConsistency actualCache

-- Helper function to create a Show instance for TVar (LRUNode s)
-- We can't show the actual TVar, so we'll show a placeholder
showTVar :: TVar a -> String
showTVar _ = "<TVar>"

-- Helper to convert Maybe (TVar (LRUNode s)) to String for assertions
showMaybeTVar :: Maybe (TVar (LRUNode s)) -> String
showMaybeTVar Nothing = "Nothing"
showMaybeTVar (Just _) = "Just <TVar>"

tests :: TestTree
tests = testGroup "TinyLRU Tests"
  [ testCase "Initialize TinyLRU" $ do
      cache <- createTinyLRU 3
      size <- atomically $ Map.size (lruCache cache)
      list <- atomically $ readTVar (lruList cache)
      assertEqual "Cache should be empty" 0 size
      -- Fixed: Use custom assertion instead of assertEqual for TVar
      let headStr = showMaybeTVar (lruHead list)
      assertEqual "Head should be Nothing" "Nothing" headStr
      let tailStr = showMaybeTVar (lruTail list)
      assertEqual "Tail should be Nothing" "Nothing" tailStr

  , testCase "Access: Cache hit and miss" $ do
      cache <- createTinyLRU 3
      now <- getTime Monotonic
      -- Cache miss
      result1 <- atomically $ access now "key1" (42 :: Int) 60 cache
      assertEqual "Cache miss returns Just value" (Just 42) result1
      size1 <- atomically $ Map.size (lruCache cache)
      assertEqual "Cache size is 1" 1 size1
      -- Cache hit
      result2 <- atomically $ access now "key1" (99 :: Int) 60 cache
      assertEqual "Cache hit returns original value" (Just 42) result2
      atomically $ checkListConsistency cache

  , testCase "Access: TTL expiration" $ do
      cache <- createTinyLRU 3
      now <- getTime Monotonic
      _ <- atomically $ access now "key1" (42 :: Int) 5 cache
      size1 <- atomically $ Map.size (lruCache cache)
      assertEqual "Cache size is 1" 1 size1
      -- Advance time beyond TTL (5 seconds)
      let now' = advanceTime now 6
      result <- atomically $ access now' "key1" (99 :: Int) 5 cache
      assertEqual "Expired key returns Just new value" (Just 99) result
      size2 <- atomically $ Map.size (lruCache cache)
      assertEqual "Cache size is 1 after expiration" 1 size2
      atomically $ checkListConsistency cache

  , testCase "Access: LRU eviction" $ do
      cache <- createTinyLRU 3
      now <- getTime Monotonic
      _ <- atomically $ access now "key1" (1 :: Int) 60 cache
      _ <- atomically $ access now "key2" (2 :: Int) 60 cache
      _ <- atomically $ access now "key3" (3 :: Int) 60 cache
      size1 <- atomically $ Map.size (lruCache cache)
      assertEqual "Cache size is 3" 3 size1
      -- Add a fourth key, should evict key1
      _ <- atomically $ access now "key4" (4 :: Int) 60 cache
      size2 <- atomically $ Map.size (lruCache cache)
      assertEqual "Cache size is 3 after eviction" 3 size2
      result <- atomically $ access now "key1" (99 :: Int) 60 cache
      assertEqual "Evicted key returns Just new value" (Just 99) result
      atomically $ checkListConsistency cache

  , testCase "Access: Invalid key" $ do
      cache <- createTinyLRU 3
      now <- getTime Monotonic
      result <- atomically $ access now "" (42 :: Int) 60 cache
      assertEqual "Empty key returns Nothing" Nothing result
      let longKey = pack $ replicate 257 'a'
      result' <- atomically $ access now longKey (42 :: Int) 60 cache
      assertEqual "Long key returns Nothing" Nothing result'
      atomically $ checkListConsistency cache

  , testCase "AllowRequestTinyLRU: Rate limiting" $ do
      cache <- createTinyLRU 3
      now <- getTime Monotonic
      allowed1 <- atomically $ allowRequestTinyLRU now cache "key1" 2 5
      assertBool "First request allowed" allowed1
      allowed2 <- atomically $ allowRequestTinyLRU now cache "key1" 2 5
      assertBool "Second request allowed" allowed2
      allowed3 <- atomically $ allowRequestTinyLRU now cache "key1" 2 5
      assertBool "Third request denied" (not allowed3)
      -- Advance time beyond period (5 seconds)
      let now' = advanceTime now 6
      allowed4 <- atomically $ allowRequestTinyLRU now' cache "key1" 2 5
      assertBool "Request allowed after period" allowed4
      atomically $ checkListConsistency cache

  , testCase "AllowRequestTinyLRU: High load" $ do
      cache <- createTinyLRU 100
      now <- getTime Monotonic
      forM_ [1..100] $ \i -> do
        let key = pack $ "key" ++ show i
        allowed <- atomically $ allowRequestTinyLRU now cache key 2 5
        assertBool ("First request for " ++ show key) allowed
      allowed <- atomically $ allowRequestTinyLRU now cache "key1" 2 5
      assertBool "Second request for key1 allowed" allowed
      allowed' <- atomically $ allowRequestTinyLRU now cache "key1" 2 5
      assertBool "Third request for key1 denied" (not allowed')
      atomically $ checkListConsistency cache

  , testCase "ResetTinyLRU" $ do
      cache <- createTinyLRU 3
      now <- getTime Monotonic
      _ <- atomically $ access now "key1" (1 :: Int) 60 cache
      _ <- atomically $ access now "key2" (2 :: Int) 60 cache
      size1 <- atomically $ Map.size (lruCache cache)
      assertEqual "Cache size is 2" 2 size1
      atomically $ resetTinyLRU cache
      size2 <- atomically $ Map.size (lruCache cache)
      assertEqual "Cache size is 0 after reset" 0 size2
      list <- atomically $ readTVar (lruList cache)
      -- Fixed: Use custom assertion instead of assertEqual for TVar
      let headStr = showMaybeTVar (lruHead list)
      assertEqual "Head is Nothing after reset" "Nothing" headStr
      let tailStr = showMaybeTVar (lruTail list)
      assertEqual "Tail is Nothing after reset" "Nothing" tailStr
      atomically $ checkListConsistency cache

  , testCase "Integration with Cache" $ do
      cache <- newTinyLRUCache
      now <- getTime Monotonic
      allowed1 <- allowRequest cache "key1" 2 5
      assertBool "First request allowed" allowed1
      allowed2 <- allowRequest cache "key1" 2 5
      assertBool "Second request allowed" allowed2
      allowed3 <- allowRequest cache "key1" 2 5
      assertBool "Third request denied" (not allowed3)
      threadDelay 6000000 -- Wait 6 seconds (> period of 5)
      now' <- getTime Monotonic
      allowed4 <- allowRequest cache "key1" 2 5
      assertBool "Request allowed after expiration" allowed4
      atomically $ checkWrappedListConsistency cache

  , testCase "Concurrent Access" $ do
      cache <- createTinyLRU 3
      now <- getTime Monotonic
      let keys = ["key1", "key2", "key3"]
      results <- replicateConcurrently 100 $ do
        key <- (keys !!) <$> randomRIO (0, 2)
        atomically $ access now key (42 :: Int) 60 cache
      let successes = length $ filter isJust results
      assertEqual "All concurrent accesses succeed" 100 successes
      size <- atomically $ Map.size (lruCache cache)
      assertEqual "Cache size is at most 3" 3 size
      atomically $ checkListConsistency cache

  , testCase "Concurrent AllowRequestTinyLRU" $ do
      cache <- createTinyLRU 3
      now <- getTime Monotonic
      results <- replicateConcurrently 100 $ atomically $ allowRequestTinyLRU now cache "key1" 10 60
      let allowed = length $ filter id results
      assertEqual "Exactly 10 requests allowed" 10 allowed
      count <- atomically $ do
        maybeNodeRef <- Map.lookup "key1" (lruCache cache)
        case maybeNodeRef of
          Just nodeRef -> do
            node <- readTVar nodeRef
            return $ maybe 0 id (decodeStrict (nodeValue node) :: Maybe Int)
          Nothing -> return 0
      assertEqual "Count is 10" 10 count
      atomically $ checkListConsistency cache

  , testCase "Concurrent Reset and Access" $ do
      cache <- createTinyLRU 3
      now <- getTime Monotonic
      results <- replicateConcurrently 50 $ do
        -- Fixed: Add explicit type annotation for randomRIO
        action <- randomRIO (0, 1 :: Int)
        if action == 0
          then atomically $ access now "key1" (42 :: Int) 60 cache
          else do
            atomically $ resetTinyLRU cache
            return Nothing
      size <- atomically $ Map.size (lruCache cache)
      assertBool "Cache size is 0 or 1" (size <= 1)
      atomically $ checkListConsistency cache

  , testCase "Access: Zero and Negative TTL" $ do
      cache <- createTinyLRU 3
      now <- getTime Monotonic
      result1 <- atomically $ access now "key1" (42 :: Int) 0 cache
      assertEqual "Zero TTL returns Just value" (Just 42) result1
      result2 <- atomically $ access now "key1" (99 :: Int) 0 cache
      assertEqual "Zero TTL hit returns original value" (Just 42) result2
      result3 <- atomically $ access now "key2" (43 :: Int) (-1) cache
      assertEqual "Negative TTL returns Just value" (Just 43) result3
      atomically $ checkListConsistency cache

  -- FIXED: Use deleteKey instead of Map.delete to properly clean up both Map and List
  , testCase "Delete Operation" $ do
      cache <- createTinyLRU 3
      now <- getTime Monotonic
      _ <- atomically $ access now "key1" (42 :: Int) 60 cache
      size1 <- atomically $ Map.size (lruCache cache)
      assertEqual "Cache size is 1" 1 size1
      -- FIXED: Use deleteKey instead of Map.delete
      atomically $ deleteKey "key1" cache
      size2 <- atomically $ Map.size (lruCache cache)
      assertEqual "Cache size is 0 after delete" 0 size2
      list <- atomically $ readTVar (lruList cache)
      -- Fixed: Use custom assertion instead of assertEqual for TVar
      let headStr = showMaybeTVar (lruHead list)
      assertEqual "Head is Nothing after delete" "Nothing" headStr
      let tailStr = showMaybeTVar (lruTail list)
      assertEqual "Tail is Nothing after delete" "Nothing" tailStr
      atomically $ checkListConsistency cache

  , testCase "Integration with Cache: Read, Write, Delete" $ do
      cache <- newTinyLRUCache
      writeCache cache "key1" (42 :: Int) 60
      result1 <- readCache cache "key1"
      assertEqual "Read returns Just 42" (Just 42) result1
      writeCache cache "key1" (99 :: Int) 60
      result2 <- readCache cache "key1"
      assertEqual "Write updates to 99" (Just 99) result2
      deleteCache cache "key1"
      result3 <- readCache cache "key1"
      assertEqual "Delete removes key" Nothing result3
      atomically $ checkWrappedListConsistency cache

  , testCase "Integration with Cache: Zone-Based Operations" $ do
      cache <- newTinyLRUCache
      writeCacheWithZone cache "192.168.1.1" "user1" (42 :: Int) 60
      result1 <- readCacheWithZone cache "192.168.1.1" "user1"
      assertEqual "Zone-based read returns Just 42" (Just 42) result1
      deleteCacheWithZone cache "192.168.1.1" "user1"
      result2 <- readCacheWithZone cache "192.168.1.1" "user1"
      assertEqual "Zone-based delete removes key" Nothing result2
      atomically $ checkWrappedListConsistency cache

  , testCase "LRU List: Single Node Eviction" $ do
      cache <- createTinyLRU 1
      now <- getTime Monotonic
      _ <- atomically $ access now "key1" (1 :: Int) 60 cache
      size1 <- atomically $ Map.size (lruCache cache)
      assertEqual "Cache size is 1" 1 size1
      _ <- atomically $ access now "key2" (2 :: Int) 60 cache
      size2 <- atomically $ Map.size (lruCache cache)
      assertEqual "Cache size is 1 after eviction" 1 size2
      result <- atomically $ access now "key1" (99 :: Int) 60 cache
      assertEqual "Evicted key returns Just new value" (Just 99) result
      atomically $ checkListConsistency cache

  , testCase "LRU List: All Expired Nodes" $ do
      cache <- createTinyLRU 3
      now <- getTime Monotonic
      _ <- atomically $ access now "key1" (1 :: Int) 5 cache
      _ <- atomically $ access now "key2" (2 :: Int) 5 cache
      let now' = advanceTime now 6
      _ <- atomically $ access now' "key3" (3 :: Int) 5 cache
      size <- atomically $ Map.size (lruCache cache)
      assertEqual "Cache size is 1 after all expire" 1 size
      result <- atomically $ access now' "key1" (99 :: Int) 5 cache
      assertEqual "Expired key returns Just new value" (Just 99) result
      atomically $ checkListConsistency cache

  , testCase "Error Handling: JSON Deserialization Failure" $ do
      cache <- createTinyLRU 3
      now <- getTime Monotonic
      nodeRef <- atomically $ addToFront now 60 cache "key1" (BS.pack [0xff, 0xff]) -- Invalid JSON
      atomically $ Map.insert nodeRef "key1" (lruCache cache)
      result <- atomically $ access now "key1" (42 :: Int) 60 cache
      assertEqual "Invalid JSON returns Just new value" (Just 42) result
      atomically $ checkListConsistency cache
  ]
