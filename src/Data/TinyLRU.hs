{-# LANGUAGE BangPatterns, FlexibleContexts, OverloadedStrings, ScopedTypeVariables, TypeApplications, DataKinds, KindSignatures #-}

-- |
-- Module      : Data.TinyLRU
-- Description : A lightweight, thread-safe, in-memory LRU cache.
-- Copyright   : (c) 2025 Oleksandr Zhabenko
-- License     : MIT
-- Maintainer  : oleksandr.zhabenko@yahoo.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module provides a simple, thread-safe, and efficient Least Recently Used (LRU)
-- cache. It is built using Software Transactional Memory (STM), making it suitable
-- for concurrent applications.
--
-- The cache supports:
--
-- *  A fixed capacity, with automatic eviction of the least recently used item.
-- *  Time-based expiration (TTL) for each entry.
-- *  Generic key-value storage, with values being 'ToJSON'/'FromJSON' serializable.
-- *  Atomic operations for safe concurrent access.
--
-- A typical use case involves creating a cache, and then using 'access' or
-- 'updateValue' within an 'atomically' block to interact with it.
--
-- == Example Usage
--
-- @
-- import Control.Concurrent.STM
-- import Data.TinyLRU
-- import Data.Text (Text)
-- import System.Clock
--
-- main :: IO ()
-- main = do
--   -- Initialize a cache with a capacity of 100 items.
--   lru <- atomically $ initTinyLRU 100
--
--   -- Add or retrieve a value.
--   let key1 = "my-key" :: Text
--   let value1 = "my-value" :: Text
--   let ttlSeconds = 3600 -- 1 hour
--
--   -- 'access' is a get-or-insert operation.
--   -- On first run, it inserts 'value1' and returns it.
--   -- On subsequent runs, it returns the existing value.
--   now <- getTime Monotonic
--   retrievedValue <- atomically $ access now key1 value1 ttlSeconds lru
--   print retrievedValue -- Should print: Just "my-value"
--
--   -- Explicitly update a value.
--   let newValue = "a-new-value" :: Text
--   updatedValue <- atomically $ updateValue now key1 newValue ttlSeconds lru
--   print updatedValue -- Should print: Just "a-new-value"
--
--   -- Use the cache for rate limiting.
--   let rateLimitKey = "user:123:login-attempts"
--   -- Allow 5 requests per 60 seconds.
--   isAllowed <- atomically $ allowRequestTinyLRU now lru rateLimitKey 5 60
--   if isAllowed
--     then putStrLn "Request allowed."
--     else putStrLn "Rate limit exceeded."
-- @
--

module Data.TinyLRU
  ( -- * Cache Type
    TinyLRUCache(..)
    -- * Core API
  , initTinyLRU
  , access
  , updateValue
  , deleteKey
  , resetTinyLRU
    -- * Rate Limiting
  , allowRequestTinyLRU
    -- * Internals
    -- | These are lower-level components and functions. Most users will not need them directly.
  , LRUList(..)
  , LRUNode(..)
  , isExpired
  , addToFront
  , removeNode
  , moveToFront
  , evictLRU
  , removeNodeFromCache
  , moveToFrontInCache
  ) where

import Control.Concurrent.STM
import StmContainers.Map (Map)
import qualified StmContainers.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (FromJSON, ToJSON, encode, decodeStrict)
import System.Clock (TimeSpec(..))
import Data.Maybe (isNothing)
import Control.Monad (when, forM_, foldM)
import qualified ListT

-- | Represents a single node in the LRU cache's doubly-linked list.
-- Each node contains the key, value, expiry time, and pointers to the
-- previous and next nodes.
data LRUNode s = LRUNode
  { nodeKey    :: !Text
    -- ^ The key associated with this cache entry.
  , nodeValue  :: !ByteString
    -- ^ The value, stored as a 'ByteString' after JSON encoding.
  , nodeExpiry :: !(Maybe TimeSpec)
    -- ^ The absolute expiration time. 'Nothing' means the entry never expires.
  , nodePrev   :: !(Maybe (TVar (LRUNode s)))
    -- ^ A transactional variable pointing to the previous node in the list. 'Nothing' if this is the head.
  , nodeNext   :: !(Maybe (TVar (LRUNode s)))
    -- ^ A transactional variable pointing to the next node in the list. 'Nothing' if this is the tail.
  }

-- | Represents the doubly-linked list used to track the LRU order.
-- It only stores pointers to the head (most recently used) and
-- tail (least recently used) of the list.
data LRUList s = LRUList
  { lruHead :: !(Maybe (TVar (LRUNode s)))
    -- ^ A pointer to the most recently used node.
  , lruTail :: !(Maybe (TVar (LRUNode s)))
    -- ^ A pointer to the least recently used node.
  }

-- | The main data structure for the LRU cache.
-- This is the handle you will use for all cache operations.
data TinyLRUCache s = TinyLRUCache
  { lruCache :: !(Map Text (TVar (LRUNode s)))
    -- ^ A transactional hash map for O(1) average time complexity lookups.
    -- Maps keys to their corresponding 'LRUNode's in the list.
  , lruList  :: !(TVar (LRUList s))
    -- ^ A transactional variable holding the 'LRUList', which manages the usage order.
  , lruCap   :: !Int
    -- ^ The maximum number of items the cache can hold.
  }

-- | Initializes a new 'TinyLRUCache' with a specified capacity.
-- This function must be run within an 'STM' transaction.
--
-- @
-- lruCache <- atomically $ initTinyLRU 1000
-- @
--
-- @param cap The maximum number of items the cache can hold. Must be > 0.
-- @return An 'STM' action that yields a new, empty 'TinyLRUCache'.
initTinyLRU :: Int -> STM (TinyLRUCache s)
initTinyLRU cap = do
  cache <- Map.new
  list <- newTVar $ LRUList Nothing Nothing
  return $ TinyLRUCache cache list cap

-- | Helper function to calculate the expiry 'TimeSpec' from a TTL in seconds.
mkExpiry :: TimeSpec -> Int -> Maybe TimeSpec
mkExpiry now ttl | ttl <= 0 = Nothing
                 | otherwise = Just $ addTTL now ttl

-- | Checks if a cache node is expired relative to the current time.
--
-- @param now The current 'TimeSpec'.
-- @param node The 'LRUNode' to check.
-- @return 'True' if the node is expired, 'False' otherwise.
isExpired :: TimeSpec -> LRUNode s -> Bool
isExpired now node =
  case nodeExpiry node of
    Nothing -> False
    Just expTime -> now >= expTime

-- | Atomically removes a node from the doubly-linked list.
-- It correctly updates the 'nodePrev' and 'nodeNext' pointers of the
-- neighboring nodes and the list's 'lruHead' and 'lruTail' pointers if necessary.
--
-- @param listTVar The 'TVar' of the 'LRUList' from which to remove the node.
-- @param nodeTVar The 'TVar' of the 'LRUNode' to remove.
removeNode :: TVar (LRUList s) -> TVar (LRUNode s) -> STM ()
removeNode listTVar nodeTVar = do
  node <- readTVar nodeTVar
  let mPrev = nodePrev node
      mNext = nodeNext node

  -- Link neighbour nodes to each other
  forM_ mPrev $ \pRef -> modifyTVar' pRef (\p -> p { nodeNext = mNext })
  forM_ mNext $ \nRef -> modifyTVar' nRef (\n -> n { nodePrev = mPrev })

  -- Atomically update the list's head and tail pointers
  modifyTVar' listTVar $ \list ->
    let newHead = if lruHead list == Just nodeTVar then mNext else lruHead list
        newTail = if lruTail list == Just nodeTVar then mPrev else lruTail list
    in list { lruHead = newHead, lruTail = newTail }

-- | Creates a new node and adds it to the front (most recently used position)
-- of the cache's linked list.
--
-- @param now The current 'TimeSpec', used for calculating expiry.
-- @param ttl The time-to-live in seconds. A value `<= 0` means it never expires.
-- @param cache The 'TinyLRUCache' instance.
-- @param key The key for the new entry.
-- @param value The 'ByteString' value for the new entry.
-- @return The 'TVar' of the newly created 'LRUNode'.
addToFront :: TimeSpec -> Int -> TinyLRUCache s -> Text -> ByteString -> STM (TVar (LRUNode s))
addToFront now ttl cache key value = do
  let expiry = mkExpiry now ttl
  list <- readTVar (lruList cache)
  nodeTVar <- newTVar $ LRUNode key value expiry Nothing (lruHead list)
  
  forM_ (lruHead list) $ \oldHeadTVar -> do
    oldHead <- readTVar oldHeadTVar
    writeTVar oldHeadTVar oldHead { nodePrev = Just nodeTVar }

  let newTail = if isNothing (lruTail list) then Just nodeTVar else lruTail list
  writeTVar (lruList cache) (LRUList (Just nodeTVar) newTail)
  
  return nodeTVar

-- | Moves an existing node to the front of the linked list, marking it as the
-- most recently used. This is a core operation for the LRU logic.
--
-- @param listTVar The 'TVar' of the 'LRUList'.
-- @param nodeTVar The 'TVar' of the 'LRUNode' to move.
moveToFront :: TVar (LRUList s) -> TVar (LRUNode s) -> STM ()
moveToFront listTVar nodeTVar = do
  list <- readTVar listTVar
  -- Only move if it's not already the head
  when (lruHead list /= Just nodeTVar) $ do
    removeNode listTVar nodeTVar
    node <- readTVar nodeTVar
    list' <- readTVar listTVar
    let mOldHead = lruHead list'
    writeTVar nodeTVar node { nodePrev = Nothing, nodeNext = mOldHead }
    
    forM_ mOldHead $ \oldHeadTVar ->
      modifyTVar' oldHeadTVar (\h -> h { nodePrev = Just nodeTVar })
    
    let newTail = if isNothing (lruTail list') then Just nodeTVar else lruTail list'
    writeTVar listTVar (LRUList (Just nodeTVar) newTail)

-- | Evicts the least recently used item from the cache. This involves removing
-- the tail of the linked list and deleting the corresponding entry from the hash map.
--
-- @param cache The 'TinyLRUCache' to perform eviction on.
evictLRU :: TinyLRUCache s -> STM ()
evictLRU cache = do
  list <- readTVar (lruList cache)
  forM_ (lruTail list) $ \tailTVar -> do
    node <- readTVar tailTVar
    Map.delete (nodeKey node) (lruCache cache)
    removeNode (lruList cache) tailTVar

-- | Helper to add a TTL in seconds to a 'TimeSpec'.
addTTL :: TimeSpec -> Int -> TimeSpec
addTTL (TimeSpec s ns) ttl = TimeSpec (s + fromIntegral (max 0 ttl)) ns

-- | Deletes an entry from the cache by its key.
-- This removes the item from both the internal map and the linked list.
--
-- @param key The key of the item to delete.
-- @param cache The 'TinyLRUCache' instance.
deleteKey :: Text -> TinyLRUCache s -> STM ()
deleteKey key cache = do
  mNodeTVar <- Map.lookup key (lruCache cache)
  forM_ mNodeTVar $ \nodeTVar -> do
    Map.delete key (lruCache cache)
    removeNode (lruList cache) nodeTVar

-- | Scans the cache and removes all expired items.
-- This is called automatically by 'access', 'updateValue', and 'allowRequestTinyLRU'.
cleanupExpired :: TimeSpec -> TinyLRUCache s -> STM ()
cleanupExpired now cache = do
  pairs <- ListT.toList $ Map.listT (lruCache cache)
  expired <- foldM (\acc (k, nodeRef) -> do
                      node <- readTVar nodeRef
                      if isExpired now node then return (k:acc) else return acc
                   ) [] pairs
  forM_ expired $ \k -> deleteKey k cache

-- | Accesses a cache entry. This is the primary "get-or-insert" function.
--
-- The logic is as follows:
--
-- 1.  It first cleans up any expired items in the cache.
-- 2.  It looks for the key.
-- 3.  If the key exists and is not expired, it moves the item to the front (as it's now
--     the most recently used) and returns its value wrapped in 'Just'.
-- 4.  If the key does not exist, or if it exists but has expired, it inserts the
--     provided new value. If the cache is full, it evicts the least recently used
--     item before insertion. It then returns the new value wrapped in 'Just'.
-- 5.  If the key is invalid (empty or too long), it returns 'Nothing'.
--
-- @param now The current 'TimeSpec', for expiry checks.
-- @param key The key to look up. Length must be between 1 and 256.
-- @param val The value to insert if the key is not found. It must have 'ToJSON' and 'FromJSON' instances.
-- @param ttl The time-to-live in seconds for the new entry if it's inserted.
-- @param cache The 'TinyLRUCache' instance.
-- @return An 'STM' action that yields 'Just' the value (either existing or newly inserted), or 'Nothing' if the key is invalid.
access :: forall a s. (FromJSON a, ToJSON a) => TimeSpec -> Text -> a -> Int -> TinyLRUCache s -> STM (Maybe a)
access now key val ttl cache
  | T.null key || T.length key > 256 = return Nothing
  | otherwise = do
      cleanupExpired now cache
      mNodeTVar <- Map.lookup key (lruCache cache)
      case mNodeTVar of
        Just nodeTVar -> do
          -- Key exists - check if expired
          node <- readTVar nodeTVar
          if isExpired now node then do
            -- Expired: delete and re-insert with new value
            deleteKey key cache
            insertNew
          else do
            -- Not expired: move to front and return existing value (cache hit)
            moveToFront (lruList cache) nodeTVar
            case decodeStrict (nodeValue node) :: Maybe a of
              Just existingVal -> return (Just existingVal)
              Nothing -> do
                -- Corrupt data, replace it
                deleteKey key cache
                insertNew
        Nothing -> insertNew
  where
    insertNew = do
      sz <- Map.size (lruCache cache)
      when (sz >= lruCap cache) $ evictLRU cache
      nodeTVar <- addToFront now ttl cache key (BL.toStrict (encode val))
      Map.insert nodeTVar key (lruCache cache)
      return (Just val)

-- | Updates or inserts a cache entry. This is the primary "write" or "upsert" function.
--
-- The logic is as follows:
--
-- 1.  It first cleans up any expired items in the cache.
-- 2.  It looks for the key.
-- 3.  If the key exists, it updates the value and expiry time, and moves it to the front.
-- 4.  If the key does not exist, it inserts the new value. If the cache is full, it
--     evicts the least recently used item first.
-- 5.  If the key is invalid (empty or too long), it returns 'Nothing'.
--
-- Unlike 'access', this function /always/ writes the provided value.
--
-- @param now The current 'TimeSpec', for expiry calculations.
-- @param key The key to update or insert. Length must be between 1 and 256.
-- @param val The new value to write. It must have 'ToJSON' and 'FromJSON' instances.
-- @param ttl The new time-to-live in seconds for the entry.
-- @param cache The 'TinyLRUCache' instance.
-- @return An 'STM' action that yields 'Just' the value that was written, or 'Nothing' if the key is invalid.
updateValue :: forall a s. (FromJSON a, ToJSON a) => TimeSpec -> Text -> a -> Int -> TinyLRUCache s -> STM (Maybe a)
updateValue now key val ttl cache
  | T.null key || T.length key > 256 = return Nothing
  | otherwise = do
      cleanupExpired now cache
      mNodeTVar <- Map.lookup key (lruCache cache)
      case mNodeTVar of
        Just nodeTVar -> do
          -- Key exists - update the value regardless of expiration
          node <- readTVar nodeTVar
          let newExpiry = mkExpiry now ttl
          writeTVar nodeTVar node { 
            nodeValue = BL.toStrict (encode val),
            nodeExpiry = newExpiry
          }
          moveToFront (lruList cache) nodeTVar
          return (Just val)
        Nothing -> do
          -- Key doesn't exist - insert new
          sz <- Map.size (lruCache cache)
          when (sz >= lruCap cache) $ evictLRU cache
          nodeTVar <- addToFront now ttl cache key (BL.toStrict (encode val))
          Map.insert nodeTVar key (lruCache cache)
          return (Just val)

-- | Resets the cache, removing all entries.
--
-- @param cache The 'TinyLRUCache' to reset.
resetTinyLRU :: TinyLRUCache s -> STM ()
resetTinyLRU cache = do
  Map.reset (lruCache cache)
  writeTVar (lruList cache) $ LRUList Nothing Nothing

-- | A specialized function for rate limiting. It uses the cache to track the number
-- of requests for a given key within a specific time period.
--
-- The logic is as follows:
--
-- 1.  It looks for the key. The value associated with the key is treated as a counter ('Int').
-- 2.  If the entry for the key exists and is not expired:
--     *   If the counter is less than the 'limit', it increments the counter,
--         refreshes the expiry time, and returns 'True' (request allowed).
--     *   If the counter has reached the 'limit', it does nothing and returns 'False' (request denied).
-- 3.  If the entry does not exist or has expired, it creates a new entry with the
--     counter set to 1 and returns 'True'.
--
-- @param now The current 'TimeSpec'.
-- @param cache The 'TinyLRUCache' instance.
-- @param key The key to identify the entity being rate-limited (e.g., a user ID or IP address).
-- @param limit The maximum number of requests allowed.
-- @param period The time period in seconds for the rate limit window.
-- @return An 'STM' action yielding 'True' if the request is allowed, 'False' otherwise.
allowRequestTinyLRU :: TimeSpec -> TinyLRUCache s -> Text -> Int -> Int -> STM Bool
allowRequestTinyLRU now cache key limit period
  | T.null key = return False
  | otherwise = do
      cleanupExpired now cache
      mNodeTVar <- Map.lookup key (lruCache cache)
      case mNodeTVar of
        Just nodeTVar -> do
          node <- readTVar nodeTVar
          if isExpired now node then do
            -- Expired: delete old entry and create new one
            deleteKey key cache
            insertNew
          else do
            -- Not expired: check and update count
            case decodeStrict (nodeValue node) :: Maybe Int of
              Just n
                | n < limit -> do
                    -- Update the value and refresh expiry time
                    let newExpiry = mkExpiry now period
                    writeTVar nodeTVar node { 
                      nodeValue = BL.toStrict (encode (n+1)),
                      nodeExpiry = newExpiry
                    }
                    moveToFront (lruList cache) nodeTVar
                    return True
                | otherwise -> do
                    moveToFront (lruList cache) nodeTVar
                    return False -- Over limit
              _ -> do
                -- Corrupt data, reset it
                deleteKey key cache
                insertNew
        Nothing -> insertNew
  where
    insertNew = do
      sz <- Map.size (lruCache cache)
      when (sz >= lruCap cache) $ evictLRU cache
      nodeTVar <- addToFront now period cache key (BL.toStrict (encode (1 :: Int)))
      Map.insert nodeTVar key (lruCache cache)
      return True

-- | Low-level function to remove a node from the cache's list.
-- Alias for 'removeNode'. Most users should use 'deleteKey' instead.
--
-- @param cache The 'TinyLRUCache' instance.
-- @param nodeTVar The 'TVar' of the 'LRUNode' to remove.
removeNodeFromCache :: TinyLRUCache s -> TVar (LRUNode s) -> STM ()
removeNodeFromCache cache = removeNode (lruList cache)

-- | Low-level function to move a node to the front of the cache's list.
-- Alias for 'moveToFront'. Most users should use 'access' or 'updateValue' which
-- call this internally.
--
-- @param cache The 'TinyLRUCache' instance.
-- @param nodeTVar The 'TVar' of the 'LRUNode' to move.
moveToFrontInCache :: TinyLRUCache s -> TVar (LRUNode s) -> STM ()
moveToFrontInCache cache = moveToFront (lruList cache)
