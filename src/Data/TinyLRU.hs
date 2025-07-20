{-# LANGUAGE BangPatterns, FlexibleContexts, OverloadedStrings, ScopedTypeVariables, TypeApplications, DataKinds, KindSignatures #-}

module Data.TinyLRU
  ( TinyLRUCache(..)
  , LRUList(..)
  , LRUNode(..)
  , initTinyLRU
  , access
  , updateValue
  , addToFront
  , removeNode
  , moveToFront
  , evictLRU
  , resetTinyLRU
  , allowRequestTinyLRU
  , isExpired
  , removeNodeFromCache
  , moveToFrontInCache
  , deleteKey
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

-- | LRU node
data LRUNode s = LRUNode
  { nodeKey :: !Text
  , nodeValue :: !ByteString
  , nodeExpiry :: !(Maybe TimeSpec) -- Nothing = never expire
  , nodePrev :: Maybe (TVar (LRUNode s))
  , nodeNext :: Maybe (TVar (LRUNode s))
  }

-- | LRU list (just pointers to head and tail)
data LRUList s = LRUList
  { lruHead :: Maybe (TVar (LRUNode s))
  , lruTail :: Maybe (TVar (LRUNode s))
  }

-- | The LRU cache
data TinyLRUCache s = TinyLRUCache
  { lruCache :: Map Text (TVar (LRUNode s))
  , lruList :: TVar (LRUList s)
  , lruCap :: !Int
  }

initTinyLRU :: Int -> STM (TinyLRUCache s)
initTinyLRU cap = do
  cache <- Map.new
  list <- newTVar $ LRUList Nothing Nothing
  return $ TinyLRUCache cache list cap

mkExpiry :: TimeSpec -> Int -> Maybe TimeSpec
mkExpiry now ttl | ttl <= 0 = Nothing
                 | otherwise = Just $ addTTL now ttl

isExpired :: TimeSpec -> LRUNode s -> Bool
isExpired now node =
  case nodeExpiry node of
    Nothing -> False
    Just expTime -> now >= expTime

-- This function is now atomic and safe
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

evictLRU :: TinyLRUCache s -> STM ()
evictLRU cache = do
  list <- readTVar (lruList cache)
  forM_ (lruTail list) $ \tailTVar -> do
    node <- readTVar tailTVar
    Map.delete (nodeKey node) (lruCache cache)
    removeNode (lruList cache) tailTVar

addTTL :: TimeSpec -> Int -> TimeSpec
addTTL (TimeSpec s ns) ttl = TimeSpec (s + fromIntegral (max 0 ttl)) ns

-- FIXED: This now correctly removes from both the Map and the List
deleteKey :: Text -> TinyLRUCache s -> STM ()
deleteKey key cache = do
  mNodeTVar <- Map.lookup key (lruCache cache)
  forM_ mNodeTVar $ \nodeTVar -> do
    Map.delete key (lruCache cache)
    removeNode (lruList cache) nodeTVar

cleanupExpired :: TimeSpec -> TinyLRUCache s -> STM ()
cleanupExpired now cache = do
  pairs <- ListT.toList $ Map.listT (lruCache cache)
  expired <- foldM (\acc (k, nodeRef) -> do
                      node <- readTVar nodeRef
                      if isExpired now node then return (k:acc) else return acc
                   ) [] pairs
  forM_ expired $ \k -> deleteKey k cache

-- FIXED: This now properly handles cache hits vs misses and updates values correctly
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

-- NEW: Function to update existing values (for write operations)
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

resetTinyLRU :: TinyLRUCache s -> STM ()
resetTinyLRU cache = do
  Map.reset (lruCache cache)
  writeTVar (lruList cache) $ LRUList Nothing Nothing

-- FIXED: This now properly handles expiration in rate limiting with more precise timing
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

removeNodeFromCache :: TinyLRUCache s -> TVar (LRUNode s) -> STM ()
removeNodeFromCache cache = removeNode (lruList cache)

moveToFrontInCache :: TinyLRUCache s -> TVar (LRUNode s) -> STM ()
moveToFrontInCache cache = moveToFront (lruList cache)
