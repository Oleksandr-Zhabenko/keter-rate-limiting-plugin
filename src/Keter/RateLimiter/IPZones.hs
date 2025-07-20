-- Keter.RateLimiter.IPZones.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Keter.RateLimiter.IPZones
Description : Management of caches specific to IP zones for rate limiting
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : stable
Portability : portable
-}

module Keter.RateLimiter.IPZones
  ( IPZoneIdentifier
  , defaultIPZone
  , ZoneSpecificCaches(..)
  , createZoneCaches
  , resetSingleZoneCaches
  , resetZoneCache
  , newZoneSpecificCaches
  , sockAddrToIPZone 
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Keter.RateLimiter.Cache
  ( Cache(..)
  , InMemoryStore(..)
  , newCache
  , createInMemoryStore
  , cacheReset
  , Algorithm(..)
  , startCustomPurgeLeakyBucket
  )
import Network.Socket (SockAddr(..))
import Data.IP (fromHostAddress)
import Numeric (showHex)
import Data.Bits
import Control.Concurrent.STM (newTVarIO, atomically, readTVar) 
import qualified StmContainers.Map as StmMap
import Debug.Trace (traceM)

-- | Type alias representing an identifier for an IP zone, supporting IPv4 and IPv6 addresses.
type IPZoneIdentifier = Text

-- | The default IP zone identifier used when no specific zone is assigned.
defaultIPZone :: IPZoneIdentifier
defaultIPZone = "default"

-- | A collection of caches dedicated to a specific IP zone.
data ZoneSpecificCaches = ZoneSpecificCaches
  { zscCounterCache     :: Cache (InMemoryStore 'FixedWindow)
  , zscTimestampCache   :: Cache (InMemoryStore 'SlidingWindow)
  , zscTokenBucketCache :: Cache (InMemoryStore 'TokenBucket)
  , zscLeakyBucketCache :: Cache (InMemoryStore 'LeakyBucket) 
  , zscTinyLRUCache     :: Cache (InMemoryStore 'TinyLRU)
  }

-- | Create a new set of caches for a single IP zone.
createZoneCaches :: IO ZoneSpecificCaches
createZoneCaches = do
  counterStore <- createInMemoryStore @'FixedWindow
  slidingStore <- createInMemoryStore @'SlidingWindow
  tokenBucketStore <- createInMemoryStore @'TokenBucket
  leakyBucketTVar <- newTVarIO =<< atomically StmMap.new
  let leakyBucketStore = LeakyBucketStore leakyBucketTVar
  leakyBucketMap <- atomically $ readTVar leakyBucketTVar
  _ <- startCustomPurgeLeakyBucket
         leakyBucketMap
         (60 :: Integer)    -- Purge interval (every 60 seconds)
         (7200 :: Integer)  -- TTL (2 hours)
  tinyLRUStore <- createInMemoryStore @'TinyLRU
  return ZoneSpecificCaches
    { zscCounterCache     = newCache FixedWindow counterStore
    , zscTimestampCache   = newCache SlidingWindow slidingStore
    , zscTokenBucketCache = newCache TokenBucket tokenBucketStore
    , zscLeakyBucketCache = newCache LeakyBucket leakyBucketStore
    , zscTinyLRUCache     = newCache TinyLRU tinyLRUStore
    }

-- | Reset all caches within the given 'ZoneSpecificCaches'.
resetSingleZoneCaches :: ZoneSpecificCaches -> IO ()
resetSingleZoneCaches zsc = do
  cacheReset (zscCounterCache zsc)
  cacheReset (zscTimestampCache zsc)
  cacheReset (zscTokenBucketCache zsc)
  cacheReset (zscLeakyBucketCache zsc)
  cacheReset (zscTinyLRUCache zsc)

-- | Reset a single cache for a specific algorithm within the given 'ZoneSpecificCaches'.
resetZoneCache :: ZoneSpecificCaches -> Algorithm -> IO ()
resetZoneCache zsc algorithm = case algorithm of
  FixedWindow   -> cacheReset (zscCounterCache zsc)
  SlidingWindow -> cacheReset (zscTimestampCache zsc)
  TokenBucket   -> cacheReset (zscTokenBucketCache zsc)
  LeakyBucket   -> cacheReset (zscLeakyBucketCache zsc)
  TinyLRU       -> cacheReset (zscTinyLRUCache zsc)

-- | Alias for 'createZoneCaches'.
newZoneSpecificCaches :: IO ZoneSpecificCaches
newZoneSpecificCaches = createZoneCaches

-- | Convert a socket address to an IP zone identifier
sockAddrToIPZone :: SockAddr -> IO Text
sockAddrToIPZone (SockAddrInet _ hostAddr) = do
  let ip = fromHostAddress hostAddr
  traceM $ "sockAddrToIPZone IPv4: HostAddress=" ++ show hostAddr ++ ", IP=" ++ show ip
  return $ T.pack $ show ip
sockAddrToIPZone (SockAddrInet6 _ _ (w1, w2, w3, w4) _) = 
  return $ T.intercalate ":" $ map (T.pack . showHexWord) 
    [w1 `shiftR` 16, w1 .&. 0xFFFF, w2 `shiftR` 16, w2 .&. 0xFFFF, 
     w3 `shiftR` 16, w3 .&. 0xFFFF, w4 `shiftR` 16, w4 .&. 0xFFFF]
  where
    showHexWord n = let s = showHex n "" in if length s < 4 then replicate (4 - length s) '0' ++ s else s
sockAddrToIPZone _ = return "default"
