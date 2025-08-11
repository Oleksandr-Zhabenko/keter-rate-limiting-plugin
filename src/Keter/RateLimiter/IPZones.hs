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

This module provides zone-based isolation for rate-limiting caches. It enables
each IP zone to maintain its own independent instances of rate-limiting
algorithms (e.g., token bucket, leaky bucket, sliding window, etc.). This
ensures multi-tenant systems can rate-limit different clients or groups in
isolation.

The primary structure here is 'ZoneSpecificCaches', which contains multiple
caches per zone. Utility functions allow dynamic creation, reset, and
lookup of caches for specific zones.

-}

module Keter.RateLimiter.IPZones
  ( -- * IP Zone Identification
    IPZoneIdentifier
  , defaultIPZone
    -- * Zone-specific Caches
  , ZoneSpecificCaches(..)
  , createZoneCaches
  , newZoneSpecificCaches
    -- * Cache Management
  , resetSingleZoneCaches
  , resetZoneCache
    -- * Address to Zone Resolution
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

--------------------------------------------------------------------------------

-- | Type alias representing an identifier for an IP zone.
--
-- This is used as a logical namespace or grouping key for assigning and isolating rate limiters.
-- Examples: @\"default\"@, @\"zone-a\"@, or @\"192.168.1.0\/24\"@.
type IPZoneIdentifier = Text

-- | The default IP zone identifier used when no specific zone is assigned.
--
-- Used as a fallback when no zone-specific routing is determined.
defaultIPZone :: IPZoneIdentifier
defaultIPZone = "default"

-- | A collection of caches dedicated to a specific IP zone.
--
-- Each cache corresponds to one of the supported rate-limiting algorithms,
-- maintained independently per zone.
data ZoneSpecificCaches = ZoneSpecificCaches
  { zscCounterCache     :: Cache (InMemoryStore 'FixedWindow)
    -- ^ Cache for Fixed Window counters.
  , zscTimestampCache   :: Cache (InMemoryStore 'SlidingWindow)
    -- ^ Cache for timestamp lists used in Sliding Window.
  , zscTokenBucketCache :: Cache (InMemoryStore 'TokenBucket)
    -- ^ Token Bucket cache.
  , zscLeakyBucketCache :: Cache (InMemoryStore 'LeakyBucket)
    -- ^ Leaky Bucket queue-based cache.
  , zscTinyLRUCache     :: Cache (InMemoryStore 'TinyLRU)
    -- ^ Optional auxiliary LRU cache.
  }

-- | Create a new set of caches for a single IP zone.
--
-- Each algorithm receives its own store. For 'LeakyBucket', a background
-- cleanup thread is also started to remove inactive entries periodically.
--
-- The cleanup thread runs every 60 seconds and removes entries older than 2 hours.
--
-- ==== __Examples__
--
-- @
-- zoneCaches <- createZoneCaches
-- cacheReset (zscTokenBucketCache zoneCaches)
-- @
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

-- | Alias for 'createZoneCaches'.
--
-- Useful for more readable builder-based usage or factory patterns.
newZoneSpecificCaches :: IO ZoneSpecificCaches
newZoneSpecificCaches = createZoneCaches

-- | Reset all caches within the given 'ZoneSpecificCaches'.
--
-- Clears all internal state, including token counts, timestamps, and queues.
--
-- ==== __Examples__
--
-- @
-- resetSingleZoneCaches zoneCaches
-- @
resetSingleZoneCaches :: ZoneSpecificCaches -> IO ()
resetSingleZoneCaches zsc = do
  cacheReset (zscCounterCache zsc)
  cacheReset (zscTimestampCache zsc)
  cacheReset (zscTokenBucketCache zsc)
  cacheReset (zscLeakyBucketCache zsc)
  cacheReset (zscTinyLRUCache zsc)

-- | Reset a single cache for a specific algorithm within the given 'ZoneSpecificCaches'.
--
-- This is useful when only one type of rate limiter needs a reset.
--
-- ==== __Examples__
--
-- @
-- resetZoneCache zoneCaches TokenBucket
-- @
resetZoneCache :: ZoneSpecificCaches -> Algorithm -> IO ()
resetZoneCache zsc algorithm = case algorithm of
  FixedWindow   -> cacheReset (zscCounterCache zsc)
  SlidingWindow -> cacheReset (zscTimestampCache zsc)
  TokenBucket   -> cacheReset (zscTokenBucketCache zsc)
  LeakyBucket   -> cacheReset (zscLeakyBucketCache zsc)
  TinyLRU       -> cacheReset (zscTinyLRUCache zsc)

-- | Convert a socket address into an IP zone identifier.
--
-- IPv4 addresses are rendered using 'fromHostAddress'. IPv6 addresses are
-- expanded and zero-padded for consistency. Any unknown or unsupported
-- address formats fall back to 'defaultIPZone'.
--
-- ==== __Examples__
--
-- @
-- zone <- sockAddrToIPZone (SockAddrInet 80 0x7f000001)
-- print zone  -- \"127.0.0.1\"
-- @
sockAddrToIPZone :: SockAddr -> IO Text
sockAddrToIPZone (SockAddrInet _ hostAddr) = do
  let ip = fromHostAddress hostAddr
  return $ T.pack $ show ip
sockAddrToIPZone (SockAddrInet6 _ _ (w1, w2, w3, w4) _) = 
  return $ T.intercalate ":" $ map (T.pack . showHexWord) 
    [w1 `shiftR` 16, w1 .&. 0xFFFF, w2 `shiftR` 16, w2 .&. 0xFFFF, 
     w3 `shiftR` 16, w3 .&. 0xFFFF, w4 `shiftR` 16, w4 .&. 0xFFFF]
  where
    showHexWord n = let s = showHex n "" in if length s < 4 then replicate (4 - length s) '0' ++ s else s
sockAddrToIPZone _ = return defaultIPZone