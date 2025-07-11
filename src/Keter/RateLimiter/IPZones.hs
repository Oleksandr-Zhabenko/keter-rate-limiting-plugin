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

This module provides functionality to manage and organise caches that are specific to different IP zones within the Keter rate limiting system.

== Overview

In rate limiting systems, it is often useful to segment traffic by IP zones to apply different limits or policies. This module defines a structure to hold multiple caches per IP zone, each cache corresponding to a different rate limiting strategy or data.

It offers utilities to create, reset, and manage these zone-specific caches efficiently.

== Key Concepts

- 'IPZoneIdentifier' is a type alias for a textual identifier of an IP zone, supporting both IPv4 and IPv6 addresses.
- 'defaultIPZone' is the default identifier used when no specific zone is assigned.
- 'ZoneSpecificCaches' groups together caches for different rate limiting algorithms:
  - Counter cache (FixedWindow Algorithm)
  - Timestamp cache (SlidingWindow Algorithm)
  - Token bucket cache (TokenBucket Algorithm)
  - Leaky bucket cache (LeakyBucket Algorithm)
  - TinyLRU cache (TinyLRU Algorithm)

== Functions

- 'createZoneCaches' creates a new set of caches for a single IP zone.
- 'resetSingleZoneCaches' clears all caches within a given 'ZoneSpecificCaches' instance.
- 'resetZoneCache' clears a single cache for a specific algorithm within a 'ZoneSpecificCaches' instance.
- 'newZoneSpecificCaches' is an alias for 'createZoneCaches' for convenience.

== Usage Example

@
do
  zoneCaches <- createZoneCaches
  -- Use zoneCaches for rate limiting operations in this IP zone
  -- Reset all caches:
  resetSingleZoneCaches zoneCaches
  -- Reset only the FixedWindow cache:
  resetZoneCache zoneCaches FixedWindow
@

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
import Keter.RateLimiter.Cache
  ( Cache(..)
  , InMemoryStore
  , newCache
  , createInMemoryStore
  , cacheReset
  , Algorithm(..)
  )
import Network.Socket (SockAddr(..))
import qualified Data.Text as T (pack, intercalate)
import Data.Bits
import Text.Printf (printf)

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
  timestampStore <- createInMemoryStore @'SlidingWindow
  tokenBucketStore <- createInMemoryStore @'TokenBucket
  leakyBucketStore <- createInMemoryStore @'LeakyBucket
  tinyLRUStore <- createInMemoryStore @'TinyLRU
  return ZoneSpecificCaches
    { zscCounterCache     = newCache FixedWindow counterStore
    , zscTimestampCache   = newCache SlidingWindow timestampStore
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
    let a = fromIntegral $ (hostAddr `shiftR` 24) .&. 0xFF
        b = fromIntegral $ (hostAddr `shiftR` 16) .&. 0xFF
        c = fromIntegral $ (hostAddr `shiftR` 8) .&. 0xFF
        d = fromIntegral $ hostAddr .&. 0xFF
    return $ T.pack $ show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d
sockAddrToIPZone (SockAddrInet6 _ _ (w1, w2, w3, w4) _) =
    return $ T.intercalate ":" $ map (T.pack . printf "%04x") [w1 `shiftR` 16, w1 .&. 0xFFFF, w2 `shiftR` 16, w2 .&. 0xFFFF, w3 `shiftR` 16, w3 .&. 0xFFFF, w4 `shiftR` 16, w4 .&. 0xFFFF]
sockAddrToIPZone _ = return "default"
