{-# LANGUAGE OverloadedStrings, TypeApplications, DataKinds #-}

{-|
Module      : Keter.RateLimiter.IPZonesTests
Description : Tests for IP-based zoning and cache management.
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : experimental
Portability : POSIX

This module contains tests for the IP zone functionality within the rate-limiting system.
IP zones are used to segment the cache, allowing for different rate-limiting rules or counters
to be applied based on the client's IP address.

The tests verify:
  * Correct conversion of 'SockAddr' (both IPv4 and IPv6) to a textual IP zone identifier.
  * Graceful handling of non-IP 'SockAddr' types (e.g., 'SockAddrUnix').
  * The functionality of incrementing and reading counters within a specific IP zone.
  * The ability to reset all caches, effectively clearing all rate-limiting state.
  * Correct expiration of cache entries based on their TTL (Time To Live).
-}
module Keter.RateLimiter.IPZonesTests (
    -- * Test Suite
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit
import Network.Socket (SockAddr(..), tupleToHostAddress)
import Keter.RateLimiter.IPZones
import Keter.RateLimiter.Cache
import Keter.RateLimiter.CacheWithZone
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)

-- | The main test suite for IP zone functionality.
-- It groups together all the individual test cases in this module.
tests :: TestTree
tests = testGroup "Keter.RateLimiter.IPZones Tests"
  [ testCase "sockAddrToIPZone produces correct IPv4 address" $ do
      let addr = SockAddrInet 0 (tupleToHostAddress (192, 168, 1, 100))
      result <- sockAddrToIPZone addr
      assertEqual "IPv4 address should be correct" "192.168.1.100" result

  , testCase "sockAddrToIPZone produces correct IPv6 address" $ do
      let addr = SockAddrInet6 0 0 (0, 0, 0, 1) 0
      result <- sockAddrToIPZone addr
      assertEqual "IPv6 address should be correct" "0000:0000:0000:0000:0000:0000:0000:0001" result

  , testCase "sockAddrToIPZone handles non-IP address" $ do
      let addr = SockAddrUnix "/tmp/socket"
      result <- sockAddrToIPZone addr
      assertEqual "Non-IP address should return default" "default" result

  , testCase "incStoreWithZone with IPv4 address" $ do
      store <- createInMemoryStore @'FixedWindow
      let cache = newCache FixedWindow store
      _ <- incStoreWithZone cache "test-throttle" "192.168.1.100" "user123" 60
      result <- readCacheWithZone cache "test-throttle" "192.168.1.100" "user123" :: IO (Maybe Int)
      assertEqual "readCacheWithZone should return Just 1" (Just 1) result

  , testCase "incStoreWithZone with IPv6 address" $ do
      store <- createInMemoryStore @'FixedWindow
      let cache = newCache FixedWindow store
      _ <- incStoreWithZone cache "test-throttle" "2001:0db8:0000:0000:0000:0000:0000:0001" "user123" 60
      result <- readCacheWithZone cache "test-throttle" "2001:0db8:0000:0000:0000:0000:0000:0001" "user123" :: IO (Maybe Int)
      assertEqual "readCacheWithZone should return Just 1" (Just 1) result

  , testCase "incStoreWithZone increments existing counter" $ do
      store <- createInMemoryStore @'FixedWindow
      let cache = newCache FixedWindow store
      _ <- incStoreWithZone cache "test-throttle" "192.168.1.100" "user123" 60
      result <- incStoreWithZone cache "test-throttle" "192.168.1.100" "user123" 60
      assertEqual "incStoreWithZone should increment to 2" 2 result

  , testCase "resetSingleZoneCaches clears all caches" $ do
      zoneCaches <- createZoneCaches
      _ <- incStoreWithZone (zscCounterCache zoneCaches) "test-throttle" "192.168.1.100" "user123" 60
      _ <- resetSingleZoneCaches zoneCaches
      result <- readCacheWithZone (zscCounterCache zoneCaches) "test-throttle" "192.168.1.100" "user123" :: IO (Maybe Int)
      assertEqual "Should not retrieve counter after reset" Nothing result

  , testCase "incStoreWithZone with expired entry" $ do
      store <- createInMemoryStore @'FixedWindow
      let cache = newCache FixedWindow store
      -- Set a short TTL of 1 second.
      _ <- incStoreWithZone cache "test-throttle" "192.168.1.100" "user123" 1
      -- Wait for the entry to expire.
      liftIO $ threadDelay (2 * 1000000) -- Wait for 2 seconds
      -- Increment again, which should create a new entry.
      _ <- incStoreWithZone cache "test-throttle" "192.168.1.100" "user123" 60
      result <- readCacheWithZone cache "test-throttle" "192.168.1.100" "user123" :: IO (Maybe Int)
      assertEqual "Should retrieve new counter after expiration" (Just 1) result
  ]
