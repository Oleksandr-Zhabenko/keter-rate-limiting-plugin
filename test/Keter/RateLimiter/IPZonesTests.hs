{-# LANGUAGE OverloadedStrings, TypeApplications, DataKinds #-}

module Keter.RateLimiter.IPZonesTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Network.Socket (SockAddr(..), tupleToHostAddress)
import Keter.RateLimiter.IPZones
import Keter.RateLimiter.Cache
import Keter.RateLimiter.CacheWithZone
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)

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
      _ <- incStoreWithZone cache "192.168.1.100" "user123" 60
      result <- readCacheWithZone cache "192.168.1.100" "user123" :: IO (Maybe Int)
      assertEqual "readCacheWithZone should return Just 1" (Just 1) result
  , testCase "incStoreWithZone with IPv6 address" $ do
      store <- createInMemoryStore @'FixedWindow
      let cache = newCache FixedWindow store
      _ <- incStoreWithZone cache "2001:0db8:0000:0000:0000:0000:0000:0001" "user123" 60
      result <- readCacheWithZone cache "2001:0db8:0000:0000:0000:0000:0000:0001" "user123" :: IO (Maybe Int)
      assertEqual "readCacheWithZone should return Just 1" (Just 1) result
  , testCase "incStoreWithZone increments existing counter" $ do
      store <- createInMemoryStore @'FixedWindow
      let cache = newCache FixedWindow store
      _ <- incStoreWithZone cache "192.168.1.100" "user123" 60
      result <- incStoreWithZone cache "192.168.1.100" "user123" 60
      assertEqual "incStoreWithZone should increment to 2" 2 result
  , testCase "resetSingleZoneCaches clears all caches" $ do
      zoneCaches <- createZoneCaches
      _ <- incStoreWithZone (zscCounterCache zoneCaches) "192.168.1.100" "user123" 60
      _ <- resetSingleZoneCaches zoneCaches
      result <- readCacheWithZone (zscCounterCache zoneCaches) "192.168.1.100" "user123" :: IO (Maybe Int)
      assertEqual "Should not retrieve counter after reset" Nothing result
  , testCase "incStoreWithZone with expired entry" $ do
      store <- createInMemoryStore @'FixedWindow
      let cache = newCache FixedWindow store
      _ <- incStoreWithZone cache "192.168.1.100" "user123" 1
      liftIO $ threadDelay (2 * 1000000) -- Wait for TTL to expire
      _ <- incStoreWithZone cache "192.168.1.100" "user123" 60
      result <- readCacheWithZone cache "192.168.1.100" "user123" :: IO (Maybe Int)
      assertEqual "Should retrieve new counter after expiration" (Just 1) result
  ]
