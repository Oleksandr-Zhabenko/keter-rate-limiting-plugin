{-# LANGUAGE OverloadedStrings, TypeApplications, DataKinds #-}

module Keter.RateLimiter.SlidingWindowTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import qualified Data.Text as T
import Keter.RateLimiter.SlidingWindow
import Keter.RateLimiter.Cache
import Control.Monad.IO.Class (liftIO)

tests :: TestTree
tests = testGroup "Keter.RateLimiter.SlidingWindow Tests"
  [ testCase "allowRequest allows request under limit for IPv4" $ do
      store <- createInMemoryStore @'SlidingWindow
      let cache = newCache SlidingWindow store
      result <- allowRequest cache "192.168.1.100:user123" 5 60
      readResult <- readCacheWithZone cache "192.168.1.100" "user123" :: IO (Maybe [Int])
      assertBool "allowRequest should allow under limit" result
      assertEqual "readCacheWithZone should return one timestamp" (Just [54684]) readResult
  , testCase "allowRequest allows request under limit for IPv6" $ do
      store <- createInMemoryStore @'SlidingWindow
      let cache = newCache SlidingWindow store
      result <- allowRequest cache "2001:0db8:0000:0000:0000:0000:0000:0001:user123" 5 60
      readResult <- readCacheWithZone cache "2001:0db8:0000:0000:0000:0000:0000:0001" "user123" :: IO (Maybe [Int])
      assertBool "allowRequest should allow under limit" result
      assertEqual "readCacheWithZone should return one timestamp" (Just [54684]) readResult
  , testCase "allowRequest denies request over limit for IPv4" $ do
      store <- createInMemoryStore @'SlidingWindow
      let cache = newCache SlidingWindow store
      _ <- writeCacheWithZone cache "192.168.1.100" "user123" [53684, 54184, 54484, 54584, 54684] 60
      result <- allowRequest cache "192.168.1.100:user123" 5 60
      assertEqual "allowRequest should deny over limit" False result
  , testCase "allowRequest denies request over limit for IPv6" $ do
      store <- createInMemoryStore @'SlidingWindow
      let cache = newCache SlidingWindow store
      _ <- writeCacheWithZone cache "2001:0db8:0000:0000:0000:0000:0000:0001" "user123" [53684, 54184, 54484, 54584, 54684] 60
      result <- allowRequest cache "2001:0db8:0000:0000:0000:0000:0000:0001:user123" 5 60
      assertEqual "allowRequest should deny over limit" False result
  , testCase "allowRequest removes expired timestamps for IPv4" $ do
      store <- createInMemoryStore @'SlidingWindow
      let cache = newCache SlidingWindow store
      _ <- writeCacheWithZone cache "192.168.1.100" "user123" [50684, 51684] 60
      _ <- allowRequest cache "192.168.1.100:user123" 5 60
      readResult <- readCacheWithZone cache "192.168.1.100" "user123" :: IO (Maybe [Int])
      assertEqual "readCacheWithZone should return only current timestamp" (Just [54684]) readResult
  , testCase "allowRequest removes expired timestamps for IPv6" $ do
      store <- createInMemoryStore @'SlidingWindow
      let cache = newCache SlidingWindow store
      _ <- writeCacheWithZone cache "2001:0db8:0000:0000:0000:0000:0000:0001" "user123" [50684, 51684] 60
      _ <- allowRequest cache "2001:0db8:0000:0000:0000:0000:0000:0001:user123" 5 60
      readResult <- readCacheWithZone cache "2001:0db8:0000:0000:0000:0000:0000:0001" "user123" :: IO (Maybe [Int])
      assertEqual "readCacheWithZone should return only current timestamp" (Just [54684]) readResult
  , testCase "allowRequest handles empty cache for non-IP address" $ do
      store <- createInMemoryStore @'SlidingWindow
      let cache = newCache SlidingWindow store
      result <- allowRequest cache "unknown:user123" 5 60
      readResult <- readCacheWithZone cache "unknown" "user123" :: IO (Maybe [Int])
      assertBool "allowRequest should allow under limit" result
      assertEqual "readCacheWithZone should return one timestamp" (Just [54684]) readResult
  ]
