{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Keter.RateLimiter.LeakyBucketStateTests
Description : Tests for the LeakyBucketState data type and its JSON handling
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : experimental
Portability : POSIX

This module contains unit tests for the 'LeakyBucketState' data type, including
its construction, equality, JSON serialization/deserialization, and validation logic.
-}
module Keter.RateLimiter.LeakyBucketStateTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Keter.RateLimiter.Types (LeakyBucketState(..))

-- | Test suite for 'LeakyBucketState' including JSON behavior and validation.
--
-- > defaultMain Keter.RateLimiter.LeakyBucketStateTests.tests
tests :: TestTree
tests = testGroup "Keter.RateLimiter.LeakyBucketState Tests"
  [ testCase "Construct and compare LeakyBucketState" $ do
      let state = LeakyBucketState { level = 2.5, lastTime = 1625073600 }
      state @?= LeakyBucketState { level = 2.5, lastTime = 1625073600 }
      level state @?= 2.5
      lastTime state @?= 1625073600

  , testCase "JSON serialization and deserialization" $ do
      let state = LeakyBucketState { level = 2.5, lastTime = 1625073600 }
      let json = encode state
      case decode json :: Maybe LeakyBucketState of
        Nothing -> assertFailure "JSON deserialization failed"
        Just decoded -> decoded @?= state

  , testCase "Invalid JSON: negative level" $ do
      let invalidJson = LBS.pack "{\"level\":-1.0,\"lastTime\":1625073600}"
      case decode invalidJson :: Maybe LeakyBucketState of
        Just _ -> assertFailure "Should fail to deserialize negative level"
        Nothing -> return () -- Expected failure

  , testCase "Invalid JSON: level exceeds maximum" $ do
      let invalidJson = LBS.pack "{\"level\":1000001.0,\"lastTime\":1625073600}"
      case decode invalidJson :: Maybe LeakyBucketState of
        Just _ -> assertFailure "Should fail to deserialize level exceeding 1000000"
        Nothing -> return () -- Expected failure

  , testCase "Invalid JSON: malformed input" $ do
      let invalidJson = LBS.pack "{\"level\":2.5}" -- Missing lastTime
      case decode invalidJson :: Maybe LeakyBucketState of
        Just _ -> assertFailure "Should fail to deserialize malformed JSON"
        Nothing -> return () -- Expected failure

  , testCase "Edge case: zero level" $ do
      let state = LeakyBucketState { level = 0.0, lastTime = 1625073600 }
      let json = encode state
      case decode json :: Maybe LeakyBucketState of
        Nothing -> assertFailure "JSON deserialization failed for zero level"
        Just decoded -> decoded @?= state

  , testCase "Edge case: large valid lastTime" $ do
      let state = LeakyBucketState { level = 500.0, lastTime = 1755734400 } -- ≈ 2025-08-01
      let json = encode state
      case decode json :: Maybe LeakyBucketState of
        Nothing -> assertFailure "JSON deserialization failed for large lastTime"
        Just decoded -> decoded @?= state
  ]
