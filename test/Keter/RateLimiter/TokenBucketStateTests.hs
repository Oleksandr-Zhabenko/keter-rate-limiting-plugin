{-# LANGUAGE OverloadedStrings #-}

module Keter.RateLimiter.TokenBucketStateTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Keter.RateLimiter.Types (TokenBucketState(..))

tests :: TestTree
tests = testGroup "Keter.RateLimiter.TokenBucketState Tests"
  [ testCase "Construct and compare TokenBucketState" $ do
      let state = TokenBucketState { tokens = 5, lastUpdate = 1625073600 }
      state @?= TokenBucketState { tokens = 5, lastUpdate = 1625073600 }
      tokens state @?= 5
      lastUpdate state @?= 1625073600

  , testCase "JSON serialization and deserialization" $ do
      let state = TokenBucketState { tokens = 5, lastUpdate = 1625073600 }
      let json = encode state
      case decode json :: Maybe TokenBucketState of
        Nothing -> assertFailure "JSON deserialization failed"
        Just decoded -> decoded @?= state

  , testCase "Invalid JSON: negative tokens" $ do
      let invalidJson = LBS.pack "{\"tokens\":-1,\"lastUpdate\":1625073600}"
      case decode invalidJson :: Maybe TokenBucketState of
        Just _ -> assertFailure "Should fail to deserialize negative tokens"
        Nothing -> return () -- Expected failure

  , testCase "Invalid JSON: malformed input" $ do
      let invalidJson = LBS.pack "{\"tokens\":5}" -- Missing lastUpdate
      case decode invalidJson :: Maybe TokenBucketState of
        Just _ -> assertFailure "Should fail to deserialize malformed JSON"
        Nothing -> return () -- Expected failure

  , testCase "Edge case: zero tokens" $ do
      let state = TokenBucketState { tokens = 0, lastUpdate = 1625073600 }
      let json = encode state
      case decode json :: Maybe TokenBucketState of
        Nothing -> assertFailure "JSON deserialization failed for zero tokens"
        Just decoded -> decoded @?= state

  , testCase "Edge case: large valid lastUpdate" $ do
      let state = TokenBucketState { tokens = 10, lastUpdate = 1755734400 } -- Approx. 2025-08-01
      let json = encode state
      case decode json :: Maybe TokenBucketState of
        Nothing -> assertFailure "JSON deserialization failed for large lastUpdate"
        Just decoded -> decoded @?= state
  ]
