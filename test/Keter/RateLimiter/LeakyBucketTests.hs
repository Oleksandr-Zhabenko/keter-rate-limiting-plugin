{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (newMVar, withMVar)
import Network.Wai (Request, Response, defaultRequest)
import qualified Network.Wai as WAI
import Network.HTTP.Types (methodGet, status429)
import Network.Socket (SockAddr(..))
import Network.HTTP.Types.Status (statusCode)
import qualified Data.Cache as DataCache
import Keter.RateLimiter.Cache
import Keter.RateLimiter.WAI

