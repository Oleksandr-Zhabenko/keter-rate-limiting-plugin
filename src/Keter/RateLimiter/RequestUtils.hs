{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Keter.RateLimiter.RequestUtils
Description : Utility functions for extracting data from WAI requests
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : stable
Portability : portable
-}

module Keter.RateLimiter.RequestUtils
  ( getClientIP
  , getRequestPath
  , getRequestMethod
  , getRequestHost
  , getRequestUserAgent
  , byIP
  , byIPAndPath
  , byIPAndUserAgent
  , byHeaderAndIP
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Wai (Request)
import qualified Network.Wai as WAI
import Network.Socket (SockAddr(..), HostAddress, HostAddress6, hostAddressToTuple)
import Network.HTTP.Types.Header (HeaderName)
import Data.Bits
import Data.IP (fromHostAddress)
import Data.CaseInsensitive (CI, mk)
import System.IO.Unsafe (unsafePerformIO)
import Numeric (showHex)

-- | Helper functions for extracting data from WAI Request
ipv4ToString :: HostAddress -> Text
ipv4ToString addr =
  let (o1, o2, o3, o4) = hostAddressToTuple addr
  in T.intercalate "." (map (T.pack . show) [o1, o2, o3, o4])

ipv6ToString :: HostAddress6 -> Text
ipv6ToString (w1, w2, w3, w4) =
  T.intercalate ":" $ map (T.pack . showHexWord)
    [w1 `shiftR` 16, w1 .&. 0xFFFF, w2 `shiftR` 16, w2 .&. 0xFFFF,
     w3 `shiftR` 16, w3 .&. 0xFFFF, w4 `shiftR` 16, w4 .&. 0xFFFF]
  where
    showHexWord n = let s = showHex n "" in if length s < 4 then replicate (4 - length s) '0' ++ s else s

getClientIP :: Request -> IO Text
getClientIP req = do
  let ip = case lookup (mk "x-forwarded-for") (WAI.requestHeaders req) of
        Just xff -> T.takeWhile (/= ',') $ TE.decodeUtf8 xff
        Nothing -> case lookup (mk "x-real-ip") (WAI.requestHeaders req) of
          Just realIP -> TE.decodeUtf8 realIP
          Nothing -> case WAI.remoteHost req of
            SockAddrInet _ addr -> ipv4ToString addr
            SockAddrInet6 _ _ addr _ -> ipv6ToString addr
            SockAddrUnix path -> T.pack path
            _ -> "default"
  return ip

getRequestPath :: Request -> Text
getRequestPath = TE.decodeUtf8 . WAI.rawPathInfo

getRequestMethod :: Request -> Text
getRequestMethod = TE.decodeUtf8 . WAI.requestMethod

getRequestHost :: Request -> Maybe Text
getRequestHost req = TE.decodeUtf8 <$> WAI.requestHeaderHost req

getRequestUserAgent :: Request -> Maybe Text
getRequestUserAgent req = TE.decodeUtf8 <$> WAI.requestHeaderUserAgent req

byIP :: Request -> Maybe Text
byIP req = Just $ unsafePerformIO $ getClientIP req

byIPAndPath :: Request -> Maybe Text
byIPAndPath req = Just $ (unsafePerformIO $ getClientIP req) <> ":" <> getRequestPath req

byIPAndUserAgent :: Request -> Maybe Text
byIPAndUserAgent req = do
  ua <- getRequestUserAgent req
  return $ (unsafePerformIO $ getClientIP req) <> ":" <> ua

byHeaderAndIP :: HeaderName -> Request -> Maybe Text
byHeaderAndIP headerName req = do
  headerValue <- lookup headerName (WAI.requestHeaders req)
  return $ (unsafePerformIO $ getClientIP req) <> ":" <> TE.decodeUtf8 headerValue
