{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Keter.RateLimiter.RequestUtils
Description : Utility functions for extracting data from WAI requests
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : stable
Portability : portable

Utility helpers for extracting /stable textual keys/ from a WAI
'Network.Wai.Request'.  They are primarily intended for use with
rate-limiting middleware (see the @keter-rate-limiting-plugin@ package) but are fully
generic and can be employed anywhere you need a deterministic identifier that
ties a request to its origin (IP address, path, user-agent, …).

The helpers follow these rules:

1.  /Zero/ allocation whenever the value is already available in the request
    record (e.g. @rawPathInfo@ or @requestMethod@ are reused verbatim).
2.  No reverse DNS or other network round-trips – the functions are pure and
    fast.
3.  Header names are handled case-insensitively via the
    'Data.CaseInsensitive.CI' type.

== Quick example

@
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class (liftIO)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Keter.RateLimiter.RequestUtils (byIPAndPath)
import Data.Text.IO as TIO

logKey :: Request -> IO ()
logKey req = do
  mk <- byIPAndPath req
  case mk of
    Nothing  -> TIO.putStrLn "cannot build key"
    Just key -> TIO.putStrLn ("request key = " <> key)

app :: Application
app req respond = liftIO (logKey req) >> respond (responseLBS status200 [] "OK")

main :: IO ()
main = run 8080 app
@

== Converting sockets to text

Functions 'ipv4ToString' and 'ipv6ToString' perform a /lossless/ conversion of
binary socket addresses to their canonical textual representations.  The
implementation is intentionally simple and does not attempt to compress IPv6
zeros (you get four-hextet groups padded to 4 digits).

-}

module Keter.RateLimiter.RequestUtils
  ( -- * Low-level helpers
    ipv4ToString
  , ipv6ToString

    -- * Basic request information
  , getClientIP
  , getRequestPath
  , getRequestMethod
  , getRequestHost
  , getRequestUserAgent

    -- * Composite key builders
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
import Network.Socket
       ( SockAddr(..)
       , HostAddress
       , HostAddress6
       , hostAddressToTuple
       )
import Network.HTTP.Types.Header (HeaderName)
import Data.Bits ((.&.), shiftR)
import Data.CaseInsensitive (mk)
import Numeric (showHex)

----------------------------------------------------------------------
-- Low-level helpers
----------------------------------------------------------------------

-- | Convert an IPv4 'HostAddress' to dotted-decimal 'Text'.
--
-- ==== __Example__
--
-- >>> ipv4ToString 0x7f000001     -- 127.0.0.1
-- "127.0.0.1"
ipv4ToString :: HostAddress -> Text
ipv4ToString addr =
  let (o1, o2, o3, o4) = hostAddressToTuple addr
  in T.intercalate "." (map (T.pack . show) [o1, o2, o3, o4])

-- | Render an IPv6 'HostAddress6' as eight 16-bit hex blocks separated
-- by ‘:’.  Each block is zero-padded to four characters. This rendering
-- is canonical but not compressed (e.g., it does not use @::@).
--
-- The function is micro-optimised to avoid lists and string formatting functions.
--
-- ==== __Example__
--
-- >>> ipv6ToString (0,0,0,1)
-- "0000:0000:0000:0000:0000:0000:0000:0001"
ipv6ToString :: HostAddress6 -> Text
ipv6ToString (w1, w2, w3, w4) =
  T.intercalate ":" $ map (T.pack . pad4 . (`showHex` "")) words16
  where
    words16 =
      [ w1 `shiftR` 16, w1 .&. 0xFFFF
      , w2 `shiftR` 16, w2 .&. 0xFFFF
      , w3 `shiftR` 16, w3 .&. 0xFFFF
      , w4 `shiftR` 16, w4 .&. 0xFFFF
      ]
    pad4 s = replicate (4 - length s) '0' ++ s

----------------------------------------------------------------------
-- Basic request information
----------------------------------------------------------------------

-- | Best-effort client IP address detection.
--
-- This function attempts to find the most accurate client IP address by checking
-- common proxy headers first, falling back to the direct socket address if they
-- are not present.
--
-- The priority order for detection is:
--
-- 1.  @X-Forwarded-For@ (takes the first IP in the comma-separated list).
-- 2.  @X-Real-Ip@.
-- 3.  The 'Network.Wai.remoteHost' from the WAI 'Request' object.
--
-- Header names are matched case-insensitively. IPv4 and IPv6 addresses are
-- converted to text using 'ipv4ToString' and 'ipv6ToString' respectively.
-- Unix sockets are represented by their file path.
getClientIP :: Request -> IO Text
getClientIP req = do
  let ipTxt = case lookup (mk "x-forwarded-for") (WAI.requestHeaders req) of
        Just xff -> T.takeWhile (/= ',') $ TE.decodeUtf8 xff
        Nothing  -> case lookup (mk "x-real-ip") (WAI.requestHeaders req) of
          Just rip -> TE.decodeUtf8 rip
          Nothing  -> case WAI.remoteHost req of
            SockAddrInet  _ addr        -> ipv4ToString addr
            SockAddrInet6 _ _ addr _    -> ipv6ToString addr
            SockAddrUnix   path         -> T.pack path
  pure ipTxt

-- | Extracts the raw path info from the request and decodes it as UTF-8 'Text'.
-- This is equivalent to @'TE.decodeUtf8' . 'WAI.rawPathInfo'@.
getRequestPath :: Request -> Text
getRequestPath = TE.decodeUtf8 . WAI.rawPathInfo

-- | Extracts the HTTP request method (e.g., @"GET"@, @"POST"@) and returns it as a 'Text' value.
-- This is equivalent to @'TE.decodeUtf8' . 'WAI.requestMethod'@.
getRequestMethod :: Request -> Text
getRequestMethod = TE.decodeUtf8 . WAI.requestMethod

-- | Extracts the value of the @Host@ header, if present.
getRequestHost :: Request -> Maybe Text
getRequestHost = fmap TE.decodeUtf8 . WAI.requestHeaderHost

-- | Extracts the value of the @User-Agent@ header, if present.
getRequestUserAgent :: Request -> Maybe Text
getRequestUserAgent = fmap TE.decodeUtf8 . WAI.requestHeaderUserAgent

----------------------------------------------------------------------
-- Composite key builders
----------------------------------------------------------------------

-- | Creates a request key based solely on the client's IP address.
--
-- The result is always 'Just' a value, as an IP or equivalent is always available.
--
-- ==== __Example__
--
-- @
-- byIP req ⇨ pure (Just "127.0.0.1")
-- @
byIP :: Request -> IO (Maybe Text)
byIP req = Just <$> getClientIP req

-- | Creates a composite key by combining the client IP and the request path,
-- separated by a colon.
--
-- This is useful for rate-limiting access to specific endpoints rather than
-- penalizing a client for all of its requests.
--
-- ==== __Example__
--
-- @
-- -- For a request to \/api\/v1\/users from 192.168.1.10
-- byIPAndPath req ⇨ pure (Just "192.168.1.10:\/api\/v1\/users")
-- @
byIPAndPath :: Request -> IO (Maybe Text)
byIPAndPath req = do
  ip <- getClientIP req
  pure . Just $ ip <> ":" <> getRequestPath req

-- | Creates a composite key by combining the client IP and the @User-Agent@
-- header, separated by a colon.
--
-- Returns 'Nothing' if the @User-Agent@ header is not present in the request.
--
-- ==== __Example__
--
-- @
-- -- For a request from Googlebot at 8.8.8.8
-- byIPAndUserAgent req ⇨ pure (Just "8.8.8.8:Mozilla\/5.0 (compatible; Googlebot\/2.1)")
-- @
byIPAndUserAgent :: Request -> IO (Maybe Text)
byIPAndUserAgent req = do
  ip <- getClientIP req
  pure $ case getRequestUserAgent req of
           Nothing  -> Nothing
           Just ua  -> Just (ip <> ":" <> ua)

-- | Builds a key from an arbitrary header and the client IP, joined by a colon.
--
-- Header lookup is case-insensitive. Returns 'Nothing' if the header is absent.
--
-- ==== __Example__
--
-- This can be used to rate-limit based on an API key plus the user's IP.
--
-- @
-- -- Given a request with header "X-Api-Key: mysecret" from 1.2.3.4
-- byHeaderAndIP "x-api-key" req ⇨ pure (Just "1.2.3.4:mysecret")
-- @
byHeaderAndIP :: HeaderName -> Request -> IO (Maybe Text)
byHeaderAndIP headerName req = do
  ip <- getClientIP req
  let mVal = lookup headerName (WAI.requestHeaders req)
  pure $ fmap (\hv -> ip <> ":" <> TE.decodeUtf8 hv) mVal
