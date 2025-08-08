{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

{-|
Module      : Keter.RateLimiter.Notifications
Description : Notification system for rate limiting events
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : stable
Portability : portable

Copyright (c) 2025 Oleksandr Zhabenko
  
This file is a ported to Haskell language code with some simplifications of Ruby on Rails
'https://github.com/rails/rails'
'https://github.com/rails/rails/blob/2318163b4b9e9604b557057c0465e2a5ef162401/activesupport/lib/active\_support/notifications.rb'
and is based on the structure of the original code of 
rack-attack, Copyright (c) David Heinemeier Hansson, under the MIT License.

This implementation is released under the MIT License.

= Overview

The @Notifications@ module supplies a **pluggable notification layer** for the
rate-limiting parts of the /keter-rate-limiting-plugin/ project. Whenever a throttle
decides to reject or allow a request, you may want to:

* store an audit trail,
* emit a metric,
* send an e-mail / Slack message, or
* perform any other side effect.

The abstraction is intentionally minimal yet flexible:

* 'Notifier' – a "generic" notifier that works with arbitrary data (converted
  to a textual representation by the caller).
* 'WAINotifier' – a convenience type alias specialised for WAI 'Network.Wai.Request' objects.

Both flavours come with:

* "do-nothing" implementations ('noopNotifier', 'noopWAINotifier') for easy disabling or testing, and
* simple console loggers ('consoleNotifier', 'consoleWAINotifier') for straightforward debugging.

You can easily lift these notifiers into your favourite effect stack by wrapping the
'IO' action with a natural transformation (e.g., @liftIO@ for any 'Control.Monad.IO.Class.MonadIO').

== Quick example

@
{-# LANGUAGE OverloadedStrings #-}
import Keter.RateLimiter.Notifications
import Network.Wai
import Network.Socket
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status429)
import Control.Monad.IO.Class (liftIO)

-- A hypothetical function called when a client hits a rate limit.
blocked :: Request -> IO Response
blocked req = do
  -- Log the event using the predefined WAI console notifier.
  -- "per-ip" is the logical name of our throttle.
  -- 100 is the limit that was exceeded.
  consoleWAINotifier "per-ip" req 100
  pure $ responseLBS status429 [] "Too Many Requests"

-- A simple WAI application.
app :: Application
app req respond = do
  -- In a real application, you would have logic here to check the rate limit.
  -- If the limit is exceeded, call 'blocked'.
  let isBlocked = False -- Placeholder for actual rate-limiting logic.
  if isBlocked
    then blocked req >>= respond
    else respond $ responseLBS status200 [] "OK"

main :: IO ()
main = run 8080 app
@

== Advanced usage patterns

=== Custom notifiers

You can create custom notifiers for integration with external systems:

@
import qualified Data.Text.IO as TIO
import Control.Exception (try, SomeException)

-- A notifier that attempts to write to a file, falling back to console on error
fileNotifier :: FilePath -> Notifier
fileNotifier path = Notifier
  { notifierName = "file-logger"
  , notifierAction = \\throttle act item limit -> do
      now <- getCurrentTime
      let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
          parts = [throttle, act, item, T.concat ["(limit: ", T.pack (show limit), ")"]]
          message = T.intercalate " " $ filter (not . T.null) parts
          fullMessage = T.concat [T.pack timestamp, " - ", message, "\\n"]
      
      result <- try $ TIO.appendFile path fullMessage
      case result of
        Left (_ :: SomeException) -> 
          -- Fallback to console logging
          notifierAction consoleNotifier throttle act item limit
        Right _ -> pure ()
  }
@

=== Combining notifiers

Multiple notifiers can be combined to send notifications to different destinations:

@
multiNotifier :: [Notifier] -> Notifier
multiNotifier notifiers = Notifier
  { notifierName = "multi"
  , notifierAction = \\throttle act item limit ->
      mapM_ (\\n -> notifierAction n throttle act item limit) notifiers
  }

-- Usage example
combinedNotifier = multiNotifier [consoleNotifier, fileNotifier "/var/log/rate-limits.log"]
@

=== Converting between notifier types

The 'waiNotifier' function allows you to use any generic 'Notifier' with WAI requests:

@
-- Use a custom notifier with WAI
myWAINotifier :: WAINotifier
myWAINotifier = waiNotifier (fileNotifier "/var/log/wai-rate-limits.log")
@

== Log format specifications

=== Generic notifier format

The 'consoleNotifier' produces log entries in the following format:

@
YYYY-MM-DD HH:MM:SS - throttleName action item (limit: N)
@

Examples:
@
2025-01-30 13:45:12 - loginAttempts blocked "user123" (limit: 5)
2025-01-30 13:45:13 - api-global blocked "192.168.1.100" (limit: 1000)
@

Note that textual items are quoted due to the use of 'show' for conversion.

=== WAI notifier format

The 'consoleWAINotifier' produces more detailed log entries specifically for HTTP requests:

@
YYYY-MM-DD HH:MM:SS - throttleName blocked METHOD PATH[?QUERY] from IP:PORT (limit: N)
@

Examples:
@
2025-01-30 13:45:12 - api-global blocked GET /v1/users?id=123 from 192.0.2.1:54321 (limit: 1000)
2025-01-30 13:45:13 - auth blocked POST /login from 10.0.0.1:443 (limit: 10)
2025-01-30 13:45:14 - resource blocked DELETE /resource/123 from 127.0.0.1:9000 (limit: 50)
@

=== Request conversion details

The 'convertWAIRequest' function creates a compact representation of WAI requests:

* **IPv4 addresses**: @192.168.1.100:8080@
* **IPv6 addresses**: @2001:0db8:0000:0000:0000:0000:0000:0001:443@
* **Unix sockets**: Port information is omitted
* **Empty query strings**: Only the path is included
* **Empty throttle names**: The throttle name is simply omitted from the output

== Thread-safety

All predefined notifiers are **thread-safe** because they only use atomic
functions from the @base@ and @text@ packages. The timestamp generation using
'getCurrentTime' is also thread-safe.

If you write a custom notifier that has internal mutable state (such as counters,
caches, or connection pools), you must ensure its operations are properly synchronized
using appropriate concurrency primitives like 'Control.Concurrent.STM.STM',
'Control.Concurrent.MVar.MVar', or 'Data.IORef.IORef' with atomic operations.

== Error handling considerations

The predefined notifiers ('consoleNotifier', 'consoleWAINotifier') do not handle
IO exceptions that might occur during logging operations. In production environments,
you may want to wrap these notifiers with appropriate error handling:

@
safeNotifier :: Notifier -> Notifier
safeNotifier baseNotifier = baseNotifier
  { notifierAction = \\throttle act item limit -> do
      result <- try $ notifierAction baseNotifier throttle act item limit
      case result of
        Left (_ :: SomeException) -> 
          -- Log the error or use a fallback strategy
          pure ()
        Right _ -> pure ()
  }
@

== Performance considerations

* The 'convertWAIRequest' function uses 'unsafePerformIO' internally for IP resolution.
  This is marked with @NOINLINE@ to prevent duplication and ensure predictable behavior.
  
* Timestamp generation occurs for every notification, which involves system calls.
  For high-throughput applications, consider batching notifications or using
  asynchronous logging mechanisms.

* The console notifiers write to 'stdout' synchronously, which may become a bottleneck
  under heavy load. Consider using buffered or asynchronous alternatives for
  production deployments.

== Testing support

The module is designed with testability in mind:

* 'noopNotifier' and 'noopWAINotifier' can be used in test environments to suppress output
* The modular design allows easy substitution of test doubles
* All notification functions are pure except for the final IO action, making them
  easy to test with custom capture mechanisms

Example test notifier:

@
createTestNotifier :: IORef [Text] -> Notifier
createTestNotifier outputRef = Notifier
  { notifierName = "test"
  , notifierAction = \\throttle act item limit -> do
      now <- getCurrentTime
      let message = -- format message as needed
      modifyIORef' outputRef (message :)
  }
@

-}

module Keter.RateLimiter.Notifications
  ( -- * Types
    Notifier(..)
  , WAINotifier

    -- * Generic notifier helpers
  , notify
  , noopNotifier
  , consoleNotifier

    -- * WAI-specific helpers
  , notifyWAI
  , waiNotifier
  , convertWAIRequest
  , noopWAINotifier
  , consoleWAINotifier
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Network.Wai (Request)
import qualified Network.Wai as Wai
import qualified Data.Text.Encoding as TE
import Network.Socket (SockAddr(..))
import Keter.RateLimiter.RequestUtils
       ( getClientIP
       , getRequestMethod
       , getRequestPath )
import System.IO.Unsafe (unsafePerformIO)

----------------------------------------------------------------------
-- Types
----------------------------------------------------------------------

-- | A generic, abstract notifier.
--
-- It consists of a name for identification and an 'IO' action that performs
-- the notification. The action receives all the necessary context to log or
-- process a rate-limiting event.
--
-- The notifier is designed to be composable and can be easily integrated
-- into different effect systems by lifting the 'IO' action appropriately.
data Notifier = Notifier
  { notifierName   :: Text
    -- ^ A human-readable name for the notifier (e.g., "console", "prometheus", "file-logger").
    --   This can be used for debugging, logging, or configuration purposes.
  , notifierAction :: Text   -- ^ throttleName: Logical name of the limiter (e.g. "login-attempt", "api-global").
                    -> Text  -- ^ action: A free-form verb, typically "blocked" or "allowed".
                    -> Text  -- ^ item: A textual representation of whatever was rate-limited.
                    -> Int   -- ^ limit: The numeric limit that triggered the event.
                    -> IO ()
    -- ^ The action to be executed upon a notification trigger.
    --   This action should be thread-safe if the notifier will be used concurrently.
  }

-- | A type alias for a notifier specialized for WAI 'Request's.
--
-- This simplifies the signature for notifiers that work directly with WAI,
-- avoiding the need to manually convert the 'Request' to 'Text' beforehand.
-- The conversion is handled internally using 'convertWAIRequest'.
type WAINotifier = Text      -- ^ throttleName: Logical name of the limiter.
                -> Request   -- ^ The WAI 'Request' that triggered the event.
                -> Int       -- ^ The numeric limit that was applied.
                -> IO ()

----------------------------------------------------------------------
-- Generic helpers
----------------------------------------------------------------------

-- | A high-level wrapper to trigger a 'Notifier'.
--
-- It simplifies calling a notifier by using a fixed action verb (@"blocked"@)
-- and automatically converting the rate-limited item to 'Text' via its 'Show'
-- instance. Note that using 'show' on a 'Text' or 'String' value will add
-- quotes around it in the output.
--
-- ==== __Examples__
--
-- @
-- -- Assuming 'consoleNotifier' is defined as in this module.
-- notify consoleNotifier "login-per-ip" ("192.0.2.1" :: Text) 20
-- -- This would log: ... blocked "192.0.2.1" ...
--
-- notify consoleNotifier "api-requests" (42 :: Int) 100
-- -- This would log: ... blocked 42 ...
--
-- -- With empty throttle names:
-- notify consoleNotifier "" ("item" :: Text) 50
-- -- This would log: ... blocked "item" ...
-- @
notify :: Show req        -- ^ The item being rate-limited, must be 'Show'able.
       => Notifier
       -> Text            -- ^ The logical name of the throttle.
       -> req             -- ^ The item itself.
       -> Int             -- ^ The limit that was applied.
       -> IO ()
notify Notifier{..} throttleName req limit =
  notifierAction throttleName "blocked" (T.pack (show req)) limit

-- | A trivial 'Notifier' that performs no action.
--
-- This is useful in tests, for disabling notifications in certain environments,
-- or as a default value. It completes immediately without any side effects.
--
-- ==== __Example usage__
--
-- @
-- -- Disable notifications in test environment
-- let notifier = if inTestMode then noopNotifier else consoleNotifier
-- notify notifier "test-throttle" ("data" :: Text) 100
-- @
noopNotifier :: Notifier
noopNotifier = Notifier
  { notifierName   = "noop"
  , notifierAction = \_ _ _ _ -> pure ()
  }

-- | A 'Notifier' that logs events to standard output ('stdout').
--
-- The log format is a single, timestamped line with the following structure:
-- @YYYY-MM-DD HH:MM:SS - throttleName action item (limit: N)@
--
-- Empty throttle names are handled gracefully by omitting them from the output.
-- The item is rendered using 'show', which will add quotes for textual types.
--
-- ==== __Example output__
--
-- @
-- 2025-01-30 13:45:12 - login-per-ip blocked "192.0.2.1" (limit: 20)
-- 2025-01-30 13:45:13 - blocked "item" (limit: 100)
-- @
--
-- The notifier is thread-safe as it only uses atomic IO operations.
consoleNotifier :: Notifier
consoleNotifier = Notifier
  { notifierName = "console"
  , notifierAction = \throttle act item limit -> do
      now <- getCurrentTime
      let ts = fmt now
          -- Create a list of the components for the message.
          parts = [throttle, act, item, T.concat ["(limit: ", T.pack (show limit), ")"]]
          -- Filter out any empty strings and join the rest with a single space.
          message = T.intercalate " " $ filter (not . T.null) parts
      TIO.putStrLn $ T.concat [ts, " - ", message]
  }
  where
    fmt = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

----------------------------------------------------------------------
-- WAI-specific helpers
----------------------------------------------------------------------

-- | A high-level wrapper to trigger a 'WAINotifier'.
--
-- This is a thin alias, defined for symmetry with 'notify'. It simply calls
-- the underlying 'WAINotifier' function directly.
--
-- ==== __Example__
--
-- @
-- notifyWAI consoleWAINotifier "api-throttle" request 100
-- @
notifyWAI :: WAINotifier -> Text -> Request -> Int -> IO ()
notifyWAI = id

-- | Lifts a generic 'Notifier' into the WAI-specific domain, creating a 'WAINotifier'.
--
-- It works by using 'convertWAIRequest' to transform the 'Request' object into a
-- summary 'Text' before passing it to the underlying 'Notifier'\'s action.
-- The action verb is fixed to "blocked".
--
-- ==== __Example__
--
-- @
-- let myWAINotifier = waiNotifier consoleNotifier
-- myWAINotifier "auth" request 10
-- -- Output: YYYY-MM-DD HH:MM:SS - auth blocked GET /login from 10.0.0.1:443 (limit: 10)
-- @
waiNotifier :: Notifier -> WAINotifier
waiNotifier Notifier{..} throttleName req limit =
  notifierAction throttleName "blocked" (convertWAIRequest req) limit

-- | Converts a WAI 'Request' to a compact, single-line textual representation.
--
-- The output format is: @METHOD PATH[?QUERY] from IP:PORT@
--
-- Port handling:
-- * For IPv4 and IPv6 addresses: includes the port number
-- * For Unix sockets or unknown socket types: omits the port
--
-- Query string handling:
-- * Non-empty query strings: included with the leading '?'
-- * Empty query strings: omitted entirely
--
-- ==== __Examples__
--
-- @
-- GET /index.html?lang=en from 127.0.0.1:8080
-- POST /login from 10.0.0.1:443
-- DELETE /resource/123 from 127.0.0.1:9000
-- GET /api/users?limit=10 from 2001:0db8:0000:0000:0000:0000:0000:0001:443
-- @
--
-- This function uses 'unsafePerformIO' internally to resolve the client's IP
-- address via 'getClientIP'. This is considered acceptable for logging and
-- notification purposes where the function's primary role is to produce a
-- human-readable summary and performance is not critically impacted by a minor,
-- contained impurity. The 'NOINLINE' pragma is used to prevent the IO action
-- from being duplicated by compiler optimizations.
convertWAIRequest :: Request -> Text
convertWAIRequest req =
  let method   = getRequestMethod req
      path     = getRequestPath req
      query    = TE.decodeUtf8 $ Wai.rawQueryString req
      clientIP = unsafePerformIO $ getClientIP req
      portStr  = case Wai.remoteHost req of
                   SockAddrInet port _ -> ":" <> T.pack (show port)
                   SockAddrInet6 port _ _ _ -> ":" <> T.pack (show port)
                   _ -> ""  -- Unix sockets or unknown; omit port
  in method <> " " <> path <> query <> " from " <> clientIP <> portStr
{-# NOINLINE convertWAIRequest #-}
  -- NOINLINE is important due to the use of unsafePerformIO,
  -- ensuring the IO action is not inadvertently duplicated.

-- | A 'WAINotifier' that performs no action.
--
-- This is the WAI-specific equivalent of 'noopNotifier'. It completes
-- immediately without any side effects and is useful for testing or
-- disabling WAI-specific notifications.
--
-- ==== __Example usage__
--
-- @
-- -- Use in test environments or to disable logging
-- let notifier = if enableLogging then consoleWAINotifier else noopWAINotifier
-- notifier "test-throttle" request 100
-- @
noopWAINotifier :: WAINotifier
noopWAINotifier _ _ _ = pure ()

-- | A 'WAINotifier' that logs formatted request data to 'stdout'.
--
-- The log format includes timestamp, throttle name, action ("blocked"),
-- formatted request information, and the limit that was exceeded:
--
-- @YYYY-MM-DD HH:MM:SS - throttleName blocked METHOD PATH[?QUERY] from IP:PORT (limit: N)@
--
-- ==== __Example output__
--
-- @
-- 2025-01-30 13:45:12 - api-global blocked GET /v1/users?id=123 from 192.0.2.1:54321 (limit: 1000)
-- 2025-01-30 13:45:13 - auth blocked POST /login from 10.0.0.1:443 (limit: 10)
-- 2025-01-30 13:45:14 - resource blocked DELETE /resource/123 from 127.0.0.1:9000 (limit: 50)
-- @
--
-- The notifier is thread-safe and handles various request types including
-- IPv4, IPv6, and Unix socket connections appropriately.
consoleWAINotifier :: WAINotifier
consoleWAINotifier throttleName req limit = do
  now <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
      requestInfo = convertWAIRequest req
  TIO.putStrLn $
    T.pack timestamp <> " - " <> throttleName <> " blocked " <>
    requestInfo <> " (limit: " <> T.pack (show limit) <> ")"
