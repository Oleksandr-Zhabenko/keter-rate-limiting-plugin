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
  
This file is a ported to Haskell language code with some simlifications of Ruby on Rails
https://github.com/rails/rails 
https://github.com/rails/rails/blob/2318163b4b9e9604b557057c0465e2a5ef162401/activesupport/lib/active_support/notifications.rb
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

The abstraction is intentionally minimal:

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

== Thread-safety

All predefined notifiers are **thread-safe** because they only use atomic
functions from the @base@ and @text@ packages. If you write a custom notifier
that has internal mutable state, you must ensure its operations are synchronized.

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
data Notifier = Notifier
  { notifierName   :: Text
    -- ^ A human-readable name for the notifier (e.g., "console", "prometheus").
  , notifierAction :: Text   -- ^ throttleName: Logical name of the limiter (e.g. "login-attempt").
                    -> Text  -- ^ action: A free-form verb, typically "blocked" or "allowed".
                    -> Text  -- ^ item: A textual representation of whatever was rate-limited.
                    -> Int   -- ^ limit: The numeric limit that triggered the event.
                    -> IO ()
    -- ^ The action to be executed upon a notification trigger.
  }

-- | A type alias for a notifier specialized for WAI 'Request's.
--
-- This simplifies the signature for notifiers that work directly with WAI,
-- avoiding the need to manually convert the 'Request' to 'Text' beforehand.
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
-- ==== __Example__
--
-- @
-- -- Assuming 'consoleNotifier' is defined as in this module.
-- notify consoleNotifier "login-per-ip" ("192.0.2.1" :: Text) 20
-- -- This would log: ... blocked "192.0.2.1" ...
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
-- or as a default value.
noopNotifier :: Notifier
noopNotifier = Notifier
  { notifierName   = "noop"
  , notifierAction = \_ _ _ _ -> pure ()
  }

-- | A 'Notifier' that logs events to standard output ('stdout').
--
-- The log format is a single, timestamped line. Note that the item is rendered
-- using 'show', which will add quotes for textual types.
--
-- @
-- 2025-01-30 13:45:12 - login-per-ip blocked "192.0.2.1" (limit: 20)
-- @
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
notifyWAI :: WAINotifier -> Text -> Request -> Int -> IO ()
notifyWAI = id

-- | Lifts a generic 'Notifier' into the WAI-specific domain, creating a 'WAINotifier'.
--
-- It works by using 'convertWAIRequest' to transform the 'Request' object into a
-- summary 'Text' before passing it to the underlying 'Notifier'\'s action.
waiNotifier :: Notifier -> WAINotifier
waiNotifier Notifier{..} throttleName req limit =
  notifierAction throttleName "blocked" (convertWAIRequest req) limit

-- | Converts a WAI 'Request' to a compact, single-line textual representation.
--
-- The output format is: @METHOD PATH?QUERY from IP:PORT@
-- For example: @GET /index.html?lang=en from 127.0.0.1:8080@
--
-- The port number is omitted for non-IP socket types (e.g., Unix sockets).
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
-- This is the WAI-specific equivalent of 'noopNotifier'.
noopWAINotifier :: WAINotifier
noopWAINotifier _ _ _ = pure ()

-- | A 'WAINotifier' that logs formatted request data to 'stdout'.
--
-- The log format is, for example:
--
-- @
-- 2025-01-30 13:45:12 - api-global blocked GET /v1/users?id=123 from 192.0.2.1:54321 (limit: 1000)
-- @
consoleWAINotifier :: WAINotifier
consoleWAINotifier throttleName req limit = do
  now <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
      requestInfo = convertWAIRequest req
  TIO.putStrLn $
    T.pack timestamp <> " - " <> throttleName <> " blocked " <>
    requestInfo <> " (limit: " <> T.pack (show limit) <> ")"
