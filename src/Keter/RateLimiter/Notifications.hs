{-# LANGUAGE OverloadedStrings #-}

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
https://github.com/rails/rails/blob/2318163b4b9e9604b557057c0465e2a5ef162401/activesupport/lib/active_support/notifications.rbAdd commentMore actions
and is based on the structure of the original code of 
rack-attack, Copyright (c) David Heinemeier Hansson, under the MIT License.

This implementation is released under the MIT License.

The module provides an abstract notification mechanism for rate limiting events within the Keter system.

== Overview

Rate limiting is a common technique used to control the amount of incoming requests or actions to a system, preventing abuse or overload. When a rate limit is exceeded, it is often necessary to notify relevant parties or systems to take appropriate action or log the event.

This module defines a 'Notifier' type that encapsulates the notification strategy and provides several implementations, including a no-operation notifier and a console logger notifier. Additionally, it provides WAI-specific notification types and functions for working with WAI requests.

== Usage

To notify about a rate limit event, use the 'notify' function with an appropriate 'Notifier'. For example:

@
notify consoleNotifier "loginAttempts" userRequest 100
@

For WAI requests, use the 'notifyWAI' function:

@
notifyWAI myWAINotifier "apiRequests" waiRequest 50
@

Or adapt an existing 'Notifier' to work with WAI requests:

@
waiNotifier consoleNotifier "loginAttempts" waiRequest 10
@

This will log a message to the console indicating that the 'loginAttempts' throttle has blocked the given request due to exceeding the limit of 100.

== When to use

Use this module when you want to integrate notifications into your rate limiting logic, allowing flexibility in how notifications are handled (e.g., logging, alerting, no action).

== When not to use

If your application does not require notifications for rate limiting events or handles them differently, this module may not be necessary.

== Exported entities

* 'Notifier' — abstract type representing a notification strategy
* 'notify' — function to trigger a notification
* 'noopNotifier' — a notifier that performs no action
* 'consoleNotifier' — a notifier that logs notifications to the console with timestamps
* 'WAINotifier' — type for WAI-specific notifications
* 'notifyWAI' — function to trigger WAI notifications
* 'waiNotifier' — adapter to use regular Notifier with WAI requests
* 'consoleWAINotifier' — a WAI notifier that logs notifications to the console with timestamps
* 'noopWAINotifier' — a WAI notifier that performs no action

-}

module Keter.RateLimiter.Notifications
  ( Notifier(..)
  , notify
  , noopNotifier
  , consoleNotifier
  , WAINotifier
  , notifyWAI
  , waiNotifier
  , convertWAIRequest
  , consoleWAINotifier
  , noopWAINotifier
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Network.Wai (Request)
import qualified Network.Wai as Wai
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Encoding as TE

-- | Abstract notifier type encapsulating notification details and action.
--
-- The 'Notifier' type holds a name and an action function.
-- The action function is called when a rate limit event occurs and receives:
--
--   * The name of the throttle (e.g., "loginAttempts")
--   * The action taken (e.g., "blocked")
--   * The item related to the event, typically request details as 'Text'
--   * The limit that was exceeded as an 'Int'
--
-- This abstraction allows different notification strategies to be implemented and swapped easily.
data Notifier = Notifier
  { notifierName :: Text
    -- ^ Identifier of the notifier, useful for logging or selection.
  , notifierAction :: Text -> Text -> Text -> Int -> IO ()
    -- ^ Function to execute the notification.
    --   Arguments are: throttle name, action, item, and limit.
  }

-- | WAI-specific notifier type for handling WAI Request objects directly.
--
-- This type is specialized for WAI applications and receives:
--
--   * The name of the throttle (e.g., "loginAttempts")
--   * The WAI 'Request' object
--   * The limit that was exceeded as an 'Int'
type WAINotifier = Text -> Request -> Int -> IO ()

-- | Notify about a rate limit event.
--
-- This function takes a 'Notifier', the throttle name, the request or item involved,
-- and the limit that was exceeded, then triggers the notifier's action.
--
-- The request is converted to 'Text' via 'show' and 'T.pack'.
--
-- ==== __Example__
--
-- > notify consoleNotifier "apiRequests" someRequest 50
notify :: Show req => Notifier -> Text -> req -> Int -> IO ()
notify notifier throttleName req limit =
  notifierAction notifier throttleName "blocked" (T.pack (show req)) limit

-- | Notify about a rate limit event using a WAI-specific notifier.
--
-- This function takes a 'WAINotifier', the throttle name, a WAI 'Request',
-- and the limit that was exceeded, then triggers the WAI notifier.
--
-- ==== __Example__
--
-- > notifyWAI myWAINotifier "apiRequests" waiRequest 50
notifyWAI :: WAINotifier -> Text -> Request -> Int -> IO ()
notifyWAI waiNotifierFunc throttleName waiReq limit =
  waiNotifierFunc throttleName waiReq limit

-- | Adapter function to use a regular 'Notifier' with WAI 'Request' objects.
--
-- This function converts a WAI 'Request' to 'Text' representation and then
-- uses the provided 'Notifier' to handle the notification.
--
-- ==== __Example__
--
-- > waiNotifier consoleNotifier "loginAttempts" waiRequest 10
waiNotifier :: Notifier -> Text -> Request -> Int -> IO ()
waiNotifier notifier throttleName waiReq limit = 
  notifierAction notifier throttleName "blocked" (convertWAIRequest waiReq) limit

-- | Convert a WAI 'Request' to a 'Text' representation.
--
-- This function extracts key information from a WAI request including:
-- method, path, query string, and remote host.
--
-- ==== __Example output__
--
-- > "GET /api/users?limit=10 from 192.168.1.100"
convertWAIRequest :: Request -> Text
convertWAIRequest req = 
  let method = TE.decodeUtf8 $ Wai.requestMethod req
      path = TE.decodeUtf8 $ Wai.rawPathInfo req
      query = TE.decodeUtf8 $ Wai.rawQueryString req
      remoteHost = " from " <> T.pack (show $ Wai.remoteHost req)
  in method <> " " <> path <> query <> remoteHost

-- | A 'Notifier' that performs no action.
--
-- Useful as a default or placeholder when notification is not required.
noopNotifier :: Notifier
noopNotifier = Notifier
  { notifierName = "noop"
  , notifierAction = \_ _ _ _ -> return ()
  }

-- | A 'Notifier' that logs notifications to the console.
--
-- Each notification is printed with a timestamp, throttle name, action,
-- item details, and the limit exceeded.
--
-- ==== __Example output__
--
-- > 2025-06-12 20:31:00 - loginAttempts blocked User {userId = 123} (limit: 10)
consoleNotifier :: Notifier
consoleNotifier = Notifier
  { notifierName = "console"
  , notifierAction = \throttle action item limit -> do
      now <- getCurrentTime
      let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
      TIO.putStrLn $
        T.pack timestamp <> " - " <> throttle <> " " <>
        action <> " " <> item <> " (limit: " <> T.pack (show limit) <> ")"
  }

-- | A 'WAINotifier' that performs no action.
--
-- Useful as a default or placeholder when WAI notification is not required.
noopWAINotifier :: WAINotifier
noopWAINotifier _ _ _ = return ()

-- | A 'WAINotifier' that logs notifications to the console.
--
-- Each notification is printed with a timestamp, throttle name,
-- WAI request details, and the limit exceeded.
--
-- ==== __Example output__
--
-- > 2025-06-12 20:31:00 - loginAttempts blocked GET /api/login from 192.168.1.100:8080 (limit: 10)
consoleWAINotifier :: WAINotifier
consoleWAINotifier throttleName req limit = do
  now <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
      requestInfo = convertWAIRequest req
  TIO.putStrLn $
    T.pack timestamp <> " - " <> throttleName <> " blocked " <>
    requestInfo <> " (limit: " <> T.pack (show limit) <> ")"
