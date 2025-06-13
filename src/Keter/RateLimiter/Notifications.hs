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

This module defines a 'Notifier' type that encapsulates the notification strategy and provides several implementations, including a no-operation notifier and a console logger notifier.

== Usage

To notify about a rate limit event, use the 'notify' function with an appropriate 'Notifier'. For example:

@
notify consoleNotifier "loginAttempts" userRequest 100
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

-}

module Keter.RateLimiter.Notifications
  ( Notifier(..)
  , notify
  , noopNotifier
  , consoleNotifier
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

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
