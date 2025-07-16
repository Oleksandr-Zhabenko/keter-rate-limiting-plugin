{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Keter.RateLimiter.Notifications
Description : Notification system for rate limiting events
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : stable
Portability : portable
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
import qualified Data.Text.Encoding as TE
import Keter.RateLimiter.RequestUtils (getClientIP, getRequestMethod, getRequestPath)
import System.IO.Unsafe (unsafePerformIO)

-- | Abstract notifier type encapsulating notification details and action.
data Notifier = Notifier
  { notifierName :: Text
  , notifierAction :: Text -> Text -> Text -> Int -> IO ()
  }

-- | WAI-specific notifier type for handling WAI Request objects directly.
type WAINotifier = Text -> Request -> Int -> IO ()

-- | Notify about a rate limit event.
notify :: Show req => Notifier -> Text -> req -> Int -> IO ()
notify notifier throttleName req limit =
  notifierAction notifier throttleName "blocked" (T.pack (show req)) limit

-- | Notify about a rate limit event using a WAI-specific notifier.
notifyWAI :: WAINotifier -> Text -> Request -> Int -> IO ()
notifyWAI waiNotifierFunc throttleName waiReq limit =
  waiNotifierFunc throttleName waiReq limit

-- | Adapter function to use a regular 'Notifier' with WAI 'Request' objects.
waiNotifier :: Notifier -> Text -> Request -> Int -> IO ()
waiNotifier notifier throttleName waiReq limit = 
  notifierAction notifier throttleName "blocked" (convertWAIRequest waiReq) limit

-- | Convert a WAI 'Request' to a 'Text' representation.
convertWAIRequest :: Request -> Text
convertWAIRequest req = 
  let method = getRequestMethod req
      path = getRequestPath req
      query = TE.decodeUtf8 $ Wai.rawQueryString req
      clientIP = unsafePerformIO $ getClientIP req
  in method <> " " <> path <> query <> " from " <> clientIP

-- | A 'Notifier' that performs no action.
noopNotifier :: Notifier
noopNotifier = Notifier
  { notifierName = "noop"
  , notifierAction = \_ _ _ _ -> return ()
  }

-- | A 'Notifier' that logs notifications to the console.
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
noopWAINotifier :: WAINotifier
noopWAINotifier _ _ _ = return ()

-- | A 'WAINotifier' that logs notifications to the console.
consoleWAINotifier :: WAINotifier
consoleWAINotifier throttleName req limit = do
  now <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
      requestInfo = convertWAIRequest req
  TIO.putStrLn $
    T.pack timestamp <> " - " <> throttleName <> " blocked " <>
    requestInfo <> " (limit: " <> T.pack (show limit) <> ")"
