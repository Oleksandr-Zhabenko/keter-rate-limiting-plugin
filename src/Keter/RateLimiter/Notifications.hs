{-# LANGUAGE OverloadedStrings #-}

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
import Control.Monad.IO.Class (MonadIO, liftIO)

-- | Abstract notifier type
data Notifier = Notifier
  { notifierName :: Text
  , notifierAction :: Text -> Text -> Text -> Int -> IO ()
  -- Arguments: throttleName, action, item (e.g., request info), limit
  }

-- | Notify about a rate limit event, using request details
notify :: Show req => Notifier -> Text -> req -> Int -> IO ()
notify notifier throttleName req limit =
  notifierAction notifier throttleName "blocked" (T.pack (show req)) limit

-- | Notifier that does nothing
noopNotifier :: Notifier
noopNotifier = Notifier
  { notifierName = "noop"
  , notifierAction = \_ _ _ _ -> return ()
  }

-- | Notifier that logs to the console with timestamp and details
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
