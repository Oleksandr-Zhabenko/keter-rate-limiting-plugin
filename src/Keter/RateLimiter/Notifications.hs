{-|
Copyright (c) 2025 Oleksandr Zhabenko
  
This file is a ported to Haskell language code with some simlifications of Ruby on Rails
https://github.com/rails/rails 
https://github.com/rails/rails/blob/2318163b4b9e9604b557057c0465e2a5ef162401/activesupport/lib/active_support/notifications.rb
and is based on the structure of the original code of 
rack-attack, Copyright (c) David Heinemeier Hansson, under the MIT License.

This implementation is released under the MIT License.
 -}


{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NumericUnderscores #-}


module Keter.RateLimiter.Notifications
  ( Subscriber
  , Event(..)
  , newSubscriber
  , notifySubscribers
  , subscribe
  , unsubscribe
  , instrumentNotification
  ) where

import Data.Unique (Unique, newUnique)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, readMVar)
import Data.Text (Text)
import qualified Data.Text as T

-- | Event type representing different attack notifications
data Event
  = RateLimiterDetected Text  -- Example: RateLimiterDetected "IP address"
  | ThresholdExceeded Text
  deriving (Show, Eq)

-- | A subscriber that listens for events
data Subscriber = Subscriber
  { subscriberId       :: Unique               -- Unique ID for comparison
  , subscriberCallback :: Event -> IO ()       -- Callback function
  }

instance Eq Subscriber where
  s1 == s2 = subscriberId s1 == subscriberId s2

-- | Create a new subscriber with a unique ID
newSubscriber :: (Event -> IO ()) -> IO Subscriber
newSubscriber callback = do
  uniqueId <- newUnique  -- Generate a unique identifier
  return $ Subscriber uniqueId callback

-- | List of subscribers stored in an MVar for safe concurrent access
type Subscribers = MVar [Subscriber]

-- | Notify all subscribers about an event
notifySubscribers :: Subscribers -> Event -> IO ()
notifySubscribers subscribers event = do
  subs <- readMVar subscribers
  mapM_ (\s -> subscriberCallback s event) subs

-- | Subscribe to events
subscribe :: Subscribers -> (Event -> IO ()) -> IO Subscriber
subscribe subscribers callback = do
  sub <- newSubscriber callback
  modifyMVar_ subscribers $ \subs -> return (sub : subs)
  return sub

-- | Unsubscribe a subscriber
unsubscribe :: Subscribers -> Subscriber -> IO ()
unsubscribe subscribers sub = do
  modifyMVar_ subscribers $ \subs -> return (filter (/= sub) subs)

-- | A function to send a notification with an instrumented payload.
instrumentNotification :: Text -> [(Text, Text)] -> (Event -> IO ()) -> IO ()
instrumentNotification eventType payload callback = do
  let event = RateLimiterDetected (T.intercalate ", " (map snd payload))
  callback event

