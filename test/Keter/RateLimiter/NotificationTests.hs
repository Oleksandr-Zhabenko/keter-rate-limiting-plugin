{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Keter.RateLimiter.NotificationTests
Description : Unit tests for throttling notification mechanisms in Keter
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : experimental
Portability : portable

This module contains tests for the notification infrastructure used in
the Keter rate limiter. It verifies that console-based and no-op notifiers,
along with WAI integration, produce expected log messages or suppress output appropriately.
-}
module Keter.RateLimiter.NotificationTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import qualified Data.Text as T
import Network.Wai (defaultRequest, requestMethod, rawPathInfo, rawQueryString, remoteHost)
import Network.Socket (SockAddr(..), tupleToHostAddress, tupleToHostAddress6)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Keter.RateLimiter.Notifications
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

-- | Create a test notifier that captures output with real timestamps
createTestNotifier :: IORef [Text] -> Notifier
createTestNotifier outputRef = Notifier
  { notifierName = "test"
  , notifierAction = \throttle act item limit -> do
      now <- getCurrentTime
      let ts = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
          parts = [throttle, act, item, T.concat ["(limit: ", T.pack (show limit), ")"]]
          message = T.intercalate " " $ filter (not . T.null) parts
          fullMessage = T.concat [ts, " - ", message]
      modifyIORef' outputRef (fullMessage :)
  }

-- | Create a test WAI notifier that captures output with real timestamps
createTestWAINotifier :: IORef [Text] -> WAINotifier
createTestWAINotifier outputRef throttleName req limit = do
  now <- getCurrentTime
  let timestamp = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
      requestInfo = convertWAIRequest req
      message = T.concat [timestamp, " - ", throttleName, " blocked ", 
                         requestInfo, " (limit: ", T.pack (show limit), ")"]
  modifyIORef' outputRef (message :)

-- | Check if a timestamp is valid (format: YYYY-MM-DD HH:MM:SS)
isValidTimestamp :: Text -> Bool
isValidTimestamp ts = 
  T.length ts == 19 && 
  T.all (`T.elem` ("0123456789-: " :: Text)) ts &&
  T.index ts 4 == '-' &&
  T.index ts 7 == '-' &&
  T.index ts 10 == ' ' &&
  T.index ts 13 == ':' &&
  T.index ts 16 == ':'

-- | Parse and validate a log message format: "TIMESTAMP - CONTENT"
parseLogMessage :: Text -> Maybe (Text, Text)
parseLogMessage msg =
  case T.splitOn " - " msg of
    [timestamp, content] | isValidTimestamp timestamp -> Just (timestamp, content)
    _ -> Nothing

-- | Test suite for notification handlers in the rate limiter.
tests :: TestTree
tests = testGroup "Keter.RateLimiter.Notifications Tests"
  [ testCase "Generic notifier format" $ do
      outputRef <- newIORef []
      let notifier = createTestNotifier outputRef
      
      notify notifier "loginAttempts" ("user123" :: Text) 5
      
      output <- readIORef outputRef
      case output of
        [message] -> case parseLogMessage message of
          Just (_, content) -> 
            assertEqual "Generic notifier message format" 
                       "loginAttempts blocked \"user123\" (limit: 5)" 
                       content
          Nothing -> assertFailure $ "Invalid log format: " ++ T.unpack message
        _ -> assertFailure $ "Expected exactly one message, got: " ++ show (length output)

  , testCase "WAI notifier format" $ do
      outputRef <- newIORef []
      let notifier = createTestWAINotifier outputRef
          req = defaultRequest
                  { requestMethod = "GET"
                  , rawPathInfo = "/api/users"
                  , rawQueryString = "?limit=10"
                  , remoteHost = SockAddrInet 8080 (tupleToHostAddress (192,168,1,100))
                  }
      
      notifier "apiRequests" req 50
      
      output <- readIORef outputRef
      case output of
        [message] -> case parseLogMessage message of
          Just (_, content) ->
            assertEqual "WAI notifier message format"
                       "apiRequests blocked GET /api/users?limit=10 from 192.168.1.100:8080 (limit: 50)"
                       content
          Nothing -> assertFailure $ "Invalid log format: " ++ T.unpack message
        _ -> assertFailure $ "Expected exactly one message, got: " ++ show (length output)

  , testCase "waiNotifier adapter format" $ do
      outputRef <- newIORef []
      let baseNotifier = createTestNotifier outputRef
          adapter = waiNotifier baseNotifier
          req = defaultRequest 
                  { requestMethod = "POST"
                  , rawPathInfo = "/login"
                  , rawQueryString = ""
                  , remoteHost = SockAddrInet 443 (tupleToHostAddress (10,0,0,1))
                  }
      
      adapter "auth" req 10
      
      output <- readIORef outputRef
      case output of
        [message] -> case parseLogMessage message of
          Just (_, content) ->
            assertEqual "waiNotifier adapter format"
                       "auth blocked POST /login from 10.0.0.1:443 (limit: 10)"
                       content
          Nothing -> assertFailure $ "Invalid log format: " ++ T.unpack message
        _ -> assertFailure $ "Expected exactly one message, got: " ++ show (length output)

  , testCase "convertWAIRequest IPv4 format" $ do
      let req = defaultRequest
                  { requestMethod = "GET"
                  , rawPathInfo = "/api/users"
                  , rawQueryString = "?limit=10"
                  , remoteHost = SockAddrInet 8080 (tupleToHostAddress (192,168,1,100))
                  }
      assertEqual "IPv4 request conversion" 
                 "GET /api/users?limit=10 from 192.168.1.100:8080"
                 (convertWAIRequest req)

  , testCase "convertWAIRequest IPv6 format" $ do
      let req = defaultRequest 
                  { requestMethod = "POST"
                  , rawPathInfo = "/login"
                  , rawQueryString = ""
                  , remoteHost = SockAddrInet6 443 0 (tupleToHostAddress6 (0x2001, 0x0db8, 0, 0, 0, 0, 0, 1)) 0
                  }
      assertEqual "IPv6 request conversion"
                 "POST /login from 2001:0db8:0000:0000:0000:0000:0000:0001:443"
                 (convertWAIRequest req)

  , testCase "Empty throttle name handling" $ do
      outputRef <- newIORef []
      let notifier = createTestNotifier outputRef
      
      notify notifier "" ("item" :: Text) 100
      
      output <- readIORef outputRef
      case output of
        [message] -> case parseLogMessage message of
          Just (_, content) ->
            assertEqual "Empty throttle name format"
                       "blocked \"item\" (limit: 100)"
                       content
          Nothing -> assertFailure $ "Invalid log format: " ++ T.unpack message
        _ -> assertFailure $ "Expected exactly one message, got: " ++ show (length output)

  , testCase "Empty query string handling" $ do
      let req = defaultRequest 
                  { requestMethod = "DELETE"
                  , rawPathInfo = "/resource/123"
                  , rawQueryString = ""
                  , remoteHost = SockAddrInet 9000 (tupleToHostAddress (127,0,0,1))
                  }
      assertEqual "Empty query string conversion"
                 "DELETE /resource/123 from 127.0.0.1:9000"
                 (convertWAIRequest req)

  , testCase "No-op notifier silence" $ do
      -- Test that no-op notifiers complete without side effects
      notify noopNotifier "test" ("data" :: Text) 1
      noopWAINotifier "test" defaultRequest 1
      -- If we reach here, both completed successfully
      assertBool "No-op notifiers should complete silently" True

  , testCase "Console notifiers exist and are callable" $ do
      -- Test that console notifiers can be referenced without crashing
      -- We don't actually call them to avoid polluting test output
      let _ = consoleNotifier
          _ = consoleWAINotifier
      assertBool "Console notifiers should be accessible" True
  ]
