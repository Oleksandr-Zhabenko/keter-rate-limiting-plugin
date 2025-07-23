{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Keter.RateLimiter.NotificationTests
Description : Unit tests for throttling notification mechanisms in Keter
Copyright   : (c) 2025 Oleksandr Zhabenko
License     : MIT
Maintainer  : oleksandr.zhabenko@yahoo.com
Stability   : experimental
Portability : POSIX

This module contains tests for the notification infrastructure used in
the Keter rate limiter. It verifies that console-based and no-op notifiers,
along with WAI integration, produce expected log messages or suppress output appropriately.
-}
module Keter.RateLimiter.NotificationTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS8
import Network.Wai (defaultRequest, requestMethod, rawPathInfo, rawQueryString, remoteHost)
import Network.Socket (SockAddr(..), tupleToHostAddress, tupleToHostAddress6)
import System.IO (hClose, stdout, hFlush)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO.Temp (withSystemTempFile)
import Control.Monad (when)
import Keter.RateLimiter.Notifications

-- | Capture stdout output during the execution of an action.
--
-- This helper is used to assert on log output produced by notifiers.
-- It ensures `stdout` is temporarily redirected and restored afterward.
withCapturedOutput :: String   -- ^ Test name (used for debugging output)
                   -> IO ()    -- ^ The IO action to run
                   -> IO Text  -- ^ The captured output (only first log line if matched)
withCapturedOutput testName action = do
  withSystemTempFile "test-output" $ \filePath handle -> do
    oldStdout <- hDuplicate stdout
    hFlush stdout
    hDuplicateTo handle stdout
    action
    hFlush handle
    output <- T.pack <$> readFile filePath
    hDuplicateTo oldStdout stdout
    hClose handle
    hClose oldStdout
    let lines = T.lines output
    when (not $ null lines) $ putStrLn $ "DEBUG: withCapturedOutput for " ++ testName ++ ": lines=" ++ show (map T.unpack lines)
    let logLine = case filter (\line -> not (T.null line) && T.isInfixOf " - " line) lines of
                    [] -> T.empty
                    (line:_) -> line
    putStrLn $ "DEBUG: withCapturedOutput for " ++ testName ++ ": selected logLine=" ++ T.unpack logLine
    return $ if T.null logLine then logLine else logLine <> "\n"

-- | Test suite for console, no-op, and WAI notification handlers in the rate limiter.
tests :: TestTree
tests = testGroup "Keter.RateLimiter.Notifications Tests"
  [ testCase "Console notifier logs correctly" $ do
      let user = "user123"
          throttleName = "loginAttempts"
      output <- withCapturedOutput "Console notifier logs correctly" $
        notify consoleNotifier throttleName user 5
      let (timestamp, rest) = T.breakOnEnd " - " output
          trimmedTimestamp = T.strip $ T.dropEnd 2 timestamp
          message = rest
          expectedMessage = throttleName <> " blocked " <> user <> " (limit: 5)\n"
          isValidTimestamp = T.length trimmedTimestamp == 19 &&
                             T.all (`T.elem` ("0123456789-: " :: Text)) trimmedTimestamp
      when (not isValidTimestamp && not (T.null trimmedTimestamp)) $
        putStrLn $ "Invalid timestamp in Console notifier..."
      assertBool "Output must start with throttleName" (T.isPrefixOf throttleName rest)
      assertEqual "Message body mismatch" expectedMessage message

  , testCase "Console WAI notifier logs correctly" $ do
      let req = defaultRequest
                  { requestMethod = "GET"
                  , rawPathInfo = "/api/users"
                  , rawQueryString = "?limit=10"
                  , remoteHost = SockAddrInet 8080 (tupleToHostAddress (192,168,1,100))
                  }
          throttleName = "apiRequests"
      output <- withCapturedOutput "Console WAI notifier logs correctly" $
        notifyWAI consoleWAINotifier throttleName req 50
      let (timestamp, rest) = T.breakOnEnd " - " output
          trimmedTimestamp = T.strip $ T.dropEnd 2 timestamp
          message = rest
          expectedMessage = throttleName <> " blocked GET /api/users?limit=10 from 192.168.1.100:8080 (limit: 50)\n"
          isValidTimestamp = T.length trimmedTimestamp == 19 &&
                             T.all (`T.elem` ("0123456789-: " :: Text)) trimmedTimestamp
      when (not isValidTimestamp && not (T.null trimmedTimestamp)) $
        putStrLn "Invalid timestamp in Console WAI notifier..."
      assertBool "Output must start with throttleName" (T.isPrefixOf throttleName rest)
      assertEqual "WAI message body mismatch" expectedMessage message

  , testCase "WAI notifier adapter works with consoleNotifier" $ do
      let req = defaultRequest { requestMethod = BS8.pack "POST", rawPathInfo = BS8.pack "/login", rawQueryString = BS8.pack "", remoteHost = SockAddrInet 8080 (tupleToHostAddress (192,168,1,100)) }
      let throttleName = "loginAttempts"
      output <- withCapturedOutput "WAI notifier adapter works with consoleNotifier" $ notifyWAI (waiNotifier consoleNotifier) throttleName req 10
      let (timestamp, rest) = if T.isInfixOf " - " output then T.breakOnEnd " - " output else (T.empty, output)
      let trimmedTimestamp = T.strip $ T.dropEnd 2 timestamp
      let message = rest
      let expectedMessage = throttleName <> " blocked POST /login from 192.168.1.100:8080 (limit: 10)\n"
      let isValidTimestamp = T.length trimmedTimestamp == 19 && T.all (`T.elem` ("0123456789-: " :: Text)) trimmedTimestamp
      when (not isValidTimestamp && not (T.null trimmedTimestamp)) $
        putStrLn $ "Invalid timestamp in WAI notifier adapter: length=" ++ show (T.length trimmedTimestamp) ++ ", timestamp=" ++ show (T.unpack trimmedTimestamp) ++ ", raw timestamp=" ++ show (T.unpack timestamp) ++ ", rest=" ++ show (T.unpack rest) ++ ", message=" ++ show (T.unpack message) ++ ", raw output=" ++ show (T.unpack output)
      assertBool ("WAI notifier adapter rest does not start with throttleName: rest=" ++ T.unpack rest ++ ", expected throttleName=" ++ T.unpack throttleName) (T.isPrefixOf throttleName rest)
      assertEqual "WAI adapter output message mismatch" expectedMessage message

  , testCase "No-op notifier produces no output" $ do
      let user = "user123"
      output <- withCapturedOutput "No-op notifier" $
        notify noopNotifier "loginAttempts" user 5
      assertEqual "Expected no output" T.empty output

  , testCase "No-op WAI notifier produces no output" $ do
      let req = defaultRequest
                  { requestMethod = "GET"
                  , rawPathInfo = "/api/users"
                  , rawQueryString = "?limit=10"
                  , remoteHost = SockAddrInet 8080 (tupleToHostAddress (192,168,1,100))
                  }
      output <- withCapturedOutput "No-op WAI notifier" $
        notifyWAI noopWAINotifier "apiRequests" req 50
      assertEqual "Expected no output" T.empty output

  , testCase "convertWAIRequest produces correct output" $ do
      let req = defaultRequest
                  { requestMethod = "GET"
                  , rawPathInfo = "/api/users"
                  , rawQueryString = "?limit=10"
                  , remoteHost = SockAddrInet 8080 (tupleToHostAddress (192,168,1,100))
                  }
      convertWAIRequest req @?= "GET /api/users?limit=10 from 192.168.1.100:8080"

  , testCase "Edge case: empty throttle name" $ do
      let user = "user123"
      let throttleName = ""
      output <- withCapturedOutput "Edge case: empty throttle name" $ notify consoleNotifier throttleName user 5
      let (timestamp, rest) = if T.isInfixOf " - " output then T.breakOnEnd " - " output else (T.empty, output)
      let trimmedTimestamp = T.strip $ T.dropEnd 2 timestamp
      let message = rest
      let expectedMessage = " blocked user123 (limit: 5)\n"
      let isValidTimestamp = T.length trimmedTimestamp == 19 && T.all (`T.elem` ("0123456789-: " :: Text)) trimmedTimestamp
      when (not isValidTimestamp && not (T.null trimmedTimestamp)) $
        putStrLn $ "Invalid timestamp in empty throttle name: length=" ++ show (T.length trimmedTimestamp) ++ ", timestamp=" ++ show (T.unpack trimmedTimestamp) ++ ", raw timestamp=" ++ show (T.unpack timestamp) ++ ", rest=" ++ show (T.unpack rest) ++ ", message=" ++ show (T.unpack message) ++ ", raw output=" ++ show (T.unpack output)
      assertBool ("Empty throttle name rest does not start with empty string: rest=" ++ T.unpack rest) (T.isPrefixOf throttleName rest)
      assertEqual "Empty throttle name output mismatch" expectedMessage message

  , testCase "Edge case: WAI request with empty query string" $ do
      let req = defaultRequest { requestMethod = BS8.pack "GET", rawPathInfo = BS8.pack "/api", rawQueryString = BS8.pack "", remoteHost = SockAddrInet 8080 (tupleToHostAddress (192,168,1,100)) }
      let throttleName = "apiRequests"
      output <- withCapturedOutput "Edge case: WAI request with empty query string" $ notifyWAI consoleWAINotifier throttleName req 25
      let (timestamp, rest) = if T.isInfixOf " - " output then T.breakOnEnd " - " output else (T.empty, output)
      let trimmedTimestamp = T.strip $ T.dropEnd 2 timestamp
      let message = rest
      let expectedMessage = throttleName <> " blocked GET /api from 192.168.1.100:8080 (limit: 25)\n"
      let isValidTimestamp = T.length trimmedTimestamp == 19 && T.all (`T.elem` ("0123456789-: " :: Text)) trimmedTimestamp
      when (not isValidTimestamp && not (T.null trimmedTimestamp)) $
        putStrLn $ "Invalid timestamp in WAI empty query string: length=" ++ show (T.length trimmedTimestamp) ++ ", timestamp=" ++ show (T.unpack trimmedTimestamp) ++ ", raw timestamp=" ++ show (T.unpack timestamp) ++ ", rest=" ++ show (T.unpack rest) ++ ", message=" ++ show (T.unpack message) ++ ", raw output=" ++ show (T.unpack output)
      assertBool ("WAI empty query string rest does not start with throttleName: rest=" ++ T.unpack rest) (T.isPrefixOf throttleName rest)
      assertEqual "Empty query string output mismatch" expectedMessage message

  , testCase "convertWAIRequest produces correct output for IPv6" $ do
      let req = defaultRequest { requestMethod = BS8.pack "GET", rawPathInfo = BS8.pack "/api/users", rawQueryString = BS8.pack "?limit=10", remoteHost = SockAddrInet6 8080 0 (tupleToHostAddress6 (0x2001, 0x0db8, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0001)) 0 }
      convertWAIRequest req @?= "GET /api/users?limit=10 from 2001:0db8:0000:0000:0000:0000:0000:0001:8080"

  , testCase "Console WAI notifier logs correctly for IPv6" $ do
      let req = defaultRequest { requestMethod = BS8.pack "POST", rawPathInfo = BS8.pack "/login", rawQueryString = BS8.pack "", remoteHost = SockAddrInet6 8080 0 (tupleToHostAddress6 (0x2001, 0x0db8, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0001)) 0 }
      let throttleName = "loginAttempts"
      output <- withCapturedOutput "Console WAI notifier logs correctly for IPv6" $ notifyWAI consoleWAINotifier throttleName req 50
      let (timestamp, rest) = if T.isInfixOf " - " output then T.breakOnEnd " - " output else (T.empty, output)
      let trimmedTimestamp = T.strip $ T.dropEnd 2 timestamp
      let message = rest
      let expectedMessage = throttleName <> " blocked POST /login from 2001:0db8:0000:0000:0000:0000:0000:0001:8080 (limit: 50)\n"
      let isValidTimestamp = T.length trimmedTimestamp == 19 && T.all (`T.elem` ("0123456789-: " :: Text)) trimmedTimestamp
      when (not isValidTimestamp && not (T.null trimmedTimestamp)) $
        putStrLn $ "Invalid timestamp in Console WAI notifier for IPv6: length=" ++ show (T.length trimmedTimestamp) ++ ", timestamp=" ++ show (T.unpack trimmedTimestamp) ++ ", raw timestamp=" ++ show (T.unpack timestamp) ++ ", rest=" ++ show (T.unpack rest) ++ ", message=" ++ show (T.unpack message) ++ ", raw output=" ++ show (T.unpack output)
      assertBool ("Console WAI notifier rest does not start with throttleName: rest=" ++ T.unpack rest ++ ", expected throttleName=" ++ T.unpack throttleName) (T.isPrefixOf throttleName rest)
      assertEqual "Console WAI output message mismatch for IPv6" expectedMessage message
  ]
