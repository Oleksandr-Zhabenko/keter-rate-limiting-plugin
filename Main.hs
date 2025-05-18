{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Keter.RateLimiter
import Keter.RateLimiter.Notifications
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (forM_)
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  putStrLn "Rate Limiter Test Application"
  
  -- Create a configuration with a simple IP-based throttle
  let ipThrottleConfig = ThrottleConfig
        { throttleLimit = 5
        , throttlePeriod = 60  -- 60 seconds
        , throttleCondition = const True  -- Apply to all requests
        , throttleKeyFn = requestIP  -- Key by IP address
        }
      
      -- Path-specific throttle
      loginThrottleConfig = ThrottleConfig
        { throttleLimit = 3
        , throttlePeriod = 300  -- 5 minutes
        , throttleCondition = \req -> requestPath req == "/login"
        , throttleKeyFn = \req -> requestIP req <> ":" <> requestPath req
        }
  
  -- Create configuration with console notifications
  let config = defaultConfiguration
        { configNotifier = consoleNotifier }
        `addThrottle` "ip-throttle" $ ipThrottleConfig
        `addThrottle` "login-throttle" $ loginThrottleConfig
  
  -- Initialize environment
  env <- initConfig config
  
  -- Simulate some requests
  let ips = ["192.168.1.1", "192.168.1.2", "192.168.1.1", "192.168.1.1", 
             "192.168.1.1", "192.168.1.1", "192.168.1.1"]
      
      -- Create requests with different IPs
      requests = map (\ip -> Request 
                      { requestMethod = "GET"
                      , requestPath = "/login"
                      , requestHost = "example.com"
                      , requestIP = T.pack ip
                      , requestHeaders = Map.empty
                      }) ips
  
  -- Process each request and show result
  forM_ (zip [1..] requests) $ \(i, req) -> do
    putStrLn $ "Request " ++ show i ++ " (IP: " ++ T.unpack (requestIP req) ++ ")"
    response <- attackMiddleware env req (return "Success")
    putStrLn $ "Response: " ++ T.unpack response
    putStrLn ""