{-|
Copyright (c) 2025 Oleksandr Zhabenko
  
This file is a ported to Haskell language code with some simlifications of rack-attack 
https://github.com/rack/rack-attack/blob/main/lib/rack/attack.rb
and is based on the structure of the original code of 
rack-attack, Copyright (c) 2016 by Kickstarter, PBC, under the MIT License.

This implementation is released under the MIT License.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module Keter.RateLimiter
  ( Env
  , Response
  , App
  , Request(..)
  , attackMiddleware
  , instrument
  , Configuration
  , defaultConfiguration
  , initConfig
  , addThrottle
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Hashable (Hashable(..))
import Data.Char (isSpace, toLower)
import Control.Exception (Exception)
import System.Clock (TimeSpec(..), toNanoSecs)
import Prelude hiding (lookup)

-- Import the Cache module
import Keter.RateLimiter.Cache (Cache, count, readCache, reset, InMemoryStore, newCache, createInMemoryStore)

-- Use Data.Text
import qualified Data.Text as T
import Data.Text (Text)

-- Import our Notifications module
import qualified Keter.RateLimiter.Notifications as Notifications

--------------------------------------------------------------------------------
-- Basic Types and Request Handling
--------------------------------------------------------------------------------
type Env = Map Text Text
type Response = (Int, [(Text, Text)], Text)
type App = Env -> IO Response

data Request = Request { reqEnv :: Env }
  deriving (Show)

newRequest :: Env -> Request
newRequest = Request

normalizePath :: Text -> Text
normalizePath path = path

--------------------------------------------------------------------------------
-- Error Types
--------------------------------------------------------------------------------
data RateLimiterError
  = Error Text
  | MisconfiguredStoreError Text
  | MissingStoreError Text
  | IncompatibleStoreError Text
  deriving (Show)

instance Exception RateLimiterError

--------------------------------------------------------------------------------
-- Global Settings and Configuration
--------------------------------------------------------------------------------
enabled :: Bool
enabled = True

throttleDiscriminatorNormalizer :: Text -> Text
throttleDiscriminatorNormalizer discriminator =
  T.map toLower (T.strip discriminator)

-- | The configuration now also holds a cache and a list of throttle rules.
data Configuration = Configuration
  { safelisted           :: Request -> Bool
  , blocklisted          :: Request -> Bool
  , throttled            :: Request -> IO Bool
  , tracked              :: Request -> IO ()
  , blocklistedResponse  :: Maybe (Env -> IO Response)
  , blocklistedResponder :: Request -> IO Response
  , throttledResponse    :: Maybe (Env -> IO Response)
  , throttledResponder   :: Request -> IO Response
  , configThrottles      :: [Throttle]
  , cache                :: Cache InMemoryStore
  }

-- | A throttle rule.
data Throttle = Throttle
  { throttleName          :: Text
  , throttleLimit         :: Int
  , throttlePeriod        :: TimeSpec
  , throttleDiscriminator :: Request -> Text
  , throttleMatch         :: Request -> Bool
  }

-- | Check if a request is throttled by applying all throttle rules.
defaultThrottled :: Configuration -> Request -> IO Bool
defaultThrottled config req = do
    let applicable = filter (\t -> throttleMatch t req) (configThrottles config)
    results <- mapM (checkThrottle config req) applicable
    return $ or results
  where
    checkThrottle :: Configuration -> Request -> Throttle -> IO Bool
    checkThrottle config req throttle = do
      let key = throttleDiscriminator throttle req <> "_" <> throttleName throttle
          periodSeconds = fromIntegral (toNanoSecs (throttlePeriod throttle) `div` 1_000_000_000)
      countVal <- count (cache config) key periodSeconds
      return $ countVal > throttleLimit throttle

-- | Create a default configuration given a cache.
defaultConfiguration :: Cache InMemoryStore -> Configuration
defaultConfiguration cacheInst = config
  where
    config = Configuration
      { safelisted           = \_ -> False
      , blocklisted          = \_ -> False
      , throttled            = defaultThrottled config
      , tracked              = \_ -> return ()
      , blocklistedResponse  = Nothing
      , blocklistedResponder = \_ -> return (403, [], "Blocked")
      , throttledResponse    = Nothing
      , throttledResponder   = \_ -> return (429, [], "Throttled")
      , configThrottles      = []
      , cache                = cacheInst
      }

initConfig :: IO Configuration
initConfig = do
    inMemoryStore <- createInMemoryStore
    let cacheInst = newCache "Keter.RateLimiter" inMemoryStore
    return $ defaultConfiguration cacheInst

-- | Add a throttle rule to an existing configuration.
addThrottle :: Text -> Int -> TimeSpec -> (Request -> Text) -> (Request -> Bool) -> Configuration -> Configuration
addThrottle name limit period discriminator matchFunc config =
    config { configThrottles = Throttle name limit period discriminator matchFunc : configThrottles config }

--------------------------------------------------------------------------------
-- Middleware
--------------------------------------------------------------------------------
attackMiddleware :: Configuration -> App -> App
attackMiddleware config app env = do
  if not enabled || Map.lookup "Keter.RateLimiter.called" env == Just "true"
    then app env
    else do
      let env1 = Map.insert "Keter.RateLimiter.called" "true" env
          path = fromMaybe "" (Map.lookup "PATH_INFO" env1)
          normalizedPath = normalizePath path
          env2 = Map.insert "PATH_INFO" normalizedPath env1
          req = newRequest env2
          -- Build a payload with Text values, not String
          payload = [("path", normalizedPath)]  -- Fixed: using Text not String
          
      -- Use separate notification and function call to avoid type issues
      Notifications.instrumentNotification "Keter.RateLimiter.request" payload (\_ -> return ())
      
      -- Process the request
      if safelisted config req
        then app env2
        else if blocklisted config req
          then case blocklistedResponse config of
                 Just respFunc -> respFunc env2
                 Nothing       -> blocklistedResponder config req
          else do
            isThrottled <- throttled config req
            if isThrottled
              then case throttledResponse config of
                     Just respFunc -> respFunc env2
                     Nothing       -> throttledResponder config req
              else do
                tracked config req
                app env2

--------------------------------------------------------------------------------
-- Instrumentation
--------------------------------------------------------------------------------
-- | A simple instrumentation function that wraps a request.
instrument :: Request -> IO ()
instrument req =
  -- Here we use the Notifications module with an empty list
  Notifications.instrumentNotification "Keter.RateLimiter" [] (\_ -> return ())
