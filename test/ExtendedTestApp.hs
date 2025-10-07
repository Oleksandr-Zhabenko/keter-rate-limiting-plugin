{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import System.Environment (lookupEnv)
import System.IO (hSetBuffering, stdout, stderr, BufferMode(LineBuffering), hPutStrLn)
import Text.Read (readMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Maybe (fromMaybe, listToMaybe)
import Data.CaseInsensitive (mk)
import Network.Socket (SockAddr(..))
import qualified Data.ByteString.Lazy.Char8 as LBS
import Control.Monad (guard)

import Keter.RateLimiter.WAI
  ( attackMiddleware, Env, initConfig, addThrottle, ThrottleConfig(..), IdentifierBy(..))
import Keter.RateLimiter.Cache (Algorithm(..))
import Keter.RateLimiter.IPZones (IPZoneIdentifier, defaultIPZone)

getClientIdentifier :: Request -> IO (Maybe T.Text)
getClientIdentifier req = pure . Just $ fromMaybe (fallbackIP req) (findClientIP req)
  where
    findClientIP :: Request -> Maybe T.Text
    findClientIP r = listToMaybe $ do
      (headerName, headerValue) <- requestHeaders r
      guard $ headerName `elem` [mk "x-forwarded-for", mk "x-real-ip"]
      pure . T.strip . fst . T.breakOn "," $ TE.decodeUtf8 headerValue

    fallbackIP :: Request -> T.Text
    fallbackIP = T.pack . show . remoteHost

getRequestIPZone :: Request -> IPZoneIdentifier
getRequestIPZone _ = defaultIPZone

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  
  portStr <- lookupEnv "PORT"
  let port = maybe 8080 id (portStr >>= readMaybe)

  env <- initConfig getRequestIPZone

  let throttleConfig = ThrottleConfig
        { throttleLimit        = 4
        , throttlePeriod       = 10
        , throttleAlgorithm    = FixedWindow
        , throttleIdentifierBy = IdIP
        , throttleTokenBucketTTL = Nothing
        }

  envWithThrottle <- addThrottle env "test-throttle" throttleConfig

  hPutStrLn stderr $ "Starting Haskell Warp server on port " ++ show port
  run port (attackMiddleware envWithThrottle baseApp)

baseApp :: Application
baseApp req respond = do
  mIdentifier <- getClientIdentifier req
  hPutStrLn stdout $ "Received request from " ++ T.unpack (fromMaybe "unknown" mIdentifier)

  respond $ responseLBS
    status200
    [("Content-Type", "text/plain")]
    "Hello from a Haskell/WAI/Warp application with rate limiting!"
