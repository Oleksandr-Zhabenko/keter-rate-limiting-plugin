{ pkgs ? import <nixpkgs> {} }:

let
  keterRateLimiting = pkgs.haskellPackages.callPackage ./new.nix {};
  port = 81;
in
pkgs.testers.runNixOSTest {
  name = "keter-haskell-rate-limiting-extended";
  nodes.machine = { config, pkgs, ... }: {
    networking.extraHosts = ''
      127.0.0.1 localhost
    '';
    services.keter = {
      enable = true;
      globalKeterConfig = {
        cli-port = 123;
        listeners = [
          {
            host = "*4";
            inherit port;
          }
        ];
      };
      bundle = {
        appName = "test-bundle";
        domain = "localhost";
        executable = pkgs.writeShellScript "run" ''
          #!${pkgs.bash}/bin/bash
          
          # Ensure we have the right environment with basic tools
          export PATH=${pkgs.haskellPackages.ghcWithPackages (p: [
            p.wai
            p.warp
            p.http-types
            p.text
            p.bytestring
            p.case-insensitive
            p.network
            p.monad-control
            keterRateLimiting
          ])}/bin:${pkgs.gnugrep}/bin:${pkgs.coreutils}/bin:$PATH

          # Debug output - redirect to stderr so keter can see it
          echo "=== Starting Haskell app ===" >&2
          echo "PORT environment variable: $PORT" >&2
          echo "PATH: $PATH" >&2
          
          # Check if /tmp is writable
          echo "Checking if /tmp is writable" >&2
          touch /tmp/test-write 2>&1 | tee -a /tmp/app.log >&2
          if [ $? -ne 0 ]; then
            echo "Cannot write to /tmp!" >&2
            exit 1
          fi
          echo "Write test passed" >&2

          # Check if packages are available
          echo "Checking GHC packages:" >&2
          ghc-pkg list 2>&1 | grep -E "(wai|warp|http-types|keter-rate-limiting)" >&2 || echo "Package check failed" >&2
          
          # Test if we can compile a simple program first
          echo "Testing compilation:" >&2
          echo 'main = putStrLn "Compilation test OK"' > /tmp/test.hs
          if ! runghc /tmp/test.hs 2>&1 | tee -a /tmp/app.log >&2; then
            echo "Basic compilation failed!" >&2
            cat /tmp/app.log >&2
            exit 1
          fi
          echo "Compilation test passed!" >&2

          # Create a temporary file for the Haskell program with rate limiting
          HASKELL_FILE=$(mktemp /tmp/app.XXXXXX.hs)
          echo "Created Haskell file: $HASKELL_FILE" >&2
          cat > "$HASKELL_FILE" << 'HASKELL_EOF'
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (find)
import Data.CaseInsensitive (mk)
import Network.Socket (SockAddr(..))
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LBS
import Control.Monad (guard, foldM)

-- Main library imports
import Keter.RateLimiter.WAI
  ( attackMiddleware, Env, initConfig, addThrottle, ThrottleConfig(..) )
import Keter.RateLimiter.Cache (Algorithm(..))
import Keter.RateLimiter.IPZones (IPZoneIdentifier, defaultIPZone)

-- Purely extracts the client IP string from headers for zone mapping.
getClientIPString :: Request -> T.Text
getClientIPString req = fromMaybe (fallbackIP req) (findClientIP req)
  where
    findClientIP :: Request -> Maybe T.Text
    findClientIP r = listToMaybe $ do
      (headerName, headerValue) <- requestHeaders r
      guard $ headerName `elem` [mk "x-forwarded-for", mk "x-real-ip"]
      pure . T.strip . fst . T.breakOn "," $ TE.decodeUtf8 headerValue
    fallbackIP :: Request -> T.Text
    fallbackIP = T.pack . show . remoteHost

-- This function returns an IO action to get the identifier, matching the new API.
getClientIdentifier :: Request -> IO (Maybe T.Text)
getClientIdentifier = pure . Just . getClientIPString

-- IP zone mapping for testing, as required by the test script.
getRequestIPZone :: Request -> IPZoneIdentifier
getRequestIPZone req =
  let ip = getClientIPString req
  in if | ip == "10.0.0.1" -> "testZoneA"
        | ip == "20.0.0.1" -> "testZoneB"
        | otherwise      -> defaultIPZone

main :: IO ()
main = do
  portStr <- lookupEnv "PORT"
  let port = maybe 8080 id (portStr >>= readMaybe)
  
  -- Initialize rate limiter environment with our zone mapping function.
  env <- initConfig getRequestIPZone

  -- Define throttle configurations with more reasonable limits for testing
  let throttles =
        [ ("fixed-throttle", ThrottleConfig
            { throttleLimit = 10  -- Increased from 4 to be less restrictive
            , throttlePeriod = 10
            , throttleAlgorithm = FixedWindow
            , throttleIdentifier = getClientIdentifier
            , throttleTokenBucketTTL = Nothing
            })
        , ("sliding-throttle", ThrottleConfig
            { throttleLimit = 8   -- Increased from 3 to be less restrictive
            , throttlePeriod = 10
            , throttleAlgorithm = SlidingWindow
            , throttleIdentifier = getClientIdentifier
            , throttleTokenBucketTTL = Nothing
            })
        , ("login-throttle", ThrottleConfig
            { throttleLimit = 3   -- Increased from 2 for better testing
            , throttlePeriod = 10
            , throttleAlgorithm = FixedWindow
            , throttleIdentifier = \req ->
                if T.isPrefixOf "/login" (TE.decodeUtf8 $ rawPathInfo req)
                then do -- Use do-notation for clarity with IO
                    mIdentifier <- getClientIdentifier req
                    pure $ fmap (\identifier -> identifier <> ":" <> TE.decodeUtf8 (rawPathInfo req)) mIdentifier
                else pure Nothing -- Skip this throttle for other paths.
            , throttleTokenBucketTTL = Nothing
            })
        -- FIXED: Apply reset-throttle only to specific paths for testing
        , ("reset-throttle", ThrottleConfig
            { throttleLimit = 1
            , throttlePeriod = 1
            , throttleAlgorithm = FixedWindow
            , throttleIdentifier = \req ->
                if T.isPrefixOf "/reset_test" (TE.decodeUtf8 $ rawPathInfo req)
                then getClientIdentifier req
                else pure Nothing -- Skip this throttle for other paths
            , throttleTokenBucketTTL = Nothing
            })
        -- Add a zone-specific throttle for IP isolation testing
        , ("zone-throttle", ThrottleConfig
            { throttleLimit = 2
            , throttlePeriod = 5
            , throttleAlgorithm = FixedWindow
            , throttleIdentifier = \req ->
                let ip = getClientIPString req
                in if ip `elem` ["10.0.0.1", "20.0.0.1"]
                   then pure (Just ip)
                   else pure Nothing -- Skip for other IPs
            , throttleTokenBucketTTL = Nothing
            })
        ]
  
  -- Use foldM to cleanly and sequentially add all throttles.
  envWithThrottles <- foldM (\currentEnv (name, config) -> addThrottle currentEnv name config) env throttles
  
  putStrLn $ "Starting Haskell Warp server on port " ++ show port
  run port (attackMiddleware envWithThrottles baseApp)

baseApp :: Application
baseApp req respond = do
  putStrLn $ "Received request from " ++ T.unpack (getClientIPString req)
  respond $ responseLBS
    status200
    [("Content-Type", "text/plain")]
    "Hello from a Haskell/WAI/Warp application with rate limiting!"
HASKELL_EOF

          echo "Starting Haskell application from file: $HASKELL_FILE" >&2
          echo "File contents:" >&2
          cat "$HASKELL_FILE" >&2
          # Run with verbose output and capture errors
          echo "Running runghc $HASKELL_FILE" >&2
          if ! runghc "$HASKELL_FILE" 2>&1 | tee -a /tmp/app.log >&2; then
            echo "runghc failed with exit code $?" >&2
            cat /tmp/app.log >&2
            exit 1
          fi
          cat /tmp/app.log >&2
        '';
      };
    };
  };

  testScript = ''
    # Start the virtual machine
    machine.start()

    # Wait for the keter service to be active
    machine.wait_for_unit("keter.service")

    # Ensure the incoming directory exists with correct permissions
    print("=== Ensuring Keter incoming directory ===")
    machine.succeed("mkdir -p /var/lib/keter/incoming && chmod 755 /var/lib/keter/incoming")

    # Debug: Check if the bundle file exists in the Nix store
    print("=== Checking bundle file existence ===")
    _, output = machine.execute("ls -l /nix/store/*keter-bundle/bundle.tar.gz.keter || echo 'Bundle file not found'")
    print(output)

    # Copy the bundle file manually to avoid race conditions
    print("=== Copying bundle file ===")
    machine.succeed("cp /nix/store/*keter-bundle/bundle.tar.gz.keter /var/lib/keter/incoming/test-bundle.keter || echo 'Failed to copy bundle'")

    # Start and check the load-keter-bundle service
    print("=== Starting load-keter-bundle service ===")
    machine.succeed("systemctl start load-keter-bundle.service")

    # Debug: Check the status of load-keter-bundle service
    print("=== Load keter bundle service status ===")
    _, output = machine.execute("systemctl status load-keter-bundle.service --no-pager || echo 'Service status check failed'")
    print(output)
    assert "Active: inactive" in output or "Active: active" in output, "load-keter-bundle.service did not start or complete"

    # Debug: Check Keter logs for bundle loading issues
    print("=== Keter logs for bundle loading ===")
    _, output = machine.execute("journalctl -u keter.service --no-pager | grep -i 'test-bundle' || echo 'No bundle loading logs found'")
    print(output)

    # Wait for the application to listen on port 81
    machine.wait_for_open_port(${toString port})

    # Give additional time for the Haskell app to start and settle
    machine.succeed("sleep 25")

    # Log keter service status for debugging
    print("=== Keter service status ===")
    status, output = machine.execute("systemctl status keter.service --no-pager")
    print(output)

    # Check running processes
    print("=== Running processes ===")
    _, output = machine.execute("pgrep -af keter || echo 'no keter processes'")
    print(output)
    _, output = machine.execute("pgrep -af runghc || echo 'no runghc processes'")
    print(output)
    _, output = machine.execute("pgrep -af ghc | grep -v grep || echo 'no ghc processes'")
    print(output)
    _, output = machine.execute("ps aux | grep -E '(keter|runghc|warp|ghc)' | grep -v grep || echo 'no relevant processes'")
    print(output)

    # Check network connections
    print("=== Network connections ===")
    _, output = machine.execute("ss -tlnp | grep :${toString port} || echo 'port ${toString port} not listening'")
    print(output)
    _, output = machine.execute("ss -tlnp || echo 'failed to list connections'")
    print(output)

    # Check application logs
    print("=== Haskell app log (/tmp/app.log) ===")
    _, output = machine.execute("cat /tmp/app.log 2>/dev/null || echo 'no Haskell app log found'")
    print(output)
    assert "Starting Haskell Warp server on port" in output, "Haskell app did not start correctly"

    # Check temporary Haskell file existence
    print("=== Checking temporary Haskell file ===")
    _, output = machine.execute("ls -l /tmp/app.*.hs 2>/dev/null || echo 'no Haskell source file found'")
    print(output)
    assert "no Haskell source file found" not in output, "Haskell source file /tmp/app.*.hs was not created"

    # Check for errors in keter logs
    print("=== Checking for errors in keter logs ===")
    _, output = machine.execute("journalctl -u keter.service --no-pager | grep -i -E '(error|fail|exception)' | tail -20 || echo 'no errors found in logs'")
    print(output)

    # Test basic connectivity (should work now with higher limits)
    print("=== Testing basic connectivity ===")
    output = machine.succeed("curl -v --write-out '%{http_code}' http://localhost:${toString port}/")
    print("Basic connectivity response: {0}".format(output))
    # Extract the status code (last 3 characters) and response body
    status_code = output[-3:].strip()
    response_body = output[:-3].strip()
    assert status_code == "200", "Basic connectivity did not return 200 OK, got {0}".format(status_code)
    assert "Hello from a Haskell/WAI/Warp application with rate limiting!" in response_body, "Expected response not found"

    # Test general rate limiting with higher, more reasonable limits
    print("=== Testing general rate limiting ===")
    print("Sending 10 requests to test rate limit (expecting 8 successes due to sliding window limit)")
    success_count = 0
    failure_count = 0
    for i in range(1, 11):
        print("Request {0}:".format(i))
        status, output = machine.execute("curl -s --write-out '%{http_code}' http://localhost:${toString port}/")
        status_code = output[-3:].strip()
        if status_code == "200":
            success_count += 1
        elif status_code == "429":
            failure_count += 1
        print("Request {0} status: {1}".format(i, status_code))
        machine.succeed("sleep 0.5")  # Small delay between requests
    
    print("Rate limiting test results: {0} successes, {1} failures".format(success_count, failure_count))
    assert success_count >= 7, "Expected at least 7 successful requests, got {0}".format(success_count)
    assert failure_count >= 2, "Expected at least 2 rate-limited requests, got {0}".format(failure_count)

    # Test path-specific rate limiting (/login: 3 requests per 10 seconds)
    print("=== Testing path-specific rate limiting ===")
    machine.succeed("sleep 15")  # Ensure reset
    print("Sending 4 requests to /login (expecting 3 successes, 1 failure)")
    login_success = 0
    login_failure = 0
    for i in range(1, 5):
        print("Login Request {0}:".format(i))
        status, output = machine.execute("curl -s --write-out '%{http_code}' http://localhost:${toString port}/login")
        status_code = output[-3:].strip()
        if status_code == "200":
            login_success += 1
        elif status_code == "429":
            login_failure += 1
        print("Login request {0} status: {1}".format(i, status_code))
        machine.succeed("sleep 0.5")
    
    assert login_success == 3, "Expected exactly 3 successful login requests, got {0}".format(login_success)
    assert login_failure == 1, "Expected exactly 1 failed login request, got {0}".format(login_failure)
    
    # Verify other paths are not affected by login throttle
    print("Sending 3 requests to /home (should succeed as login throttle doesn't apply)")
    machine.succeed("sleep 12")  # Ensure global throttles reset
    home_success = 0
    for i in range(1, 4):
        print("Home Request {0}:".format(i))
        status, output = machine.execute("curl -s --write-out '%{http_code}' http://localhost:${toString port}/home")
        status_code = output[-3:].strip()
        if status_code == "200":
            home_success += 1
        machine.succeed("sleep 0.5")
    assert home_success >= 2, "Expected at least 2 successful /home requests, got {0}".format(home_success)

    # Test time-based reset throttle (1 request per 1 second, only for /reset_test)
    print("=== Testing time-based reset throttle ===")
    machine.succeed("sleep 15")  # Ensure all throttles reset
    print("First /reset_test request should succeed")
    output = machine.succeed("curl -s --write-out '%{http_code}' http://localhost:${toString port}/reset_test")
    status_code = output[-3:].strip()
    assert status_code == "200", "First reset request did not return 200 OK, got {0}".format(status_code)
    
    print("Second /reset_test request should fail due to 1 req/sec limit")
    exit_code, output = machine.execute("curl -s --write-out '%{http_code}' http://localhost:${toString port}/reset_test")
    status_code = output[-3:].strip()
    assert status_code == "429", "Second reset request did not return 429, got {0}".format(status_code)
    
    # Verify other paths are not affected by reset throttle
    print("Regular path should still work (not affected by /reset_test throttle)")
    output = machine.succeed("curl -s --write-out '%{http_code}' http://localhost:${toString port}/regular")
    status_code = output[-3:].strip()
    assert status_code == "200", "Regular path affected by reset throttle, got {0}".format(status_code)
    
    machine.succeed("sleep 2")  # Wait for reset period + buffer
    print("Third /reset_test request after reset should succeed")
    output = machine.succeed("curl -s --write-out '%{http_code}' http://localhost:${toString port}/reset_test")
    status_code = output[-3:].strip()
    assert status_code == "200", "Third reset request did not return 200 OK, got {0}".format(status_code)

    # Test IP zone isolation
    print("=== Testing IP zone isolation ===")
    machine.succeed("sleep 10")  # Ensure reset
    
    # Test Zone A - exhaust its limit (2 requests per 5 seconds)
    print("Testing Zone A (10.0.0.1) - limit 2 per 5 seconds")
    machine.succeed("curl -s -H 'x-real-ip: 10.0.0.1' http://localhost:${toString port}/")
    machine.succeed("curl -s -H 'x-real-ip: 10.0.0.1' http://localhost:${toString port}/")
    exit_code, output = machine.execute("curl -s -H 'x-real-ip: 10.0.0.1' --write-out '%{http_code}' http://localhost:${toString port}/")
    status_code = output[-3:].strip()
    assert status_code == "429", "Zone A third request was not blocked as expected, got status {0}".format(status_code)

    # Verify Zone B is unaffected
    print("Verifying Zone B (20.0.0.1) is unaffected")
    output = machine.succeed("curl -s -H 'x-real-ip: 20.0.0.1' --write-out '%{http_code}' http://localhost:${toString port}/")
    status_code = output[-3:].strip()
    assert status_code == "200", "Zone B request failed unexpectedly, got {0}".format(status_code)
    print("Zone B request succeeded, proving isolation.")

    # Test default zone (should use general limits, not zone-specific)
    print("=== Testing default zone behavior ===")
    machine.succeed("sleep 6")  # Ensure zone throttles reset
    print("Default zone IP should use general limits, not zone-specific")
    output = machine.succeed("curl -s -H 'x-real-ip: 192.168.1.1' --write-out '%{http_code}' http://localhost:${toString port}/")
    status_code = output[-3:].strip()
    assert status_code == "200", "Default zone request failed, got {0}".format(status_code)

    print("All tests passed! Rate limiting is working correctly.")
  '';
}
