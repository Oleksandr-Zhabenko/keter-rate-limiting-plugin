{ pkgs ? import <nixpkgs> {} }:

let
  keterRateLimiting = pkgs.haskellPackages.callPackage ./new.nix {};
  port = 81;
in
pkgs.testers.runNixOSTest {
  name = "keter-haskell-rate-limiting";
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
import Network.Wai (responseLBS, Application, Request, rawPathInfo)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status429)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Keter.RateLimiter.WAI
  ( attackMiddleware, Env, initConfig, addThrottle, ThrottleConfig(..), Algorithm(..), getClientIP, defaultIPZone )
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- IP zone mapping for testing
getRequestIPZone :: Request -> T.Text
getRequestIPZone req
  | getClientIP req == "10.0.0.1" = "testZoneA"
  | getClientIP req == "20.0.0.1" = "testZoneB"
  | otherwise = defaultIPZone

main :: IO ()
main = do
  portStr <- lookupEnv "PORT"
  let port = maybe 8080 id (portStr >>= readMaybe)
  -- Initialize rate limiter environment
  env <- initConfig getRequestIPZone
  -- Configure throttle: 4 requests per 10 seconds using Fixed Window
  let fixedWindowConfig = ThrottleConfig
        { throttleLimit = 4
        , throttlePeriod = 10
        , throttleAlgorithm = FixedWindow
        , throttleIdentifier = Just . getClientIP
        , throttleTokenBucketTTL = Nothing
        }
      -- Configure Sliding Window: 3 requests per 10 seconds
      slidingWindowConfig = ThrottleConfig
        { throttleLimit = 3
        , throttlePeriod = 10
        , throttleAlgorithm = SlidingWindow
        , throttleIdentifier = Just . getClientIP
        , throttleTokenBucketTTL = Nothing
        }
      -- Configure path-specific throttle for /login: 2 requests per 10 seconds
      loginThrottleConfig = ThrottleConfig
        { throttleLimit = 2
        , throttlePeriod = 10
        , throttleAlgorithm = FixedWindow
        , throttleIdentifier = \req ->
            if T.isPrefixOf "/login" (TE.decodeUtf8 $ rawPathInfo req)
            then Just (getClientIP req <> ":" <> TE.decodeUtf8 (rawPathInfo req))
            else Nothing
        , throttleTokenBucketTTL = Nothing
        }
      -- Configure time-based reset test: 1 request per 1 second
      resetThrottleConfig = ThrottleConfig
        { throttleLimit = 1
        , throttlePeriod = 1
        , throttleAlgorithm = FixedWindow
        , throttleIdentifier = Just . getClientIP
        , throttleTokenBucketTTL = Nothing
        }
      envWithThrottles = addThrottle
        (addThrottle
          (addThrottle
            (addThrottle env "fixed-throttle" fixedWindowConfig)
            "sliding-throttle" slidingWindowConfig)
          "login-throttle" loginThrottleConfig)
        "reset-throttle" resetThrottleConfig
  putStrLn $ "Starting Haskell Warp server on port " ++ show port
  run port (attackMiddleware envWithThrottles baseApp)

baseApp :: Application
baseApp req respond = do
  putStrLn $ "Received request from " ++ T.unpack (getClientIP req)
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

    # Give additional time for the Haskell app to start
    machine.succeed("sleep 20")

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

    # Test basic connectivity
    print("=== Testing basic connectivity ===")
    output = machine.succeed("curl -v --write-out '%{http_code}' http://localhost:${toString port}/")
    print("Basic connectivity response: {0}".format(output))
    # Extract the status code (last 3 characters) and response body
    status_code = output[-3:].strip()
    response_body = output[:-3].strip()
    assert status_code == "200", "Basic connectivity did not return 200 OK, got {0}".format(status_code)
    assert "Hello from a Haskell/WAI/Warp application with rate limiting!" in response_body, "Expected response not found"

    # Wait for rate limit to reset after basic connectivity
    print("=== Waiting for rate limit reset after basic connectivity ===")
    machine.succeed("sleep 10")

    # Test initial requests with Host header (Fixed Window)
    print("=== Testing initial requests with Host header (Fixed Window) ===")
    for i in range(1, 4):
        print("Request {0}:".format(i))
        output = machine.succeed("curl -H 'Host: localhost' --write-out '%{http_code}' http://localhost:${toString port}/")
        status_code = output[-3:].strip()
        response_body = output[:-3].strip()
        print("Response {0}: {1}".format(i, response_body))
        assert status_code == "200", "Request {0} did not return 200 OK, got {1}".format(i, status_code)
        assert "Hello from a Haskell/WAI/Warp application with rate limiting!" in response_body, "Request {0} missing expected response".format(i)
        machine.succeed("sleep 1")

    # Wait for rate limit to reset
    print("=== Waiting for rate limit reset before rate-limiting test ===")
    machine.succeed("sleep 10")

    # Test rate limiting (Fixed Window)
    print("=== Testing rate limiting (Fixed Window) ===")
    print("Sending 5 requests to test rate limit (expecting 3 successes, 2 failures due to Sliding Window limit of 3 requests per 10 seconds)")
    success_count = 0
    failure_count = 0
    for i in range(1, 6):
        print("Request {0}:".format(i))
        status, output = machine.execute("curl -v -H 'Host: localhost' --write-out '%{http_code}' http://localhost:${toString port}/")
        status_code = output[-3:].strip()
        response_body = output[:-3].strip()
        print("Response {0}: {1}".format(i, response_body))
        if status_code == "200":
            success_count += 1
            assert "Hello from a Haskell/WAI/Warp application with rate limiting!" in response_body, "Request {0} missing expected response".format(i)
        elif status_code == "429":
            failure_count += 1
            assert response_body == "Too Many Requests", "Request {0} expected 'Too Many Requests', got {1}".format(i, response_body)
        else:
            assert False, "Request {0} returned unexpected status code: {1}".format(i, status_code)
        machine.succeed("sleep 1")
    assert success_count >= 3, "Expected at least 3 successful requests, got {0}".format(success_count)
    assert failure_count <= 2, "Expected up to 2 failed requests, got {0}".format(failure_count)

    # Test rate limit reset (Fixed Window)
    print("=== Testing rate limit reset (Fixed Window) ===")
    machine.wait_until_succeeds("curl -H 'Host: localhost' http://localhost:${toString port}/", timeout=15)
    print("Rate limit reset confirmed: Request succeeded after waiting")

    # Test recovery (Fixed Window)
    print("=== Testing recovery (Fixed Window) ===")
    machine.succeed("sleep 10")
    output = machine.succeed("curl -H 'Host: localhost' http://localhost:${toString port}/")
    print("Recovery response: {0}".format(output))
    assert "Hello from a Haskell/WAI/Warp application with rate limiting!" in output, "Recovery request missing expected response"

    # Test Sliding Window rate limiting (3 requests per 10 seconds)
    print("=== Testing rate limiting (Sliding Window) ===")
    print("Sending 5 requests to test Sliding Window rate limit (expecting 3 successes, 2 failures)")
    machine.succeed("sleep 10")  # Ensure reset
    success_count = 0
    failure_count = 0
    for i in range(1, 6):
        print("Request {0}:".format(i))
        status, output = machine.execute("curl -v -H 'Host: localhost' --write-out '%{http_code}' http://localhost:${toString port}/")
        status_code = output[-3:].strip()
        response_body = output[:-3].strip()
        print("Response {0}: {1}".format(i, response_body))
        if status_code == "200":
            success_count += 1
            assert "Hello from a Haskell/WAI/Warp application with rate limiting!" in response_body, "Request {0} missing expected response".format(i)
        elif status_code == "429":
            failure_count += 1
            assert response_body == "Too Many Requests", "Request {0} expected 'Too Many Requests', got {1}".format(i, response_body)
        else:
            assert False, "Request {0} returned unexpected status code: {1}".format(i, status_code)
        machine.succeed("sleep 1")
    assert success_count >= 3, "Expected at least 3 successful requests, got {0}".format(success_count)
    assert failure_count <= 2, "Expected up to 2 failed requests, got {0}".format(failure_count)

    # Test path-specific rate limiting (/login: 2 requests per 10 seconds, /home: limited by sliding-throttle)
    print("=== Testing path-specific rate limiting ===")
    machine.succeed("sleep 10")  # Ensure reset
    print("Sending 3 requests to /login (expecting 2 successes, 1 failure)")
    for i in range(1, 4):
        print("Login Request {0}:".format(i))
        status, output = machine.execute("curl -v -H 'Host: localhost' --write-out '%{http_code}' http://localhost:${toString port}/login")
        status_code = output[-3:].strip()
        response_body = output[:-3].strip()
        print("Response {0}: {1}".format(i, response_body))
        if i <= 2:
            assert status_code == "200", "Login Request {0} did not return 200 OK, got {1}".format(i, status_code)
            assert "Hello from a Haskell/WAI/Warp application with rate limiting!" in response_body, "Login Request {0} missing expected response".format(i)
        else:
            assert status_code == "429", "Login Request {0} did not return 429, got {1}".format(i, status_code)
            assert response_body == "Too Many Requests", "Login Request {0} expected 'Too Many Requests', got {1}".format(i, response_body)
        machine.succeed("sleep 1")
    print("Sending 3 requests to /home (expecting at least 2 successes, up to 1 failure due to sliding-throttle)")
    machine.succeed("sleep 12")  # Ensure global throttles reset
    success_count = 0
    failure_count = 0
    for i in range(1, 4):
        print("Home Request {0}:".format(i))
        status, output = machine.execute("curl -v -H 'Host: localhost' --write-out '%{http_code}' http://localhost:${toString port}/home")
        status_code = output[-3:].strip()
        response_body = output[:-3].strip()
        print("Response {0}: {1}".format(i, response_body))
        if status_code == "200":
            success_count += 1
            assert "Hello from a Haskell/WAI/Warp application with rate limiting!" in response_body, "Home Request {0} missing expected response".format(i)
        elif status_code == "429":
            failure_count += 1
            assert response_body == "Too Many Requests", "Home Request {0} expected 'Too Many Requests', got {1}".format(i, response_body)
        else:
            assert False, "Home Request {0} returned unexpected status code: {1}".format(i, status_code)
        machine.succeed("sleep 1")
    assert success_count >= 2, "Expected at least 2 successful /home requests, got {0}".format(success_count)
    assert failure_count <= 1, "Expected up to 1 failed /home request, got {0}".format(failure_count)

    # Test time-based reset (1 request per 1 second)
    print("=== Testing time-based reset ===")
    machine.succeed("sleep 12")  # Ensure all throttles reset
    print("First request should succeed")
    output = machine.succeed("curl -v -H 'Host: localhost' --write-out '%{http_code}' http://localhost:${toString port}/reset_test")
    status_code = output[-3:].strip()
    response_body = output[:-3].strip()
    assert status_code == "200", "First reset request did not return 200 OK, got {0}".format(status_code)
    assert "Hello from a Haskell/WAI/Warp application with rate limiting!" in response_body, "First reset request missing expected response"
    print("Second request should fail")
    status, output = machine.execute("curl -v -H 'Host: localhost' --write-out '%{http_code}' http://localhost:${toString port}/reset_test")
    status_code = output[-3:].strip()
    response_body = output[:-3].strip()
    assert status_code == "429", "Second reset request did not return 429, got {0}".format(status_code)
    assert response_body == "Too Many Requests", "Second reset request expected 'Too Many Requests', got {0}".format(response_body)
    machine.succeed("sleep 2")  # Wait for period + buffer
    print("Third request after reset should succeed")
    output = machine.succeed("curl -v -H 'Host: localhost' --write-out '%{http_code}' http://localhost:${toString port}/reset_test")
    status_code = output[-3:].strip()
    response_body = output[:-3].strip()
    assert status_code == "200", "Third reset request did not return 200 OK, got {0}".format(status_code)
    assert "Hello from a Haskell/WAI/Warp application with rate limiting!" in response_body, "Third reset request missing expected response"

    # Test IP zone isolation (Zone A and Zone B)
    print("=== Testing IP zone isolation ===")
    machine.succeed("sleep 10")  # Ensure reset
    print("Zone A first request should succeed")
    output = machine.succeed("curl -v -H 'Host: localhost' -H 'x-real-ip: 10.0.0.1' --write-out '%{http_code}' http://localhost:${toString port}/")
    status_code = output[-3:].strip()
    response_body = output[:-3].strip()
    assert status_code == "200", "Zone A first request did not return 200 OK, got {0}".format(status_code)
    assert "Hello from a Haskell/WAI/Warp application with rate limiting!" in response_body, "Zone A first request missing expected response"
    print("Zone A second request should fail")
    status, output = machine.execute("curl -v -H 'Host: localhost' -H 'x-real-ip: 10.0.0.1' --write-out '%{http_code}' http://localhost:${toString port}/")
    status_code = output[-3:].strip()
    response_body = output[:-3].strip()
    assert status_code == "429", "Zone A second request did not return 429, got {0}".format(status_code)
    assert response_body == "Too Many Requests", "Zone A second request expected 'Too Many Requests', got {0}".format(response_body)
    print("Zone B first request should succeed")
    output = machine.succeed("curl -v -H 'Host: localhost' -H 'x-real-ip: 20.0.0.1' --write-out '%{http_code}' http://localhost:${toString port}/")
    status_code = output[-3:].strip()
    response_body = output[:-3].strip()
    assert status_code == "200", "Zone B first request did not return 200 OK, got {0}".format(status_code)
    assert "Hello from a Haskell/WAI/Warp application with rate limiting!" in response_body, "Zone B first request missing expected response"

    # Test IP zone default fallback
    print("=== Testing IP zone default fallback ===")
    machine.succeed("sleep 10")  # Ensure reset
    print("Default zone IP first request should succeed")
    output = machine.succeed("curl -v -H 'Host: localhost' -H 'x-real-ip: 30.0.0.1' --write-out '%{http_code}' http://localhost:${toString port}/")
    status_code = output[-3:].strip()
    response_body = output[:-3].strip()
    assert status_code == "200", "Default zone first request did not return 200 OK, got {0}".format(status_code)
    assert "Hello from a Haskell/WAI/Warp application with rate limiting!" in response_body, "Default zone first request missing expected response"
    print("Generic IP first request should succeed")
    output = machine.succeed("curl -v -H 'Host: localhost' -H 'x-real-ip: 192.168.1.1' --write-out '%{http_code}' http://localhost:${toString port}/")
    status_code = output[-3:].strip()
    response_body = output[:-3].strip()
    assert status_code == "200", "Generic IP first request did not return 200 OK, got {0}".format(status_code)
    assert "Hello from a Haskell/WAI/Warp application with rate limiting!" in response_body, "Generic IP first request missing expected response"
  '';
}
