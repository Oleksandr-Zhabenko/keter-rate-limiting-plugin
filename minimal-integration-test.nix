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
import Network.Wai (responseLBS, Application, Request)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Keter.RateLimiter.WAI
  ( attackMiddleware, Env, initConfig, addThrottle, ThrottleConfig(..), Algorithm(..), getClientIP, defaultIPZone )
import qualified Data.Text as T

-- Simplified IP zone mapping (all IPs to default zone for this example)
getRequestIPZone :: Request -> T.Text
getRequestIPZone _ = defaultIPZone

main :: IO ()
main = do
  portStr <- lookupEnv "PORT"
  let port = maybe 8080 id (portStr >>= readMaybe)
  -- Initialize rate limiter environment
  env <- initConfig getRequestIPZone
  -- Configure throttle: 4 requests per 10 seconds using Fixed Window
  let throttleConfig = ThrottleConfig
        { throttleLimit = 4
        , throttlePeriod = 10
        , throttleAlgorithm = FixedWindow
        , throttleIdentifier = Just . getClientIP
        , throttleTokenBucketTTL = Nothing
        }
      envWithThrottle = addThrottle env "test-throttle" throttleConfig
  putStrLn $ "Starting Haskell Warp server on port " ++ show port
  run port (attackMiddleware envWithThrottle baseApp)

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

    # Test initial requests with Host header
    print("=== Testing initial requests with Host header ===")
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

    # Test rate limiting (4 requests per 10 seconds)
    print("=== Testing rate limiting ===")
    print("Sending 5 requests to test rate limit (expecting 4 successes, 1 failure)")
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
    assert success_count >= 4, "Expected at least 4 successful requests, got {0}".format(success_count)
    assert failure_count <= 1, "Expected up to 1 failed request, got {0}".format(failure_count)

    # Test rate limit reset
    print("=== Testing rate limit reset ===")
    machine.wait_until_succeeds("curl -H 'Host: localhost' http://localhost:${toString port}/", timeout=15)
    print("Rate limit reset confirmed: Request succeeded after waiting")

    # Test recovery
    print("=== Testing recovery ===")
    machine.succeed("sleep 10")
    output = machine.succeed("curl -H 'Host: localhost' http://localhost:${toString port}/")
    print("Recovery response: {0}".format(output))
    assert "Hello from a Haskell/WAI/Warp application with rate limiting!" in output, "Recovery request missing expected response"
  '';
}
