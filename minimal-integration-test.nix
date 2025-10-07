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

          echo "=== Starting Haskell app ==="
          echo "PORT environment variable: $PORT"
          echo "PATH: $PATH"
          
          echo "Checking if /tmp is writable"
          touch /tmp/test-write 2>&1 | tee -a /tmp/app.log
          if [ $? -ne 0 ]; then
            echo "Cannot write to /tmp!"
            exit 1
          fi
          echo "Write test passed"

          echo "Checking GHC packages:"
          ghc-pkg list 2>&1 | grep -E "(wai|warp|http-types|keter-rate-limiting)" || echo "Package check failed"
          
          echo "Testing compilation:"
          echo 'main = putStrLn "Compilation test OK"' > /tmp/test.hs
          if ! runghc /tmp/test.hs 2>&1 | tee -a /tmp/app.log; then
            echo "Basic compilation failed!"
            cat /tmp/app.log
            exit 1
          fi
          echo "Compilation test passed"

          echo "Copying test application"
          cp ${./test/TestApp.hs} /tmp/app.hs
          
          echo "Starting Haskell application from file: /tmp/app.hs"
          echo "File contents:"
          cat /tmp/app.hs
          echo "Running runghc /tmp/app.hs"
          if ! runghc /tmp/app.hs 2>&1 | tee -a /tmp/app.log; then
            echo "runghc failed with exit code $?"
            cat /tmp/app.log
            exit 1
          fi
          cat /tmp/app.log
        '';
      };
    };
  };

  testScript = ''
    machine.start()

    machine.wait_for_unit("keter.service")

    print("=== Ensuring Keter incoming directory ===")
    machine.succeed("mkdir -p /var/lib/keter/incoming && chmod 755 /var/lib/keter/incoming")

    print("=== Checking bundle file existence ===")
    _, output = machine.execute("ls -l /nix/store/*keter-bundle/bundle.tar.gz.keter || echo 'Bundle file not found'")
    print(output)

    print("=== Copying bundle file ===")
    machine.succeed("cp /nix/store/*keter-bundle/bundle.tar.gz.keter /var/lib/keter/incoming/test-bundle.keter || echo 'Failed to copy bundle'")

    print("=== Starting load-keter-bundle service ===")
    machine.succeed("systemctl start load-keter-bundle.service")

    print("=== Load keter bundle service status ===")
    _, output = machine.execute("systemctl status load-keter-bundle.service --no-pager || echo 'Service status check failed'")
    print(output)
    assert "Active: inactive" in output or "Active: active" in output, "load-keter-bundle.service did not start or complete"

    print("=== Keter logs for bundle loading ===")
    _, output = machine.execute("journalctl -u keter.service --no-pager | grep -i 'test-bundle' || echo 'No bundle loading logs found'")
    print(output)

    machine.wait_for_open_port(${toString port})

    machine.succeed("sleep 20")

    print("=== Keter service status ===")
    status, output = machine.execute("systemctl status keter.service --no-pager")
    print(output)

    print("=== Running processes ===")
    _, output = machine.execute("pgrep -af keter || echo 'no keter processes'")
    print(output)
    _, output = machine.execute("pgrep -af runghc || echo 'no runghc processes'")
    print(output)
    _, output = machine.execute("pgrep -af ghc | grep -v grep || echo 'no ghc processes'")
    print(output)
    _, output = machine.execute("ps aux | grep -E '(keter|runghc|warp|ghc)' | grep -v grep || echo 'no relevant processes'")
    print(output)

    print("=== Network connections ===")
    _, output = machine.execute("ss -tlnp | grep :${toString port} || echo 'port ${toString port} not listening'")
    print(output)
    _, output = machine.execute("ss -tlnp || echo 'failed to list connections'")
    print(output)

    print("=== Haskell app log (/tmp/app.log) ===")
    _, output = machine.execute("cat /tmp/app.log 2>/dev/null || echo 'no Haskell app log found'")
    print(output)
    assert "Starting Haskell Warp server on port" in output, "Haskell app did not start correctly"

    print("=== Checking Haskell source file ===")
    _, output = machine.execute("ls -l /tmp/app.hs 2>/dev/null || echo 'no Haskell source file found'")
    print(output)
    assert "no Haskell source file found" not in output, "Haskell source file /tmp/app.hs was not created"

    print("=== Checking for errors in keter logs ===")
    _, output = machine.execute("journalctl -u keter.service --no-pager | grep -i -E '(error|fail|exception)' | tail -20 || echo 'no errors found in logs'")
    print(output)

    print("=== Testing basic connectivity ===")
    output = machine.succeed("curl -v --write-out '%{http_code}' http://localhost:${toString port}/")
    print("Basic connectivity response: {0}".format(output))
    status_code = output[-3:].strip()
    response_body = output[:-3].strip()
    assert status_code == "200", "Basic connectivity did not return 200 OK, got {0}".format(status_code)
    assert "Hello from a Haskell/WAI/Warp application with rate limiting!" in response_body, "Expected response not found"

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

    print("=== Waiting for rate limit reset before rate-limiting test ===")
    machine.succeed("sleep 10")

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
            assert "Too Many Requests" in response_body, "Request {0} expected 'Too Many Requests', got {1}".format(i, response_body)
        else:
            assert False, "Request {0} returned unexpected status code: {1}".format(i, status_code)
        machine.succeed("sleep 1")
    assert success_count == 4, "Expected exactly 4 successful requests, got {0}".format(success_count)
    assert failure_count == 1, "Expected exactly 1 failed request, got {0}".format(failure_count)

    print("=== Testing rate limit reset ===")
    machine.wait_until_succeeds("curl -H 'Host: localhost' http://localhost:${toString port}/", timeout=15)
    print("Rate limit reset confirmed: Request succeeded after waiting")

    print("=== Testing recovery ===")
    machine.succeed("sleep 10")
    output = machine.succeed("curl -H 'Host: localhost' http://localhost:${toString port}/")
    print("Recovery response: {0}".format(output))
    assert "Hello from a Haskell/WAI/Warp application with rate limiting!" in output, "Recovery request missing expected response"
  '';
}
