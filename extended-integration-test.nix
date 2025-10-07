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

          echo "=== Starting Haskell app ===" >&2
          echo "PORT environment variable: $PORT" >&2
          echo "PATH: $PATH" >&2

          echo "Checking if /tmp is writable" >&2
          touch /tmp/test-write 2>&1 | tee -a /tmp/app.log >&2
          if [ $? -ne 0 ]; then
            echo "Cannot write to /tmp!" >&2
            exit 1
          fi
          echo "Write test passed" >&2

          echo "Checking GHC packages:" >&2
          ghc-pkg list 2>&1 | grep -E "(wai|warp|http-types|keter-rate-limiting)" >&2 || echo "Package check failed" >&2

          echo "Testing compilation:" >&2
          echo 'main = putStrLn "Compilation test OK"' > /tmp/test.hs
          if ! runghc /tmp/test.hs 2>&1 | tee -a /tmp/app.log >&2; then
            echo "Basic compilation failed!" >&2
            cat /tmp/app.log >&2
            exit 1
          fi
          echo "Compilation test passed" >&2

          echo "Copying extended test application" >&2
          cp ${./test/ExtendedTestApp.hs} /tmp/app.hs
          
          echo "Starting Haskell application from file: /tmp/app.hs" >&2
          echo "File contents:" >&2
          cat /tmp/app.hs >&2
          echo "Running runghc /tmp/app.hs" >&2
          if ! runghc /tmp/app.hs 2>&1 | tee -a /tmp/app.log >&2; then
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

    print("=== Testing rate limiting with x-forwarded-for IPv4 ===")
    machine.succeed("sleep 10")
    success_count = 0
    failure_count = 0
    for i in range(1, 6):
        print("Request {0}:".format(i))
        output = machine.succeed("curl -H 'x-forwarded-for: 192.168.1.1' -H 'Host: localhost' --write-out '%{http_code}' http://localhost:${toString port}/")
        status_code = output[-3:].strip()
        response_body = output[:-3].strip()
        if status_code == "200":
            success_count += 1
            assert "Hello from a Haskell/WAI/Warp application with rate limiting!" in response_body
        elif status_code == "429":
            failure_count += 1
            assert "Too Many Requests" in response_body
        else:
            assert False
        machine.succeed("sleep 1")
    assert success_count == 4
    assert failure_count == 1

    print("=== Testing rate limiting with x-real-ip IPv6 ===")
    machine.succeed("sleep 10")
    success_count = 0
    failure_count = 0
    for i in range(1, 6):
        print("Request {0}:".format(i))
        output = machine.succeed("curl -H 'x-real-ip: ::1' -H 'Host: localhost' --write-out '%{http_code}' http://localhost:${toString port}/")
        status_code = output[-3:].strip()
        response_body = output[:-3].strip()
        if status_code == "200":
            success_count += 1
            assert "Hello from a Haskell/WAI/Warp application with rate limiting!" in response_body
        elif status_code == "429":
            failure_count += 1
            assert "Too Many Requests" in response_body
        else:
            assert False
        machine.succeed("sleep 1")
    assert success_count == 4
    assert failure_count == 1

    print("=== Testing concurrent requests (moderate) ===")
    machine.succeed("sleep 10")
    output = machine.succeed("bash -lc 'for i in $(seq 1 6); do curl -s -o /dev/null -w \"%{http_code}\" -H \"Host: localhost\" http://localhost:81 & done; wait'")
    success_count = output.count("200")
    failure_count = output.count("429")
    assert success_count == 4, "Expected 4 successful concurrent requests, got {}".format(success_count)
    assert failure_count >= 1, "Expected at least 1 failed request, got {}".format(failure_count)
 
    print("=== Testing DoS-like concurrent requests ===")
    machine.succeed("sleep 10")
    machine.succeed('echo "" > /tmp/app.log')
    machine.succeed("bash -lc 'for i in $(seq 1 20); do curl -s -o /dev/null -H \"Host: localhost\" http://localhost:${toString port} & done; wait'")
    machine.succeed("sleep 2")
    _, output = machine.execute('grep -c "Received request from" /tmp/app.log')
    success_count = int(output.strip())
    assert success_count == 4, "Expected 4 successful DoS requests, got {}".format(success_count)

    print("=== Testing different IPs via XFF ===")
    machine.succeed("sleep 10")
    for j in range(1, 3):
      for i in range(1, 5):
          ip = "192.168.1.{}".format(j)
          print("Request for IP {} - {}".format(ip, i))
          output = machine.succeed(f"curl -H 'x-forwarded-for: {ip}' -H 'Host: localhost' --write-out '%{{http_code}}' http://localhost:${toString port}/")
          status_code = output[-3:].strip()
          assert status_code == "200"
          machine.succeed("sleep 1")

    print("=== Testing mixed x-forwarded-for ===")
    machine.succeed("sleep 10")
    success_count = 0
    failure_count = 0
    for i in range(1, 6):
        print("Request {0}:".format(i))
        output = machine.succeed("curl -H 'x-forwarded-for: 192.168.2.1, 10.0.0.1' -H 'Host: localhost' --write-out '%{http_code}' http://localhost:${toString port}/")
        status_code = output[-3:].strip()
        if status_code == "200":
            success_count += 1
        elif status_code == "429":
            failure_count += 1
        machine.succeed("sleep 1")
    assert success_count == 4
    assert failure_count == 1

    print("=== Testing different first IP in mixed XFF ===")
    machine.succeed("sleep 10")
    for i in range(1, 5):
        ip = "192.168.3.{}".format(i)
        print("Request for IP {} ".format(ip))
        output = machine.succeed("curl -H 'x-forwarded-for: {}, 10.0.0.1' -H 'Host: localhost' --write-out '%{http_code}' http://localhost:${toString port}/")
        status_code = output[-3:].strip()
        assert status_code == "200"
        machine.succeed("sleep 1")

    print("=== Testing timing reset with longer sleep ===")
    machine.succeed("sleep 10")
    for i in range(1, 5):
        machine.succeed("curl -H 'Host: localhost' http://localhost:${toString port}/")
    output = machine.succeed("curl -H 'Host: localhost' --write-out '%{http_code}' http://localhost:${toString port}/")
    assert output[-3:].strip() == "429"
    machine.succeed("sleep 10")
    output = machine.succeed("curl -H 'Host: localhost' --write-out '%{http_code}' http://localhost:${toString port}/")
    assert output[-3:].strip() == "200"

    print("=== Testing fallback to socket IP ===")
    machine.succeed("sleep 10")
    success_count = 0
    failure_count = 0
    for i in range(1, 6):
        print("Request {0}:".format(i))
        output = machine.succeed("curl -H 'Host: localhost' --write-out '%{http_code}' http://localhost:${toString port}/")
        status_code = output[-3:].strip()
        if status_code == "200":
            success_count += 1
        elif status_code == "429":
            failure_count += 1
        machine.succeed("sleep 1")
    assert success_count == 4
    assert failure_count == 1

    print("=== Testing rate limiting with x-real-ip IPv4 ===")
    machine.succeed("sleep 10")
    success_count = 0
    failure_count = 0
    for i in range(1, 6):
        print("Request {0}:".format(i))
        output = machine.succeed("curl -H 'x-real-ip: 192.168.4.1' -H 'Host: localhost' --write-out '%{http_code}' http://localhost:${toString port}/")
        status_code = output[-3:].strip()
        if status_code == "200":
            success_count += 1
        elif status_code == "429":
            failure_count += 1
        machine.succeed("sleep 1")
    assert success_count == 4
    assert failure_count == 1

    print("=== Testing concurrent with different IPs ===")
    machine.succeed("sleep 10")
    machine.succeed('echo "" > /tmp/app.log')
    machine.succeed("bash -lc 'for i in $(seq 1 4); do curl -s -o /dev/null -H \"x-forwarded-for: 192.168.5.$i\" -H \"Host: localhost\" http://localhost:${toString port} & done; wait'")
    machine.succeed("sleep 2")
    _, output = machine.execute('grep -c "Received request from" /tmp/app.log')
    success_count = int(output.strip())
    assert success_count == 4, "Expected 4 successful concurrent different IP requests, got {}".format(success_count)

    print("=== Testing zero requests after reset ===")
    machine.succeed("sleep 10")
    output = machine.succeed("curl -H 'Host: localhost' --write-out '%{http_code}' http://localhost:${toString port}/")
    assert output[-3:].strip() == "200"
  '';
}
