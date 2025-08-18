# keter-rate-limiting-plugin

**keter-rate-limiting-plugin** is a modern, high-performance, and highly customizable rate-limiting plugin for [Keter](https://github.com/snoyberg/keter). It addresses [issue \#301](https://github.com/snoyberg/keter/issues/301) and brings robust, production-grade request throttling to Haskell web applications, featuring efficient in-memory caching with HashMap-based lookups and IP zone isolation.

This library is inspired by [rack-attack](https://github.com/rack/rack-attack) and [Ruby on Rails](https://github.com/rails/rails) (for Keter.RateLimiter.Notifications) and provides a powerful middleware for Keter-managed applications, though it can be integrated with any WAI-compatible Haskell web stack.

## Features

- **Five window algorithms**:
    - Fixed Window
    - Sliding Window
    - Token Bucket
    - Leaky Bucket
    - TinyLRU (Least Recently Used)
- **IP Zone Support**: Isolate caches and throttling policies per IP zone, customer segment, or any other logical grouping with efficient HashMap-based zone lookups.
- **Declarative Configuration**: Define throttling rules using JSON/YAML configuration with automatic serialization support.
- **Flexible Client Identification**: Multiple strategies for identifying clients (IP, headers, cookies, combinations).
- **Configurable Zone Derivation**: Flexible strategies for deriving IP zones from requests.
- **WAI Middleware**: Integrates seamlessly as a middleware into any WAI application.
- **Convenient and Customizable API**:
    - Use declarative configuration for common scenarios with automatic setup.
    - Or, for advanced use, fully control cache key structure and throttling logic.
- **Memory-efficient**: Designed for large-scale, high-traffic deployments with automatic cleanup of expired entries and HashMap-based O(1) average-case lookups.
- **Easy Integration**: Minimal code changes are required to get started.

## Why Use This Plugin?

- **Scalability**: Per-zone caches with HashMap-based storage and flexible throttling allow you to scale from single-user apps to multi-tenant platforms.
- **Performance**: The in-memory backend is built on efficient STM-based containers with HashMap optimizations for high-concurrency workloads.
- **Security**: Protects your application from abusive clients and denial-of-service attacks.
- **Flexibility**: Choose between declarative configuration and full programmatic customization.
- **Production-Ready**: Inspired by industry-standard tools, thoroughly documented, and designed for reliability with efficient data structures.
- **Open Source**: MIT licensed and community-friendly.

## Installation

Add the package to your `build-depends` in your project's `.cabal` file or `package.yaml`.

**For Cabal:**

```cabal
build-depends:
  , keter-rate-limiting-plugin
```

**For Stack (`package.yaml`):**

```yaml
dependencies:
- keter-rate-limiting-plugin
```

Then, rebuild your project. No external C libraries are required.

## Quick Start

### Declarative Configuration (Recommended)

The recommended approach uses declarative configuration that can be loaded from JSON or YAML files:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Keter.RateLimiter.WAI
import Keter.RateLimiter.Cache (Algorithm(..))
import Network.Wai (responseLBS, Application)
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)

-- A simple application that runs behind the middleware.
myApp :: Application
myApp _ respond = respond $ responseLBS status200 [] "Hello, you are not rate limited!"

main :: IO ()
main = do
  -- 1. Define declarative configuration
  let config = RateLimiterConfig
        { rlZoneBy = ZoneIP  -- Separate zones by client IP
        , rlThrottles = 
            [ RLThrottle "api"   100 3600 FixedWindow IdIP Nothing        -- 100 requests/hour by IP
            , RLThrottle "login" 5   300  TokenBucket  IdIP (Just 600)    -- 5 login attempts/5min by IP with 10min idle timeout
            ]
        }

  -- 2. Build middleware from configuration
  middleware <- buildRateLimiter config

  -- 3. Apply middleware to your application
  let appWithMiddleware = middleware myApp

  putStrLn "Server starting on port 8080..."
  run 8080 appWithMiddleware
```

### JSON Configuration

You can also load configuration from JSON files:

```json
{
  "zone_by": "ip",
  "throttles": [
    {
      "name": "api",
      "limit": 100,
      "period": 3600,
      "algorithm": "fixed_window",
      "identifier_by": "ip"
    },
    {
      "name": "login",
      "limit": 5,
      "period": 300,
      "algorithm": "token_bucket",
      "identifier_by": "ip",
      "token_bucket_ttl": 600
    }
  ]
}
```

### Advanced Programmatic Configuration

For more control, you can build the environment programmatically:

```haskell
import Keter.RateLimiter.WAI
import Keter.RateLimiter.Cache (Algorithm(..))
import Keter.RateLimiter.IPZones (defaultIPZone)
import Data.Text.Encoding (encodeUtf8)

main :: IO ()
main = do
  -- 1. Initialize environment with custom zone logic
  env <- initConfig $ \req -> 
    case requestHeaderHost req of
      Just "api.example.com" -> "api_zone"
      Just "admin.example.com" -> "admin_zone"
      _ -> defaultIPZone

  -- 2. Add throttle configurations
  let apiThrottle = ThrottleConfig
        { throttleLimit      = 1000
        , throttlePeriod     = 3600
        , throttleAlgorithm  = FixedWindow
        , throttleIdentifier = \req -> fmap (encodeUtf8 . show) (remoteHost req)
        , throttleTokenBucketTTL = Nothing
        }

  let loginThrottle = ThrottleConfig
        { throttleLimit      = 5
        , throttlePeriod     = 300
        , throttleAlgorithm  = TokenBucket
        , throttleIdentifier = \req -> fmap (encodeUtf8 . show) (remoteHost req)
        , throttleTokenBucketTTL = Just 600
        }

  env' <- addThrottle env "api" apiThrottle
  env'' <- addThrottle env' "login" loginThrottle

  -- 3. Create middleware
  let middleware = buildRateLimiterWithEnv env''
      appWithMiddleware = middleware myApp

  putStrLn "Server starting on port 8080..."
  run 8080 appWithMiddleware
```

## Configuration Reference

### Client Identification Strategies (`IdentifierBy`)

- `"ip"` - Identify by client IP address
- `"ip+path"` - Identify by IP address and request path
- `"ip+ua"` - Identify by IP address and User-Agent header
- `{"header": "X-API-Key"}` - Identify by custom header value
- `{"cookie": "session_id"}` - Identify by cookie value
- `{"header+ip": "X-User-ID"}` - Identify by header value combined with IP

### Zone Derivation Strategies (`ZoneBy`)

- `"default"` - All requests use the same cache (no zone separation)
- `"ip"` - Separate zones by client IP address
- `{"header": "X-Tenant-ID"}` - Separate zones by custom header value

### Rate Limiting Algorithms

- **`FixedWindow`** - Traditional fixed-window counting
- **`SlidingWindow`** - Precise sliding-window with timestamp tracking
- **`TokenBucket`** - Allow bursts up to capacity, refill over time
- **`LeakyBucket`** - Smooth rate limiting with configurable leak rate
- **`TinyLRU`** - Least-recently-used eviction for memory efficiency

## Example Usage

### Using the Convenient API

The `CacheWithZone` module provides helpers that automatically compose cache keys from the algorithm, zone, and user key, simplifying common use cases while leveraging efficient HashMap-based zone lookups.

```haskell
import Keter.RateLimiter.Cache
import Keter.RateLimiter.CacheWithZone

-- Create a store and cache for the Fixed Window algorithm
fixedWindowStore <- createInMemoryStore @'FixedWindow
let cache = newCache FixedWindow fixedWindowStore

-- Increment a counter for a user in a specific zone.
-- The key "rate_limiter:zoneX:userX" is created automatically.
-- The request is allowed if the count is within the limit.
-- Zone lookup uses HashMap for O(1) average performance.
isAllowed <- allowFixedWindowRequest cache "zoneX" "userX" 100 3600 -- 100 requests per hour
```

### Using the Customizable API

For more complex scenarios, you can manually construct cache keys and interact directly with the `Cache` module. This gives you full control over the key structure while still benefiting from HashMap-optimized storage.

```haskell
import Keter.RateLimiter.Cache

-- Use the same cache from the previous example.
let customKey = "rate_limiter:fixed_window:logins:zoneY:userY"

-- Manually increment the counter for the custom key.
newCount <- incrementCache cache customKey 60 -- TTL of 60 seconds

-- Manually read the value.
mVal <- readCache cache customKey :: IO (Maybe Int)
```

### Multi-Algorithm Configuration Example

```haskell
let config = RateLimiterConfig
      { rlZoneBy = ZoneHeader "X-Tenant-ID"  -- Separate by tenant
      , rlThrottles = 
          [ RLThrottle "api_burst"     100  60   TokenBucket   IdIP              (Just 300)
          , RLThrottle "api_sustained" 1000 3600 FixedWindow   IdIP              Nothing
          , RLThrottle "login"         5    300  LeakyBucket   IdIP              Nothing
          , RLThrottle "admin"         50   3600 SlidingWindow (IdHeader "X-Admin-Key") Nothing
          , RLThrottle "lru_cache"     1000 60   TinyLRU       IdIPAndPath       Nothing
          ]
      }
```

## Performance Characteristics

This library is optimized for high-performance scenarios:

- **HashMap-based zone caches**: O(1) average-case lookup for IP zone cache resolution
- **HashMap-based throttle storage**: O(1) average-case retrieval of throttle configurations
- **STM-based concurrent access**: Thread-safe operations with minimal contention
- **Memory-efficient algorithms**: Automatic cleanup of expired entries across all rate limiting algorithms
- **Scalable architecture**: Designed to handle thousands of concurrent requests with minimal overhead

## Testing

This package includes an extensive test suite covering all supported rate-limiting algorithms, IP zone isolation, cache management, and HashMap-based performance optimizations.

To run the tests:

```bash
cabal test
```

or

```bash
stack test
```

## When to Use This Library

- You need robust and efficient request throttling for your Haskell web application.
- You want to protect your service from abuse and DoS attacks.
- You require per-zone or per-user isolation of throttling policies with efficient lookups.
- You value both declarative configuration and the ability to customize behavior as needed.
- You need high-performance rate limiting that can scale to handle large numbers of concurrent requests and zones.

## Migration from Earlier Versions

If you're upgrading from an earlier version that used the programmatic API, the declarative configuration approach is now recommended:

**Old approach:**
```haskell
env <- initConfig getZoneFunction
env' <- addThrottle env "api" throttleConfig
let middleware = attackMiddleware env'
```

**New recommended approach:**
```haskell
let config = RateLimiterConfig { ... }
middleware <- buildRateLimiter config
```

The old programmatic API is still fully supported for advanced use cases.

## License

MIT License © 2025 Oleksandr Zhabenko

## References

- [rack-attack (Ruby)](https://github.com/rack/rack-attack)
- [keter (Haskell)](https://github.com/snoyberg/keter)
