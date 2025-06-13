# keter-rate-limiting-plugin

**keter-rate-limiting-plugin** is a modern, high-performance, and highly customisable rate limiting plugin for [keter](https://github.com/snoyberg/keter). It addresses [issue \#301](https://github.com/snoyberg/keter/issues/301) and brings robust, production-grade request throttling to Haskell web applications, with efficient in-memory caching, and IP zone isolation.

This library is inspired by [rack-attack](https://github.com/rack/rack-attack) (for `Keter.RateLimiter` and `Keter.RateLimiter.Cache`) and [Ruby on Rails](https://github.com/rails/rails) (for `Keter.RateLimiter.Notifications`). The plugin is intended for use as a drop-in middleware for keter-managed applications, but can be integrated with any Haskell web stack.

## Features

- **Four window algorithms**:
    - Fixed Window
    - Sliding Window
    - Token Bucket
    - Leaky Bucket
- **IP Zone Support**: Isolate caches and throttling policies per IP zone or customer segment.
- **Flexible Throttle Configuration**: Set limits, periods, algorithms, and unique identifiers per throttle.
- **Notification Hooks**: Pluggable notifications when rate limits are exceeded.
- **Convenient and Customisable API**:
    - Use simple wrappers for common scenarios (automatic key composition).
    - Or, for advanced use, fully control cache key structure and throttling logic.
- **Easy Integration**: Minimal code changes required to get started.


## Why Use This Plugin?

- **Scalability**: Per-zone caches and flexible throttling allow you to scale from single-user apps to multi-tenant platforms.
- **Security**: Protects your application from abusive clients and denial-of-service attacks.
- **Flexibility**: Choose between convenience (wrappers) and full customisability (manual key management).
- **Production-Ready**: Inspired by industry-standard tools; thoroughly tested and documented.
- **Open Source**: MIT licensed and community-friendly.


## Installation

1. **Add the package to your build-depends:**
    - In your `.cabal` file or `stack.yaml`:

```
build-depends: keter-rate-limiting-plugin
```

## Quick Start

```haskell
import Keter.RateLimiter.Snappy

main :: IO ()
main = do
  env <- initConfig (\req -> if requestIP req == "10.0.0.1" then "zoneA" else defaultIPZone)
  let throttle = ThrottleConfig 10 60 FixedWindow (\req -> Just (requestIP req))
      env' = addThrottle env "ip-throttle" throttle
  -- Wrap your application with attackMiddleware
  runApp $ attackMiddleware env' myApp
```


## Example Usage

### Using the Convenient API

```haskell
store <- createInMemoryStore
let cache :: Cache (InMemoryStore "counter")
    cache = newCache "counter" store
v1 <- incStoreWithZone cache "login-throttle" "zoneX" "userX" 10 :: IO Int
mVal <- readCacheWithZone cache "login-throttle" "zoneX" "userX" :: IO (Maybe Int)
```


### Using the Customisable API

```haskell
let customKey = "login-throttle:zoneY:userY:extra"
v1 <- incStore cache customKey 10 :: IO Int
mVal <- readCache cache customKey :: IO (Maybe Int)
```

Both approaches are fully supported: wrappers are recommended for most use cases, but you retain complete control for special requirements.

## Testing

This package includes an extensive test suite (see test/Main.hs) covering:

- All supported rate limiting algorithms
- IP zone isolation and cache reset
- Both wrapper-based and customisable API usage

To run the tests:

```
cabal test
```

or

```
stack test
```


## When to Use This Library

- You need simple and efficient request throttling for your Haskell web application.
- You want to protect your service from abuse and DoS attacks.
- You require per-zone or per-user isolation of throttling policies.
- You value both convenience and the ability to customise behaviour as needed.


## Alternatives

If you would like to use memory more efficiently (with fast, yet significant compression) there is alternative similar package that uses Google Snappy compression, see:
[keter-rate-limiting-plugin-snappy](https://github.com/Oleksandr-Zhabenko/keter-rate-limiting-plugin)

## Licence

MIT License © 2025 Oleksandr Zhabenko

## References

- [rack-attack (Ruby)](https://github.com/rack/rack-attack)
- [keter (Haskell)](https://github.com/snoyberg/keter)

**keter-rate-limiting-plugin**: modern, fast, flexible, and fully customisable rate limiting for Haskell and keter.

```
