<img src="https://r2cdn.perplexity.ai/pplx-full-logo-primary-dark%402x.png" class="logo" width="120"/>

# use British English, do not change anything except examples with the changed api

Ось оновлені приклади у README.md із використанням нового API (з полем `throttleTokenBucketTTL` у `ThrottleConfig`), решта тексту залишена без змін, з британським англійським:

---

## Quick Start

```haskell
import Keter.RateLimiter.Snappy

main :: IO ()
main = do
  env <- initConfig (\req -> if requestIP req == "10.0.0.1" then "zoneA" else defaultIPZone)
  let throttle = ThrottleConfig
        { throttleLimit = 10
        , throttlePeriod = 60
        , throttleAlgorithm = FixedWindow
        , throttleIdentifier = \req -> Just (requestIP req)
        , throttleTokenBucketTTL = Nothing
        }
      env' = addThrottle env "ip-throttle" throttle
  runApp $ attackMiddleware env' myApp
```


---

## Example Usage

### Using the Convenient API

```haskell
store <- createInMemoryStore
let cache :: Cache (InMemoryStore "counter")
    cache = newCache "counter" store
v1 <- incStoreWithZone cache "login-throttle" "zoneX" "userX" 10 :: IO Int
mVal <- readCacheWithZone cache "login-throttle" "zoneX" "userX" :: IO (Maybe Int)
```


---

### Using the Customisable API

```haskell
let customKey = "login-throttle:zoneY:userY:extra"
v1 <- incStore cache customKey 10 :: IO Int
mVal <- readCache cache customKey :: IO (Maybe Int)
```


---

### Token Bucket Example (with TTL)

```haskell
let tokenBucketThrottle = ThrottleConfig
      { throttleLimit = 5
      , throttlePeriod = 60
      , throttleAlgorithm = TokenBucket
      , throttleIdentifier = \req -> Just (requestIP req)
      , throttleTokenBucketTTL = Just 3600 -- TTL 1 hour
      }
    env'' = addThrottle env' "token-bucket" tokenBucketThrottle
```


---

Якщо потрібні додаткові приклади або допомога — звертайтеся!

