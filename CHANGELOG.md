# Revision history for keter-rate-limiting-plugin

## 0.1.0.0 -- 2025-08-08

* First version. Released on an unsuspecting world.

## 0.1.0.1 -- 2025-08-08

* First version revised A. Fixed description and some cleaning up.

## 0.1.0.2 -- 2025-08-08

* First version revised B. Fixed some documentation issues.

## 0.1.1.0 -- 2025-08-11

* First version revised C. Added new functions to support more middleware functionality for better keter integration. Changed api for some functions because of the necessity to take into account also throttle names for every request (especially important for multiple throttles per zone/user etc). Improved documentation. Added more tests and some dependencies (most of them are already dependencies — at least indirect — of keter) and removed not used any more dependency on containers. 

## 0.1.2.0 -- 2025-08-18

* First version revised D. Changed Keter.RateLimiter.WAI module to more declaraive approach recommended by @jappeace for keter integration. Reflected the changes in the README.md file.

