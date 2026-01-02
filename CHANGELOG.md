# Changelog

## 0.1.0 (2026-01-02)


### Features

* implement async handler for non-blocking log output ([#7](https://github.com/tylerbutler/gleam_log/issues/7)) ([248cecd](https://github.com/tylerbutler/gleam_log/commit/248cecd1312abb7081c1778bb7eb4cb80ff0e4d5))
* implement Erlang :logger backend integration ([#13](https://github.com/tylerbutler/gleam_log/issues/13)) ([06c52d6](https://github.com/tylerbutler/gleam_log/commit/06c52d69a2fb509f6d93bc16f5fa51a18c7e5b91))
* implement handler error callbacks for monitoring handler failures ([#14](https://github.com/tylerbutler/gleam_log/issues/14)) ([be0938d](https://github.com/tylerbutler/gleam_log/commit/be0938d04a7317cfda11d22ca4dd2d6bebc083c1))
* implement runtime level changes ([#8](https://github.com/tylerbutler/gleam_log/issues/8)) ([3b76b00](https://github.com/tylerbutler/gleam_log/commit/3b76b00dbd0795fc7e69cfbf1c9d4a456db992a1))
* implement sampling and rate limiting ([#11](https://github.com/tylerbutler/gleam_log/issues/11)) ([60d550f](https://github.com/tylerbutler/gleam_log/commit/60d550fe9722f84c68f0aee602c4305d817ed00a))
* implement scoped context for request-scoped metadata ([#12](https://github.com/tylerbutler/gleam_log/issues/12)) ([7e89a7d](https://github.com/tylerbutler/gleam_log/commit/7e89a7d03dd41f85de931443b2e9854418245c81))
* implement time-based log rotation ([#9](https://github.com/tylerbutler/gleam_log/issues/9)) ([9c5859f](https://github.com/tylerbutler/gleam_log/commit/9c5859f782f177c0fb942abf8b1777ce734680f5))
* initial gleam_log implementation ([#1](https://github.com/tylerbutler/gleam_log/issues/1)) ([664a35f](https://github.com/tylerbutler/gleam_log/commit/664a35ffb662262d00c335ddf0829a55eb980e98))


### Bug Fixes

* resolve import cycle between platform, handler, and config modules ([#16](https://github.com/tylerbutler/gleam_log/issues/16)) ([eba169e](https://github.com/tylerbutler/gleam_log/commit/eba169e5df125029847224adfb82e3ce29d76ab0))

## Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).
