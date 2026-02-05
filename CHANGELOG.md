# Changelog

## [0.2.0](https://github.com/tylerbutler/birch/compare/v0.1.2...v0.2.0) (2026-02-03)


### ⚠ BREAKING CHANGES

* rename package from gleam_log to birch ([#21](https://github.com/tylerbutler/birch/issues/21))

### Features

* add error result helpers, time providers, and caller id capture ([#24](https://github.com/tylerbutler/birch/issues/24)) ([5d7f7d3](https://github.com/tylerbutler/birch/commit/5d7f7d378adcf73add371f2a06da82a05b00b630))
* **async:** implement actor-based async handler using gleam_otp ([#23](https://github.com/tylerbutler/birch/issues/23)) ([50201c7](https://github.com/tylerbutler/birch/commit/50201c72d08338544af3962098a3a471249fc2ba))
* **claude:** add FFI validation hook ([4e98959](https://github.com/tylerbutler/birch/commit/4e989597820807208ad4d9c2d254a3dcaa4308ee))
* **examples:** add 17 comprehensive example applications ([#27](https://github.com/tylerbutler/birch/issues/27)) ([a24b5ff](https://github.com/tylerbutler/birch/commit/a24b5ffa135d556e15298872068b9b86636a8cfe))
* **handler:** add consola-style handler with box output, grouping, and semantic types ([844a70b](https://github.com/tylerbutler/birch/commit/844a70b1176c3e2dffb51f526587650875cf67d5))
* **handler:** unify console handlers with customizable level formatting ([#29](https://github.com/tylerbutler/birch/issues/29)) ([bb7caea](https://github.com/tylerbutler/birch/commit/bb7caea642fb42729f1d9fa4c3ff6bbf6d721c98))
* implement async handler for non-blocking log output ([#7](https://github.com/tylerbutler/birch/issues/7)) ([248cecd](https://github.com/tylerbutler/birch/commit/248cecd1312abb7081c1778bb7eb4cb80ff0e4d5))
* implement Erlang :logger backend integration ([#13](https://github.com/tylerbutler/birch/issues/13)) ([06c52d6](https://github.com/tylerbutler/birch/commit/06c52d69a2fb509f6d93bc16f5fa51a18c7e5b91))
* implement handler error callbacks for monitoring handler failures ([#14](https://github.com/tylerbutler/birch/issues/14)) ([be0938d](https://github.com/tylerbutler/birch/commit/be0938d04a7317cfda11d22ca4dd2d6bebc083c1))
* implement JSON builder pattern for customizable JSON output ([#17](https://github.com/tylerbutler/birch/issues/17)) ([546617d](https://github.com/tylerbutler/birch/commit/546617d0360210f03f488e46a68c85e6f6f26ce6))
* implement runtime level changes ([#8](https://github.com/tylerbutler/birch/issues/8)) ([3b76b00](https://github.com/tylerbutler/birch/commit/3b76b00dbd0795fc7e69cfbf1c9d4a456db992a1))
* implement sampling and rate limiting ([#11](https://github.com/tylerbutler/birch/issues/11)) ([60d550f](https://github.com/tylerbutler/birch/commit/60d550fe9722f84c68f0aee602c4305d817ed00a))
* implement scoped context for request-scoped metadata ([#12](https://github.com/tylerbutler/birch/issues/12)) ([7e89a7d](https://github.com/tylerbutler/birch/commit/7e89a7d03dd41f85de931443b2e9854418245c81))
* implement time-based log rotation ([#9](https://github.com/tylerbutler/birch/issues/9)) ([9c5859f](https://github.com/tylerbutler/birch/commit/9c5859f782f177c0fb942abf8b1777ce734680f5))
* initial gleam_log implementation ([#1](https://github.com/tylerbutler/birch/issues/1)) ([664a35f](https://github.com/tylerbutler/birch/commit/664a35ffb662262d00c335ddf0829a55eb980e98))


### Bug Fixes

* **ci:** add semver confirmation bypass for pre-1.0 publish ([480d1cd](https://github.com/tylerbutler/birch/commit/480d1cdb7ec844d52134814b0fc7c647ab0d9a5f))
* **ci:** bypass pre-1.0 semver confirmation in publish workflow ([c8f742d](https://github.com/tylerbutler/birch/commit/c8f742daeeb77ee0cdb9f60c4bc569d2b059f074))
* resolve import cycle between platform, handler, and config modules ([#16](https://github.com/tylerbutler/birch/issues/16)) ([eba169e](https://github.com/tylerbutler/birch/commit/eba169e5df125029847224adfb82e3ce29d76ab0))


### Code Refactoring

* rename package from gleam_log to birch ([#21](https://github.com/tylerbutler/birch/issues/21)) ([636ee50](https://github.com/tylerbutler/birch/commit/636ee507f0ac8bda49b8f7b19599972825fabab0))
* simplify code patterns and reduce FFI ([#30](https://github.com/tylerbutler/birch/issues/30)) ([d4fdcad](https://github.com/tylerbutler/birch/commit/d4fdcadd3a4461c8a20daa7260e28067ae095cf1))

## [0.1.2](https://github.com/tylerbutler/birch/compare/v0.1.1...v0.1.2) (2026-02-03)


### Bug Fixes

* **ci:** bypass pre-1.0 semver confirmation in publish workflow ([c8f742d](https://github.com/tylerbutler/birch/commit/c8f742daeeb77ee0cdb9f60c4bc569d2b059f074))

## [0.1.1](https://github.com/tylerbutler/birch/compare/v0.1.0...v0.1.1) (2026-01-24)


### Features

* **claude:** add FFI validation hook ([4e98959](https://github.com/tylerbutler/birch/commit/4e989597820807208ad4d9c2d254a3dcaa4308ee))
* **examples:** add 17 comprehensive example applications ([#27](https://github.com/tylerbutler/birch/issues/27)) ([a24b5ff](https://github.com/tylerbutler/birch/commit/a24b5ffa135d556e15298872068b9b86636a8cfe))
* **handler:** add consola-style handler with box output, grouping, and semantic types ([844a70b](https://github.com/tylerbutler/birch/commit/844a70b1176c3e2dffb51f526587650875cf67d5))
* **handler:** unify console handlers with customizable level formatting ([#29](https://github.com/tylerbutler/birch/issues/29)) ([bb7caea](https://github.com/tylerbutler/birch/commit/bb7caea642fb42729f1d9fa4c3ff6bbf6d721c98))


### Code Refactoring

* simplify code patterns and reduce FFI ([#30](https://github.com/tylerbutler/birch/issues/30)) ([d4fdcad](https://github.com/tylerbutler/birch/commit/d4fdcadd3a4461c8a20daa7260e28067ae095cf1))

## 0.1.0 (2026-01-03)


### ⚠ BREAKING CHANGES

* rename package from gleam_log to birch ([#21](https://github.com/tylerbutler/birch/issues/21))

### Features

* add error result helpers, time providers, and caller id capture ([#24](https://github.com/tylerbutler/birch/issues/24)) ([5d7f7d3](https://github.com/tylerbutler/birch/commit/5d7f7d378adcf73add371f2a06da82a05b00b630))
* **async:** implement actor-based async handler using gleam_otp ([#23](https://github.com/tylerbutler/birch/issues/23)) ([50201c7](https://github.com/tylerbutler/birch/commit/50201c72d08338544af3962098a3a471249fc2ba))
* implement async handler for non-blocking log output ([#7](https://github.com/tylerbutler/birch/issues/7)) ([248cecd](https://github.com/tylerbutler/birch/commit/248cecd1312abb7081c1778bb7eb4cb80ff0e4d5))
* implement Erlang :logger backend integration ([#13](https://github.com/tylerbutler/birch/issues/13)) ([06c52d6](https://github.com/tylerbutler/birch/commit/06c52d69a2fb509f6d93bc16f5fa51a18c7e5b91))
* implement handler error callbacks for monitoring handler failures ([#14](https://github.com/tylerbutler/birch/issues/14)) ([be0938d](https://github.com/tylerbutler/birch/commit/be0938d04a7317cfda11d22ca4dd2d6bebc083c1))
* implement JSON builder pattern for customizable JSON output ([#17](https://github.com/tylerbutler/birch/issues/17)) ([546617d](https://github.com/tylerbutler/birch/commit/546617d0360210f03f488e46a68c85e6f6f26ce6))
* implement runtime level changes ([#8](https://github.com/tylerbutler/birch/issues/8)) ([3b76b00](https://github.com/tylerbutler/birch/commit/3b76b00dbd0795fc7e69cfbf1c9d4a456db992a1))
* implement sampling and rate limiting ([#11](https://github.com/tylerbutler/birch/issues/11)) ([60d550f](https://github.com/tylerbutler/birch/commit/60d550fe9722f84c68f0aee602c4305d817ed00a))
* implement scoped context for request-scoped metadata ([#12](https://github.com/tylerbutler/birch/issues/12)) ([7e89a7d](https://github.com/tylerbutler/birch/commit/7e89a7d03dd41f85de931443b2e9854418245c81))
* implement time-based log rotation ([#9](https://github.com/tylerbutler/birch/issues/9)) ([9c5859f](https://github.com/tylerbutler/birch/commit/9c5859f782f177c0fb942abf8b1777ce734680f5))
* initial gleam_log implementation ([#1](https://github.com/tylerbutler/birch/issues/1)) ([664a35f](https://github.com/tylerbutler/birch/commit/664a35ffb662262d00c335ddf0829a55eb980e98))


### Bug Fixes

* resolve import cycle between platform, handler, and config modules ([#16](https://github.com/tylerbutler/birch/issues/16)) ([eba169e](https://github.com/tylerbutler/birch/commit/eba169e5df125029847224adfb82e3ce29d76ab0))


### Code Refactoring

* rename package from gleam_log to birch ([#21](https://github.com/tylerbutler/birch/issues/21)) ([636ee50](https://github.com/tylerbutler/birch/commit/636ee507f0ac8bda49b8f7b19599972825fabab0))

## Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).
