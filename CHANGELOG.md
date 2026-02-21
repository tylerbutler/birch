# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## v0.3.0 - 2026-02-21


#### Added

##### Formatter API in `birch/erlang_logger`

- `setup()` and `setup_with_config(config)` — configure birch formatting on the default `:logger` handler. Formatting is also set up automatically on first use via `ensure_formatter_configured()`.
- `install_formatter()`, `install_formatter_with(format)`, `install_formatter_on(handler_id, format)` — install birch formatting on specific `:logger` handlers.
- `remove_formatter()`, `remove_formatter_from(handler_id)` — restore OTP default formatting.
- `console.build_format_fn(config)` — create a standalone formatter closure from a `ConsoleConfig`, useful for custom `:logger` configurations.


#### Changed

##### Default to OTP `:logger` on Erlang

Logs are now routed through `:logger` instead of being written directly to the console. birch controls how log messages are formatted (timestamps, colors, metadata) while `:logger` controls where they go (console, file, etc.). This means birch-formatted output now applies to OTP and library log events too, giving you consistent formatting across your entire application.

The `default_handler_id` constant changed from `"birch"` to `"default"`. The old value is still available as the deprecated `legacy_handler_id`.

The JavaScript target is unchanged and continues using the console handler with colors.

##### Use BEAM formatter callback instead of handler callback for `:logger` integration

birch plugs into `:logger` as a **formatter** rather than a **handler**. This means your existing BEAM handler configuration (console, file, etc.) stays in control of output, and birch just handles how messages look. This is a simpler, more compatible approach that works better alongside other OTP logging tools.

See the new `install_formatter()` / `remove_formatter()` functions in `birch/erlang_logger`.


#### Deprecated

##### Legacy API aliases and wrappers

The following are deprecated and will be removed in 1.0:
- **Type aliases**: `LogLevel`, `LogHandler`, `LogMetadata`, `TimestampFormatter`, `Config` — use the underlying types directly.
- **Logger wrapper functions**: `logger_info`, `logger_debug`, etc. — use the logger methods directly.
- **`_m` suffix variants**: `info_m`, `debug_m`, etc. — use the standard functions with metadata parameter.
- **Other**: `config.empty()`, `config.get_on_error()`, `erlang_logger.erlang_level_to_atom()`.

##### Handler-based `:logger` functions in `birch/erlang_logger`

Replace `install_logger_handler()` and `uninstall_logger_handler()` (and their `_with_id` variants) with `install_formatter()` / `remove_formatter()`. These will be removed in 1.0.


## v0.2.1 - 2026-02-15

### Features

- add Erlang code coverage support
- add LCOV export for Codecov integration
- add template patterns for consistency ([#42](https://github.com/tylerbutler/birch/issues/42))

### Bug Fixes

- **ci:** add JS target stub to async_actor to fix warnings-as-errors build ([#49](https://github.com/tylerbutler/birch/issues/49))
- **claude:** add required frontmatter to ffi-reviewer agent ([#37](https://github.com/tylerbutler/birch/issues/37))

### Code Refactoring

- remove dead code, deduplicate, and simplify codebase
- rename coverage tasks for clarity
- replace timestamp ffi with gleam_time ([#36](https://github.com/tylerbutler/birch/issues/36))

## v0.2.0 - 2026-02-03

### BREAKING CHANGES

- rename package from gleam_log to birch ([#21](https://github.com/tylerbutler/birch/issues/21))

### Features

- add error result helpers, time providers, and caller id capture ([#24](https://github.com/tylerbutler/birch/issues/24))
- **async:** implement actor-based async handler using gleam_otp ([#23](https://github.com/tylerbutler/birch/issues/23))
- **claude:** add FFI validation hook
- **examples:** add 17 comprehensive example applications ([#27](https://github.com/tylerbutler/birch/issues/27))
- **handler:** add consola-style handler with box output, grouping, and semantic types
- **handler:** unify console handlers with customizable level formatting ([#29](https://github.com/tylerbutler/birch/issues/29))
- implement async handler for non-blocking log output ([#7](https://github.com/tylerbutler/birch/issues/7))
- implement Erlang :logger backend integration ([#13](https://github.com/tylerbutler/birch/issues/13))
- implement handler error callbacks for monitoring handler failures ([#14](https://github.com/tylerbutler/birch/issues/14))
- implement JSON builder pattern for customizable JSON output ([#17](https://github.com/tylerbutler/birch/issues/17))
- implement runtime level changes ([#8](https://github.com/tylerbutler/birch/issues/8))
- implement sampling and rate limiting ([#11](https://github.com/tylerbutler/birch/issues/11))
- implement scoped context for request-scoped metadata ([#12](https://github.com/tylerbutler/birch/issues/12))
- implement time-based log rotation ([#9](https://github.com/tylerbutler/birch/issues/9))
- initial gleam_log implementation ([#1](https://github.com/tylerbutler/birch/issues/1))

### Bug Fixes

- **ci:** add semver confirmation bypass for pre-1.0 publish
- **ci:** bypass pre-1.0 semver confirmation in publish workflow
- resolve import cycle between platform, handler, and config modules ([#16](https://github.com/tylerbutler/birch/issues/16))

### Code Refactoring

- rename package from gleam_log to birch ([#21](https://github.com/tylerbutler/birch/issues/21))
- simplify code patterns and reduce FFI ([#30](https://github.com/tylerbutler/birch/issues/30))

## v0.1.2 - 2026-02-03

### Bug Fixes

- **ci:** bypass pre-1.0 semver confirmation in publish workflow

## v0.1.1 - 2026-01-24

### Features

- **claude:** add FFI validation hook
- **examples:** add 17 comprehensive example applications ([#27](https://github.com/tylerbutler/birch/issues/27))
- **handler:** add consola-style handler with box output, grouping, and semantic types
- **handler:** unify console handlers with customizable level formatting ([#29](https://github.com/tylerbutler/birch/issues/29))

### Code Refactoring

- simplify code patterns and reduce FFI ([#30](https://github.com/tylerbutler/birch/issues/30))

## v0.1.0 - 2026-01-03

### BREAKING CHANGES

- rename package from gleam_log to birch ([#21](https://github.com/tylerbutler/birch/issues/21))

### Features

- add error result helpers, time providers, and caller id capture ([#24](https://github.com/tylerbutler/birch/issues/24))
- **async:** implement actor-based async handler using gleam_otp ([#23](https://github.com/tylerbutler/birch/issues/23))
- implement async handler for non-blocking log output ([#7](https://github.com/tylerbutler/birch/issues/7))
- implement Erlang :logger backend integration ([#13](https://github.com/tylerbutler/birch/issues/13))
- implement handler error callbacks for monitoring handler failures ([#14](https://github.com/tylerbutler/birch/issues/14))
- implement JSON builder pattern for customizable JSON output ([#17](https://github.com/tylerbutler/birch/issues/17))
- implement runtime level changes ([#8](https://github.com/tylerbutler/birch/issues/8))
- implement sampling and rate limiting ([#11](https://github.com/tylerbutler/birch/issues/11))
- implement scoped context for request-scoped metadata ([#12](https://github.com/tylerbutler/birch/issues/12))
- implement time-based log rotation ([#9](https://github.com/tylerbutler/birch/issues/9))
- initial gleam_log implementation ([#1](https://github.com/tylerbutler/birch/issues/1))

### Bug Fixes

- resolve import cycle between platform, handler, and config modules ([#16](https://github.com/tylerbutler/birch/issues/16))

### Code Refactoring

- rename package from gleam_log to birch ([#21](https://github.com/tylerbutler/birch/issues/21))

