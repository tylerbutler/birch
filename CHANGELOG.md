# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## v0.3.0 - 2026-02-21


#### Added

- New `:logger` formatter API in `birch/erlang_logger`.

  `setup()` and `setup_with_config(config)` configure birch formatting on the default `:logger` handler. `ensure_formatter_configured()` provides idempotent setup (called automatically on first use).

`install_formatter()`, `install_formatter_with(format)`, and `install_formatter_on(handler_id, format)` install birch as a formatter on specific `:logger` handlers. `remove_formatter()` and `remove_formatter_from(handler_id)` restore OTP defaults.

`console.build_format_fn(config)` creates a formatter closure from a `ConsoleConfig`, usable independently of a handler (e.g., for `:logger` formatter configuration).

#### Changed

- Default to BEAM `:logger` on Erlang target.

  On Erlang, birch now forwards logs through OTP's `:logger` system by default instead of writing directly to the console. birch installs a `:logger` **formatter** (`format/2` callback) that applies birch's presentation (timestamps, colors, simple/fancy styles, metadata) to both birch-originated and OTP/library log events. The `:logger` handler controls output (console, file, etc.) while birch controls formatting.

`forward_to_logger_raw()` now sends structured metadata (logger name, metadata, caller ID) to `:logger` instead of pre-formatting the message, so the formatter can reconstruct full `LogRecord` values.

`default_handler_id` changed from `"birch"` to `"default"` to reflect the target handler. The old value is preserved as the deprecated `legacy_handler_id` constant.

JavaScript target continues to use the console handler with colors.
- Invert :logger integration to use BEAM's formatter callback instead of handler callback.

  birch now provides a :logger **formatter** (via `format/2`) rather than installing itself as a :logger **handler**. The existing BEAM handler controls output (console, file, etc.) while birch controls formatting. This eliminates brittle manual Gleam record construction and opaque type access in the Erlang FFI.

New API: `install_formatter()`, `install_formatter_with(format)`, `install_formatter_on(handler_id, format)`, `remove_formatter()`, `remove_formatter_from(handler_id)`.

#### Deprecated

- Deprecate type aliases (LogLevel, LogHandler, LogMetadata, TimestampFormatter, Config), logger_* wrapper functions, _m suffix variants, config.empty(), config.get_on_error(), and erlang_logger.erlang_level_to_atom(). These will be removed in 1.0.
- Deprecate `install_logger_handler()`, `install_logger_handler_with_id()`, `uninstall_logger_handler()`, and `uninstall_logger_handler_with_id()` in `birch/erlang_logger`. Use `install_formatter()` / `remove_formatter()` instead.

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

