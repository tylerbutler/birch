# Codebase Concerns

**Analysis Date:** 2025-02-27

## Pre-1.0 API Stability

**Status:** API is pre-1.0 and unstable

The README explicitly warns:
> birch is not yet 1.0. This means the API is unstable and features and APIs may be removed in minor releases.

- Files: `README.md`, `gleam.toml` (version 0.3.0)
- Impact: Users should not rely on current API surface for production. Breaking changes expected before 1.0.
- Priority: High - affects all users
- Recommendation: Lock to current minor version range until 1.0 release. Track changelog between minor versions.

## Cross-Platform FFI Parity Risk

**Issue:** Large FFI surface with platform-specific implementations that must stay in sync.

- Files: `src/birch_ffi.erl` (428 lines), `src/birch_ffi.mjs` (868 lines), `src/birch_erlang_logger_ffi.erl`, `src/birch_erlang_logger_ffi.mjs`
- Critical functions: timestamp, color detection, async writers, scope context, compression
- Risk: Behavioral divergence between Erlang and JavaScript targets
- Impact: Tests must run on both targets; bugs may only surface on one platform
- Recommendations:
  1. Maintain strong test coverage on both targets (currently done via `just test` and `just test-js`)
  2. Consider integration tests that verify parity (e.g., same timestamps, same color detection logic)
  3. Document expected differences (e.g., AsyncLocalStorage availability on Node.js only)

## Scoped Context Limitations on JavaScript

**Issue:** JavaScript lacks Erlang's process dictionary; implementation varies by runtime.

- Files: `src/birch/scope.gleam`, `src/birch_ffi.mjs`
- Current implementation:
  - Node.js: Uses AsyncLocalStorage (available Node.js 16+)
  - Other runtimes (Deno, Bun, browser): Falls back to stack-based context (no async propagation)
- Limitation: `scope.is_available()` returns false on non-Node.js targets
- Impact:
  - Scoped context works within a single async operation on Node.js
  - Does not propagate across await boundaries on Deno/Bun
  - Not usable in browser environments
- Recommendations:
  1. Update documentation to clearly state Node.js-only async propagation
  2. Consider alternative patterns for Deno/Bun (e.g., explicit context passing)
  3. Add tests in `test/integration/` for each runtime to verify behavior

## File Handler Rotation Incomplete

**Issue:** Time-based rotation has basic implementation; cleanup logic may have edge cases.

- Files: `src/birch/handler/file.gleam` (330 lines)
- Implemented: Size-based rotation with numeric suffixes, time-based rotation with timestamp suffixes
- Known gaps:
  - **Compression fallback fragile**: If gzip compression fails, silently falls back to uncompressed (line 178-186). Failed compression is logged but file is still rotated, potentially losing the original.
  - **Marker file cleanup**: `.rotation` marker files accumulate if time-based rotation is used but never completes. No automatic cleanup.
  - **Timestamp precision**: `format_rotation_timestamp()` truncates ISO 8601 to first 13 chars (hourly) or 10 chars (daily). Fragile if timestamp format changes.
  - **Directory separator hardcoded**: Uses "/" for path splitting (line 269, 304), won't work on Windows.
  - **File listing order**: `cleanup_old_rotated_files()` sorts by string comparison (line 243), works for numeric and timestamp suffixes but may fail if mixed formats.
- Impact: Production deployments with file rotation may lose logs or accumulate stale files
- Priority: Medium-High (experimental feature)
- Recommendations:
  1. Add integration tests with real file operations (create, rotate, verify)
  2. Fix Windows path handling: use `simplifile` path utilities instead of manual "/" splitting
  3. Make compression failure more explicit: consider returning Result from rotation function
  4. Add marker file cleanup on rotation success
  5. Document rotation behavior and edge cases in module docs

## Async Handler Queue Complexity

**Issue:** Async handler has three different overflow behaviors with potential data loss scenarios.

- Files: `src/birch/handler/async.gleam`, `src/birch/internal/async_actor.gleam`
- Overflow modes:
  - `DropOldest` (0): Removes oldest message when queue full - can lose earlier logs
  - `DropNewest` (1): Drops the new message - silently ignores recent logs
  - `Block` (2): Tries to block but on OTP just adds to mailbox (mailbox provides backpressure)
- Issue: No warning or metric when logs are dropped. Silent data loss.
- Impact: Applications may not realize logs are being discarded under high load
- Recommendations:
  1. Add error callback mechanism to notify about dropped records
  2. Log dropped count on flush (e.g., "dropped 42 records due to queue overflow")
  3. Add configuration to fail-safe (e.g., fall back to sync logging on overflow instead of dropping)
  4. Document queue overflow behavior clearly with examples

## persistent_term GC Risk (Erlang Only)

**Issue:** Global configuration cached in `persistent_term` may not be garbage collected efficiently.

- Files: `src/birch_ffi.erl` (lines 67-92, 95-119)
- Implementation: Uses `persistent_term` for global config and cached default logger
- Risk: According to recent Erlang documentation (OTP 26.2+), `persistent_term` entries are only GC'd when the module that created them is unloaded. If birch is used in a long-lived application, stale configurations may accumulate.
- Current code:
  - `set_global_config()` updates config every time `log.configure()` is called
  - `set_cached_default_logger()` updates on every logger creation/modification
  - No cleanup of previous values
- Impact: Memory growth in applications that call `log.configure()` multiple times
- Priority: Medium
- Recommendations:
  1. Document the GC behavior in `birch_ffi.erl` comments
  2. Consider alternative storage for mutable data (e.g., ETS table) if long-lived mutability is needed
  3. Add a clear note in CLAUDE.md about this limitation
  4. Benchmark memory impact with repeated `configure()` calls

## Handler Error Isolation Incomplete

**Issue:** Handler write functions can fail; error handling path is minimal.

- Files: `src/birch/handler/file.gleam` (lines 99-107), all handlers
- Current behavior:
  - File handler errors go to stderr via `io.println_error()`
  - Console/JSON handlers rely on Gleam's `io` module (may panic on error)
  - No mechanism to notify application when handlers fail
  - No retry logic for transient failures
- Impact:
  - Disk full errors are printed to stderr but not surfaced to application code
  - Critical logs may fail silently
  - No observability into handler health
- Recommendations:
  1. Add handler error callback: `fn(handler_name: String, error: String) -> Nil`
  2. Provide default error handler that logs to stderr
  3. Update `handler.gleam` interface to support error callbacks
  4. Document error contract for custom handlers

## Platform Color Detection Heuristics

**Issue:** TTY detection relies on environment variable inspection; may be inaccurate.

- Files: `src/birch_ffi.erl` (lines 19-61), `src/birch_ffi.mjs`
- Logic:
  - Erlang: Checks `io:getopts(standard_io)` for terminal flag, falls back to TERM env var
  - JavaScript: Checks `process.stdout.isTTY` on Node, checks process.env.TERM elsewhere
- Limitations:
  - May detect TTY incorrectly in CI/CD environments (GitHub Actions, GitLab CI may or may not set TERM)
  - Colors forced off if TERM=dumb even in piped output where ANSI would be safe
  - No way to override color behavior in code (only via env vars)
- Impact: Colored output in wrong contexts (e.g., CI logs with color codes), or no color where wanted
- Recommendations:
  1. Add logger configuration option to force colors on/off
  2. Check `FORCE_COLOR` env var (de facto standard) before environment detection
  3. Add integration tests checking behavior with various TERM values
  4. Document color detection logic for users

## Level Comparison Edge Cases

**Issue:** Sampling and level filtering use strict integer comparison; behavior with custom levels unclear.

- Files: `src/birch/level.gleam`, `src/birch/sampling.gleam`
- Levels defined: Trace(0), Debug(1), Info(2), Notice(3), Warn(4), Err(5), Critical(6), Alert(7), Fatal(8)
- Sampling logic: `level.gt(log_level, sample_config.level)` to determine if above threshold
- Risk: If someone manually constructs a level outside the standard set, comparison may fail
- Minor risk due to opaque type enforcement, but worth noting
- Recommendations:
  1. Add tests for all level comparisons in both directions
  2. Document level ordering in PRD (already does)
  3. Consider adding `level.min()` and `level.max()` helpers

## Formatter Output Stability

**Issue:** Metadata formatting delegates to `metadata_value_to_string()` which may change behavior.

- Files: `src/birch/formatter.gleam`, `src/birch/record.gleam`
- Issue: Metadata value formatting (StringVal, IntVal, FloatVal, BoolVal) relies on `metadata_value_to_string()`
- Risk: If formatter changes how values are stringified (e.g., float precision), logs change format
- Impact: Log parsing/integration tests may break on formatter updates
- Recommendations:
  1. Document formatter stability contract (e.g., "float values have 2 decimal places")
  2. Add comprehensive formatter tests covering all metadata types
  3. Consider versioning formatter output for backward compatibility

## Erlang :logger Integration Default Behavior

**Issue:** Auto-installation of birch formatter on `:logger` default handler may override user settings.

- Files: `src/birch/erlang_logger.gleam` (lines 28-36)
- Current behavior: When birch is used on Erlang without explicit handlers, it installs its formatter on `:logger`'s default handler automatically
- Risk:
  - If user has configured `:logger` in `config.exs` (Elixir) or via OTP config, birch's auto-install overrides it
  - Affects ALL `:logger` output, not just birch logs (OTP supervisor reports, etc.)
  - No way to opt-in without explicit handler configuration
- Impact: Confusing behavior in Elixir applications where logging config exists in config.exs
- Recommendations:
  1. Add explicit setup via `erlang_logger.setup()` call (already documented, good)
  2. Make auto-install only happen on first explicit log call (currently happens on config)
  3. Consider adding warning when auto-install detects existing :logger config
  4. Document this clearly in README

## Test Coverage Mapping to Compiled Code

**Issue:** Coverage reports show compiled Erlang/JavaScript code, not Gleam source.

- Files: Mentioned in CLAUDE.md Known Limitations section
- Issue: Line numbers in coverage reports don't match Gleam source
- Impact: Coverage percentage reports are inaccurate for Gleam
- Workaround: Use `just coverage` for module-level metrics locally
- Limitation inherent to Gleam compilation model
- Recommendations:
  1. Keep using module-level metrics locally (current approach)
  2. Don't rely on hosted coverage services (Codecov, Coveralls) for meaningful results
  3. Maintain manual checklist of critical paths to test (handlers, formatting, rotation)

## Stale Build Artifacts Risk

**Issue:** Unclean builds can leave stale compiled artifacts that affect coverage and test results.

- Documented in CLAUDE.md: "If coverage shows unexpected modules (e.g., 0% coverage for non-existent files), run `rm -rf build/dev && just build`"
- Impact: Confusing coverage reports, potential test side effects
- Recommendations:
  1. Add pre-test cleanup to `just test` commands
  2. Document in contribution guide: "Always clean build: `rm -rf build && gleam build`"
  3. Consider adding Git pre-commit hook to prevent checking in stale artifacts

## JavaScript FFI Missing Window/Global Object Check

**Issue:** JavaScript FFI assumes global environment availability; may fail in Workers or edge runtimes.

- Files: `src/birch_ffi.mjs` (868 lines)
- Risk:
  - Web Workers don't have `process` object (Node.js detection fails)
  - Edge runtimes (Cloudflare Workers, Deno Deploy) have limited globals
  - Browser environments have `window` but not `process.stderr`
- Current behavior: Unclear how these edge cases are handled
- Impact: Library may panic or behave unexpectedly in non-standard JS environments
- Recommendations:
  1. Add explicit environment detection: `if (typeof process !== 'undefined')`
  2. Provide fallback implementations for Worker/edge environments
  3. Add tests for Worker environments using `jest --detectOpenHandles`
  4. Document supported JavaScript environments in README

## Missing JavaScript GZIP Implementation

**Issue:** File handler compression uses platform FFI but JavaScript implementation may be incomplete.

- Files: `src/birch_ffi.mjs` function `compress_file_gzip()`
- Risk: JavaScript implementation may:
  - Not have gzip available (requires external dependency)
  - Have different behavior than Erlang implementation
  - Fail silently or throw unexpected errors
- Impact: File rotation with compression may not work on JavaScript targets
- Recommendations:
  1. Review JavaScript gzip implementation (check if zlib is available)
  2. Add explicit fallback: if gzip unavailable, return Error with helpful message
  3. Add tests for compression on both Erlang and JavaScript
  4. Document JavaScript file handler limitations in console handler docs

## Sampling Rate Randomness Unseeded

**Issue:** `float.random()` is used for sampling without control over random source.

- Files: `src/birch/sampling.gleam` (line 77)
- Issue: Using `gleam/float.random()` means randomness is uncontrolled and non-reproducible
- Impact:
  - Tests using sampling cannot be deterministic
  - No way to seed random for reproducible test scenarios
  - Tests may flake due to random chance
- Recommendations:
  1. Add optional custom random function parameter to sampling config
  2. Provide test helper with seeded randomness: `sampling.config_with_random(level, rate, fn() { ... })`
  3. Update tests to use deterministic test helpers

## Missing Configuration Validation

**Issue:** No validation of configuration parameters at setup time.

- Files: `src/birch/config.gleam`, `src/birch.gleam` (configure function)
- Risk:
  - Invalid queue sizes (0 or negative) accepted for async handlers
  - Invalid rotation byte limits accepted for file handlers
  - Invalid sample rates (> 1.0 or < 0.0) are clamped but others may not be
- Impact: Runtime failures or surprising behavior instead of early error detection
- Recommendations:
  1. Add validation functions: `validate_async_config()`, `validate_file_config()`
  2. Return Result from `configure()`: `configure(config) -> Result(Nil, String)`
  3. Add validation tests for edge cases (negative sizes, invalid rates, etc.)

## Global Logger Cache Invalidation Order

**Issue:** When global config changes, cached default logger must be invalidated; order matters.

- Files: `src/birch.gleam`, `src/birch_ffi.erl` (cache management)
- Risk: If global config is set but cached logger not cleared before next use, stale logger is returned
- Current implementation: `configure()` clears cache after setting config
- Edge case: If configure() is called twice in succession, second call may use stale cache from first
- Recommendations:
  1. Add tests for repeated configure() calls
  2. Document cache invalidation strategy in code comments
  3. Consider using single atomic operation instead of separate get/set

## Dependency Version Bounds

**Issue:** Dependencies have broad version ranges; breaking changes possible.

- `gleam.toml` specifies ranges like `">= 0.48.0 and < 2.0.0"`
- Risk: Major version jumps (0.x → 1.0) may introduce breaking changes
- Example: `gleam_stdlib >= 0.48.0 and < 2.0.0` allows 1.0.0 which could break compatibility
- Recommendations:
  1. Monitor upstream libraries for major version announcements
  2. Test against latest allowed versions in CI
  3. Pin to minor version ranges in development: `~> 0.48`

## Missing Panic/Exception Documentation

**Issue:** Handler write functions are documented as potentially failing, but panic behavior unclear.

- Files: All handler modules (console, file, json)
- Risk:
  - What happens if handler panics (Erlang raises exception)?
  - Does panic propagate or is it caught?
  - No error callback to know about handler failure
- Impact: Application crash scenarios not documented
- Recommendations:
  1. Document panic contract for handler interface
  2. Add `safe_call()` wrapper around handler write calls
  3. Provide error callback for failed writes

## Scoped Logger Internal Module Visibility

**Issue:** `birch/internal/scoped_logger.gleam` is internal but may be useful to users.

- Files: `src/birch/internal/scoped_logger.gleam`
- Marked as internal in `gleam.toml`
- Risk: API may change; users can't rely on it
- Recommendations:
  1. If API is stable, consider making it public
  2. Otherwise, maintain stable FFI bridge in `birch/scope.gleam`

---

## Summary Table

| Concern | Severity | File(s) | Fix Approach |
|---------|----------|---------|--------------|
| Pre-1.0 API unstable | High | gleam.toml | Expected; document migration path when 1.0 released |
| Cross-platform FFI parity | High | birch_ffi.erl, birch_ffi.mjs | Maintain test coverage on both targets |
| Scoped context JS limitations | Medium | scope.gleam, birch_ffi.mjs | Document async propagation limitations per runtime |
| File rotation edge cases | Medium-High | handler/file.gleam | Add integration tests, fix Windows paths, improve error handling |
| Async handler data loss | Medium | handler/async.gleam | Add error callbacks, document overflow behavior |
| persistent_term GC risk | Medium | birch_ffi.erl | Document limitation, benchmark impact |
| Handler error isolation | Medium | handler/file.gleam | Add error callback mechanism |
| Color detection heuristics | Low-Medium | birch_ffi.erl, birch_ffi.mjs | Add force color config option |
| Sampling randomness unseeded | Low-Medium | sampling.gleam | Add injectable random source for testing |
| Config validation missing | Low | config.gleam | Add validation functions, return Results |
| JavaScript FFI edge cases | Medium | birch_ffi.mjs | Add environment checks, handle Workers |
| Stale build artifacts | Low | Build process | Add cleanup to CI/pre-commit |

