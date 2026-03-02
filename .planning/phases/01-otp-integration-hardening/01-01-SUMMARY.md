---
phase: 01-otp-integration-hardening
plan: 01
subsystem: erlang-logger-ffi
tags: [ffi, otp, health-check, record-safety]
dependency_graph:
  requires: []
  provides: [record-safe-ffi, is-healthy-api]
  affects: [birch_erlang_logger_ffi, erlang_logger]
tech_stack:
  added: []
  patterns: [erlang-record-include, include_lib-for-gleam-headers]
key_files:
  created: []
  modified:
    - src/birch_erlang_logger_ffi.erl
    - src/birch/erlang_logger.gleam
    - src/birch_erlang_logger_ffi.mjs
    - test/erlang_logger_test.gleam
decisions:
  - Used include_lib instead of include for Gleam-generated HRL files (Gleam compiles FFI from _gleam_artefacts, so relative include path does not resolve; include_lib with app-relative path works correctly)
metrics:
  duration: 210s
  completed: 2026-03-02T01:20:54Z
  tasks_completed: 2
  tasks_total: 2
  files_modified: 4
---

# Phase 1 Plan 1: Safe Record Access and Health Check Summary

Replace fragile erlang:element/N tuple access with compile-time-safe record accessors, add authoritative health check querying real :logger state.

## Completed Tasks

| Task | Name | Commit | Key Files |
|------|------|--------|-----------|
| 1 | Replace tuple indexing with record accessor pattern (OTP-03) | 298c1d2 | src/birch_erlang_logger_ffi.erl |
| 2 | Add is_healthy/0 health check (OTP-02) | 4e8ed2b | src/birch_erlang_logger_ffi.erl, src/birch/erlang_logger.gleam, src/birch_erlang_logger_ffi.mjs, test/erlang_logger_test.gleam |

## Changes Made

### Task 1: Record Accessor Pattern (OTP-03)

Replaced `erlang:element(5, LogRecord)` with Gleam-generated record syntax `LogRecord#log_record.message`. Added `-include_lib("birch/include/birch@record_LogRecord.hrl")` to import the record definition. Also replaced the raw tuple construction `{log_record, ...}` in `build_log_record_from_otp/3` with `#log_record{...}` record construction syntax.

This ensures any field reordering in the Gleam `LogRecord` type is caught at Erlang compile time rather than causing silent runtime corruption.

### Task 2: Health Check API (OTP-02)

Added `is_healthy/0` to the Erlang FFI that queries the actual `:logger` handler configuration (via `logger:get_handler_config(default)`) rather than trusting the `persistent_term` cache used by `ensure_initialized/0`. The check verifies both that the birch module is the formatter AND that `format_fn` is present in the config map.

Exposed as `is_healthy()` in `erlang_logger.gleam` with JavaScript stub returning `false`. Added 3 tests covering: healthy when installed, false when removed, and detection of stale persistent_term cache.

## Decisions Made

1. **include_lib over include**: Gleam copies FFI `.erl` files to `build/dev/erlang/birch/_gleam_artefacts/` for compilation, while HRL files are in `build/dev/erlang/birch/include/`. The relative `-include` path doesn't resolve from `_gleam_artefacts`. Using `-include_lib("birch/include/...")` resolves via Erlang's application path, which works correctly.

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 3 - Blocking] Changed -include to -include_lib for header resolution**
- **Found during:** Task 1
- **Issue:** Plan specified `-include("birch@record_LogRecord.hrl")` but Gleam compiles FFI from `_gleam_artefacts/` directory where relative include path doesn't resolve
- **Fix:** Used `-include_lib("birch/include/birch@record_LogRecord.hrl")` which resolves via Erlang application path
- **Files modified:** src/birch_erlang_logger_ffi.erl
- **Commit:** 298c1d2

## Verification Results

- All 256 Erlang tests pass (253 existing + 3 new health check tests)
- All 252 JavaScript tests pass
- No `erlang:element` calls remain in `birch_erlang_logger_ffi.erl`
- `is_healthy()` correctly returns true when formatter installed, false when removed
