---
status: complete
phase: 03-api-surface-and-idiomatic-gleam-audit
source: [03-01-SUMMARY.md, 03-02-SUMMARY.md, 03-03-SUMMARY.md]
started: 2026-03-07T04:00:00Z
updated: 2026-03-07T04:10:00Z
---

## Current Test
<!-- OVERWRITE each test - shows where we are -->

number: done
name: All tests complete
result: 9/9 passed

## Tests

### 1. Bare getter naming on Logger type
expected: `logger.level(lgr)` returns the logger's level, `logger.handlers(lgr)` returns its handlers, and `logger.context(lgr)` returns its context. The old `get_level`, `get_handlers`, `get_context` no longer exist. Run: `rg 'pub fn get_level|pub fn get_handlers|pub fn get_context' src/birch/logger.gleam` should return nothing. Run: `rg 'pub fn level|pub fn handlers|pub fn context' src/birch/logger.gleam` should show the bare getter functions.
result: pass

### 2. Bare getter naming on Handler and Record types
expected: `handler.error_callback(h)` works (not `get_error_callback`). `record.metadata(rec, key)` works (not `get_metadata`). `record.caller_id(rec)` works (not `get_caller_id`). `level_formatter.target_width(f)` works (not `get_target_width`). Run: `rg 'pub fn get_' src/birch/handler.gleam src/birch/record.gleam src/birch/level_formatter.gleam` should return nothing.
result: pass

### 3. Config module keeps get_ prefix (principled exception)
expected: `config.get_level(cfg)` and `config.get_on_error(cfg)` still use the `get_` prefix because bare names conflict with ConfigOption constructors. Run: `rg 'pub fn get_level|pub fn get_on_error|pub fn get_handlers|pub fn get_context|pub fn get_sampling' src/birch/config.gleam` should show these accessors with `get_` prefix.
result: pass

### 4. LogRecord is opaque with accessors
expected: `record.gleam` declares `pub opaque type LogRecord`. Accessor functions exist: `timestamp()`, `level()`, `logger_name()`, `message()`, `all_metadata()`. Run: `rg 'pub opaque type LogRecord' src/birch/record.gleam` should match. Run: `rg 'pub fn timestamp|pub fn level|pub fn logger_name|pub fn message|pub fn all_metadata' src/birch/record.gleam` should show all five accessors.
result: pass

### 5. GlobalConfig is opaque with constructor and accessors
expected: `config.gleam` declares `pub opaque type GlobalConfig`. A `new_config()` constructor exists. Accessors `get_level`, `get_handlers`, `get_context`, `get_on_error`, `get_sampling` exist. Run: `rg 'pub opaque type GlobalConfig' src/birch/config.gleam` should match. Run: `rg 'pub fn new_config' src/birch/config.gleam` should show the constructor.
result: pass

### 6. SampleConfig is opaque with accessors
expected: `config.gleam` declares `pub opaque type SampleConfig`. Constructor `new_sample_config()` exists. Accessors `sample_config_level()` and `sample_config_rate()` exist in config.gleam, with convenience wrappers `sample_level()` and `rate()` in sampling.gleam. Run: `rg 'pub opaque type SampleConfig' src/birch/config.gleam` should match.
result: pass

### 7. All tests pass on both targets
expected: Run `just test` (with async_actor.gleam stashed). All 256+ tests pass on both Erlang and JavaScript targets with zero failures. This confirms all renames, opaque transitions, and accessor usage are correct end-to-end.
result: pass (256 passed, 0 failures on both targets)

### 8. Module audit: no use/result.try changes needed
expected: The 03-03-SUMMARY.md documents that all 15 modules were reviewed and zero `use`/`result.try` candidates were found. This is correct because the codebase uses `case` for control flow branching, not sequential Result pipelines. Verify: `rg 'use result' src/birch/` should return no matches (confirming no forced refactoring).
result: pass (zero matches confirmed)

### 9. Builder patterns consistent
expected: All builder types use `with_*` convention (Logger, Handler, AsyncConfig) or `add_*` (JsonBuilder). All return the same type. Verify by checking 03-03-SUMMARY.md builder pattern table shows all "Consistent: Yes".
result: pass (all 4 builder types show "Yes")

## Summary

total: 9
passed: 9
issues: 0
pending: 0
skipped: 0

## Gaps

[none yet]
