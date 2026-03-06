---
phase: 03-api-surface-and-idiomatic-gleam-audit
plan: 02
subsystem: api
tags: [gleam, opaque-types, encapsulation, accessors]

# Dependency graph
requires:
  - phase: 03-01
    provides: "Bare getter naming convention for type-level accessors"
provides:
  - "LogRecord, GlobalConfig, SampleConfig are opaque types"
  - "Full accessor function coverage for all opaque type fields"
  - "new_config and new_sample_config constructors for opaque types"
affects: [03-03]

# Tech tracking
tech-stack:
  added: []
  patterns: ["opaque types with accessor functions for encapsulation", "new_* constructor pattern for opaque types"]

key-files:
  created: []
  modified:
    - src/birch/record.gleam
    - src/birch/config.gleam
    - src/birch/sampling.gleam
    - src/birch/formatter.gleam
    - src/birch/handler.gleam
    - src/birch/handler/console.gleam
    - src/birch/handler/json.gleam
    - src/birch/erlang_logger.gleam
    - src/birch.gleam
    - test/birch_test.gleam
    - test/property_test.gleam
    - test/erlang_logger_test.gleam
    - examples/08-custom-handlers/src/birch_example_08_custom_handlers.gleam

key-decisions:
  - "SampleConfig accessors live in config.gleam (where the opaque type is defined) with delegating wrappers in sampling.gleam"
  - "Renamed LogRecord parameters from 'record' to 'rec' to avoid module name shadowing conflicts"
  - "Added new_sample_config constructor to config.gleam since SampleConfig constructor became private"

patterns-established:
  - "Opaque type accessors defined in the module where the type is declared"
  - "Constructor functions (new_config, new_sample_config) for creating opaque types from external modules"

requirements-completed: [API-03]

# Metrics
duration: 13min
completed: 2026-03-06
---

# Phase 03 Plan 02: Opaque Types Summary

**Made LogRecord, GlobalConfig, and SampleConfig opaque with full accessor coverage, enforcing encapsulation boundaries before 1.0**

## Performance

- **Duration:** 13 min (801s)
- **Started:** 2026-03-06T15:14:27Z
- **Completed:** 2026-03-06T15:27:48Z
- **Tasks:** 2
- **Files modified:** 13

## Accomplishments
- Added 5 LogRecord accessors (timestamp, level, logger_name, message, all_metadata)
- Added 3 GlobalConfig accessors (get_handlers, get_context, get_sampling) and new_config constructor
- Added SampleConfig accessors (sample_config_level, sample_config_rate) and new_sample_config constructor
- Updated all consumers across 13 files to use accessor functions
- Made all three types opaque with zero test regressions (256 tests pass on both targets)

## Task Commits

Each task was committed atomically:

1. **Task 1: Add accessor functions and constructor** - `d1c1031` (feat)
2. **Task 2: Update all consumers, then toggle types to opaque** - `9668a35` (refactor)

## Files Created/Modified
- `src/birch/record.gleam` - Added timestamp, level, logger_name, message, all_metadata accessors; made LogRecord opaque
- `src/birch/config.gleam` - Added get_handlers, get_context, get_sampling, get_on_error, new_config, new_sample_config, sample_config_level, sample_config_rate; made GlobalConfig and SampleConfig opaque
- `src/birch/sampling.gleam` - Added sample_level, rate delegating accessors; updated to use config module constructors
- `src/birch/formatter.gleam` - Replaced direct field access with accessor calls
- `src/birch/handler.gleam` - Replaced direct field access with accessor calls
- `src/birch/handler/console.gleam` - Replaced all 10 direct field accesses with accessor calls
- `src/birch/handler/json.gleam` - Replaced all 5 direct field accesses with accessor calls
- `src/birch/erlang_logger.gleam` - Replaced all 8 direct field accesses with accessor calls
- `src/birch.gleam` - Replaced GlobalConfig constructor and field accesses with new_config and accessors
- `test/birch_test.gleam` - Updated ~30 field accesses to use accessor functions
- `test/property_test.gleam` - Updated 4 field accesses to use accessor functions
- `test/erlang_logger_test.gleam` - Updated 2 field accesses to use accessor functions
- `examples/08-custom-handlers/src/birch_example_08_custom_handlers.gleam` - Updated direct field access to use accessors

## Decisions Made
- SampleConfig accessors placed in config.gleam (where the opaque type lives), with convenience wrappers in sampling.gleam
- Renamed LogRecord parameters from `record` to `rec` to avoid shadowing the `birch/record` module import
- Added `new_sample_config` constructor to config.gleam since the `SampleConfig(...)` constructor became private with opaque

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 1 - Bug] Fixed example 08 direct field access**
- **Found during:** Task 2 (Update all consumers)
- **Issue:** examples/08-custom-handlers uses `record.level` and `record.message` direct field access which would fail with opaque LogRecord
- **Fix:** Updated to use `record.level(rec)` and `record.message(rec)` accessor calls
- **Files modified:** examples/08-custom-handlers/src/birch_example_08_custom_handlers.gleam
- **Verification:** Example compiles successfully
- **Committed in:** 9668a35 (Task 2 commit)

**2. [Rule 3 - Blocking] Moved SampleConfig accessors to config.gleam**
- **Found during:** Task 2 (Making types opaque)
- **Issue:** SampleConfig is opaque in config.gleam, so sampling.gleam cannot access its fields directly
- **Fix:** Added sample_config_level, sample_config_rate, and new_sample_config to config.gleam; sampling.gleam delegates to these
- **Files modified:** src/birch/config.gleam, src/birch/sampling.gleam
- **Verification:** Build succeeds, all tests pass
- **Committed in:** 9668a35 (Task 2 commit)

**3. [Rule 3 - Blocking] Fixed erlang_logger_test.gleam field accesses**
- **Found during:** Task 2 (Making types opaque)
- **Issue:** erlang_logger_test.gleam had direct GlobalConfig field accesses not listed in plan
- **Fix:** Added config import and replaced field accesses with accessor calls
- **Files modified:** test/erlang_logger_test.gleam
- **Verification:** Build succeeds, all tests pass
- **Committed in:** 9668a35 (Task 2 commit)

---

**Total deviations:** 3 auto-fixed (1 bug, 2 blocking)
**Impact on plan:** All auto-fixes necessary for correctness. No scope creep.

## Issues Encountered
- Pre-existing syntax error in src/birch/internal/async_actor.gleam required stash during build (same as plan 01)

## User Setup Required

None - no external service configuration required.

## Next Phase Readiness
- All three core types (LogRecord, GlobalConfig, SampleConfig) are now opaque
- Encapsulation boundaries enforced -- external modules must use accessors
- Ready for 03-03 (remaining API surface audit items)

---
*Phase: 03-api-surface-and-idiomatic-gleam-audit*
*Completed: 2026-03-06*
