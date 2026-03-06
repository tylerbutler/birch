---
phase: 03-api-surface-and-idiomatic-gleam-audit
plan: 01
subsystem: api
tags: [gleam, naming-conventions, accessors, idiomatic-gleam]

# Dependency graph
requires: []
provides:
  - "Bare getter naming convention for all type-level accessors"
  - "Consistent API surface matching gleam_stdlib patterns"
affects: [03-02, 03-03]

# Tech tracking
tech-stack:
  added: []
  patterns: ["bare getter style for type accessors (level, handlers, context vs get_level, get_handlers, get_context)"]

key-files:
  created: []
  modified:
    - src/birch/logger.gleam
    - src/birch/handler.gleam
    - src/birch/record.gleam
    - src/birch/level_formatter.gleam
    - test/birch_test.gleam
    - test/property_test.gleam

key-decisions:
  - "Direct rename without deprecated aliases -- pre-1.0, no backward compatibility needed"
  - "Keep get_ prefix on birch.gleam module-level getters (get_level, get_config) -- these are global state accessors, not type-level"
  - "Keep get_ prefix on config.gleam getters -- naming conflict with ConfigOption constructors"

patterns-established:
  - "Bare getter convention: type-level accessors use bare names (logger.level, handler.error_callback, record.metadata)"
  - "Module-level state accessors keep get_ prefix (birch.get_level, birch.get_config)"

requirements-completed: [API-01]

# Metrics
duration: 3min
completed: 2026-03-06
---

# Phase 03 Plan 01: Getter Rename Summary

**Renamed 7 type-level get_* accessors to bare getter style across logger, handler, record, and level_formatter modules with full test coverage**

## Performance

- **Duration:** 3 min (191s)
- **Started:** 2026-03-06T15:09:11Z
- **Completed:** 2026-03-06T15:12:22Z
- **Tasks:** 2
- **Files modified:** 6

## Accomplishments
- Renamed all 7 type-level get_* functions to bare getter style matching gleam_stdlib conventions
- Updated all test references across birch_test.gleam and property_test.gleam
- All 256 tests pass on both Erlang and JavaScript targets

## Task Commits

Each task was committed atomically:

1. **Task 1: Rename get_* to bare getters in source modules** - `52d0d34` (refactor)
2. **Task 2: Update all test files, examples, and internal callers** - `f868e33` (refactor)

## Files Created/Modified
- `src/birch/logger.gleam` - Renamed get_level -> level, get_handlers -> handlers, get_context -> context
- `src/birch/handler.gleam` - Renamed get_error_callback -> error_callback
- `src/birch/record.gleam` - Renamed get_metadata -> metadata, get_caller_id -> caller_id
- `src/birch/level_formatter.gleam` - Renamed get_target_width -> target_width
- `test/birch_test.gleam` - Updated ~20 references to use new bare getter names
- `test/property_test.gleam` - Updated 3 references and renamed test functions

## Decisions Made
- Direct rename without deprecated aliases -- pre-1.0, no backward compatibility needed
- Kept get_ prefix on birch.gleam module-level getters (get_level, get_config) -- global state accessors
- Kept get_ prefix on config.gleam getters (get_level, get_on_error) -- naming conflict with ConfigOption constructors

## Deviations from Plan

None - plan executed exactly as written.

## Issues Encountered
- Pre-existing corrupt atom table error in build artifacts required clean rebuild (rm -rf build/dev)
- Pre-existing syntax error in src/birch/internal/async_actor.gleam required temporary stash during build verification (not related to this plan)

## User Setup Required

None - no external service configuration required.

## Next Phase Readiness
- Bare getter convention established for all type-level accessors
- Ready for 03-02 (further API surface audit) and 03-03

---
*Phase: 03-api-surface-and-idiomatic-gleam-audit*
*Completed: 2026-03-06*
