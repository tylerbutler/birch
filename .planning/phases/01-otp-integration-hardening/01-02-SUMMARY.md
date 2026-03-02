---
phase: 01-otp-integration-hardening
plan: 02
subsystem: erlang-logger-ffi
tags: [ffi, otp, crash-recovery, report-cb, formatter]
dependency_graph:
  requires:
    - phase: 01-01
      provides: record-safe-ffi, is-healthy-api
  provides:
    - crash-resilient-formatter
    - correct-report-cb-handling
  affects: [birch_erlang_logger_ffi, erlang_logger_test]
tech_stack:
  added: []
  patterns: [try-catch-fallback-formatting, FORMATTER_ERROR-marker]
key_files:
  created: []
  modified:
    - src/birch_erlang_logger_ffi.erl
    - test/erlang_logger_test.gleam
    - test/birch_logger_test_ffi.erl
    - test/birch_logger_test_ffi.mjs
key_decisions:
  - "Fallback format uses same helper functions (format_timestamp, level_to_string, format_msg) with nested try/catch for defense in depth"
  - "FORMATTER_ERROR marker in fallback output makes crashes immediately visible in log streams"
patterns_established:
  - "try/catch wrapping for all formatter callback code paths"
  - "Nested try/catch on format_msg in fallback for defense in depth"
requirements_completed: [OTP-01]
metrics:
  duration: 204s
  completed: 2026-03-02T01:28:19Z
  tasks_completed: 2
  tasks_total: 2
  files_modified: 4
---

# Phase 1 Plan 2: Formatter Crash Recovery and report_cb Fix Summary

**try/catch crash recovery in format/2 with FORMATTER_ERROR fallback marker, plus corrected OTP report_cb 1-arg/2-arg return type handling**

## Performance

- **Duration:** 204s
- **Started:** 2026-03-02T01:24:15Z
- **Completed:** 2026-03-02T01:28:19Z
- **Tasks:** 2
- **Files modified:** 4

## Accomplishments

- format/2 now catches all exceptions from the Gleam FormatFn and emits a clean fallback with FORMATTER_ERROR marker
- 1-arg report_cb correctly handles {Format, Args} return (was treating it as chardata)
- 2-arg report_cb correctly handles chardata return (was treating it as {Format, Args})
- Handler survives formatter crashes and remains fully functional

## Task Commits

Each task was committed atomically:

1. **Task 1: Wrap format/2 in try/catch with fallback output** - `d6c422d` (fix)
2. **Task 2: Fix report_cb 1-arg/2-arg return type handling** - `9ecf049` (fix)

## Files Created/Modified

- `src/birch_erlang_logger_ffi.erl` - try/catch in format/2, fixed report_cb handling in format_msg/2
- `test/erlang_logger_test.gleam` - Crash recovery test, 2-arg report_cb test
- `test/birch_logger_test_ffi.erl` - install_crashing_formatter/0, otp_logger_report_with_cb_2arg/0, fixed otp_logger_report_with_cb/0
- `test/birch_logger_test_ffi.mjs` - JS stubs for new test helpers

## Decisions Made

1. **Fallback uses same helpers with nested try/catch**: The fallback path in format/2 reuses format_timestamp, level_to_string, and format_msg (which are independently tested and simpler than the full pipeline). format_msg itself is wrapped in a nested try/catch in case it contributed to the original crash.

2. **FORMATTER_ERROR marker for visibility**: The fallback output includes a FORMATTER_ERROR marker between the level and message so crashes are immediately visible when scanning log output, without requiring external monitoring.

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 1 - Bug] Fixed existing test helper returning wrong type for 1-arg report_cb**
- **Found during:** Task 2
- **Issue:** The existing `otp_logger_report_with_cb/0` test helper used a 1-arg callback that returned chardata (io_lib:format result) instead of the correct {Format, Args} tuple per OTP spec. This meant the existing Test 5 was passing with the old (wrong) code because both the code and the test had the same bug.
- **Fix:** Changed test helper to return `{"Test report: ~ts", [Msg]}` tuple. The existing Test 5 still passes because the fixed code now correctly handles the {Format, Args} return from 1-arg callbacks.
- **Files modified:** test/birch_logger_test_ffi.erl
- **Commit:** 9ecf049

---

**Total deviations:** 1 auto-fixed (1 bug fix)
**Impact on plan:** Essential correction -- the test helper had the same bug as the production code, masking the issue.

## Issues Encountered

None.

## User Setup Required

None - no external service configuration required.

## Next Phase Readiness

- Crash recovery and report_cb correctness complete
- Ready for plan 03 (remaining OTP integration hardening tasks)
- 258 Erlang tests pass, 254 JavaScript tests pass

---
*Phase: 01-otp-integration-hardening*
*Completed: 2026-03-02*
