# Project State

## Project Reference

See: .planning/PROJECT.md (updated 2026-02-27)

**Core value:** Every module in birch should reflect how an experienced Gleam/OTP developer would write it — idiomatic, production-hardened, and measurably performant.
**Current focus:** Phase 2 — Resource and Safety Hardening

## Current Position

Phase: 2 of 4 (Resource and Safety Hardening) -- IN PROGRESS
Plan: 1 of 3 in current phase (just completed 02-01)
Status: Plan Complete
Last activity: 2026-03-04 — Completed 02-01-PLAN.md (Single persistent_term key + ETS async registry)

Progress: [███░░░░░░░] 25%

## Performance Metrics

**Velocity:**
- Total plans completed: 4
- Average duration: 263s
- Total execution time: 0.29 hours

**By Phase:**

| Phase | Plans | Total | Avg/Plan |
|-------|-------|-------|----------|
| 1 | 3 | 1016s | 339s |
| 2 | 1 | 157s | 157s |

**Recent Trend:**
- Last 5 plans: 01-01 (210s), 01-02 (204s), 01-03 (602s), 02-01 (157s)
- Trend: Stable (02-01 faster due to simpler implementation)

*Updated after each plan completion*

## Accumulated Context

### Decisions

Decisions are logged in PROJECT.md Key Decisions table.
Recent decisions affecting current work:

- [Project setup]: Use hyperfine for benchmarks — standard CLI benchmarking tool, user preference
- [Project setup]: Full codebase review (not just critical paths) — pre-1.0, everything matters
- [Project setup]: Fix issues, don't just document — active improvement pass, not a report
- [01-01]: Used include_lib instead of include for Gleam-generated HRL files — Gleam compiles FFI from _gleam_artefacts directory
- [01-02]: Fallback format uses same helpers with nested try/catch for defense in depth
- [01-02]: FORMATTER_ERROR marker in fallback output for immediate crash visibility
- [01-03]: Consolidated handler-modifying tests into combined test to fix gleeunit concurrency race conditions
- [01-03]: Verify handler level via config check rather than debug-message round-trip for deterministic testing
- [02-01]: Single birch_state key reduces GC passes from 2 to 1 on config changes
- [02-01]: ETS async registry uses lazy initialization (created on first access)
- [02-01]: Return error for nil config/logger to match Gleam Result semantics

### Pending Todos

None yet.

### Blockers/Concerns

- [Phase 1]: Dual output path runtime verification (VALIDATE-1) has MEDIUM confidence — needs runtime testing to confirm guard in `default_handlers()` works as documented
- [Phase 2]: Next: async handler buffer overflow strategies need specification (DropOldest, DropNewest, Block)
- [Phase 4]: hyperfine + gleescript integration needs a concrete implementation design to avoid measuring BEAM startup time

## Session Continuity

Last session: 2026-03-04
Stopped at: Completed 02-01-PLAN.md (Resource and Safety Hardening - single persistent_term key + ETS async registry)
Resume file: None
