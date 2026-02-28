# Roadmap: Birch Pre-1.0 Hardening

## Overview

This milestone hardens birch from a working implementation to a production-ready logging library. The work follows a priority-ordered sequence: OTP integration correctness first (silent log loss is the highest-risk failure mode), resource and safety hardening second (fixes that would distort benchmark results if left unfixed), API surface cleanup third (pre-1.0 is the last chance to fix naming before users depend on it), then benchmarking last (measuring the corrected implementation to establish regression baselines).

## Phases

**Phase Numbering:**
- Integer phases (1, 2, 3): Planned milestone work
- Decimal phases (2.1, 2.2): Urgent insertions (marked with INSERTED)

Decimal phases appear between their surrounding integers in numeric order.

- [ ] **Phase 1: OTP Integration Hardening** - Fix formatter crash, safe Erlang FFI, validated level mapping
- [ ] **Phase 2: Resource and Safety Hardening** - persistent_term consolidation, async actor correctness, file handler optimization
- [ ] **Phase 3: API Surface and Idiomatic Gleam Audit** - Naming consistency, opaque types, idiomatic patterns module-by-module
- [ ] **Phase 4: Benchmarking Suite** - gleamy_bench microbenchmarks + hyperfine CLI comparison vs raw :logger

## Phase Details

### Phase 1: OTP Integration Hardening
**Goal**: The OTP :logger integration is correct and cannot silently remove itself or corrupt log output
**Depends on**: Nothing (first phase)
**Requirements**: OTP-01, OTP-02, OTP-03, OTP-04, OTP-05
**Success Criteria** (what must be TRUE):
  1. A crash inside `format/2` does not remove the :logger handler — a fallback string is emitted and logging continues
  2. `is_healthy/0` returns false if the :logger handler has been removed, even when the persistent_term cache says otherwise
  3. The Erlang FFI accesses Gleam record fields via accessor functions, not fragile tuple position indexing
  4. All 9 birch levels round-trip through OTP's 8 levels with documented and tested mapping behavior
  5. The :logger primary level interaction is documented: birch does not silently drop debug/info messages due to primary level filtering
**Plans:** 3 plans

Plans:
- [ ] 01-01-PLAN.md — Safe FFI record access + is_healthy health check (OTP-03, OTP-02)
- [ ] 01-02-PLAN.md — Formatter crash recovery + report_cb fix (OTP-01)
- [ ] 01-03-PLAN.md — Level round-trip property tests + primary level config (OTP-04, OTP-05)

### Phase 2: Resource and Safety Hardening
**Goal**: The runtime resource usage is correct and the async handler is safe under load
**Depends on**: Phase 1
**Requirements**: RES-01, RES-02, RES-03, RES-04, RES-05, RES-06
**Success Criteria** (what must be TRUE):
  1. `configure()` and `set_level()` write to a single persistent_term key, not multiple keys per call
  2. The async writer registry uses an ETS table — registry updates no longer trigger global GC passes
  3. The async actor tracks queue length as an O(1) integer field, not by calling `list.length` on every message
  4. The async actor detects when its monitored process crashes and recovers or logs the failure
  5. The file handler caches file size in memory and only stat()s disk during rotation checks
**Plans**: TBD

### Phase 3: API Surface and Idiomatic Gleam Audit
**Goal**: The public API reflects idiomatic Gleam conventions and is ready to stabilize at 1.0
**Depends on**: Phase 2
**Requirements**: API-01, API-02, API-03, API-04, API-05
**Success Criteria** (what must be TRUE):
  1. All public functions follow consistent naming — getter/setter style is uniform across modules (no `name()` vs `get_level()` inconsistency)
  2. `use`/`result.try` pipelines are applied where chained Result-handling exists; no manual pattern-match unwrapping in those places
  3. All types that should be opaque are opaque — no internal structure leaks through the public API
  4. Builder pattern functions are consistent — all builder steps return the same type and follow the same chaining convention
  5. Every module has been reviewed for non-idiomatic patterns and flagged issues are fixed (evidenced by a module-by-module checklist in the plan summary)
**Plans**: TBD

### Phase 4: Benchmarking Suite
**Goal**: Performance characteristics are measured, documented, and committed as regression baselines
**Depends on**: Phase 3
**Requirements**: BENCH-01, BENCH-02, BENCH-03, BENCH-04, BENCH-05, BENCH-06, BENCH-07, BENCH-08, BENCH-09, BENCH-10
**Success Criteria** (what must be TRUE):
  1. A `bench/` directory exists with gleamy_bench microbenchmarks and gleescript escripts runnable via `just bench`
  2. Filtered log overhead is measured and reported — the no-op cost when a message is below the configured level
  3. Log call throughput baseline is established and committed — msgs/sec for a standard info call with metadata
  4. The "birch tax" is documented — overhead vs equivalent raw `:logger` call, measured by hyperfine
  5. All 10 benchmark scenarios (BENCH-01 through BENCH-10) produce numeric results committed to the repository
**Plans**: TBD

## Progress

**Execution Order:**
Phases execute in numeric order: 1 → 2 → 3 → 4

| Phase | Plans Complete | Status | Completed |
|-------|----------------|--------|-----------|
| 1. OTP Integration Hardening | 0/3 | Planned | - |
| 2. Resource and Safety Hardening | 0/? | Not started | - |
| 3. API Surface and Idiomatic Gleam Audit | 0/? | Not started | - |
| 4. Benchmarking Suite | 0/? | Not started | - |
