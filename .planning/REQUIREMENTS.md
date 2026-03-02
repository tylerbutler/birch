# Requirements: Birch Pre-1.0 Hardening

**Defined:** 2026-02-27
**Core Value:** Every module in birch should reflect how an experienced Gleam/OTP developer would write it — idiomatic, production-hardened, and measurably performant.

## v1 Requirements

Requirements for this milestone. Each maps to roadmap phases.

### OTP Integration Hardening

- [ ] **OTP-01**: Formatter `format/2` callback wraps entire body in try/catch with fallback formatted string
- [x] **OTP-02**: `is_healthy/0` health check verifies :logger handler is still installed (not just persistent_term cache)
- [x] **OTP-03**: Erlang FFI replaces `erlang:element(5, LogRecord)` with safe accessor pattern that won't break on field reorder
- [ ] **OTP-04**: All 9 birch levels correctly map to/from OTP's 8 levels with round-trip consistency validated
- [ ] **OTP-05**: :logger primary level interaction documented and handled (debug/info not silently dropped)

### Resource & Safety Hardening

- [ ] **RES-01**: Global config and cached logger consolidated into fewer persistent_term keys to minimize GC passes
- [ ] **RES-02**: Async writer registry moved from persistent_term to ETS table
- [ ] **RES-03**: Async actor replaces O(n) `list.length(state.pending)` with O(1) integer tracking
- [ ] **RES-04**: Async actor queue uses proper queue data structure for DropOldest instead of linked list
- [ ] **RES-05**: Async actor process monitoring added with crash detection
- [ ] **RES-06**: File handler caches file size in memory instead of stat per write

### API & Idiomatic Gleam

- [ ] **API-01**: API naming consistency resolved (e.g., `name()` vs `get_level()` style inconsistency)
- [ ] **API-02**: Idiomatic `use`/`result.try` pipelines applied where appropriate across modules
- [ ] **API-03**: Opaque type boundaries reviewed and tightened where types leak internal structure
- [ ] **API-04**: Builder patterns follow Gleam conventions consistently
- [ ] **API-05**: Module-by-module review for non-idiomatic patterns completed and fixes applied

### Benchmarking

- [ ] **BENCH-01**: Benchmark infrastructure set up (gleamy_bench dev dep, gleescript for escripts, bench/ directory)
- [ ] **BENCH-02**: Filtered log overhead measured (no-op cost when level is filtered — target: sub-microsecond)
- [ ] **BENCH-03**: Log call throughput baseline established (msgs/sec)
- [ ] **BENCH-04**: Overhead vs raw :logger measured and documented ("birch tax")
- [ ] **BENCH-05**: Metadata overhead measured (0, 5, 20 metadata pairs)
- [ ] **BENCH-06**: Lazy evaluation benefit measured (lazy vs eager when filtered)
- [ ] **BENCH-07**: Sampling overhead measured
- [ ] **BENCH-08**: Scoped context overhead measured
- [ ] **BENCH-09**: Async handler throughput compared to sync
- [ ] **BENCH-10**: Multi-handler dispatch overhead measured (1, 3, 5 handlers)

## v2 Requirements

Deferred to future release. Tracked but not in current roadmap.

### API Cleanup

- **APIV2-01**: Remove 15+ deprecated functions and type aliases from birch.gleam
- **APIV2-02**: Cross-platform FFI behavioral parity verified between Erlang and JavaScript

### Extended Benchmarking

- **BENCHV2-01**: File handler I/O cost measured (console vs file handler overhead)
- **BENCHV2-02**: JavaScript target benchmarking (Node.js, Deno, Bun comparison)

## Out of Scope

| Feature | Reason |
|---------|--------|
| New features | This is a quality/performance pass, not feature work |
| Breaking API redesign | Backward-compatible fixes only; redesign would block 1.0 |
| JavaScript target benchmarking | JS runtimes have incomparable performance characteristics; not actionable for BEAM-focused library |
| Documentation site quality | Separate concern from library code quality |
| Feature completeness assessment | Quality pass, not gap analysis |
| Load testing under sustained pressure | Short burst benchmarks sufficient for library quality validation |
| Comparison with other Gleam logging libraries | Ecosystem too young; compare only against raw :logger |

## Traceability

Which phases cover which requirements. Updated during roadmap creation.

| Requirement | Phase | Status |
|-------------|-------|--------|
| OTP-01 | Phase 1 | Pending |
| OTP-02 | Phase 1 | Complete |
| OTP-03 | Phase 1 | Complete |
| OTP-04 | Phase 1 | Pending |
| OTP-05 | Phase 1 | Pending |
| RES-01 | Phase 2 | Pending |
| RES-02 | Phase 2 | Pending |
| RES-03 | Phase 2 | Pending |
| RES-04 | Phase 2 | Pending |
| RES-05 | Phase 2 | Pending |
| RES-06 | Phase 2 | Pending |
| API-01 | Phase 3 | Pending |
| API-02 | Phase 3 | Pending |
| API-03 | Phase 3 | Pending |
| API-04 | Phase 3 | Pending |
| API-05 | Phase 3 | Pending |
| BENCH-01 | Phase 4 | Pending |
| BENCH-02 | Phase 4 | Pending |
| BENCH-03 | Phase 4 | Pending |
| BENCH-04 | Phase 4 | Pending |
| BENCH-05 | Phase 4 | Pending |
| BENCH-06 | Phase 4 | Pending |
| BENCH-07 | Phase 4 | Pending |
| BENCH-08 | Phase 4 | Pending |
| BENCH-09 | Phase 4 | Pending |
| BENCH-10 | Phase 4 | Pending |

**Coverage:**
- v1 requirements: 26 total
- Mapped to phases: 26
- Unmapped: 0 ✓

---
*Requirements defined: 2026-02-27*
*Last updated: 2026-02-27 after initial definition*
