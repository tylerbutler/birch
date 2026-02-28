# Birch Pre-1.0 Hardening

## What This Is

A comprehensive code quality and performance hardening pass for birch, a cross-platform Gleam logging library (Erlang + JavaScript targets). This milestone reviews the entire codebase for idiomatic Gleam patterns, OTP/BEAM best practices, and production readiness — then implements fixes and establishes performance baselines through benchmarking.

## Core Value

Every module in birch should reflect how an experienced Gleam/OTP developer would write it — idiomatic, production-hardened, and measurably performant.

## Requirements

### Validated

- ✓ Cross-platform logging (Erlang + JavaScript) — existing
- ✓ Structured logging with typed metadata — existing
- ✓ Multiple handlers (console, file, JSON, async) — existing
- ✓ OTP :logger integration — existing
- ✓ Lazy evaluation for expensive formatting — existing
- ✓ Scoped context propagation — existing
- ✓ Log sampling — existing
- ✓ RFC 5424 log levels — existing

### Active

- [ ] Full codebase reviewed for idiomatic Gleam patterns
- [ ] OTP logger integration reviewed for correctness, production readiness, and idiomatic BEAM usage
- [ ] All handlers reviewed for bad patterns and anti-patterns
- [ ] Cross-platform FFI reviewed for consistency and correctness
- [ ] API surface reviewed for Gleam conventions
- [ ] Non-idiomatic patterns identified and fixed
- [ ] Benchmark strategy document covering throughput, latency, sampling overhead
- [ ] Runnable benchmarks using hyperfine
- [ ] Performance baselines established
- [ ] Comparison benchmarks against raw :logger and alternatives

### Out of Scope

- New features — this is a quality/performance pass, not feature work
- JavaScript target performance optimization — focus is BEAM target for benchmarks
- Documentation site changes — content updates only if API changes
- Breaking API changes — prefer backward-compatible improvements unless the fix is critical

## Context

- birch is at v0.3.0, pre-1.0 — this is the right time for a quality gate
- The author is newer to Gleam — patterns may work but not be idiomatic
- OTP logger integration was recently overhauled (direct emission, no double-formatting)
- RFC 5424 levels were recently added with :logger forwarding
- The codebase has 7 structured analysis documents in `.planning/codebase/` from the mapping phase
- Key concern areas identified in CONCERNS.md: FFI parity, persistent_term GC, async handler overflow, pre-1.0 API stability

## Constraints

- **Tool**: Benchmarks must use hyperfine for actual measurement
- **Targets**: Code review covers both Erlang and JavaScript targets; benchmarks focus on Erlang/BEAM
- **Outcome**: Issues get fixed, not just documented — this is an active improvement pass
- **Compatibility**: Fixes should be backward-compatible where possible

## Key Decisions

| Decision | Rationale | Outcome |
|----------|-----------|---------|
| Use hyperfine for benchmarks | User preference, standard CLI benchmarking tool | — Pending |
| Full codebase review (not just critical paths) | Pre-1.0 — everything matters | — Pending |
| Fix issues, don't just document | Want the codebase actually improved, not a report | — Pending |
| Compare against raw :logger | Establishes meaningful performance context | — Pending |

---
*Last updated: 2026-02-27 after initialization*
