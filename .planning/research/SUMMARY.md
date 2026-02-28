# Project Research Summary

**Project:** birch (Gleam logging library)
**Domain:** Pre-1.0 hardening — code review, OTP integration validation, and BEAM benchmarking
**Researched:** 2026-02-27
**Confidence:** HIGH

## Executive Summary

Birch is a Gleam logging library targeting both BEAM and JavaScript runtimes, with OTP `:logger` integration as its primary code path on Erlang. The library is architecturally sound: it follows the correct OTP pattern of formatter-only integration (not custom handler registration), which delegates overload protection and output management to `logger_std_h` while birch controls formatting. The recent overhaul to direct emission (`b720de3`) removed double-formatting and was a correct architectural decision confirmed by how Elixir's Logger works. The core challenge now is hardening a working implementation to production quality standards before a 1.0 release.

The recommended approach for this milestone is two parallel tracks: a systematic code review against a priority-ordered checklist (OTP integration correctness first, then handler safety, idiomatic Gleam patterns, API surface cleanup), and a benchmarking suite using gleamy_bench for in-process microbenchmarks and hyperfine for CLI-level comparisons. The review must be manual — Gleam has no external linter, and the compiler's `--warnings-as-errors` flag (already in CI) is necessary but insufficient for idiomatic patterns and OTP best practices. Benchmarks must be carefully designed to avoid measuring BEAM startup time rather than logging performance.

The primary risks are concrete and code-visible: a formatter crash silently removes the `:logger` handler causing complete log loss (the single highest-risk item), `persistent_term` writes triggering global GC on every `configure()` call, the async actor queue using O(n) `list.length` in a hot path, and the Erlang FFI accessing Gleam record fields by fragile tuple position. All five critical pitfalls have clear fixes identified by the research. The milestone should focus review effort on these items before addressing the broader code quality and API surface concerns.

## Key Findings

### Recommended Stack

Gleam has no standalone linter — the compiler is the only static analysis tool. `gleam build --warnings-as-errors` (already in the `build-strict` justfile target) is the ceiling of automated analysis. Code review for idiomatic patterns and OTP best practices is inherently a manual checklist exercise. For benchmarking, the recommended approach is two layers: `gleamy_bench` (pure Gleam, hex.pm v0.6.0) for in-process microbenchmarks without VM startup noise, and `hyperfine` for CLI-level wall-clock comparison of birch vs raw `:logger`. The `gleescript` package (v1.5.2) is needed to bundle Gleam modules into escripts for hyperfine. No new runtime dependencies are required — only dev dependencies.

**Core technologies:**
- `gleam build --warnings-as-errors`: Static analysis — only automated analysis tool available for Gleam
- `gleamy_bench` (v0.6.0): In-process microbenchmarks — pure Gleam, no Elixir/Erlang dependency, measures IPS and percentiles
- `gleescript` (v1.5.2): Escript bundling — converts Gleam modules into CLI-runnable escripts for hyperfine
- `hyperfine`: CLI benchmarking — wall-clock comparison with warmup and statistical reporting
- ETS capture handler (custom, already implemented): OTP logger testing — idiomatic BEAM approach, captures `:logger` events for assertion
- `logger:get_handler_config/1` (OTP 27 stdlib): Integration verification — already in use for formatter installation checks

### Expected Features

Research reframed "features" as review dimensions and benchmark scenarios. The table stakes for this milestone are the review areas and benchmarks that cannot be skipped if the review is to be meaningful.

**Must complete (code review):**
- OTP `:logger` integration correctness — recent overhaul needs validation; bugs here lose logs silently
- Handler error isolation — safety contract: handler failures must never crash the application
- Idiomatic Gleam patterns — module by module; compiler does not catch non-idiomatic code
- API surface consistency — pre-1.0 is the last chance to fix naming before users depend on it
- Cross-platform FFI review — JS FFI is 868 LOC handling 4 runtimes; Erlang FFI has fragile tuple indexing

**Must complete (benchmarking):**
- Filtered log overhead (no-op cost) — the hot path; should be sub-microsecond
- Log call throughput — the baseline; confirms birch can handle production volumes
- Overhead vs raw `:logger` — the "birch tax" users need to know about

**Defer to post-1.0 or lower priority:**
- JavaScript target benchmarking — JS runtimes have incomparable performance characteristics; not actionable for a BEAM-focused library
- File handler rotation edge cases — experimental feature; time-based rotation not yet complete
- Sampling determinism — niche feature; low impact; can be addressed after 1.0
- Full API redesign — backward-compatible fixes only; redesign would block 1.0 rather than enable it
- Documentation site quality — separate concern from library code quality

### Architecture Approach

Birch's architecture is correct and should not be restructured. The formatter-only OTP integration pattern (installing a custom formatter on `logger_std_h` rather than registering a custom handler) is exactly how Elixir's Logger works, and for the same reasons: OTP's built-in handlers provide battle-tested overload protection, burst limiting, and mode switching that would need to be reimplemented from scratch in a custom handler. The pass-through LogRecord via `:logger` metadata (`birch_log_record` key) is the correct way to avoid decompose/recompose overhead and is the documented OTP pattern. The main architectural concern is not the design but the correctness of the implementation in specific edge cases.

**Major components:**
1. `birch.gleam` (Public API) — module-level convenience, global config via `persistent_term`, scoped context entry point
2. `birch/logger.gleam` — named logger instances, log emission, metadata merging, dual output path to :logger and birch handlers
3. `birch/erlang_logger.gleam` + `birch_erlang_logger_ffi.erl` — OTP formatter registration, `format/2` callback, level mapping, emit API
4. `birch/handler/*.gleam` — birch-native handlers (console, file, JSON, async actor); independent of OTP integration
5. `birch/formatter.gleam` — pure LogRecord-to-String functions; enforced as pure by the `fn(LogRecord) -> String` type
6. OTP `:logger` / `:logger_std_h` (external) — output routing, overload protection, burst limiting; birch delegates to this intentionally

### Critical Pitfalls

1. **Formatter crash removes the :logger handler entirely** — Any exception in `format/2` causes OTP to remove the handler; all logging stops silently, including OTP supervisor reports. Logs stop with no error signal. Fix: wrap the entire `format/2` body in try/catch returning a fallback formatted string; add `erlang_logger.is_healthy/0` health check that verifies the handler is still installed (not just the `persistent_term` cache).

2. **`persistent_term` writes trigger global GC** — Every `configure()`, `set_level()`, and `reset_config()` call causes 2 global GC passes across all processes. Fix: consolidate global config and cached logger into a single `persistent_term` key; move the async writer registry from `persistent_term` to an ETS table (async registry is updated frequently, making it a poor fit for `persistent_term`).

3. **Erlang FFI accesses Gleam record fields by fragile tuple position** — `erlang:element(5, LogRecord)` to get the message field; if Gleam field order changes, the FFI silently reads wrong data or crashes. Fix: replace positional access with Gleam-exported accessor functions, or use Gleam-generated Erlang header files with record syntax.

4. **Async actor queue uses O(n) `list.length` per message** — `async_actor.gleam` line 132 calls `list.length(state.pending)` on every incoming message; under high throughput this becomes O(n^2) behavior. Fix: track queue length as an integer field in `State`; use a queue data structure for `DropOldest` instead of a linked list.

5. **Benchmarking BEAM with hyperfine measures VM startup, not logging** — BEAM startup is 50-200ms; short benchmarks measure startup time, not logging performance. Fix: each benchmark invocation must run 10K-100K log operations so the workload dwarfs startup; complement hyperfine with in-VM `gleamy_bench` measurements.

## Implications for Roadmap

Based on research, the work naturally divides into two sequential tracks, with code review preceding benchmarking because measuring performance of broken or fragile code is misleading.

### Phase 1: OTP Integration Hardening

**Rationale:** The formatter crash pitfall (Pitfall 1) is the single highest-risk item: it causes complete silent log loss in production. The Gleam tuple position dependency (Pitfall 3) is the second highest: it can silently corrupt log output or trigger the formatter crash. Both must be fixed before any other review work, because they affect the correctness of all subsequent testing and benchmarking. Research orders the review checklist with level mapping and OTP integration first for this reason.

**Delivers:** A formatter that cannot silently remove itself; safe Erlang FFI that does not depend on Gleam compiler internals; validated level mapping between birch's 9 levels and OTP's 8; confirmed dual-output path behavior (when to emit to `:logger` vs birch handlers); documented `:logger` primary level interaction.

**Addresses:** OTP :logger integration correctness, handler error isolation (from FEATURES.md review dimensions)

**Avoids:** Pitfall 1 (formatter crash), Pitfall 3 (tuple position dependency), Anti-Pattern 1 (custom handler registration), Anti-Pattern 3 (ignoring :logger level filtering)

**Specific items:** Add try/catch in `format/2`; add `is_healthy/0` health check; replace `erlang:element(5, LogRecord)` with accessor functions; validate `VALIDATE-1` through `VALIDATE-8` items from ARCHITECTURE.md; confirm `:logger` primary level is set to `all` when birch formatter is installed.

### Phase 2: Resource and Safety Hardening

**Rationale:** After the OTP integration is verified correct, the remaining critical pitfalls involve resource management and process safety. These are independent of OTP correctness but should be fixed before benchmarking — measuring a system with O(n^2) async queue behavior would produce misleading benchmark data. The `persistent_term` GC issue (Pitfall 2) and async actor problems (Pitfalls 5, 8, 9) all fall into this category.

**Delivers:** `persistent_term` usage consolidated to minimize global GC passes; async writer registry moved to ETS; async actor queue using O(1) length tracking; async actor process monitoring with crash detection and recovery; file handler caching file size in memory instead of stat per write.

**Addresses:** Resource cleanup patterns, async handler actor design (from FEATURES.md review dimensions)

**Avoids:** Pitfall 2 (`persistent_term` GC storms), Pitfall 5 (O(n) queue), Pitfall 8 (async registry in `persistent_term`), Pitfall 9 (unsupervised async actor), Pitfall 7 (file handler stat per write)

### Phase 3: API Surface and Idiomatic Gleam Audit

**Rationale:** With the critical correctness and safety issues addressed, the review moves to code quality and API shape. This is where the pre-1.0 investment pays off: removing deprecated items, fixing naming consistency, and ensuring idiomatic Gleam patterns are used throughout. This phase must happen before 1.0 because all these changes are potentially breaking.

**Delivers:** All 15+ deprecated functions and type aliases removed from `birch.gleam`; API naming consistency fixed (`name()` vs `get_level()` inconsistency resolved); `internal_modules` in `gleam.toml` audited and corrected; idiomatic Gleam patterns applied module-by-module (use/result.try pipelines, opaque type boundaries, builder patterns); configuration validation added; cross-platform behavioral parity verified between Erlang and JavaScript FFI.

**Addresses:** API surface consistency, idiomatic Gleam patterns, cross-platform behavioral parity, type safety of public API (from FEATURES.md review dimensions)

**Avoids:** Pitfall 10 (deprecated API cruft confusing users), Pitfall 11 (non-deterministic sampling in tests)

### Phase 4: Benchmarking Suite

**Rationale:** Benchmarking comes after code review because measuring the performance of corrected code is more meaningful and stable. The benchmark design must be done carefully to avoid measuring BEAM startup time rather than logging performance (Pitfall 4). Research specifies the exact benchmark ordering: filtered log overhead first (most important number), then throughput, then birch-vs-raw-logger overhead.

**Delivers:** In-VM microbenchmarks via `gleamy_bench` measuring filtered log overhead, log throughput, metadata overhead, lazy evaluation benefit, sampling overhead, scoped context overhead, async handler throughput, multi-handler dispatch, file handler I/O cost; CLI-level benchmarks via hyperfine comparing birch vs raw `:logger`; benchmark infrastructure committed to repository for regression tracking.

**Addresses:** All 10 benchmark dimensions from FEATURES.md

**Avoids:** Pitfall 4 (benchmarking startup time), Pitfall 13 (process isolation failure)

**Uses:** `gleamy_bench` (v0.6.0), `gleescript` (v1.5.2), `hyperfine` (system tool) from STACK.md

### Phase 5: Pre-Release Checklist

**Rationale:** Final gate before 1.0: dependency version verification, CI matrix testing, JavaScript target smoke check, changelog and documentation review. These are explicitly deferred to last because they depend on the stable API surface established in Phase 3.

**Delivers:** Dependency version bounds tightened to tested versions; CI matrix testing minimum and maximum allowed dependency versions; JavaScript target FFI spot-checked against Node/Deno/Bun; `is_healthy()` integrated into any application health check examples.

**Avoids:** Pitfall 14 (dependency bounds allowing untested major versions), Pitfall 12 (file handler path separator on Windows — at minimum documented)

### Phase Ordering Rationale

- OTP integration must come first because it affects all logging output; broken integration produces misleading test results in subsequent phases
- Resource/safety hardening before benchmarking because O(n^2) async queue and `persistent_term` GC distort benchmark numbers
- API cleanup before benchmarking because benchmarks should measure the final API, not a deprecated one
- Benchmarking last (but before pre-release) because it validates the fixed implementation and produces regression baselines
- The review ordering from FEATURES.md (level mapping → OTP integration → handler error isolation → idiomatic Gleam → FFI → API surface → resource cleanup → cross-platform parity) maps directly onto phases 1-3

### Research Flags

Phases likely needing deeper research during planning:
- **Phase 1 (OTP Integration):** The `VALIDATE-1` dual output path concern has MEDIUM confidence (logic analysis, not runtime-verified); needs runtime testing to confirm the guard in `default_handlers()` actually works as documented. The `:logger` primary level interaction may require researching whether `set_primary_config(#{level => all})` is safe to call from library code.
- **Phase 2 (Resource Hardening):** ETS table design for the async writer registry needs to be specified; `gleam/queue` availability and API needs verification before recommending it as the replacement for linked list in async actor.
- **Phase 4 (Benchmarking):** hyperfine + gleescript integration pattern needs to be prototyped; the "calibrated startup baseline" approach for reporting "logs per second" needs a concrete implementation design.

Phases with standard patterns (skip research-phase):
- **Phase 3 (API Audit):** Gleam deprecated function removal and naming consistency are well-understood; no research needed.
- **Phase 5 (Pre-release):** Dependency version verification and CI matrix setup are standard; no research needed.

## Confidence Assessment

| Area | Confidence | Notes |
|------|------------|-------|
| Stack | HIGH | All tools verified via hex.pm, official docs, and existing codebase usage. The "no Gleam linter exists" finding is definitive. |
| Features | HIGH | Review dimensions and benchmark scenarios derived from existing codebase (CONCERNS.md, source code), OTP docs, and domain knowledge of logging library requirements. |
| Architecture | HIGH | OTP `:logger` architecture verified against official kernel docs and OTP source. Birch's formatter-only approach confirmed correct by Elixir Logger reference implementation. Most VALIDATE items are HIGH confidence from source code analysis. |
| Pitfalls | HIGH | All 5 critical pitfalls are directly code-visible or documented OTP behavior. No speculation — each pitfall cites exact source file lines or official documentation. |

**Overall confidence:** HIGH

### Gaps to Address

- **Dual output path runtime verification (VALIDATE-1):** The logic analysis of Phase 1 suggests the guard works correctly, but it needs actual runtime testing with both `:logger` formatter installed AND explicit birch handler configured. Should be addressed in Phase 1 planning with a specific test case.
- **gleam/queue availability:** The O(1) queue replacement for the async actor (Pitfall 5 fix) assumes `gleam/queue` is available or usable from Erlang. Verify before committing to this approach; the Erlang `:queue` FFI path in `birch_ffi.erl` may be the better choice.
- **`:logger` primary level management:** When birch installs its formatter, should it also call `logger:set_primary_config(#{level => all})`? This affects ALL other logger handlers in the system and may be too aggressive. Research the right answer before Phase 1 implementation.
- **Sampling testability scope:** Pitfall 11 fix (injectable random source) changes the `SampleConfig` type. Decide whether this is worth a breaking change pre-1.0, or if it is better handled post-1.0 with a documented limitation.

## Sources

### Primary (HIGH confidence)

- [Erlang Logger Chapter (kernel v10.5)](https://www.erlang.org/doc/apps/kernel/logger_chapter.html) — handler crash behavior, formatter-only pattern, overload protection architecture
- [Erlang Logger API (kernel v10.5)](https://www.erlang.org/doc/apps/kernel/logger.html) — `add_handler`, `get_handler_config`, type specs
- [Erlang Logger Cookbook](https://www.erlang.org/doc/apps/kernel/logger_cookbook.html) — practical formatter patterns
- [Erlang `persistent_term` documentation](https://www.erlang.org/docs/27/apps/erts/persistent_term.html) — global GC behavior and update warnings
- [Erlang Benchmarking documentation](https://www.erlang.org/doc/system/benchmarking.html) — process isolation, measurement duration
- [Erlang logger_formatter.erl source](https://github.com/erlang/otp/blob/master/lib/kernel/src/logger_formatter.erl) — newline convention, `format/2` return type, `check_config`
- [Elixir Logger (v1.19.5)](https://hexdocs.pm/logger/Logger.html) — reference implementation of `:logger` integration from higher-level language
- [gleamy_bench v0.6.0](https://hex.pm/packages/gleamy_bench) — Gleam-native microbenchmark library
- [gleescript v1.5.2](https://hex.pm/packages/gleescript) — escript bundling for hyperfine integration
- [hyperfine](https://github.com/sharkdp/hyperfine) — CLI benchmarking tool
- [Gleam for Erlang users cheatsheet](https://gleam.run/cheatsheets/gleam-for-erlang-users/) — custom type tuple representation (confirms Pitfall 3)
- birch source: `async_actor.gleam` lines 132-137, 170-187 (Pitfall 5 confirmed)
- birch source: `birch_erlang_logger_ffi.erl` line 75 (Pitfall 3 confirmed)
- birch source: `birch_ffi.erl` lines 231-240 (Pitfall 8 confirmed)

### Secondary (MEDIUM confidence)

- [Erlang OTP logger overload issue #7417](https://github.com/erlang/otp/issues/7417) — known performance bug in `logger_olp`
- [persistent_term Erlang blog](https://www.erlang.org/blog/persistent_term/) — best practices for write-rarely patterns
- [gleam_otp documentation](https://hexdocs.pm/gleam_otp/index.html) — actor lifecycle and supervision patterns

### Tertiary (LOW confidence)

- File handler path separator Windows behavior (Pitfall 12): MEDIUM risk claim; depends on how Erlang normalizes paths on Windows. Needs validation if Windows support is a goal.

---
*Research completed: 2026-02-27*
*Ready for roadmap: yes*
