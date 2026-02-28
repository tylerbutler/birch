# Feature Landscape: Pre-1.0 Code Review and Benchmarking

**Domain:** Gleam/OTP logging library code review and performance benchmarking
**Researched:** 2026-02-27

## Table Stakes

Review areas that any serious code review of a Gleam/OTP logging library must cover. Skipping these means the review is incomplete.

### Code Review Dimensions

| Review Area | Why Expected | Complexity | Notes |
|-------------|--------------|------------|-------|
| Idiomatic Gleam patterns | Author is newer to Gleam; non-idiomatic code works but confuses contributors and signals immaturity | Med | `use`/`result.try` pipelines, exhaustive pattern matching, opaque type usage, builder patterns |
| API surface consistency | Pre-1.0 is the last chance to fix naming before users depend on it | Med | `_m` suffixes, `_lazy` suffixes, deprecated re-exports, getter naming inconsistency (`name()` vs `get_level()`) |
| OTP `:logger` integration correctness | This is the primary log path on BEAM; bugs here lose logs silently | High | Recent overhaul (direct emission) needs validation: level mapping, metadata translation, formatter callback contract |
| Handler error isolation | Handler failures must never crash the application -- this is the core safety contract of a logging library | Med | `safe_call` wrapper, error callback mechanism, exception propagation paths |
| Type safety of public API | Gleam's selling point is type safety; a library that undermines it with stringly-typed or unvalidated APIs damages trust | Med | MetadataValue enum completeness, opaque type boundaries, internal module leakage |
| Cross-platform behavioral parity | Library claims cross-platform support; divergent behavior without documentation is a bug | High | FFI surface (428 LOC Erlang, 868 LOC JavaScript), scope context differences, default handler differences |
| Resource cleanup patterns | Logging libraries run for the lifetime of the application; resource leaks compound | Med | persistent_term GC on config updates, async writer lifecycle, file handle management, process dictionary cleanup in scopes |
| Configuration validation | Invalid config at runtime is worse than a compile-time error; missing validation creates hard-to-debug production issues | Low | No validation on queue sizes, rotation bytes, sample rates (except clamping) |

### Benchmarking Dimensions

| Benchmark Area | Why Expected | Complexity | Notes |
|----------------|--------------|------------|-------|
| Log call throughput (msgs/sec) | The fundamental performance question: how many log messages can birch emit per second | Med | Measure with hyperfine running a simple Gleam program that logs N messages in a loop |
| Filtered log overhead (no-op cost) | Most log calls in production are filtered out (e.g., Debug when level is Info); the cost of NOT logging matters more than the cost of logging | Low | This is the hot path -- measure `log.debug()` when level is Info; should be sub-microsecond |
| Overhead vs raw `:logger` | Users need to know the cost of using birch vs calling `:logger` directly; this is the "birch tax" | Med | Compare birch `log.info()` vs `logger:log(info, ...)` directly; target: under 2x overhead |
| Metadata overhead | Structured logging with metadata is birch's value prop; measuring its cost validates the design | Low | Compare logging with 0, 5, and 20 metadata pairs; watch for linear scaling |
| Lazy evaluation benefit | Lazy variants exist to avoid expensive formatting; benchmarks should prove they actually help | Low | Compare `log.debug("expensive: " <> expensive_fn())` vs `log.debug_lazy(fn() { "expensive: " <> expensive_fn() })` when filtered |

## Differentiators

Review areas specific to birch's unique characteristics -- its cross-platform nature, OTP integration, and Gleam ecosystem position.

| Review Area | Value Proposition | Complexity | Notes |
|-------------|-------------------|------------|-------|
| Erlang FFI idiomaticity | The Erlang FFI (birch_ffi.erl) is written by a Gleam developer, not an Erlang expert; OTP conventions may be violated | High | Check: module structure, function naming, process dictionary usage patterns, persistent_term best practices (atoms for level cache, not full config structs) |
| JavaScript FFI robustness | JS FFI is 2x larger than Erlang FFI and handles 4 runtimes (Node, Deno, Bun, browser); edge cases likely | High | Check: typeof guards for `process`, AsyncLocalStorage availability, Worker/edge runtime fallbacks, zlib availability per runtime |
| Scope context correctness | Scoped context via process dictionary (Erlang) and AsyncLocalStorage (Node.js) is subtle concurrency territory | High | Check: cleanup on exception paths (try/after), depth tracking correctness, nested scope behavior, concurrent process isolation |
| Async handler actor design | Uses gleam_otp actor for background queue; actor design patterns matter for production reliability | Med | Check: mailbox overflow behavior, flush semantics, graceful shutdown, actor restart strategy |
| persistent_term usage patterns | Erlang docs explicitly warn about GC cost of updating persistent_term; birch stores full config structs | Med | Check: whether config uses atoms (cheap to update) or complex terms (expensive GC); frequency of `configure()` calls in typical usage |
| File handler rotation edge cases | Size-based and time-based rotation with compression; many moving parts | Med | Check: concurrent write during rotation, path separator handling (hardcoded "/"), marker file cleanup, compression failure recovery |
| Level mapping between birch and OTP | birch has 9 levels (Trace through Fatal); OTP `:logger` has 8 (no Trace); mapping must be correct in both directions | Low | Check: Trace mapping, Fatal/Emergency equivalence, round-trip consistency |
| Sampling determinism for testing | Sampling uses `float.random()` which is non-deterministic; tests may flake | Low | Check: whether sampling is testable, injectable random source, boundary conditions at rate=0.0 and rate=1.0 |
| Console color detection | TTY and color depth detection via environment variable heuristics; wrong in CI environments | Low | Check: FORCE_COLOR support (de facto standard), NO_COLOR support (no-color.org), CI environment detection |

### Differentiator Benchmarks

| Benchmark Area | Value Proposition | Complexity | Notes |
|----------------|-------------------|------------|-------|
| Async handler throughput | Async handler exists to decouple log emission from I/O; must prove it actually reduces caller-side latency | Med | Compare sync vs async handler for same output; measure caller-side time, not total time |
| Sampling overhead | Sampling adds a random check per log call; should be negligible | Low | Compare sampled vs unsampled at same level; the cost should be the `float.random()` call only |
| Scoped context overhead | Scope context reads process dictionary on every log call; measure the cost | Low | Compare logging with and without active scope context; process dictionary reads should be O(1) but verify |
| Multi-handler dispatch | Users may attach multiple handlers; measure linear overhead per handler | Low | Compare 1, 3, and 5 handlers; verify overhead is proportional |
| File handler I/O cost | File handler adds disk I/O; measure the overhead vs console handler | Med | Compare console handler vs file handler for same volume; separate I/O time from formatting time |

## Anti-Features

Things to deliberately NOT review or benchmark in this milestone. Including them would waste effort or create misleading results.

| Anti-Feature | Why Avoid | What to Do Instead |
|--------------|-----------|-------------------|
| JavaScript target benchmarking | JS runtimes have vastly different performance characteristics (JIT warmup, GC pauses); benchmarks would be misleading and not actionable for a BEAM-focused library | Note JS is out of scope; revisit if JS becomes a primary target |
| Documentation site quality | docs-site/ is a separate concern from library code quality; reviewing it conflates library readiness with marketing | Review only doc comments in source code (they ARE the API documentation); leave docs-site for a separate pass |
| Feature completeness assessment | This is a quality pass, not a feature gap analysis; evaluating "what's missing" distracts from "is what exists correct" | Note any obvious gaps discovered during review but do not scope them as work items |
| Micro-benchmark precision (ns-level) | hyperfine measures process startup + runtime; Erlang BEAM has JIT, scheduler, GC that make nanosecond-precision measurements unreliable at the CLI level | Use hyperfine for relative comparisons (birch vs raw :logger); do not claim absolute per-call nanosecond costs |
| Breaking API redesign | The project aims for backward-compatible fixes; a full API redesign is out of scope and would block the 1.0 release rather than enable it | Document API concerns as recommendations for future consideration; implement only backward-compatible changes |
| Comparison with other Gleam logging libraries | Gleam's ecosystem is young; there are few mature logging libraries to compare against, and the comparison would not inform code quality improvements | Compare only against raw `:logger` as the baseline; note ecosystem alternatives in passing |
| Load testing under sustained pressure | Sustained multi-minute load tests require infrastructure and produce data more relevant to deployment tuning than library quality | Use short burst benchmarks (hyperfine's default 10+ runs, 3+ seconds each) for statistical validity |

## Feature Dependencies

```
Configuration validation → Handler error isolation (validation prevents errors that handlers must catch)

OTP :logger integration correctness → Level mapping between birch and OTP (integration depends on correct mapping)

Cross-platform behavioral parity → JavaScript FFI robustness (parity requires JS FFI to be correct)
Cross-platform behavioral parity → Erlang FFI idiomaticity (parity requires Erlang FFI to be correct)

Scope context correctness → Scoped context overhead benchmark (can't benchmark broken code)

Async handler actor design → Async handler throughput benchmark (design must be correct before measuring speed)

Handler error isolation → File handler rotation edge cases (rotation errors flow through handler error path)

Filtered log overhead → Lazy evaluation benefit (lazy is only relevant when filtering is fast)

Log call throughput → All other benchmarks (throughput is the baseline; other benchmarks are deltas)
```

### Review Ordering (Recommended)

1. **Level mapping and type safety** -- foundation; errors here invalidate everything above
2. **OTP :logger integration correctness** -- the primary code path on BEAM
3. **Handler error isolation** -- the safety contract
4. **Idiomatic Gleam patterns** -- module by module, depth-first
5. **Erlang FFI idiomaticity** -- requires OTP familiarity, independent of Gleam review
6. **JavaScript FFI robustness** -- independent of Erlang review, can be parallel
7. **API surface consistency** -- informed by patterns discovered in steps 4-6
8. **Resource cleanup patterns** -- requires understanding from all prior steps
9. **Configuration validation** -- low complexity, informed by all prior work
10. **Cross-platform behavioral parity** -- final check that both sides agree

### Benchmark Ordering (Recommended)

1. **Filtered log overhead** -- the most important number; baseline for "birch costs nothing when not logging"
2. **Log call throughput** -- the second most important number; baseline for "birch can handle N msgs/sec"
3. **Overhead vs raw :logger** -- the "birch tax" number users care about
4. **Metadata overhead** -- validates structured logging design
5. **Lazy evaluation benefit** -- validates lazy variant API exists for a reason
6. **Sampling overhead** -- validates sampling feature cost
7. **Scoped context overhead** -- validates scope feature cost
8. **Async handler throughput** -- validates async handler value proposition
9. **Multi-handler dispatch** -- validates handler architecture
10. **File handler I/O cost** -- measures I/O-bound handler cost

## MVP Recommendation

Prioritize the review and benchmarking as two parallel tracks:

**Review Track (must complete):**
1. OTP :logger integration correctness (recent overhaul, highest risk)
2. Handler error isolation (safety contract)
3. Idiomatic Gleam patterns across all modules (the stated goal)
4. API surface consistency (pre-1.0 cleanup)
5. Cross-platform FFI review (both targets)

**Benchmark Track (must complete):**
1. Filtered log overhead (the number that matters most)
2. Log call throughput (the baseline)
3. Overhead vs raw :logger (the comparison users want)

**Defer:**
- File handler rotation edge cases: These are real issues (documented in CONCERNS.md) but are experimental features and can be addressed post-1.0
- JavaScript FFI robustness beyond basic correctness: JS target is secondary; detailed Worker/edge runtime support can wait
- Sampling determinism: Low impact; sampling is a niche feature and flaky tests are annoying but not blocking

## Sources

- [Gleam Language Tour - Opaque Types](https://tour.gleam.run/advanced-features/opaque-types/)
- [Gleam Phantom Types (FOSDEM 2024)](https://archive.fosdem.org/2024/schedule/event/fosdem-2024-2064-gleam-in-the-machine-phantom-types-and-the-builder-pattern-/)
- [Gleam `use` expressions](https://gleam.run/news/v0.25-introducing-use-expressions/)
- [Gleam OTP actor documentation](https://hexdocs.pm/gleam_otp/gleam/otp/actor.html)
- [Erlang persistent_term best practices](https://www.erlang.org/blog/persistent_term/)
- [Erlang persistent_term documentation](https://www.erlang.org/doc/apps/erts/persistent_term.html)
- [hyperfine benchmarking tool](https://github.com/sharkdp/hyperfine)
- [erlperf - Erlang benchmarking suite](https://github.com/max-au/erlperf)
- [Erlang OTP 21 logger design](https://blog.erlang.org/My-OTP-21-Highlights/)
- [Erlang benchmarking documentation](https://www.erlang.org/doc/system/benchmarking.html)
- [Go logging benchmarks methodology](https://betterstack-community.github.io/go-logging-benchmarks/) (methodology reference)
- [C++ logging benchmark methodology](https://github.com/MuggleWei/cc_log_benchmark) (scenario design reference)
- [What I've Learned Writing Gleam](https://nohzafk.github.io/posts/2025-12-27-what-i-ve-learned-writting-gleam/)
- [Gleam error handling patterns](https://www.gleambits.co/guide/essentials/error.html)
- birch codebase: `.planning/codebase/CONCERNS.md`, `ARCHITECTURE.md`, `CONVENTIONS.md`, `TESTING.md`
