# Technology Stack: Pre-1.0 Hardening Tooling

**Project:** birch (Gleam logging library)
**Researched:** 2026-02-27
**Focus:** Code review tooling, BEAM benchmarking, OTP logger validation

## Recommended Stack

### 1. Gleam Code Analysis and Linting

| Technology | Version | Purpose | Why |
|------------|---------|---------|-----|
| `gleam build --warnings-as-errors` | Gleam 1.14.0 | Static analysis, unused code, exhaustive patterns | Built-in; detects unused imports, private functions, unreachable code, non-exhaustive patterns. Already in justfile as `build-strict`. No external linter exists for Gleam. |
| Gleam Language Server | 1.14.0 | IDE-assisted code review | Provides go-to-type, auto-remove unused imports, exhaustive case generation, unreachable clause removal |
| `gleam format --check` | 1.14.0 | Style enforcement | Gleam has one canonical format. Already in CI. |
| Manual code review | N/A | Idiomatic Gleam patterns | No Gleam linter equivalent to clippy/eslint exists. Idiomatic patterns must be reviewed by hand or AI-assisted review. |

**Confidence:** HIGH -- verified via Gleam compiler docs, GitHub issues, and Gleam blog posts.

**Key finding: There is no standalone Gleam linter.** Gleam's compiler is the only static analysis tool. It catches: unused imports, unused private functions, non-exhaustive pattern matches, unreachable code (after `panic`), unused variables, empty modules with no public definitions, and truncated int segment values. The `--warnings-as-errors` flag promotes all of these to errors.

**What the compiler does NOT catch:**
- Non-idiomatic patterns (e.g., using `list.fold` where `list.map` suffices)
- Suboptimal API design (e.g., inconsistent naming conventions)
- OTP anti-patterns in FFI code (e.g., `persistent_term` misuse)
- Cross-platform behavioral inconsistencies in FFI

**Implication for this milestone:** Code review for idiomatic Gleam and OTP best practices must be done manually (by a human or AI reviewer), not by tooling. The compiler's `--warnings-as-errors` is necessary but not sufficient. The review pass is inherently a manual audit with checklist.

### 2. BEAM Benchmarking

| Technology | Version | Purpose | Why |
|------------|---------|---------|-----|
| hyperfine | latest | CLI-level benchmark orchestration | User requirement. Measures wall-clock time including BEAM VM startup. Good for comparing birch vs raw `:logger` as separate processes. |
| gleamy_bench | 0.6.0 | In-process Gleam microbenchmarks | Pure Gleam, works on both Erlang and JS targets. Measures IPS, min, percentiles. Use for hot-path benchmarks (log emit, format, level filter). |
| gleescript | 1.5.2 | Bundle Gleam into escript for hyperfine | Converts `gleam run -m` into standalone escript. Required for hyperfine to benchmark BEAM code (hyperfine needs a CLI command). |
| erlperf | 2.3.0 | Erlang-native microbenchmarks (optional) | More precise than gleamy_bench for BEAM-specific measurements. Low-overhead mode (1-2ns/iteration). Use only if gleamy_bench lacks precision for sub-microsecond operations. |

**Confidence:** HIGH for hyperfine + gleamy_bench + gleescript. MEDIUM for erlperf (not tested from Gleam, but available on hex.pm and callable from Erlang FFI).

#### Benchmarking Strategy: Two Layers

**Layer 1: hyperfine (CLI-level, wall-clock)**

hyperfine measures the full execution cycle: BEAM VM startup + application boot + benchmark workload + shutdown. This is what the project requirements specify.

The approach:

1. Create benchmark scripts as Gleam modules with `main()` functions (e.g., `bench/bench_throughput.gleam`)
2. Bundle each into an escript via gleescript: `gleam run -m gleescript`
3. Run with hyperfine:

```bash
# Compare birch logging vs raw :logger
hyperfine --warmup 3 \
  './bench_birch_throughput' \
  './bench_raw_logger_throughput'

# Compare formatter overhead
hyperfine --warmup 3 \
  './bench_human_readable_format' \
  './bench_simple_format' \
  './bench_json_format'
```

**Important caveats for hyperfine + BEAM:**
- BEAM VM startup takes 50-200ms. This dominates short benchmarks. Each benchmark script must run enough iterations internally (10K-100K log calls) so the workload dwarfs startup.
- Use `--warmup 3` to let the OS page in the BEAM binary and JIT warm up.
- Use `--min-runs 10` (hyperfine default) for statistical significance.
- Do NOT use `gleam run -m bench_foo` directly -- the Gleam build tool adds overhead. Use escripts.

**Layer 2: gleamy_bench (in-process, microbenchmark)**

For measuring hot-path performance without VM startup noise. Run inside a single BEAM instance:

```gleam
import gleamy/bench

pub fn main() {
  bench.run(
    [bench.Input("1000 logs", 1000)],
    [
      bench.Function("birch info", fn(n) { run_birch_info(n) }),
      bench.Function("raw :logger", fn(n) { run_raw_logger(n) }),
    ],
    [bench.Duration(3000), bench.Warmup(500)],
  )
  |> bench.table([bench.IPS, bench.Min, bench.P(99)])
  |> io.println()
}
```

This measures:
- Log throughput (messages/sec)
- Level filtering overhead (log below threshold, should be near-zero)
- Formatter cost (human_readable vs simple vs JSON)
- Metadata serialization cost
- Sampling overhead
- Lazy evaluation savings

#### Why NOT These Alternatives

| Tool | Why Not |
|------|---------|
| glychee 1.1.2 | Wraps Elixir's Benchee. Requires Elixir as a dependency. Adds unnecessary complexity for a pure Gleam project. gleamy_bench is Gleam-native and sufficient. |
| erlperf as primary | Requires Erlang FFI to invoke from Gleam. Overkill for this use case. Only use if gleamy_bench proves insufficiently precise for sub-microsecond measurements. |
| timer:tc (Erlang stdlib) | Raw measurement, no warmup/statistics. Must build your own harness. gleamy_bench provides this out of the box. |
| Benchee directly | Elixir-only. Not usable from Gleam without glychee wrapper. |

### 3. OTP Logger Testing and Validation

| Technology | Version | Purpose | Why |
|------------|---------|---------|-----|
| ETS capture handler (custom) | OTP 27 | Capture `:logger` events for assertion | Already implemented in `test/birch_logger_test_ffi.erl`. The idiomatic OTP approach: install a custom handler/formatter that writes to ETS, then read back and assert. |
| `logger:get_handler_config/1` | OTP 27 | Verify formatter installation | Erlang stdlib API. Use to assert birch formatter is correctly installed on `default` handler. Already used in `birch_erlang_logger_ffi.erl`. |
| `logger:add_handler/3` | OTP 27 | Install test-specific handler | OTP API for adding a handler that captures events to a process mailbox or ETS. Use for verifying events reach `:logger` with correct metadata. |
| gleeunit | >= 1.0.0 | Test runner | Already in use. No change needed. |
| qcheck | >= 1.0.0 | Property-based testing | Already in use. Use for property tests on level round-trips, metadata serialization, formatter output invariants. |

**Confidence:** HIGH -- based on OTP official documentation and existing test infrastructure in the birch codebase.

#### OTP Logger Test Patterns

**Pattern 1: Capture formatter output (already implemented)**

birch already does this correctly in `erlang_logger_test.gleam`. The pattern:

1. Create ETS buffer via `new_capture_buffer()`
2. Install capture formatter on `default` handler via `erlang_logger.install_formatter_on`
3. Log through birch
4. Sleep briefly (`:logger` dispatch is asynchronous within the same process)
5. Read buffer and assert

**Pattern 2: Validate handler installation state**

```erlang
%% Verify birch formatter is installed
{ok, #{formatter := {birch_erlang_logger_ffi, #{format_fn := _}}}} =
    logger:get_handler_config(default).
```

This is already done in `is_formatter_configured/0` but should also be used in tests to verify state after `setup()` / `remove_formatter()`.

**Pattern 3: Validate OTP event pass-through**

Install a separate test handler (not the formatter on `default`) that captures raw `:logger` events, then verify birch LogRecords arrive with correct metadata:

```erlang
%% Test handler that sends events to test process
-module(birch_test_logger_handler).
-export([log/2]).

log(#{meta := #{birch_log_record := Record}} = _Event, #{test_pid := Pid}) ->
    Pid ! {captured, Record},
    ok;
log(Event, #{test_pid := Pid}) ->
    Pid ! {captured_otp, Event},
    ok.
```

Install with `logger:add_handler(test_capture, birch_test_logger_handler, #{config => #{test_pid => self()}})`.

**Pattern 4: persistent_term validation**

After `configure()` / `setup()`, verify persistent_term state:

```erlang
true = persistent_term:get(birch_logger_initialized, false).
```

After `remove_formatter()`:

```erlang
false = persistent_term:get(birch_logger_initialized, false).
```

#### What NOT to Use for Logger Testing

| Tool | Why Not |
|------|---------|
| Common Test (ct) | Heavyweight test framework. birch uses gleeunit. Adding ct would fragment test infrastructure. |
| ExUnit.CaptureLog | Elixir-only. Not usable from Gleam. |
| Mocking libraries | Gleam has no mocking library. ETS-based capture is the idiomatic BEAM approach and already works. |
| lager | Deprecated logging framework. Not relevant to OTP `:logger`. |

## Supporting Libraries

| Library | Version | Purpose | When to Add |
|---------|---------|---------|-------------|
| gleamy_bench | 0.6.0 | Benchmarking | Phase: benchmarking setup. Add as dev dependency. |
| gleescript | 1.5.2 | Escript bundling for hyperfine | Phase: benchmarking setup. Add as dev dependency. |

## Installation

```bash
# Benchmarking tools (dev dependencies)
gleam add gleamy_bench --dev
gleam add gleescript --dev

# hyperfine (system tool, not a Gleam package)
# Install via package manager:
# Ubuntu: sudo apt install hyperfine
# macOS: brew install hyperfine
# cargo: cargo install hyperfine
```

No new runtime dependencies. All additions are dev-only.

## Alternatives Considered

| Category | Recommended | Alternative | Why Not |
|----------|-------------|-------------|---------|
| Linting | `gleam build --warnings-as-errors` + manual review | No alternative exists | Gleam has no external linter. The compiler is it. |
| Benchmarking (CLI) | hyperfine + gleescript | Direct `gleam run` | `gleam run` includes build tool overhead. escript is faster and more representative. |
| Benchmarking (in-process) | gleamy_bench | glychee | glychee requires Elixir dependency. gleamy_bench is pure Gleam. |
| Benchmarking (in-process) | gleamy_bench | erlperf | erlperf requires Erlang FFI. Only needed if gleamy_bench precision is insufficient. |
| Logger testing | Custom ETS capture handler | Common Test | ct is heavyweight and foreign to the Gleam ecosystem. ETS capture is already working. |

## BEAM-Specific Benchmarking Considerations

### persistent_term GC Impact

birch uses `persistent_term` for global config, cached logger, async writer registry, and `:logger` initialization flag. The official Erlang documentation warns:

> "Updating or deleting a persistent term will trigger a global GC if the term does not fit in one machine word. All processes will be made runnable at once."

**Benchmark this specifically:** Measure the cost of `log.configure()` (which updates persistent_term) under load. This is a critical production concern.

### BEAM JIT Warmup

OTP 27 includes the JIT compiler. Benchmark scripts must:
1. Run warmup iterations (gleamy_bench handles this; hyperfine uses `--warmup`)
2. Not measure the first N iterations where JIT compilation occurs
3. Be aware that JIT behavior changes based on code path frequency

### Process Dictionary vs persistent_term

Scoped context uses the process dictionary (fast, per-process). Global config uses persistent_term (fast reads, expensive writes). Benchmarks should separately measure:
- Process-local operations (scope context merge, level check)
- Global operations (config lookup, handler dispatch)

### Asynchronous Logger Dispatch

`:logger` handler callbacks execute asynchronously. The existing test pattern of `sleep(100)` after logging is a pragmatic workaround but fragile. For benchmarks, use synchronous patterns:
- For throughput: log N messages, then flush, measure total time
- For latency: use in-process gleamy_bench (no `:logger` async dispatch involved)

## Sources

### Gleam Tooling
- [Gleam Language Server](https://gleam.run/language-server/) -- IDE features and code actions
- [Gleam Compiler Warnings (GitHub Issues)](https://github.com/gleam-lang/gleam/issues/3808) -- unused code detection behavior
- [Exhaustive Gleam](https://gleam.run/news/v0.33-exhaustive-gleam/) -- pattern match exhaustiveness
- [Happy Holidays 2025 Release](https://gleam.run/news/the-happy-holidays-2025-release/) -- Gleam 1.14 features

### Benchmarking
- [gleamy_bench v0.6.0 (hex.pm)](https://hex.pm/packages/gleamy_bench) -- published 2025-02-17
- [gleamy_bench docs](https://hexdocs.pm/gleamy_bench/index.html) -- API and usage
- [gleescript v1.5.2 (hex.pm)](https://hex.pm/packages/gleescript) -- escript bundling
- [gleescript docs](https://hexdocs.pm/gleescript/) -- usage instructions
- [hyperfine (GitHub)](https://github.com/sharkdp/hyperfine) -- CLI benchmarking tool
- [erlperf v2.3.0 (hex.pm)](https://hex.pm/packages/erlperf) -- Erlang benchmarking suite
- [Erlang Benchmarking Guide](https://www.erlang.org/doc/system/benchmarking.html) -- official best practices

### OTP Logger
- [OTP Logger Chapter](https://www.erlang.org/doc/apps/kernel/logger_chapter.html) -- handler/formatter architecture
- [OTP Logger API](https://www.erlang.org/doc/apps/kernel/logger.html) -- add_handler, get_handler_config
- [OTP Logger Cookbook](https://www.erlang.org/doc/apps/kernel/logger_cookbook.html) -- configuration patterns
- [persistent_term docs](https://www.erlang.org/doc/apps/erts/persistent_term.html) -- GC behavior warnings
- [Clever Use of persistent_term (Erlang Blog)](https://www.erlang.org/blog/persistent_term/) -- best practices

### Gleam Ecosystem
- [glychee v1.1.2 (hex.pm)](https://hex.pm/packages/glychee) -- Benchee wrapper (not recommended)
- [Gleam Exercism Track](https://exercism.org/tracks/gleam) -- idiomatic patterns via mentoring
