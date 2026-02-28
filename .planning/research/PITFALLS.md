# Domain Pitfalls

**Domain:** Gleam/OTP logging library hardening (pre-1.0 quality gate)
**Researched:** 2026-02-27

## Critical Pitfalls

Mistakes that cause rewrites, production failures, or broken releases.

### Pitfall 1: Formatter Crash Removes :logger Handler Entirely

**What goes wrong:** If birch's `format/2` callback crashes (any exception in the Erlang FFI code), OTP's `:logger` **removes the handler from configuration** and prints a short error to the terminal. From that point on, ALL logging stops silently -- birch logs, OTP supervisor reports, application lifecycle messages. The system appears healthy but is flying blind.

**Why it happens:** The `birch_erlang_logger_ffi:format/2` function constructs Gleam LogRecord tuples from OTP log events and calls Gleam format functions through FFI. Any mismatch -- unexpected `:logger` message format, a `report_cb` callback that returns unexpected data, a Gleam record with a changed field position after recompilation -- causes the formatter to crash.

**Consequences:**
- Complete loss of logging output with no obvious error signal
- OTP's diagnostic message ("Handler default removed") is easy to miss in production
- The `persistent_term` cache (`birch_logger_initialized`) still says `true`, so birch never tries to re-install
- Applications continue running silently without any logging

**Warning signs:**
- Logs suddenly stop appearing (no error, just silence)
- `logger:get_handler_config(default)` returns `{error, {not_found, default}}` in a running system
- The `birch_logger_initialized` persistent_term is `true` but no handler is installed

**Prevention:**
1. Wrap the entire `format/2` body in a try/catch that returns a fallback formatted string on any error, rather than letting the exception propagate to `:logger`
2. The fallback format should include the error reason itself so crashes are visible in output
3. Add a health-check function: `erlang_logger.is_healthy()` that verifies the handler is actually installed (not just the persistent_term cache)
4. Add a defensive guard in `build_log_record_from_otp/3` for each message format variant, with a catch-all that uses `io_lib:format("~p", [Msg])` for truly unexpected formats
5. Test the formatter with intentionally malformed `:logger` events (bad `report_cb`, unexpected message types, huge terms)

**Detection:** Add `erlang_logger.is_healthy/0` that does `logger:get_handler_config(default)` and verifies birch is still the formatter. Call this from health checks.

**Phase mapping:** Code review phase -- this is the single highest-risk item in the OTP integration.

**Confidence:** HIGH -- this is documented OTP behavior (sources: [Logging chapter](https://www.erlang.org/doc/apps/kernel/logger_chapter.html), verified against OTP 27 docs).

---

### Pitfall 2: persistent_term Updates Trigger Global GC on All Processes

**What goes wrong:** Every call to `configure()`, `set_level()`, or `reset_config()` writes to `persistent_term`, which triggers a **global garbage collection scan of every process in the system**. In a production system with thousands or tens of thousands of processes, this causes a latency spike. Multiple rapid calls (e.g., `configure()` followed by `set_level()`) multiply the impact.

**Why it happens:** `persistent_term` is designed for "write-rarely, read-often" data. When a term is updated or deleted, OTP schedules every process to scan its heap for references to the old value. Processes that still reference the old term get a full-sweep GC. At most two processes do full-sweep GC simultaneously, but all processes get scheduled for heap scans.

**Consequences:**
- Latency spike proportional to number of processes in the system
- `configure()` currently does: `set_global_config` (1 persistent_term write) + `clear_cached_default_logger` (1 persistent_term erase) = 2 global GC passes
- `set_level()` does the same: 2 global GC passes
- `reset_config()` does: `clear_global_config` (1 erase) + `clear_cached_default_logger` (1 erase) = 2 global GC passes
- In the birch FFI, the async writer registry also uses `persistent_term` and updates on every `make_async()` and `register_writer()`/`unregister_writer()` call

**Warning signs:**
- Latency spikes correlated with log configuration changes
- Erlang Observer shows GC activity across all processes simultaneously
- Benchmarks of `configure()` show unexpectedly high latency

**Prevention:**
1. Document clearly that `configure()`, `set_level()`, and `reset_config()` are startup-only operations (already partially done in docstrings -- verify it is prominent enough)
2. Combine global config and cached logger into a single persistent_term key (a tuple or map) to reduce from 2 writes to 1 per configuration change
3. Move the async writer registry from `persistent_term` to an ETS table -- the registry is updated frequently (on every `make_async()` call) and is a poor fit for persistent_term
4. Add a benchmark that measures `configure()` latency with 10K+ processes to establish baseline

**Detection:** Benchmark `configure()` with varying process counts.

**Phase mapping:** Code review phase for the consolidation fix; benchmarking phase for measurement.

**Confidence:** HIGH -- persistent_term GC behavior is [documented in OTP official docs](https://www.erlang.org/docs/27/apps/erts/persistent_term.html).

---

### Pitfall 3: Gleam Type Representation Dependency in Erlang FFI

**What goes wrong:** The `birch_erlang_logger_ffi.erl` file directly accesses Gleam record fields by tuple position (e.g., `erlang:element(5, LogRecord)` on line 75 to get the message field). It also pattern-matches on Gleam type variant atoms (e.g., `erlang_emergency`, `fatal`, `log_record`). If the Gleam compiler changes its record representation, or if fields are reordered in the Gleam source, the FFI silently reads wrong data or crashes.

**Why it happens:** Gleam compiles custom types to Erlang tuples: `{variant_name, field1, field2, ...}`. The Erlang FFI bypasses Gleam's type system and directly indexes into these tuples. This coupling to the internal representation is fragile.

**Consequences:**
- Wrong field extracted (e.g., getting logger_name instead of message) -- logs contain garbage
- Pattern match failure on variant atoms -- formatter crash, which triggers Pitfall 1
- Extremely hard to debug because the code compiles and passes type checks

**Warning signs:**
- Log messages contain wrong content after changing field order in `record.gleam`
- Formatter crashes after adding/removing fields from LogRecord
- Tests pass on one target but fail on another

**Prevention:**
1. Replace `erlang:element(5, LogRecord)` with accessor functions exported from Gleam. The `record` module should export `message(record)`, `level(record)`, etc. that the Erlang FFI calls
2. Alternatively, include the Gleam-generated Erlang header files (`-include("birch/record_LogRecord.hrl")`) and use record syntax
3. Add integration tests that verify the FFI extracts the correct fields from a known LogRecord
4. Add a comment in both `record.gleam` and `birch_erlang_logger_ffi.erl` noting the coupling, so field reordering triggers a review

**Detection:** Test that constructs a LogRecord in Gleam, passes it through the `:logger` formatter FFI, and verifies the output contains the expected message text.

**Phase mapping:** Code review phase -- fix the tuple indexing before any field changes.

**Confidence:** HIGH -- Gleam custom type representation is documented in the [Gleam for Erlang users cheatsheet](https://gleam.run/cheatsheets/gleam-for-erlang-users/). The `erlang:element/2` usage is visible in `birch_erlang_logger_ffi.erl` line 75.

---

### Pitfall 4: Benchmarking BEAM with hyperfine Measures Startup, Not Logging

**What goes wrong:** Using `hyperfine` to benchmark birch measures **BEAM VM startup + code loading + JIT warmup + logging + shutdown**, not just logging performance. BEAM startup takes 50-200ms depending on loaded applications. JIT compilation (BeamAsm) adds further warmup cost on first execution of each code path. The actual logging operation might be microseconds, completely invisible inside hundreds of milliseconds of startup noise.

**Why it happens:** `hyperfine` benchmarks entire command executions. Each run starts a fresh BEAM VM, loads OTP applications, compiles code paths through the JIT, runs the benchmark, and shuts down. The logging hot path is a tiny fraction of total wall time.

**Consequences:**
- Benchmark results reflect BEAM startup time, not birch performance
- Comparative benchmarks (birch vs raw `:logger`) may show near-identical results because startup dominates both
- `hyperfine --warmup N` helps with disk caching but does NOT help with JIT warmup (each run is a fresh VM)
- Results vary wildly between runs due to OS scheduling during the startup-heavy phase

**Warning signs:**
- All benchmark variants show similar times (~150-250ms) regardless of log volume
- Adding more log calls barely changes total time
- High variance in results (>10% coefficient of variation)

**Prevention:**
1. Use hyperfine with `--setup` to pre-compile code and `--warmup` for disk cache
2. Design benchmark scripts that do a **large number of log operations per invocation** (10K-1M) so logging time dominates startup time
3. Use `--shell=none` to avoid shell startup overhead (run `erl -noshell -s ...` directly)
4. Report "logs per second" calculated from (total_logs / measured_time) minus calibrated startup baseline
5. Complement hyperfine with in-VM benchmarking using [erlperf](https://github.com/max-au/erlperf) or `timer:tc/3` for microsecond-level measurements of hot paths
6. Use hyperfine's `--export-json` to capture results for regression tracking
7. Each benchmark invocation should have a preamble that runs the logging hot path 1000+ times to trigger JIT compilation before the measured loop

**Detection:** Compare benchmark of "log 1 message" vs "log 100K messages" -- if times are similar, startup is dominating.

**Phase mapping:** Benchmarking phase -- this must be designed correctly from the start.

**Confidence:** HIGH -- BEAM startup overhead is well-known; [Erlang benchmarking docs](https://www.erlang.org/doc/system/benchmarking.html) explicitly warn about process isolation and recommend restarting the emulator between runs while also ensuring measurements last "at least several seconds."

---

### Pitfall 5: Async Actor Queue Uses list.length for Overflow Check (O(n) per log)

**What goes wrong:** In `async_actor.gleam`, the overflow check on line 133 calls `list.length(state.pending)` on every incoming `Log` message. Gleam lists are linked lists -- `list.length` is O(n). Under high throughput with a large `max_queue_size` (e.g., 5000), every single log message triggers a walk of up to 5000 list nodes just to count them. The `handle_overflow` function for `DropOldest` also calls `list.length(pending)` again, plus `list.take(pending, list.length(pending) - 1)` which is another O(n) traversal.

**Why it happens:** Natural Gleam style uses lists, and `list.length` looks harmless. But in a hot loop processing potentially thousands of messages per second, O(n) per message becomes O(n^2) over the queue.

**Consequences:**
- Async handler becomes slower than synchronous handler at high queue sizes
- Backpressure increases non-linearly -- the fuller the queue, the slower each new message is processed
- Under sustained load, the actor mailbox grows because it cannot process messages fast enough, eventually leading to memory pressure or OTP system-wide slowdown

**Warning signs:**
- Async handler throughput drops as queue size config increases
- Actor mailbox length grows unbounded during load tests
- CPU profiling shows time spent in `list:length/1` and `list:take/3`

**Prevention:**
1. Track queue length as an integer field in `State` instead of computing it. Increment on enqueue, decrement on dequeue, reset on flush
2. For `DropOldest`, use a queue data structure (e.g., `gleam/queue` or Erlang's `:queue`) with O(1) amortized push/pop instead of a list with O(n) drop-from-end
3. Alternatively, since the Erlang implementation in `birch_ffi.erl` already uses `:queue` (lines 138, 173-183), consider using the Erlang FFI path for the actor too, or expose queue operations to Gleam

**Detection:** Benchmark async handler with queue_size=100 vs queue_size=5000 and compare throughput.

**Phase mapping:** Code review phase for the fix; benchmarking phase for verification.

**Confidence:** HIGH -- this is directly visible in the source code at `async_actor.gleam` lines 132-137 and 170-187.

## Moderate Pitfalls

### Pitfall 6: Double Logging Path on BEAM (emit to :logger AND birch handlers)

**What goes wrong:** In `logger.gleam` line 309-312, `emit_record` sends every log both to `:logger` via `emit_to_beam` AND to all birch handlers via `handler.handle_all`. If a user adds a console handler to their logger configuration (a natural thing to do), they get duplicate output -- once from birch's handler and once from `:logger`'s default handler which also uses birch's formatter.

**Why it happens:** The dual-path exists to support users who want additional handlers beyond `:logger` (e.g., JSON file output). But the default path already goes through `:logger`, and new users adding `console.handler()` as a birch handler don't realize :logger is already outputting to console.

**Prevention:**
1. Document this behavior prominently in the console handler module and in the getting-started guide
2. Consider adding a check: if birch formatter is installed on `:logger` default handler AND user adds a console handler, emit a warning
3. On BEAM, `default_handlers()` returns `[]` (correct) -- but `birch.new()` also calls `get_config()` which returns the global config's handlers. If users add handlers via `config_handlers([console.handler()])`, those get the duplicate path

**Phase mapping:** Code review phase -- documentation and potentially a diagnostic warning.

**Confidence:** HIGH -- directly visible in source code.

---

### Pitfall 7: File Handler Checks Rotation on Every Write (stat syscall per log)

**What goes wrong:** `write_to_file` in `file.gleam` calls `should_rotate_by_size` before every append, which calls `file_size` which calls `simplifile.file_info`. This is a filesystem stat() syscall on every single log message. Under high throughput, this adds significant I/O overhead.

**Why it happens:** Size-based rotation needs to know the current file size. Checking on every write is the simplest correct implementation.

**Prevention:**
1. Track file size in memory, incrementing by message byte length on each write, and only do a real stat() check periodically (e.g., every 100 writes or every N seconds)
2. For time-based rotation, `should_rotate_by_time` reads a marker file from disk on every write -- cache the last period in memory instead

**Phase mapping:** Code review phase for the optimization; benchmarking phase to measure impact.

**Confidence:** HIGH -- directly visible in source code at `file.gleam` lines 68-109.

---

### Pitfall 8: Async Writer Registry in persistent_term Updated on Every make_async Call

**What goes wrong:** In `birch_ffi.erl`, `register_writer` (line 231-234) and `unregister_writer` (line 236-240) update a `persistent_term` key every time an async writer starts or stops. Each update triggers a global GC (see Pitfall 2). If an application creates async handlers dynamically (e.g., per-request file handlers), this causes repeated global GC passes.

**Why it happens:** The registry needs mutable shared state. `persistent_term` was chosen for fast reads. But the write path is expensive and suitable only for static registrations at startup.

**Prevention:**
1. Move the async writer registry to an ETS table (`public`, `set` or `named_table`) -- ETS updates are process-local and do not trigger global GC
2. Alternatively, use a registered gen_server as the registry (more OTP-idiomatic but adds one message hop for lookups)
3. If persistent_term must be used, document that async handlers should only be created at startup

**Phase mapping:** Code review phase.

**Confidence:** HIGH -- directly visible in source code and consistent with persistent_term documentation.

---

### Pitfall 9: No Supervision for Async Actor Processes

**What goes wrong:** The async actor started by `async_actor.start` via `actor.start` creates an unlinked, unsupervised process. If this process crashes (e.g., handler write throws an exception), it dies silently. The `ActorRegistry` still holds a reference to the dead process. All subsequent `async_send` calls send messages to a dead process -- messages are silently discarded by the BEAM. No error, no warning, just lost logs.

**Why it happens:** `actor.start` in `gleam_otp` starts a standalone process. birch registers it in a dictionary but doesn't link it to a supervisor or monitor it for exits.

**Prevention:**
1. Monitor the actor process and detect crashes -- on crash, log a warning to stderr and either restart the actor or fall back to synchronous logging
2. Provide guidance (and ideally a helper function) for adding the async actor to a supervision tree
3. At minimum, check if the process is alive before sending in `async_actor.send`, and recreate if dead
4. Add `process.monitor` to detect actor deaths and clean up the registry

**Phase mapping:** Code review phase for the crash detection; could defer full supervision to a future milestone.

**Confidence:** HIGH -- `gleam_otp` actor behavior is [documented](https://hexdocs.pm/gleam_otp/index.html); birch's lack of monitoring is visible in `async.gleam`.

---

### Pitfall 10: Pre-1.0 API Surface Has Significant Deprecated Cruft

**What goes wrong:** birch's public API in `birch.gleam` contains 15+ `@deprecated` functions and type aliases. When users install birch and explore the API, they see a large surface of deprecated items mixed with the current API. On Hex, the documentation will show all public functions regardless of deprecation. This creates confusion about which API to actually use and gives an impression of instability.

**Why it happens:** The API evolved through multiple iterations (handler-based :logger integration, then formatter-based, then direct emission). Deprecated items were kept for backward compatibility during the 0.x phase.

**Prevention:**
1. Before 1.0, remove all deprecated items -- pre-1.0 is the time for breaking changes
2. Gleam now actively discourages 0.x versions on Hex, asking for confirmation before publishing. Going to 1.0 with a clean API surface sends a strong stability signal
3. Audit the `internal_modules` list in `gleam.toml` -- `birch/sampling` and `birch/handler/async` are listed as internal but contain public functions that users might depend on. Decide: make them truly public, or move their public API to the main module
4. Remove deprecated type aliases (`LogLevel`, `LogHandler`, `LogMetadata`, `Config`, `TimestampFormatter`)

**Phase mapping:** Code review phase (API audit).

**Confidence:** HIGH -- directly visible in source code. Gleam's stance on 0.x is documented in [release notes](https://gleam.run/news/improved-performance-and-publishing/).

## Minor Pitfalls

### Pitfall 11: Sampling Uses Non-Deterministic Random in Tests

**What goes wrong:** `sampling.gleam` uses `float.random()` for probabilistic sampling. Tests that verify sampling behavior are inherently flaky because they depend on random chance. A test that checks "10% sample rate should emit roughly 10% of messages" could fail or pass depending on the random sequence.

**Prevention:**
1. Add an injectable random source to `SampleConfig` for testing: `SampleConfig(level, rate, random_fn: Option(fn() -> Float))`
2. In tests, inject a deterministic sequence: `fn() { 0.05 }` (always below 10% threshold)
3. For production, default to `float.random()`

**Phase mapping:** Code review phase.

**Confidence:** HIGH -- directly visible in source code at `sampling.gleam` line 77.

---

### Pitfall 12: File Handler Path Separator Hardcoded to "/"

**What goes wrong:** `get_parent_dir` and `get_filename` in `file.gleam` split paths on "/" exclusively. On Windows (if Gleam/Erlang is used there), paths use "\" and the handler fails to create parent directories or identify filenames correctly.

**Prevention:**
1. Use `simplifile` path utilities if available, or `filepath` library
2. Alternatively, document that file handler is Unix-only and the JavaScript target may have different path conventions
3. Since birch targets BEAM primarily and Windows Erlang uses "/" internally in many cases, this may be lower risk than it appears -- verify with actual testing

**Phase mapping:** Code review phase (low priority).

**Confidence:** MEDIUM -- depends on how Erlang normalizes paths on Windows.

---

### Pitfall 13: Benchmarking Process Isolation Failure

**What goes wrong:** Running multiple benchmark scenarios in the same BEAM process causes later benchmarks to have larger heaps (from previous test allocations), fewer GC pauses, and different JIT compilation state. Results become incomparable.

**Prevention:**
1. Each benchmark scenario should run in a freshly spawned process (Erlang official recommendation)
2. Consider restarting the emulator between scenarios for maximum isolation
3. For hyperfine-based benchmarks, each invocation naturally gets a fresh VM -- but in-VM benchmarks (timer:tc, erlperf) need explicit process isolation

**Phase mapping:** Benchmarking phase.

**Confidence:** HIGH -- per [Erlang benchmarking documentation](https://www.erlang.org/doc/system/benchmarking.html).

---

### Pitfall 14: Dependency Version Bounds Allow Untested Major Versions

**What goes wrong:** `gleam.toml` specifies `gleam_stdlib = ">= 0.48.0 and < 2.0.0"` -- this allows gleam_stdlib 1.x which could have breaking changes. Similarly for `gleam_json = ">= 2.0.0 and < 4.0.0"`. Users on newer dependency versions may hit incompatibilities that birch hasn't been tested against.

**Prevention:**
1. Before 1.0 release, verify against latest versions of all dependencies
2. Tighten bounds to tested versions: use `">= 0.48.0 and < 1.0.0"` if only 0.x has been tested
3. Add CI matrix testing against minimum and maximum allowed dependency versions

**Phase mapping:** Pre-release checklist (after code review and benchmarking).

**Confidence:** MEDIUM -- depends on how aggressively dependencies change.

## Phase-Specific Warnings

| Phase Topic | Likely Pitfall | Mitigation |
|-------------|---------------|------------|
| OTP :logger code review | Formatter crash removes handler (Pitfall 1) | Add try/catch fallback in format/2, add health check function |
| OTP :logger code review | Tuple position dependency (Pitfall 3) | Replace erlang:element/2 with Gleam accessor functions |
| General code review | persistent_term GC storms (Pitfall 2) | Consolidate to single key; move async registry to ETS |
| General code review | Async actor O(n) queue (Pitfall 5) | Track length as integer; consider queue data structure |
| General code review | Unsupervised async actor (Pitfall 9) | Add process monitoring and crash recovery |
| General code review | Deprecated API cruft (Pitfall 10) | Remove all deprecated items before 1.0 |
| General code review | Double logging path (Pitfall 6) | Document behavior; consider diagnostic warning |
| General code review | File handler stat per write (Pitfall 7) | Cache file size in memory |
| Benchmarking | Startup time dominates results (Pitfall 4) | High iteration counts per invocation; in-VM measurement complement |
| Benchmarking | Process isolation (Pitfall 13) | Fresh process per scenario; restart emulator between variants |
| Pre-release | Dependency bounds (Pitfall 14) | Tighten to tested versions; CI matrix |

## Sources

- [Erlang persistent_term documentation (OTP 27)](https://www.erlang.org/docs/27/apps/erts/persistent_term.html) -- global GC behavior, update warnings
- [Erlang Logging chapter (kernel)](https://www.erlang.org/doc/apps/kernel/logger_chapter.html) -- handler crash behavior, formatter requirements, overload protection
- [Erlang Benchmarking documentation](https://www.erlang.org/doc/system/benchmarking.html) -- process isolation, measurement duration, fresh process per test
- [Gleam for Erlang users cheatsheet](https://gleam.run/cheatsheets/gleam-for-erlang-users/) -- custom type Erlang representation
- [Gleam publishing and versioning](https://gleam.run/news/improved-performance-and-publishing/) -- 0.x version discouragement
- [gleam_otp documentation](https://hexdocs.pm/gleam_otp/index.html) -- actor lifecycle, supervision
- [Gleam internal modules](https://gleam.run/writing-gleam/gleam-toml/) -- internal_modules behavior
- [erlperf benchmarking tool](https://github.com/max-au/erlperf) -- in-VM BEAM benchmarking alternative
- [hyperfine](https://github.com/sharkdp/hyperfine) -- command-line benchmarking tool warmup and calibration
- [Erlang OTP logger overload protection discussion](https://erlangforums.com/t/custom-logger-handlers-overload-protection/2763) -- custom handler responsibilities
