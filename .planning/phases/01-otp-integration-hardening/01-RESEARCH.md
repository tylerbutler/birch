# Phase 1: OTP Integration Hardening - Research

**Researched:** 2026-02-27
**Domain:** Erlang OTP :logger integration, Gleam FFI patterns, level mapping
**Confidence:** HIGH

## Summary

Phase 1 hardens birch's OTP :logger integration layer. The five requirements target distinct failure modes: formatter crash resilience (OTP-01), health check accuracy (OTP-02), FFI fragility from tuple position indexing (OTP-03), level mapping round-trip correctness (OTP-04), and :logger primary level filtering interaction (OTP-05).

The most impactful finding from this research is that **OTP 27's `logger_std_h` already catches formatter crashes** in `try_format/3` and falls back to the default formatter rather than removing the handler. This means OTP-01 is less critical for handler survival than initially documented in the project PITFALLS.md, but still important: the fallback produces ugly output ("FORMATTER CRASH: ...") and birch loses control of formatting until the root cause is fixed. A try/catch in birch's own `format/2` that returns a properly formatted fallback string is still the correct fix -- it preserves birch-style formatting even when internal processing fails.

A second significant finding: birch's `report_cb` handling in the FFI has the 1-arg and 2-arg callback return types swapped relative to the OTP specification. The try/catch blocks currently mask this, but the output for structured OTP reports using 1-arg `report_cb` may be incorrect (the `{Format, Args}` tuple gets passed to `unicode:characters_to_binary` instead of being formatted via `io_lib:format`).

**Primary recommendation:** Address each requirement in isolation. OTP-03 (accessor pattern) should be done first because it is a prerequisite for safe FFI changes needed by OTP-01. OTP-01 (try/catch in format/2) should include fixing the report_cb argument handling. OTP-04 (level mapping) and OTP-02 (health check) are independent. OTP-05 (documentation) depends on understanding the full integration but requires no code beyond setting :logger primary level.

<phase_requirements>
## Phase Requirements

| ID | Description | Research Support |
|----|-------------|-----------------|
| OTP-01 | Formatter `format/2` wraps body in try/catch with fallback | OTP 27 catches formatter crashes in `try_format/3` with default formatter fallback, but birch should still handle internally for clean output. The `report_cb` 1-arg/2-arg handling is swapped and should be fixed as part of this. See "Pitfall: Formatter Crash Recovery" and "Finding: report_cb Return Type Mismatch" sections. |
| OTP-02 | `is_healthy/0` verifies :logger handler is installed, not just persistent_term | Current `is_initialized()` only checks persistent_term cache. Need a new `is_healthy/0` that calls `logger:get_handler_config(default)` and verifies birch formatter. See "Pattern: Health Check Implementation" section. |
| OTP-03 | Erlang FFI replaces `erlang:element(5, LogRecord)` with safe accessor | Gleam generates Erlang record header files for custom types. Use `-include` with record syntax or export Gleam accessor functions. See "Pattern: Safe FFI Record Access" section. |
| OTP-04 | All 9 birch levels round-trip through OTP's 8 levels with documented mapping | Trace and Debug both map to `debug`; round-trip collapses Trace to Debug. Already tested but needs property test coverage and documentation in module docs. See "Level Mapping Analysis" section. |
| OTP-05 | :logger primary level interaction documented and handled | Default primary level is `notice`. Debug/Info messages are silently dropped. Either set primary level to `all` on formatter install, or document with clear guidance. See "Pattern: Primary Level Configuration" section. |
</phase_requirements>

## Standard Stack

### Core (Already in Project)
| Library | Version | Purpose | Why Standard |
|---------|---------|---------|--------------|
| gleam_otp | >= 1.0.0 | OTP integration, actor system | Runtime dependency, already in gleam.toml |
| gleam_erlang | >= 1.0.0 | Erlang interop types | Runtime dependency, already in gleam.toml |
| gleeunit | >= 1.0.0 | Test runner | Dev dependency, already in use |
| qcheck | >= 1.0.0 | Property-based testing | Dev dependency, already in use for level roundtrip tests |

### OTP APIs Used (No Additional Dependencies)
| API | Purpose | Notes |
|-----|---------|-------|
| `logger:get_handler_config/1` | Health check verification | Already used in `is_formatter_configured/0` |
| `logger:update_handler_config/3` | Formatter installation | Already used in `install_formatter/2` |
| `logger:set_primary_config/2` | Set primary log level | New usage for OTP-05 |
| `persistent_term:get/2`, `put/2` | Fast initialization cache | Already used, semantics unchanged |

### Alternatives Considered
| Instead of | Could Use | Tradeoff |
|------------|-----------|----------|
| Erlang record `-include` | Gleam accessor functions called from FFI | Record include is more idiomatic Erlang; accessor functions require exporting from Gleam module. Record include is recommended. |
| `logger:set_primary_config(level, all)` on install | Documentation-only approach | Setting level automatically is more user-friendly but changes global state unexpectedly. Document + optional helper recommended. |

## Architecture Patterns

### Pattern: Safe FFI Record Access (OTP-03)

**What:** Replace `erlang:element(N, LogRecord)` with Erlang record syntax using Gleam-generated header files.

**When to use:** Any time Erlang FFI code accesses fields of a Gleam custom type.

**How it works:** The Gleam compiler generates an Erlang header file for each custom type. For `LogRecord` in `birch/record.gleam`, Gleam generates a header that can be included:

```erlang
%% In birch_erlang_logger_ffi.erl
-include("birch/record_LogRecord.hrl").

%% Then use record syntax instead of erlang:element/2:
emit_to_logger(GleamLevel, LogRecord) ->
    Level = gleam_level_to_atom(GleamLevel),
    Message = LogRecord#log_record.message,
    logger:log(Level, "~ts", [Message], #{
        birch_log_record => LogRecord
    }),
    nil.
```

**Verification:** The exact header file name and record name depend on Gleam's compilation output. Check `build/dev/erlang/birch/_gleam_artefacts/` for the generated header files after `gleam build`.

**Why this is better:** If fields are reordered in `record.gleam`, the Gleam compiler regenerates the header, and the Erlang compiler catches mismatches at compile time rather than producing silent runtime corruption.

**Alternative approach:** Export accessor functions from Gleam and call them from Erlang FFI:

```gleam
// In record.gleam -- these already exist as field accessors on LogRecord
// record.message, record.level, etc. work because LogRecord has a single variant
```

Since `LogRecord` has a single constructor variant, Gleam automatically generates accessor functions. The FFI can call these:

```erlang
%% Alternative: call Gleam accessor functions
Message = birch@record:message(LogRecord),
```

Note: Gleam module names use `@` as separator in Erlang (e.g., `birch/record` becomes `birch@record`). This approach has a small function call overhead but is completely safe against field reordering.

**Confidence:** HIGH -- Gleam's Erlang record header generation is documented in the [Gleam for Erlang users cheatsheet](https://gleam.run/cheatsheets/gleam-for-erlang-users/). The accessor function approach uses standard Gleam record accessors.

### Pattern: Formatter Crash Recovery (OTP-01)

**What:** Wrap the entire `format/2` body in a try/catch that returns a fallback-formatted string.

**Current behavior in OTP 27:** When `format/2` crashes, `logger_h_common:try_format/3` catches the exception and falls back to the OTP default formatter (`logger_formatter`). The handler is NOT removed. But the output switches to OTP's default format, losing birch's formatting.

**What birch should do:** Catch exceptions inside birch's own `format/2` and return a sensible fallback, keeping birch-style formatting:

```erlang
format(#{level := Level, msg := Msg, meta := Meta} = Event, #{format_fn := FormatFn} = Config) ->
    try
        LogRecord = case maps:get(birch_log_record, Meta, undefined) of
            undefined -> build_log_record_from_otp(Level, Msg, Meta);
            Record -> Record
        end,
        Formatted = FormatFn(LogRecord),
        [Formatted, $\n]
    catch
        Class:Reason:Stacktrace ->
            %% Log the crash internally for debugging
            ?LOG_INTERNAL(debug, Event,
                [{formatter_crashed, ?MODULE},
                 {reason, {Class, Reason, Stacktrace}}]),
            %% Return a fallback-formatted string that preserves visibility
            Timestamp = format_timestamp(maps:get(time, Meta, undefined)),
            FallbackMsg = format_msg(Msg, Meta),
            [Timestamp, <<" | ">>,
             level_to_string(Level), <<" | FORMATTER_ERROR | ">>,
             FallbackMsg, <<" | crash: ">>,
             iolist_to_binary(io_lib:format("~p:~p", [Class, Reason])),
             $\n]
    end;
```

**Key decisions:**
1. The fallback format includes `FORMATTER_ERROR` marker so crashes are visible in output
2. The crash reason is included so debugging is possible without enabling :logger internal debug
3. The fallback uses the same basic format functions (timestamp, level, message) which are independently tested and simpler
4. The `?LOG_INTERNAL` macro may not be available -- use `logger:internal_log(debug, ...)` directly or omit internal logging

**Confidence:** HIGH -- OTP 27 `try_format/3` behavior verified in [logger_h_common.erl source](https://github.com/erlang/otp/blob/OTP-27.2.1/lib/kernel/src/logger_h_common.erl).

### Pattern: Health Check Implementation (OTP-02)

**What:** Add `is_healthy/0` that verifies the actual :logger handler state, not just the persistent_term cache.

**Implementation:**

```gleam
// In erlang_logger.gleam
/// Check if birch's :logger integration is healthy.
///
/// Unlike `is_initialized()` which checks a fast cache, this function
/// verifies the actual :logger handler state. Use for health checks
/// and monitoring, not in hot paths.
///
/// Returns True only if:
/// 1. The default :logger handler exists
/// 2. The birch formatter is installed on it
pub fn is_healthy() -> Bool {
  do_is_healthy()
}

@external(erlang, "birch_erlang_logger_ffi", "is_healthy")
@external(javascript, "../birch_erlang_logger_ffi.mjs", "is_healthy")
fn do_is_healthy() -> Bool
```

```erlang
%% In birch_erlang_logger_ffi.erl
-spec is_healthy() -> boolean().
is_healthy() ->
    case logger:get_handler_config(default) of
        {ok, #{formatter := {birch_erlang_logger_ffi, #{format_fn := _}}}} ->
            true;
        _ ->
            false
    end.
```

This is essentially the existing `is_formatter_configured/0` exposed as a public Gleam function. The key difference from `is_initialized/0` (which checks `persistent_term`) is that `is_healthy/0` makes a real :logger API call and reflects actual state.

**Performance note:** `logger:get_handler_config/1` involves a `gen_server:call` to `logger_server`. It should NOT be called on every log emission -- only for periodic health checks.

**Confidence:** HIGH -- uses existing `is_formatter_configured/0` pattern.

### Pattern: Primary Level Configuration (OTP-05)

**What:** Ensure :logger's primary level does not silently drop birch messages.

**The problem:** OTP :logger default primary level is `notice`. Messages at `debug` and `info` level are dropped before reaching the handler/formatter. Users who set birch to `Debug` level see no debug output through :logger.

**Recommended approach -- set primary level on formatter install:**

```erlang
%% In install_formatter/2, after successful handler config update:
install_formatter(HandlerId, FormatFn) ->
    Result = update_handler_formatter(HandlerId, {?MODULE, #{format_fn => FormatFn}}),
    case Result of
        {ok, nil} ->
            persistent_term:put(birch_logger_initialized, true),
            %% Set primary level to all so birch controls filtering
            logger:set_primary_config(level, all);
        _ -> ok
    end,
    Result.
```

**Tradeoff:** Setting primary level to `all` means :logger passes everything through to handlers. This is correct because birch does its own level filtering before calling `logger:log/4`. However, non-birch :logger handlers would also receive all messages. If the user has other :logger handlers with their own level configs, those handler-level filters still apply.

**Alternative -- documentation only:** Document in `erlang_logger.gleam` module docs and in a helper function:

```gleam
/// Configure the OTP :logger primary level to allow all log events through.
///
/// By default, OTP :logger's primary level is `notice`, which silently
/// drops `debug` and `info` messages before they reach any handler.
/// Call this function at startup if you want birch to control level filtering.
pub fn allow_all_levels() -> Nil {
  do_set_primary_level_all()
}
```

**Recommendation:** Set primary level to `all` automatically when installing the formatter (the first approach). This is what Elixir's Logger does -- it sets `:logger` primary level to match its own configuration. Users who explicitly configure :logger can override.

**Confidence:** HIGH -- OTP :logger primary level behavior is fundamental and [well-documented](https://www.erlang.org/doc/apps/kernel/logger_chapter.html).

## Level Mapping Analysis (OTP-04)

### Current Mapping

Birch has 9 levels; OTP has 8. The mapping in both Gleam and Erlang FFI:

| Birch Level | Int | Erlang Level | Round-trip Result |
|-------------|-----|-------------|-------------------|
| Trace | 0 | debug | Debug (lossy) |
| Debug | 1 | debug | Debug |
| Info | 2 | info | Info |
| Notice | 3 | notice | Notice |
| Warn | 4 | warning | Warn |
| Err | 5 | error | Err |
| Critical | 6 | critical | Critical |
| Alert | 7 | alert | Alert |
| Fatal | 8 | emergency | Fatal |

**Lossy mapping:** `Trace -> debug -> Debug`. Trace is not recoverable after an OTP round-trip. This is documented in `erlang_logger.gleam` and tested in `forward_to_beam_level_roundtrip_test`.

### Consistency Check

The Gleam mapping (`gleam_level_to_erlang`/`erlang_level_to_gleam`) and the Erlang FFI mapping (`gleam_level_to_atom`/`erlang_level_to_gleam`) are consistent:

- **Gleam side:** `erlang_logger.gleam` lines 145-175 -- complete, covers all 9 Gleam levels and all 8 Erlang levels
- **Erlang side:** `birch_erlang_logger_ffi.erl` lines 38-58 -- uses atom matching, includes catch-all `erlang_level_to_gleam(_) -> info` for unknown levels

**Potential issue:** The Erlang FFI has a catch-all clause `erlang_level_to_gleam(_) -> info` for unknown atoms. The Gleam side does not have this -- it uses exhaustive pattern matching on `ErlangLevel` variants. If a new OTP log level were added, the Erlang FFI would silently map it to `info` while the Gleam side would cause a compile error (which is the correct behavior for Gleam).

### What OTP-04 Requires

1. All 9 levels are tested in both directions (already done in `erlang_logger_test.gleam`)
2. Round-trip consistency is tested (already done in `forward_to_beam_level_roundtrip_test`)
3. The Trace->Debug lossy mapping is documented (already documented in Gleam module docs)
4. Property tests validate ordering is preserved through round-trips (qcheck tests exist in `property_test.gleam` for level properties but may not cover the OTP mapping)

**Remaining work:** Add qcheck property test for OTP round-trip: for all levels, `to_erlang |> to_gleam` preserves ordering (`compare(roundtrip(a), roundtrip(b)) == compare(a, b)` except for Trace). Ensure module-level documentation covers the mapping table explicitly.

**Confidence:** HIGH -- all code paths verified by reading source.

## Don't Hand-Roll

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Record field access in Erlang FFI | Positional tuple indexing (`erlang:element/N`) | Gleam-generated record headers or accessor functions | Field reordering causes silent corruption; compiler catches header mismatches |
| :logger handler health check | Ad-hoc persistent_term polling | `logger:get_handler_config/1` API | persistent_term can be stale; the API reflects actual state |
| Formatter error handling | Letting exceptions propagate to OTP | try/catch with fallback format in `format/2` | OTP's fallback loses birch formatting; internal catch preserves it |
| Level filtering interaction | Assuming birch controls all filtering | `logger:set_primary_config(level, all)` | OTP primary level silently drops messages before birch sees them |

## Common Pitfalls

### Pitfall 1: report_cb Return Type Mismatch

**What goes wrong:** The current `format_msg/2` has the 1-arg and 2-arg `report_cb` return types swapped. Per OTP docs:
- **1-arg form:** Returns `{io:format(), [term()]}` -- a format string + args tuple
- **2-arg form:** Returns `unicode:chardata()` -- already formatted string

But birch's code does:
- 1-arg: `unicode:characters_to_binary(Fun(Report))` -- treats return as chardata (wrong)
- 2-arg: `{Format, Args} = Fun(Report, Config)` then `io_lib:format(Format, Args)` -- treats return as tuple (wrong)

**Why it hasn't crashed yet:** Both branches have try/catch. When the wrong interpretation fails, it falls through to `io_lib:format("~p", [Report])`. But for simple reports where `{Format, Args}` happens to be valid chardata (rare), the 1-arg branch produces garbage output.

**How to fix:**
```erlang
Fun when is_function(Fun, 1) ->
    try
        {Format, Args} = Fun(Report),
        unicode:characters_to_binary(io_lib:format(Format, Args))
    catch _:_ ->
        unicode:characters_to_binary(io_lib:format("~p", [Report]))
    end;
Fun when is_function(Fun, 2) ->
    try
        unicode:characters_to_binary(
            Fun(Report, #{single_line => true, depth => 30, chars_limit => unlimited}))
    catch _:_ ->
        unicode:characters_to_binary(io_lib:format("~p", [Report]))
    end;
```

**Confidence:** HIGH -- verified against [OTP logger API docs](https://www.erlang.org/doc/apps/kernel/logger.html) `report_cb()` type spec.

### Pitfall 2: Formatter Crash Does NOT Remove Handler (Updated Understanding)

**What actually happens:** In OTP 27, `logger_h_common:try_format/3` catches formatter exceptions and falls back to OTP's default formatter (`logger_formatter`). The handler remains installed. The formatted output becomes `"FORMATTER CRASH: {original_message}"` using OTP's default format.

**Previous understanding (from PITFALLS.md):** Stated that formatter crash removes the handler entirely. This was based on the general :logger documentation ("If a filter or handler still crashes, Logger will remove the filter or handler"). However, the actual OTP source code shows that formatter crashes are caught *inside* the handler's processing pipeline, before they would propagate to the level where handler removal occurs.

**Why birch should still add try/catch:** Even though the handler survives:
1. Output switches to OTP's default format, losing birch's formatting
2. The crash message is ugly and not birch-style
3. Every subsequent log through :logger triggers a new formatter crash and fallback
4. Birch's internal try/catch produces a cleaner fallback that preserves birch formatting

**Confidence:** HIGH -- verified by reading [OTP 27 logger_h_common.erl source](https://github.com/erlang/otp/blob/OTP-27.2.1/lib/kernel/src/logger_h_common.erl).

### Pitfall 3: persistent_term Cache Stale After Handler Removal

**What goes wrong:** If an external tool or OTP process removes the default handler (or replaces its formatter), the `birch_logger_initialized` persistent_term still reads `true`. `is_initialized()` returns true, but birch's formatter is gone. `ensure_formatter_configured()` no-ops because it trusts the cache.

**How to avoid:** `is_healthy/0` must call `logger:get_handler_config(default)` directly, ignoring the persistent_term cache. Document that `is_initialized()` is a fast cache for hot paths, while `is_healthy()` is the authoritative check for monitoring.

**Confidence:** HIGH -- directly visible in code.

### Pitfall 4: Gleam Record Header Path May Vary

**What goes wrong:** The Gleam compiler's generated record header file path depends on the package name and module path. The exact filename pattern is `{module_name}_{TypeName}.hrl`. If the Gleam compiler changes the naming convention, the `-include` directive breaks at compile time (which is detectable, unlike the current silent runtime failure).

**How to avoid:** After implementing the `-include` approach, verify the header exists after `gleam build`:
```bash
find build/dev/erlang/birch/_gleam_artefacts/ -name "*LogRecord*"
```

**Confidence:** MEDIUM -- header generation is documented but exact naming needs runtime verification.

### Pitfall 5: Setting Primary Level to `all` Affects All Handlers

**What goes wrong:** If birch sets `logger:set_primary_config(level, all)`, all :logger handlers (not just the default one with birch's formatter) receive all log events. A user who has added another handler with no handler-level filter gets flooded.

**How to avoid:** Set the handler-level filter on `default` to `all` instead of changing the primary level:
```erlang
logger:set_handler_config(default, level, all)
```
This only affects the `default` handler. Other handlers keep their own level filtering. However, if the user has installed birch's formatter on a non-default handler, that handler also needs its level adjusted.

**Recommendation:** Use handler-level config (`set_handler_config`) rather than primary config (`set_primary_config`) to minimize side effects.

**Confidence:** HIGH -- OTP :logger level filtering hierarchy is [well-documented](https://www.erlang.org/doc/apps/kernel/logger_chapter.html).

## Code Examples

### Current Tuple Indexing (Fragile -- OTP-03 Target)

```erlang
%% Current code in birch_erlang_logger_ffi.erl line 74
%% LogRecord = {log_record, Timestamp, Level, LoggerName, Message, Metadata, CallerId}
Message = erlang:element(5, LogRecord),
```

### Safe Record Access via Include (Recommended Fix)

```erlang
%% After fix -- compile-time safe
-include("birch@record_LogRecord.hrl").

emit_to_logger(GleamLevel, LogRecord) ->
    Level = gleam_level_to_atom(GleamLevel),
    Message = LogRecord#log_record.message,
    logger:log(Level, "~ts", [Message], #{
        birch_log_record => LogRecord
    }),
    nil.
```

Note: The exact include path must be verified after `gleam build`. The `@` separator in module names may affect the header filename.

### Safe Record Access via Accessor Functions (Alternative Fix)

```erlang
emit_to_logger(GleamLevel, LogRecord) ->
    Level = gleam_level_to_atom(GleamLevel),
    Message = birch@record:message(LogRecord),
    logger:log(Level, "~ts", [Message], #{
        birch_log_record => LogRecord
    }),
    nil.
```

### Health Check Implementation (OTP-02)

```erlang
%% New function in birch_erlang_logger_ffi.erl
-spec is_healthy() -> boolean().
is_healthy() ->
    case logger:get_handler_config(default) of
        {ok, #{formatter := {birch_erlang_logger_ffi, #{format_fn := _}}}} ->
            true;
        _ ->
            %% Handler missing, different formatter, or other issue
            false
    end.
```

### Primary Level Configuration (OTP-05)

```erlang
%% Set handler-level filter to allow all events through
%% (preferred over changing primary level)
logger:set_handler_config(default, level, all).

%% Or set primary level (affects all handlers)
logger:set_primary_config(level, all).
```

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| Handler registration (`logger:add_handler`) | Formatter-only registration | OTP 21+ | Birch correctly uses formatter approach already |
| `erlang:element/N` for FFI record access | Gleam-generated record headers or accessor functions | Always available | OTP-03 brings birch up to best practice |
| Trust persistent_term for health | Verify actual :logger state | N/A | OTP-02 adds real health checking |
| Assume birch controls all filtering | Account for :logger primary level | N/A | OTP-05 closes a silent message-dropping gap |

**Deprecated/outdated in birch:**
- `forward_to_beam()`, `forward_to_logger()`, and related deprecated functions in `erlang_logger.gleam` -- these are from the old handler-based approach and should be removed in a future phase (out of scope for Phase 1)

## Open Questions

1. **Gleam record header exact filename**
   - What we know: Gleam generates `.hrl` files for custom types with single variants
   - What's unclear: The exact filename convention (is it `record_LogRecord.hrl` or `birch@record_LogRecord.hrl` or something else?)
   - Recommendation: Run `gleam build` and inspect `build/dev/erlang/birch/_gleam_artefacts/` to find the exact filename. If the header approach is too fragile, use the accessor function approach instead.

2. **`?LOG_INTERNAL` macro availability**
   - What we know: OTP's `logger_h_common.erl` uses `?LOG_INTERNAL` to log formatter crashes internally
   - What's unclear: Whether this macro is available to external modules or is private to OTP
   - Recommendation: Use `logger:internal_log/3` if available, or simply omit internal logging and rely on the visible `FORMATTER_ERROR` marker in the fallback output.

3. **Handler-level vs primary-level configuration for OTP-05**
   - What we know: Both approaches work; handler-level is more targeted
   - What's unclear: Whether users commonly add other :logger handlers alongside birch
   - Recommendation: Use handler-level (`set_handler_config(default, level, all)`) and document the interaction. Provide `allow_all_levels/0` as a convenience function.

## Validation Architecture

### Test Framework
| Property | Value |
|----------|-------|
| Framework | gleeunit >= 1.0.0 + qcheck >= 1.0.0 |
| Config file | gleam.toml (dev-dependencies section) |
| Quick run command | `just test-erlang` |
| Full suite command | `just test` |

### Phase Requirements to Test Map
| Req ID | Behavior | Test Type | Automated Command | File Exists? |
|--------|----------|-----------|-------------------|-------------|
| OTP-01 | format/2 returns fallback on crash | unit (Erlang FFI) | `just test-erlang` | Partial -- erlang_logger_test.gleam exists but no crash test |
| OTP-01 | report_cb 1-arg and 2-arg forms correct | unit (Erlang FFI) | `just test-erlang` | Partial -- report_cb test exists but doesn't verify correct format/args handling |
| OTP-02 | is_healthy returns false when handler removed | unit (Erlang FFI) | `just test-erlang` | No -- need new test |
| OTP-02 | is_healthy returns true when formatter installed | unit (Erlang FFI) | `just test-erlang` | No -- need new test |
| OTP-03 | FFI extracts correct fields after accessor change | integration | `just test-erlang` | Partial -- existing round-trip tests validate indirectly |
| OTP-04 | All 9 levels round-trip correctly | unit + property | `just test-erlang` | Yes -- erlang_logger_test.gleam has roundtrip test |
| OTP-04 | Ordering preserved through round-trip | property | `just test-erlang` | No -- need qcheck property test |
| OTP-05 | Debug/Info messages not dropped by :logger | integration | `just test-erlang` | No -- need new test |

### Sampling Rate
- **Per task commit:** `just test-erlang`
- **Per wave merge:** `just test`
- **Phase gate:** Full suite green before `/gsd:verify-work`

### Wave 0 Gaps
- [ ] Test for formatter crash recovery (OTP-01): Install a deliberately crashing format function, log a message, verify output contains fallback format
- [ ] Test for report_cb correctness (OTP-01): Send OTP report with 1-arg `report_cb` returning `{Format, Args}`, verify formatted correctly
- [ ] Test for is_healthy (OTP-02): Install formatter, verify healthy; remove formatter, verify not healthy; verify persistent_term cache mismatch detected
- [ ] Property test for level round-trip ordering (OTP-04): qcheck that `compare(roundtrip(a), roundtrip(b))` preserves ordering for non-Trace levels
- [ ] Test for debug-level messages through :logger (OTP-05): Set handler level to `all`, log at debug, verify message reaches formatter

## Sources

### Primary (HIGH confidence)
- [OTP 27 logger_h_common.erl source](https://github.com/erlang/otp/blob/OTP-27.2.1/lib/kernel/src/logger_h_common.erl) -- `try_format/3` crash recovery behavior
- [OTP 27 logger_std_h.erl source](https://github.com/erlang/otp/blob/OTP-27.2.1/lib/kernel/src/logger_std_h.erl) -- Handler write path, no handler removal on formatter crash
- [OTP Logger Chapter (kernel v10.5)](https://www.erlang.org/doc/apps/kernel/logger_chapter.html) -- Primary level default (`notice`), handler/filter crash documentation
- [OTP Logger API (kernel v10.5)](https://www.erlang.org/doc/apps/kernel/logger.html) -- `report_cb()` type spec (1-arg returns `{Format, Args}`, 2-arg returns `chardata`)
- [Gleam for Erlang users cheatsheet](https://gleam.run/cheatsheets/gleam-for-erlang-users/) -- Custom type tuple representation, record header generation
- [Gleam Record Accessors](https://tour.gleam.run/data-types/record-accessors/) -- Single-variant types get automatic accessors

### Secondary (MEDIUM confidence)
- [Gleam Externals Guide](https://gleam.run/documentation/externals/) -- FFI patterns and type annotation requirements

### Tertiary (LOW confidence)
- Gleam record header exact filename convention -- needs runtime verification after `gleam build`

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH - no new dependencies, all existing OTP APIs
- Architecture: HIGH - patterns verified against OTP 27 source code
- Pitfalls: HIGH - report_cb mismatch verified against OTP type specs; formatter crash behavior verified against source
- Level mapping: HIGH - all code paths read and verified

**Research date:** 2026-02-27
**Valid until:** 2026-06-27 (stable domain -- OTP :logger API is mature and rarely changes)
