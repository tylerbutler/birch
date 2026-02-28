# Architecture Patterns: OTP :logger Integration

**Domain:** Gleam logging library with OTP :logger integration
**Researched:** 2026-02-27

## Recommended Architecture

Birch's current architecture follows the correct OTP pattern: **formatter-only integration, not handler registration**. This is the right design. The library installs a custom formatter on the existing `:logger` default handler (`:logger_std_h`), letting OTP control output routing and overload protection, while birch controls formatting.

### How OTP :logger Actually Works (Reference Architecture)

The OTP :logger pipeline operates in three stages:

```
Application code
  -> logger:log(Level, Msg, Metadata)
    -> Primary filters (global)
      -> Per-handler level check
        -> Per-handler filters
          -> Handler module (logger_std_h, logger_disk_log_h)
            -> Formatter module format/2 callback
              -> Output (console, file, etc.)
```

Key architectural constraint: **Handlers manage output destinations and overload protection. Formatters convert log events to strings. These are separate concerns.**

The handler callback (`HModule:log/2`) receives the formatter configuration as part of handler config and calls `FModule:format(LogEvent, FConfig)` to get the formatted string, then writes it to the output device. The `logger_std_h` handler provides:

- **Overload protection via `logger_olp`**: Three queue-length thresholds (sync_mode at 10, drop_mode at 200, flush at 1000 messages)
- **Burst limiting**: 500 events per 1000ms window by default
- **Mode switching**: Async -> sync -> drop -> flush as load increases
- **Optional handler termination**: If queue or memory exceeds configurable limits

### Component Boundaries

| Component | Responsibility | Communicates With |
|-----------|---------------|-------------------|
| `birch.gleam` (Public API) | Module-level convenience, global config, scoped context | Logger, Config, Scope |
| `birch/logger.gleam` | Named logger instances, log emission, metadata merging | Handler, Record, ErlangLogger, Platform |
| `birch/erlang_logger.gleam` | :logger formatter registration, level mapping, emit API | `birch_erlang_logger_ffi.erl` |
| `birch_erlang_logger_ffi.erl` | FFI: format/2 callback, emit_to_logger, install/remove formatter | Erlang :logger system |
| `birch/handler.gleam` + `handler/*.gleam` | Birch-native handlers (console, file, JSON, async) | Record, Formatter, Platform |
| `birch/formatter.gleam` | LogRecord -> String conversion | Record, Level |
| OTP `:logger` (external) | Log routing, filtering, overload protection | Birch formatter via format/2 |
| OTP `:logger_std_h` (external) | Default handler: output to console/file | Birch formatter |

### Data Flow

**Birch log on BEAM (current implementation):**

```
1. birch log.info("message", metadata)
2. Logger checks should_log (level + sampling)
3. If enabled:
   a. Get timestamp, merge metadata (call > scope > logger context)
   b. Create LogRecord tuple
   c. Call emit_to_beam(record) -> erlang_logger.emit(record)
      -> birch_erlang_logger_ffi:emit_to_logger(Level, LogRecord)
        -> logger:log(Level, "~ts", [Message], #{birch_log_record => LogRecord})
          -> OTP :logger pipeline (filters, level checks)
            -> logger_std_h (default handler)
              -> birch_erlang_logger_ffi:format/2
                -> Detects birch_log_record in metadata
                -> Calls FormatFn(LogRecord) directly
                -> Returns [FormattedString, $\n]
              -> Output via io:put_chars
   d. Also dispatch to any birch handlers (handler.handle_all)
```

**OTP/library log on BEAM (non-birch event):**

```
1. Erlang/OTP code calls logger:warning("something")
2. OTP :logger pipeline routes to logger_std_h
3. logger_std_h calls birch_erlang_logger_ffi:format/2
4. No birch_log_record in metadata -> builds LogRecord from OTP event:
   a. Extracts timestamp from meta.time
   b. Converts Erlang level atom to Gleam Level
   c. Extracts logger name from meta.mfa or meta.domain
   d. Formats message (handles report, string, format+args)
   e. Converts metadata map to Gleam Metadata list
   f. Constructs LogRecord tuple
5. Calls FormatFn(LogRecord) -> formatted string
6. Returns [FormattedString, $\n]
```

## Patterns to Follow

### Pattern 1: Formatter-Only Integration (Correct)

**What:** Register birch as a formatter on existing :logger handlers, not as a custom handler.

**Why:** OTP's built-in handlers (`:logger_std_h`, `:logger_disk_log_h`) provide battle-tested overload protection, burst limiting, and mode switching. Writing your own handler means reimplementing all of this. The formatter approach delegates output control to OTP while controlling presentation.

**Birch does this correctly.** The `install_formatter/2` function calls `logger:update_handler_config(Id, formatter, {birch_erlang_logger_ffi, #{format_fn => FormatFn}})`.

### Pattern 2: Pass-Through LogRecord via Metadata (Correct)

**What:** Pass the entire birch LogRecord through :logger metadata to avoid decompose/recompose overhead.

**Why:** The formatter callback receives the full log event including metadata. By stashing the LogRecord in metadata under `birch_log_record`, the formatter can skip building a new LogRecord from scratch. This preserves birch's typed metadata, custom timestamp, and logger name without loss.

**Birch does this correctly** in `emit_to_logger/2`:
```erlang
logger:log(Level, "~ts", [Message], #{birch_log_record => LogRecord})
```

**Confidence:** HIGH -- This is the documented way to pass custom data through :logger. The metadata map accepts arbitrary `atom() => term()` entries.

### Pattern 3: Handle All OTP Message Types (Mostly Correct)

**What:** The format/2 callback must handle all three :logger message types:
1. `{string, CharData}` -- plain string
2. `{Format, Args}` -- io:format string with arguments
3. `{report, Report}` -- structured report (map or key-value list)

**How Elixir does it:** Elixir's Logger uses a "translator" system that intercepts OTP reports as primary filters and converts them to Elixir-formatted messages before they reach handlers. This is more complex but gives Elixir-style formatting for OTP supervisor reports, crash reports, etc.

**Birch's approach** (handle in the formatter) is simpler and correct for a Gleam library. The `format_msg/2` function in `birch_erlang_logger_ffi.erl` handles all three types plus `report_cb` callbacks.

**Confidence:** HIGH -- verified against OTP `log_event()` type spec.

### Pattern 4: report_cb Two-Argument Form (Correct)

**What:** OTP structured reports can have a `report_cb` with one or two arguments:
- **1-arg form:** `fun(Report) -> {Format, Args}` -- formatter controls depth/chars_limit
- **2-arg form:** `fun(Report, #{depth, chars_limit, single_line}) -> unicode:chardata()` -- callback controls depth/chars_limit

**Why this matters:** The two-argument form returns a final string. The formatter cannot apply depth or chars_limit constraints to the returned string -- the callback is responsible for obeying those parameters.

**Birch handles both forms** in `format_msg/2`, passing `#{single_line => true, depth => 30}` to the two-argument form. This is correct.

**Confidence:** HIGH -- matches OTP documentation for `report_cb_config()` type.

### Pattern 5: format/2 Return Type Must Be unicode:chardata() (Correct)

**What:** The OTP formatter callback `format(LogEvent, FConfig)` must return `unicode:chardata()`, which the handler writes using `io:put_chars/1,2`.

**Birch does this correctly** by returning `[Formatted, $\n]` -- a valid iolist (which is a subtype of `unicode:chardata()`). The newline is appended by the formatter, which matches OTP's `logger_formatter` convention (the default formatter includes `"\n"` as the last template element).

**Confidence:** HIGH -- verified against OTP source code for `logger_formatter.erl`.

## How Elixir's Logger Does It (Reference Implementation)

Elixir's Logger serves as the reference implementation for :logger integration from a higher-level language. Key patterns:

1. **Full :logger integration since Elixir v1.10:** Elixir Logger shares the same log level, metadata system, and handler infrastructure with Erlang :logger. Custom Elixir backends are deprecated in favor of :logger handlers.

2. **Translator system:** Elixir installs primary filters that intercept OTP reports and translate them to Elixir-formatted messages. This is more sophisticated than birch's approach but also more complex.

3. **Default handler:** Elixir configures the `:default` handler (`:logger_std_h`) with its own formatter at boot. It does NOT install a custom handler -- it uses the existing OTP handler infrastructure.

4. **Metadata handling:** Elixir uses process metadata (`logger:set_process_metadata/1`) for per-process context. This integrates with OTP's metadata merging (event metadata > process metadata > primary metadata).

5. **Handler deprecation:** Elixir explicitly deprecated its own backend system in favor of OTP handlers, validating that the OTP handler infrastructure is the correct approach.

**Key insight for birch:** Elixir does NOT reinvent handlers. It uses OTP's `logger_std_h` for output and overload protection, and controls only the formatting layer. Birch follows this same pattern, which is correct.

## Specific Things to Validate in Birch's Integration

### VALIDATE-1: Dual Output Path (Potential Issue)

**Concern:** `emit_record` in `logger.gleam` does both:
1. `emit_to_beam(final_record)` -- sends to :logger (which outputs via its handler)
2. `handler.handle_all(logger.handlers, final_record)` -- dispatches to birch handlers

On BEAM with default config, `logger.handlers` is `[]` (empty), so step 2 is a no-op. But if a user adds both a birch handler AND uses the default :logger integration, they get double output.

**Assessment:** The current design handles this correctly for the default case. The documentation warns about this in `erlang_logger.gleam` (lines 63-78). However, a user who does `log.configure([log.config_handlers([console.handler()])])` will get output BOTH from the birch console handler AND from :logger's default handler (if the birch formatter is still installed).

**Recommendation:** When explicit birch handlers are configured, birch should either:
- (a) Skip `emit_to_beam()` entirely, OR
- (b) Still emit to :logger (for other :logger handlers) but NOT auto-install the birch formatter on the default handler

Option (b) is what the current code does (see `default_handlers()` on Erlang: the `ensure_formatter_configured()` call only happens in `default_handlers()`, not when explicit handlers are set). This is the correct approach. Validate that this works as documented.

**Confidence:** MEDIUM -- logic analysis of code, not runtime-verified.

### VALIDATE-2: persistent_term Usage for Initialization Flag

**Concern:** `ensure_initialized()` uses `persistent_term:get(birch_logger_initialized, false)` for fast repeated checks. `persistent_term` writes trigger global GC. The initialization flag is written once (on first log call), so this is fine.

**BUT:** `install_formatter/2` sets `birch_logger_initialized` to `true`, and `remove_formatter/1` sets it to `false`. If a test teardown calls `remove_formatter()` and then logs again, this triggers two `persistent_term` writes (remove: set to false, then auto-install: set to true), each causing a global GC.

**Assessment:** Acceptable for production (initialization happens once). May cause minor performance issues in test suites with frequent setup/teardown. Not a correctness issue.

**Confidence:** HIGH -- directly verified in source code.

### VALIDATE-3: LogRecord Tuple Structure Assumption

**Concern:** In `emit_to_logger/2`, the message is extracted from the LogRecord by position:
```erlang
Message = erlang:element(5, LogRecord),
```

This assumes the Gleam compiler's representation of the `LogRecord` custom type as a tuple where the 5th element is the message. If Gleam changes its compilation strategy for custom types, this will silently break.

**Assessment:** This is a known fragility of Gleam FFI. Gleam compiles custom types to tuples with the constructor name as the first element, followed by fields in order. For `LogRecord = {log_record, Timestamp, Level, LoggerName, Message, Metadata, CallerId}`, element 5 is indeed `Message`. However, if the LogRecord type definition changes (field reordering, adding fields before message), this breaks.

**Recommendation:** Consider defining a helper function in Gleam that extracts the message, or use Gleam's own record access pattern, to avoid positional tuple access in FFI.

**Confidence:** HIGH -- verified against `record.gleam` source: fields are timestamp(2), level(3), logger_name(4), message(5), metadata(6), caller_id(7).

### VALIDATE-4: :logger Level Filtering Before Formatter

**Concern:** OTP :logger has its own level filtering. The default primary level is `notice`. This means debug and info level logs emitted by birch will be SILENTLY DROPPED by :logger before reaching the formatter.

**Current behavior:** Birch calls `logger:log(Level, ...)`, but if the :logger primary level or handler level is set higher than the birch level, the message never reaches the formatter.

**Assessment:** This is a significant integration concern. Users who set birch to `Debug` level but don't also configure `:logger`'s primary level to `debug` will see no debug/info output through :logger (though birch handlers, if configured, would still see them).

**Recommendation:** When installing the birch formatter, also set `:logger`'s primary level to `all` (or at least `debug`) so birch controls filtering. Alternatively, document this interaction clearly.

**Confidence:** HIGH -- this is fundamental OTP :logger behavior, confirmed in official documentation.

### VALIDATE-5: check_config/1 Callback Not Implemented

**Concern:** The OTP formatter behavior has an optional `check_config/1` callback that validates formatter configuration when `logger:set_handler_config/3` or `logger:update_formatter_config/2,3` is called.

**Assessment:** birch does not implement this callback. This is acceptable (it's optional), but means that invalid formatter configuration will not be caught until `format/2` is called. Since birch's formatter config is just `#{format_fn => FormatFn}`, there's minimal risk of invalid config.

**Recommendation:** Consider implementing `check_config/1` to verify that `format_fn` is a valid function. Low priority.

**Confidence:** HIGH -- verified by grep: no `check_config` in birch source.

### VALIDATE-6: Metadata Key Filtering

**Concern:** `format_metadata/1` in the FFI filters out internal :logger keys (`time`, `mfa`, `file`, `line`, `gl`, `pid`, `domain`, `report_cb`) and birch-specific keys (`birch_log_record`, `birch_logger_name`, `birch_metadata`, `birch_caller_id`).

**Assessment:** This filtering is correct and necessary. Without it, internal OTP metadata would leak into birch's formatted output. The key list is comprehensive.

**Potential issue:** If OTP adds new internal metadata keys in future versions, they would appear in birch's output. This is low risk -- OTP rarely adds new metadata keys, and any additions would be documented.

**Confidence:** HIGH -- verified against OTP `metadata()` type definition.

### VALIDATE-7: Timestamp Handling for OTP Events

**Concern:** When building a LogRecord from an OTP event, birch uses the `:logger` `time` metadata field (microseconds since epoch) and formats it to ISO 8601. If `time` is `undefined`, it falls back to `calendar:universal_time()`.

**Assessment:** Correct. OTP always provides `time` in metadata (it's auto-inserted by `:logger`), so the `undefined` fallback is defensive but unlikely to trigger.

**Confidence:** HIGH -- verified against OTP documentation: `time` is auto-inserted.

### VALIDATE-8: Newline Handling in format/2

**Concern:** birch's `format/2` returns `[Formatted, $\n]`. The OTP `logger_std_h` handler writes this using `io:put_chars/1`. The newline must be part of the formatter output because the handler does not add one.

**Assessment:** Correct. OTP's default `logger_formatter` also includes `"\n"` as the last template element. Birch follows this convention.

**Confidence:** HIGH -- verified against OTP `logger_formatter.erl` source.

## Anti-Patterns to Avoid

### Anti-Pattern 1: Registering a Custom :logger Handler

**What:** Calling `logger:add_handler(birch, birch_handler_module, Config)` to register birch as a full :logger handler.

**Why bad:** You lose OTP's built-in overload protection (`logger_olp`), burst limiting, mode switching, and handler restart logic. You'd have to reimplement all of this in your handler module.

**Instead:** Use formatter-only integration. Birch already does this correctly.

### Anti-Pattern 2: Double Formatting

**What:** Formatting the log message BEFORE sending to :logger, then having the formatter format it again.

**Why bad:** Results in output like `"2024-01-01 | INFO | myapp | 2024-01-01 | INFO | myapp | Hello"`.

**Instead:** Pass the raw LogRecord through metadata and format once in the formatter callback. Birch's current approach (direct emission with `birch_log_record` metadata key) avoids this.

### Anti-Pattern 3: Ignoring :logger Level Filtering

**What:** Assuming birch's level check is the only one that matters.

**Why bad:** :logger has its own primary level (default: `notice`) and per-handler levels. If birch emits at `debug` but :logger's primary level is `notice`, the message is silently dropped before reaching the formatter.

**Instead:** Either set :logger's primary level to `all` when installing the birch formatter, or document the interaction.

### Anti-Pattern 4: Synchronous Blocking in format/2

**What:** Doing expensive work (network calls, file I/O, large computations) inside the format/2 callback.

**Why bad:** The format/2 callback runs in the context of the handler process. If it blocks, it backs up the handler's message queue, which can trigger overload protection (sync mode -> drop mode -> flush).

**Instead:** Keep format/2 fast. Do only string formatting. Birch's approach (calling `FormatFn(LogRecord)`) is fine as long as the format function is fast.

### Anti-Pattern 5: Mutating State in format/2

**What:** Using process dictionary, ETS writes, or other side effects in the formatter.

**Why bad:** The formatter may be called from different processes depending on handler configuration. Side effects can cause race conditions.

**Instead:** The formatter should be a pure function: LogRecord in, string out. Birch's formatter (`formatter.Formatter = fn(LogRecord) -> String`) enforces this by type.

## Scalability Considerations

| Concern | At 100 msg/s | At 10K msg/s | At 1M msg/s |
|---------|-------------|-------------|-------------|
| :logger overhead | Negligible | :logger handles via async mode | :logger drops messages via overload protection |
| LogRecord in metadata | Negligible -- small tuple copy | Moderate -- tuples are shared-nothing on BEAM | Consider: does BEAM copy the tuple on every message send? Yes, but shallow copies of tuples are fast |
| persistent_term reads | Sub-microsecond | Sub-microsecond | Sub-microsecond (no contention) |
| Format function execution | Negligible | Becomes bottleneck if slow | MUST be fast -- :logger will drop messages |
| Dual output (birch + :logger) | Negligible | 2x output volume if both active | Never do this at high volume |

## Build Order Implications

1. **Foundation:** `birch/level.gleam`, `birch/record.gleam` -- these define types used everywhere
2. **Formatter layer:** `birch/formatter.gleam` -- pure functions, no dependencies on :logger
3. **OTP integration:** `birch/erlang_logger.gleam` + `birch_erlang_logger_ffi.erl` -- depends on record, level, formatter
4. **Logger layer:** `birch/logger.gleam` -- depends on erlang_logger (for emit_to_beam)
5. **Public API:** `birch.gleam` -- depends on everything above

For the review milestone, the OTP integration layer (step 3) should be validated BEFORE reviewing the logger and public API layers, because correctness issues in the :logger integration affect all log output on BEAM.

## Sources

- [Erlang Logger Chapter (kernel v10.5)](https://www.erlang.org/doc/apps/kernel/logger_chapter.html) -- PRIMARY reference for :logger architecture, overload protection, handler/formatter distinction
- [Erlang Logger API (kernel v10.5)](https://www.erlang.org/doc/apps/kernel/logger.html) -- Type specs for log_event(), metadata(), formatter_config()
- [Erlang Logger Cookbook (kernel v10.5)](https://www.erlang.org/doc/apps/kernel/logger_cookbook.html) -- Practical examples for custom formatters
- [Erlang logger_formatter (kernel v10.5)](https://www.erlang.org/doc/apps/kernel/logger_formatter.html) -- format/2 callback spec, template system
- [Elixir Logger (v1.19.5)](https://hexdocs.pm/logger/Logger.html) -- Reference implementation of :logger integration from higher-level language
- [Elixir Logger source](https://github.com/elixir-lang/elixir/blob/main/lib/logger/lib/logger.ex) -- Handler installation, translator system, metadata handling
- [OTP logger_formatter.erl source](https://github.com/erlang/otp/blob/master/lib/kernel/src/logger_formatter.erl) -- Newline convention, msg type handling, check_config
- [jsonlog (tsloughter)](https://github.com/tsloughter/jsonlog) -- Community JSON formatter, shows correct formatter pattern
- [persistent_term blog (erlang.org)](https://www.erlang.org/blog/persistent_term/) -- Performance characteristics and GC implications
- [OTP logger overload issue #7417](https://github.com/erlang/otp/issues/7417) -- Known performance bug in logger_olp

---

*Architecture analysis: 2026-02-27*
