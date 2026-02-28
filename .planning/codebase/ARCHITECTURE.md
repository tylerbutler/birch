# Architecture

**Analysis Date:** 2026-02-27

## Pattern Overview

**Overall:** Layered architecture with separation between public API, core logging logic, handlers, and platform-specific FFI.

**Key Characteristics:**
- Immutable logger instances with fluent builder pattern for configuration
- Handler-based pluggable output with async support
- Cross-platform abstraction through FFI (Erlang and JavaScript)
- Direct integration with OTP logger on BEAM target
- Scoped context propagation using process dictionary (Erlang) or AsyncLocalStorage (Node.js)
- Lazy evaluation support to avoid expensive message formatting

## Layers

**Public API Layer:**
- Purpose: High-level convenience functions for application code
- Location: `src/birch.gleam`
- Contains: Module-level functions (`info()`, `error()`, etc.), global configuration, scoped logger/context
- Depends on: Logger, Config, Level, Handler, Scope
- Used by: Application code and libraries

**Logger Layer:**
- Purpose: Named logger instances with configuration and log emission
- Location: `src/birch/logger.gleam`
- Contains: Logger opaque type, per-logger functions (trace, debug, info, etc.), timestamp handling, metadata merging
- Depends on: Level, Handler, Record, Platform, Time
- Used by: Public API, application code

**Record Layer:**
- Purpose: Represents a single log event with all its metadata
- Location: `src/birch/record.gleam`
- Contains: LogRecord type with timestamp, level, logger name, message, metadata, caller ID
- Depends on: Level
- Used by: Logger, Handlers, Formatters, OTP integration

**Handler Layer:**
- Purpose: Routes log records to output destinations
- Location: `src/birch/handler.gleam` and `src/birch/handler/*.gleam`
- Contains: Handler interface, built-in handlers (console, file, JSON, async, null)
- Depends on: Record, Formatter, Platform
- Used by: Logger layer, application configuration

**Configuration Layer:**
- Purpose: Manages global and per-logger settings
- Location: `src/birch/config.gleam`
- Contains: GlobalConfig type, configuration options, sampling settings, persistent term storage (Erlang)
- Depends on: Level, Handler, Record
- Used by: Public API, default logger caching

**Formatting Layer:**
- Purpose: Transforms log records into human-readable strings
- Location: `src/birch/formatter.gleam` and `src/birch/level_formatter.gleam`
- Contains: Formatter type, human_readable/simple formatters, level formatting with colors
- Depends on: Record, Level, ANSI
- Used by: Handlers, console output

**Platform Abstraction Layer:**
- Purpose: Bridges Gleam to Erlang/JavaScript runtime differences
- Location: `src/birch/internal/platform.gleam`
- Contains: FFI declarations for TTY detection, async writers, compression, safe execution, scope context
- Depends on: Time, Record
- Used by: Logger, handlers, scope, async handler

**OTP Integration Layer (Erlang only):**
- Purpose: Integrates with standard `:logger` system
- Location: `src/birch/erlang_logger.gleam`
- Contains: Formatter registration, OTP event translation, automatic setup
- Depends on: Record, Formatter, Console handler
- Used by: Default configuration on BEAM

**Scoped Context Layer:**
- Purpose: Request-scoped metadata that automatically applies to logs
- Location: `src/birch/scope.gleam` and `src/birch/internal/scoped_logger.gleam`
- Contains: with_scope wrapper, process dictionary/AsyncLocalStorage integration
- Depends on: Platform, Record
- Used by: Logger (metadata merging), public API

**Sampling Layer (internal):**
- Purpose: Probabilistic filtering of high-volume log messages
- Location: `src/birch/sampling.gleam`
- Contains: Sampling configuration and logic
- Depends on: Level
- Used by: Logger level checking, public API module functions

## Data Flow

**Standard Log Emission:**

1. Application calls `log.info("message")` or `logger.info(lgr, "message", metadata)`
2. Public API / Logger layer calls `should_log()` to check if level is enabled and sampling applies
3. If enabled:
   - Get timestamp (platform FFI or custom provider)
   - Merge metadata: call args → scope context → logger context
   - Optionally capture caller ID (FFI)
   - Create LogRecord
   - Emit to BEAM `:logger` (if on Erlang target)
   - Dispatch to all attached handlers
4. Each handler:
   - Checks if record level passes handler's min_level filter
   - Calls formatter to convert LogRecord → String
   - Calls write function to output (console, file, async queue, etc.)
   - If error, invoke error_callback (if set)

**Scoped Context Flow:**

1. Application calls `log.with_scope([#("request_id", "123")], fn() { ... })`
2. Scope layer merges new context with current context (new values first for shadowing)
3. Stores merged context and depth in process dictionary (Erlang) or AsyncLocalStorage (Node.js)
4. Executes work function
5. On exit (success or exception via try/after), restores previous context and depth
6. During log emission, logger's merge_metadata includes scope context

**OTP Integration Flow (Erlang only):**

1. Logger calls `emit_to_beam(record)` which sends LogRecord to `:logger`
2. `:logger` default handler receives event with birch_log_record metadata
3. Birch formatter callback recognizes birch_log_record and formats directly
4. Non-birch OTP logs are translated to LogRecord and formatted via report_cb
5. Formatted output goes to file/console configured for `:logger` default handler

**Async Handler Flow:**

1. Handler calls `platform.start_async_writer()` to create background queue
2. Each log dispatch calls `platform.async_send()` to queue LogRecord
3. Background task batches and flushes records at interval or on explicit flush
4. Application calls `log.flush_async_writers()` (or `logger.flush_handlers()`) to wait for completion

**State Management:**

- **Global configuration:** Stored via `config.set_global_config()` in persistent_term (Erlang) or module variable (JavaScript)
- **Default logger cache:** FFI-backed cache (`get_cached_default_logger()`, `set_cached_default_logger()`)
- **Scope context:** Process dictionary stack (Erlang) or AsyncLocalStorage + fallback (JavaScript)
- **Logger instance:** Immutable record passed by value, never mutated
- **Async writers:** Stored in map keyed by writer ID, managed by FFI layer

## Key Abstractions

**Logger:**
- Purpose: Named logging context with configuration
- Examples: `src/birch/logger.gleam` (type definition and operations)
- Pattern: Immutable record with builder methods for adding handlers, context, level filters
- Key operations: new, with_level, with_handler, with_context, trace/debug/info/warn/error/fatal

**Handler:**
- Purpose: Pluggable output destination
- Examples: `src/birch/handler.gleam` (interface), `src/birch/handler/console.gleam`, `src/birch/handler/file.gleam`, `src/birch/handler/json.gleam`
- Pattern: Opaque type wrapping a formatter + write function + optional error callback
- Composition: Handlers can be stacked (logger with multiple handlers dispatches to all)

**LogRecord:**
- Purpose: Immutable representation of a single log event
- Examples: `src/birch/record.gleam`
- Pattern: Type containing timestamp, level, logger name, message, metadata, optional caller ID
- Immutability ensures handlers receive consistent view even with concurrent access

**Formatter:**
- Purpose: LogRecord → String conversion with style variants
- Examples: `formatter.human_readable()`, `formatter.simple()`, console fancy style
- Pattern: Function type `fn(LogRecord) -> String` for composition

**Level:**
- Purpose: Severity classification following RFC 5424
- Examples: Trace < Debug < Info < Notice < Warn < Err < Critical < Alert < Fatal
- Pattern: Integer comparison for O(1) filtering

**Metadata:**
- Purpose: Typed key-value pairs attached to logs
- Examples: `[#("request_id", StringVal("123")), #("duration_ms", IntVal(42))]`
- Pattern: List of tuples with MetadataValue enum for type safety

**GlobalConfig:**
- Purpose: Application-wide logging defaults
- Examples: Default level, handlers, context, sampling, error callbacks
- Pattern: Immutable record with builder options, stored in persistent_term (Erlang)

**Scope:**
- Purpose: Request-scoped context that propagates to all logs in execution context
- Examples: `with_scope([#("user_id", "...")], fn() { ... })`
- Pattern: Stack of metadata contexts stored per process/execution context

## Entry Points

**Module-Level Functions (`src/birch.gleam`):**
- Location: `src/birch.gleam` (lines 446-570 for simple logging)
- Triggers: Application calls `log.info("message")` directly
- Responsibilities: Check sampling, fetch default logger, delegate to logger layer

**Named Logger Functions (`src/birch/logger.gleam`):**
- Location: `src/birch/logger.gleam` (lines 365-488 for level-specific functions)
- Triggers: Application calls `logger.info(my_logger, "message", metadata)`
- Responsibilities: Check should_log, emit record, dispatch to handlers

**Global Configuration (`src/birch.gleam`):**
- Location: `src/birch.gleam` (lines 104-232)
- Triggers: Application calls `log.configure([...])` at startup
- Responsibilities: Merge options, store in persistent_term, invalidate cached default logger

**Scoped Context (`src/birch.gleam` and `src/birch/scope.gleam`):**
- Location: `src/birch.gleam` (line 719), `src/birch/scope.gleam`
- Triggers: `log.with_scope([...], fn() { ... })`
- Responsibilities: Push/pop scope context stack, merge into log metadata

**OTP Integration (Erlang only):**
- Location: `src/birch/erlang_logger.gleam`
- Triggers: Automatic on first log if no explicit handlers configured
- Responsibilities: Register formatter with `:logger` default handler

## Error Handling

**Strategy:** Handler failures are isolated; they never crash the application.

**Patterns:**
- Safe call wrapper (`platform.safe_call()`) catches exceptions in handler write functions
- If handler write fails, error callback is invoked (if configured)
- Application can monitor handler failures via error callback without blocking log flow
- File handler write failures log to stderr but continue
- Async handler queue overflow configurable (drop oldest, drop newest, or block)

**Key locations:**
- Error handling: `src/birch/handler.gleam` lines 88-113 (safe_call + error_callback invocation)
- Error types: `src/birch/handler.gleam` lines 22-35 (HandlerError record)

## Cross-Cutting Concerns

**Logging:** Internal logging avoided; uses FFI's safe_call for error handling

**Validation:**
- Logger names must be non-empty strings (not validated, assumed by caller)
- Metadata values are typed via MetadataValue enum
- Log levels compared via integer comparison for correctness

**Authentication:** Not applicable (logging library)

**Timestamp Generation:**
- Default: `gleam_time` module with FFI fallback for ISO 8601 formatting
- Custom: Per-logger time provider for testing (deterministic timestamps)
- Caller can override via `with_time_provider()` or `with_custom_timestamp()`

**Cross-Platform Support:**
- Gleam code uses `@target(erlang)` and `@target(javascript)` attributes for conditional compilation
- FFI divided into `birch_ffi.erl` (Erlang) and `birch_ffi.mjs` (JavaScript)
- Scope context uses process dictionary (Erlang) or AsyncLocalStorage (Node.js)
- Default handlers differ: OTP logger (Erlang) vs console (JavaScript)

**Concurrency:**
- Erlang: Process dictionary per process ensures scope context isolation
- JavaScript: AsyncLocalStorage per async context (Node.js) or stack-based fallback
- Logger instances are immutable and thread-safe
- Async handlers use FFI-managed queues to batch writes

---

*Architecture analysis: 2026-02-27*
