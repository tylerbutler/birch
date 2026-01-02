# Patterns from glimt to Consider Adopting

**Research Date:** December 2025
**glimt Version Analyzed:** 0.5.0

This document analyzes patterns from the [glimt logging library](https://github.com/JohnBjrk/glimt) that could enhance gleam_log.

---

## Executive Summary

glimt has several patterns worth adopting:

| Pattern | Priority | Recommendation |
|---------|----------|----------------|
| Actor-based async instances | High | Adopt for async handlers |
| Generic data/context types | Medium | Consider for typed metadata |
| Serializer builder pattern | Medium | Adopt for JSON customization |
| Direct/Actor instance distinction | High | Adopt for explicit sync/async choice |
| Process ID tracking | Low | Consider for debugging |

---

## 1. Actor-Based Async Instances

### glimt Pattern

glimt distinguishes between two instance types:

```gleam
pub type LoggerInstance(data, context) {
  // Synchronous - runs in calling process
  Direct(
    name: Option(String),
    level_min_value: Int,
    dispatch: fn(LogMessage) -> Nil,
  )

  // Asynchronous - runs in separate process
  Actor(
    name: Option(String),
    level_min_value: Int,
    subject: Subject(LogMessage),
  )
}
```

Actors use OTP Subjects (gleam_otp) for message passing, providing true async logging.

### Current gleam_log Approach

Our handlers are synchronous only. The implementation plan mentions async but doesn't specify the mechanism.

### Recommendation

**Adopt this pattern for Phase 2.3 (Async Handler):**

```gleam
// In src/gleam_log/handler/async.gleam
pub type AsyncHandler {
  AsyncHandler(
    name: String,
    subject: Subject(LogRecord),
    min_level: Result(Level, Nil),
  )
}

// Start an async handler actor
pub fn start(handler: Handler) -> Result(AsyncHandler, StartError)

// Convenience wrapper
pub fn make_async(handler: Handler) -> Handler
```

**Benefits:**
- Uses established OTP patterns (battle-tested)
- Natural backpressure via Subject mailbox
- Clean separation of sync vs async

**Trade-off:**
- Requires gleam_otp dependency (Erlang only)
- Need JavaScript alternative (setTimeout queue)

### Implementation Status: ✅ IMPLEMENTED

The actor-based async pattern has been fully implemented:

- **Erlang target**: Uses `gleam_otp` actor pattern with proper OTP Subjects
  - `src/gleam_log/internal/async_actor.gleam` - OTP actor implementation
  - `src/gleam_log/handler/async.gleam` - Public API with `@target(erlang)` functions
  - Features: `shutdown_handler()`, `shutdown_all()`, `get_subject()` for supervision integration

- **JavaScript target**: Falls back to setTimeout-based batching (FFI implementation)
  - Maintains API compatibility across targets
  - Uses the existing `AsyncWriter` class in `gleam_log_ffi.mjs`

**Usage:**
```gleam
import gleam_log/handler/async
import gleam_log/handler/console

// Create async handler (uses OTP actor on Erlang, setTimeout on JS)
let async_console = async.make_async(console.handler(), async.default_config())

// Erlang-only: Get the OTP Subject for supervision tree integration
let assert Ok(subject) = async.get_subject("async:console")

// Flush before shutdown
async.flush()

// Erlang-only: Graceful shutdown
async.shutdown_all()
```

---

## 2. Generic Data and Context Types

### glimt Pattern

glimt uses generic type parameters for data and context:

```gleam
pub opaque type Logger(data, context) {
  Logger(
    name: String,
    level_min_value: Int,
    context: Option(context),
    instances: List(LoggerInstance(data, context)),
  )
}

pub type LogMessage(data, context, result_type) {
  LogMessage(
    // ... standard fields ...
    data: Option(data),
    context: Option(context),
    error: Option(Result(result_type, result_type)),
  )
}
```

This allows strongly-typed custom data:

```gleam
type RequestData {
  RequestData(method: String, path: String, duration_ms: Int)
}

let logger: Logger(RequestData, Nil) = new("http")
logger |> log_with_data(Info, "request complete", RequestData("GET", "/api", 42))
```

### Current gleam_log Approach

We use `List(#(String, String))` for metadata - simple but untyped:

```gleam
log.info("request complete", [
  #("method", "GET"),
  #("path", "/api"),
  #("duration_ms", "42"),  // Must convert to string
])
```

### Recommendation

**Consider but don't fully adopt.**

**Pros of glimt approach:**
- Type safety for structured data
- No string conversion needed
- Compile-time checks

**Cons:**
- More complex API (generic parameters everywhere)
- Harder cross-platform serialization
- Users must define custom types upfront

**Middle-ground option:**

```gleam
// Keep current string-based metadata as primary API
log.info_m("message", [#("key", "value")])

// Add optional typed variant for power users
pub fn log_with_data(
  logger: Logger,
  level: Level,
  message: String,
  data: a,
  serialize: fn(a) -> List(#(String, String)),
) -> Nil
```

This preserves simplicity while enabling typed data when needed.

---

## 3. Serializer Builder Pattern

### glimt Pattern

JSON serializer uses a builder pattern for customization:

```gleam
import glimt/serializer/json.{builder, add_standard_log_message, add_data, build}

let custom_serializer =
  builder()
  |> add_standard_log_message()
  |> add_data(fn(data) { [#("request", my_data_serializer(data))] })
  |> add_context(fn(ctx) { [#("trace_id", ctx.trace_id)] })
  |> build()
```

### Current gleam_log Approach

Our JSON handler has a fixed format:

```gleam
// src/gleam_log/handler/json.gleam
pub fn format_json(record: LogRecord) -> String {
  // Fixed field order, no customization
  [
    #("timestamp", json.string(record.timestamp)),
    #("level", json.string(level.to_string_lowercase(record.level))),
    #("logger", json.string(record.logger_name)),
    #("message", json.string(record.message)),
  ]
  |> list.append(metadata_fields)
  |> json.object
  |> json.to_string
}
```

### Recommendation

**Adopt this pattern for JSON customization:**

```gleam
// src/gleam_log/handler/json.gleam

pub type JsonBuilder {
  JsonBuilder(fields: List(fn(LogRecord) -> List(#(String, Json))))
}

pub fn builder() -> JsonBuilder {
  JsonBuilder(fields: [])
}

pub fn add_timestamp(b: JsonBuilder) -> JsonBuilder {
  add_field(b, fn(r) { [#("timestamp", json.string(r.timestamp))] })
}

pub fn add_level(b: JsonBuilder) -> JsonBuilder {
  add_field(b, fn(r) { [#("level", json.string(level.to_string_lowercase(r.level)))] })
}

pub fn add_message(b: JsonBuilder) -> JsonBuilder {
  add_field(b, fn(r) { [#("message", json.string(r.message))] })
}

pub fn add_metadata(b: JsonBuilder) -> JsonBuilder {
  add_field(b, fn(r) {
    list.map(r.metadata, fn(pair) { #(pair.0, json.string(pair.1)) })
  })
}

pub fn add_custom(b: JsonBuilder, extractor: fn(LogRecord) -> List(#(String, Json))) -> JsonBuilder {
  add_field(b, extractor)
}

pub fn build(b: JsonBuilder) -> formatter.Formatter {
  fn(record) {
    b.fields
    |> list.flat_map(fn(f) { f(record) })
    |> json.object
    |> json.to_string
  }
}

// Convenience: default JSON format
pub fn standard_builder() -> JsonBuilder {
  builder()
  |> add_timestamp()
  |> add_level()
  |> add_field(fn(r) { [#("logger", json.string(r.logger_name))] })
  |> add_message()
  |> add_metadata()
}
```

**Usage:**

```gleam
// Default behavior (unchanged)
let handler = json.handler()

// Custom JSON format
let custom_handler =
  json.standard_builder()
  |> json.add_custom(fn(_) { [#("service", json.string("my-app"))] })
  |> json.build()
  |> fn(formatter) { handler.new("json", platform.write_stdout, formatter) }
```

---

## 4. Process ID Tracking

### glimt Pattern

LogMessage includes process information:

```gleam
pub type LogMessage(data, context, result_type) {
  LogMessage(
    // ...
    pid: Pid,                      // Calling process
    instance_pid: Option(Pid),     // Handler actor (if async)
    // ...
  )
}
```

### Current gleam_log Approach

We don't track process IDs.

### Recommendation

**Consider for debugging but make optional:**

```gleam
// In LogRecord
pub type LogRecord {
  LogRecord(
    // ... existing fields ...
    caller_pid: Option(String),  // String for cross-platform compat
  )
}

// Platform FFI
@external(erlang, "gleam_log_ffi", "get_self_pid_string")
@external(javascript, "./gleam_log_ffi.mjs", "get_self_pid_string")
pub fn get_caller_id() -> String
```

**Erlang:** Return `pid_to_list(self())`
**JavaScript:** Return thread/worker ID or "main"

This aids debugging concurrent applications without breaking JS compatibility.

---

## 5. Error Attachment Pattern

### glimt Pattern

Separate functions for error logging:

```gleam
pub fn error(logger: Logger, message: String, error: Result(a, e)) -> Nil
pub fn fatal(logger: Logger, message: String, error: Result(a, e)) -> Nil
pub fn log_error_with_data(logger, level, message, error, data) -> Nil
```

### Current gleam_log Approach

Errors must be converted to metadata strings manually.

### Recommendation

**Add error convenience functions:**

```gleam
// src/gleam_log/logger.gleam

/// Log an error with an associated Result
pub fn error_result(
  logger: Logger,
  message: String,
  result: Result(a, e),
  metadata: Metadata,
) -> Nil {
  let error_metadata = case result {
    Ok(_) -> []
    Error(e) -> [#("error", string.inspect(e))]
  }
  log(logger, level.Err, message, list.append(error_metadata, metadata))
}

/// Log a fatal error with an associated Result
pub fn fatal_result(
  logger: Logger,
  message: String,
  result: Result(a, e),
  metadata: Metadata,
) -> Nil {
  let error_metadata = case result {
    Ok(_) -> []
    Error(e) -> [#("error", string.inspect(e))]
  }
  log(logger, level.Fatal, message, list.append(error_metadata, metadata))
}
```

---

## 6. Instance Naming

### glimt Pattern

Each instance can have a name for identification:

```gleam
Direct(
  name: Some("file-handler"),  // or None for anonymous
  level_min_value: 2,
  dispatch: dispatch_fn,
)
```

### Current gleam_log Approach

Handlers have names: `handler.name(h)` returns the name.

**Status:** Already implemented similarly. No changes needed.

---

## 7. Time Provider Customization

### glimt Pattern

```gleam
pub fn with_time_provider(
  logger: Logger(a, b),
  now: fn() -> String,
) -> Logger(a, b)
```

Allows custom timestamp formatting or mocking for tests.

### Recommendation

**Consider for testing support:**

```gleam
// For testing - inject custom timestamp
pub fn with_time_provider(
  logger: Logger,
  provider: fn() -> String,
) -> Logger

// Example test usage
let test_logger =
  logger.new("test")
  |> logger.with_time_provider(fn() { "2024-01-01T00:00:00.000Z" })
```

This is particularly useful for deterministic test output.

---

## Implementation Priority

Based on this analysis, here's the recommended order for adopting glimt patterns:

### High Priority (Phase 2)

1. **Actor-based async instances** - ✅ IMPLEMENTED (uses gleam_otp on Erlang)
2. **Serializer builder pattern** - JSON customization

### Medium Priority (Phase 3)

3. **Error result convenience functions** - Quality of life improvement
4. **Time provider for testing** - Test support

### Low Priority (Future)

5. **Process ID tracking** - Debugging enhancement
6. **Generic typed data** - Only if users request it

---

## Implementation Summary

| Pattern | Priority | Status | Notes |
|---------|----------|--------|-------|
| Actor-based async instances | High | ✅ Implemented | Uses gleam_otp Subjects on Erlang, FFI on JS |
| Serializer builder pattern | High | ❌ Not implemented | JSON format is fixed |
| Error result functions | Medium | ❌ Not implemented | Manual error conversion needed |
| Time provider | Medium | ❌ Not implemented | Uses platform.timestamp_iso8601() |
| Process ID tracking | Low | ❌ Not implemented | Not critical |
| Generic typed data | Low | ❌ Not implemented | String metadata is sufficient |

---

## References

- [glimt GitHub Repository](https://github.com/JohnBjrk/glimt)
- [glimt Hexdocs](https://hexdocs.pm/glimt/)
- [gleam_otp Subject documentation](https://hexdocs.pm/gleam_otp/)
