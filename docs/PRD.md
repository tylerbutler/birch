# birch - Product Requirements Document

**Version:** 1.0
**Date:** December 2025
**Status:** Draft

The name "birch" comes from birch trees, whose white bark gleams in the light.

-----

## 1. Overview

### 1.1 Problem Statement

The Gleam ecosystem lacks a full-featured logging library with cross-platform support. The existing option (glimt) has been unmaintained since July 2023, uses outdated dependencies, supports only the Erlang target, and lacks features like file output, log rotation, and rate limiting.

### 1.2 Vision

Build a cross-platform logging library for Gleam that is simple for basic use cases and extensible for advanced needs. The library should feel native to Gleam's functional style while providing the reliability expected in server and application environments.

### 1.3 Success Criteria

- Works identically on Erlang and JavaScript targets
- Zero-configuration startup for common cases
- Adoptable incrementally (simple → structured → distributed)
- Performance overhead under 1μs for filtered (below-threshold) log calls
- Active maintenance with Gleam version compatibility

-----

## 2. Target Users

### 2.1 Primary Users

|User Type                |Needs                                              |Example Use Case                        |
|-------------------------|---------------------------------------------------|----------------------------------------|
|**Application Developer**|Quick setup, readable output, basic filtering      |Building a web app with Wisp            |
|**Library Author**       |Non-intrusive logging, configurable by consumer    |Adding debug output to a database driver|
|**Platform Engineer**    |Structured logs, external integrations, correlation|Running Gleam services in Kubernetes    |

### 2.2 User Stories

1. **As an application developer**, I want to add logging with one import and one function call so that I can debug my application immediately.
1. **As a library author**, I want to emit logs that are silent by default but visible when the consumer enables them, so that I don't pollute their output.
1. **As a platform engineer**, I want JSON-formatted logs with trace IDs and timestamps so that I can aggregate them in our observability stack.
1. **As a developer**, I want logs to work the same whether I'm targeting Erlang or JavaScript so that I don't have to maintain separate logging code.

-----

## 3. Goals and Non-Goals

### 3.1 Goals

|Priority|Goal                                             |
|--------|-------------------------------------------------|
|**P0**  |Cross-target support (Erlang + JavaScript)       |
|**P0**  |Standard log levels with filtering               |
|**P0**  |Pluggable output handlers (console, file, custom)|
|**P0**  |Structured logging (key-value metadata)          |
|**P1**  |Async/non-blocking output option                 |
|**P1**  |File output with rotation                        |
|**P1**  |JSON formatter                                   |
|**P1**  |Context propagation (request IDs, etc.)          |
|**P2**  |Sampling and rate limiting                       |
|**P2**  |Runtime log level changes                        |
|**P2**  |Erlang :logger backend integration               |

### 3.2 Non-Goals

- **Distributed tracing** - Use dedicated tracing libraries (OpenTelemetry bindings)
- **Log aggregation/shipping** - External tools handle this (Fluentd, Vector, etc.)
- **GUI or web interface** - Out of scope
- **Metrics collection** - Separate concern from logging
- **Automatic instrumentation** - Explicit logging only

-----

## 4. Functional Requirements

### 4.1 Core Logging

#### FR-1: Log Levels

The library MUST support the following log levels in order of severity:

```
TRACE < DEBUG < INFO < WARN < ERROR < FATAL
```

Each level MUST have a corresponding convenience function:

```gleam
log.trace("detailed diagnostic")
log.debug("debugging info")
log.info("normal operation")
log.warn("potential issue")
log.error("error occurred")
log.fatal("system cannot continue")
```

#### FR-2: Level Filtering

- Logs below the configured threshold MUST NOT be processed or formatted
- The default level SHOULD be `INFO`
- Level MUST be configurable globally and per-logger
- Level checks MUST be fast (simple integer comparison)

#### FR-3: Message Formatting

Log messages MUST support:

- Static strings: `log.info("Server started")`
- Interpolated values: `log.info("User " <> user_id <> " logged in")`
- Lazy evaluation: `log.debug_lazy(fn() { expensive_debug_string() })`

#### FR-4: Structured Metadata

Every log call MUST accept optional key-value metadata:

```gleam
log.info("Request complete", [
  #("method", "POST"),
  #("path", "/api/users"),
  #("duration_ms", int.to_string(42)),
])
```

Metadata values MUST be strings (formatted by caller) for simplicity and cross-target compatibility.

### 4.2 Loggers and Handlers

#### FR-5: Logger Creation

```gleam
// Default global logger
log.info("message")

// Named logger
let logger = log.new("myapp.database")
logger |> log.info("connected")

// Logger with preset context
let request_logger = log.new("myapp.http")
  |> log.with_context([#("request_id", req_id)])
```

#### FR-6: Handlers

Handlers receive formatted log records and write them to destinations.

**Required handlers:**

|Handler       |Target       |Notes                         |
|--------------|-------------|------------------------------|
|`console`     |stdout/stderr|Default, colored output in TTY|
|`file`        |File system  |With rotation support         |
|`json_console`|stdout       |JSON-formatted output         |
|`null`        |Nowhere      |For testing/disabling         |
|`custom`      |User-defined |Handler interface             |

#### FR-7: Handler Configuration

```gleam
log.configure([
  log.level(log.Debug),
  log.handlers([
    handler.console(handler.ConsoleConfig(color: True, target: Stdout)),
    handler.file(handler.FileConfig(
      path: "/var/log/myapp.log",
      rotation: handler.SizeRotation(max_bytes: 10_000_000, keep: 5),
    )),
  ]),
])
```

#### FR-8: Multiple Handlers

- A logger MUST support multiple simultaneous handlers
- Each handler MAY have its own level filter
- Handler failures MUST NOT crash the application

### 4.3 Context and Correlation

#### FR-9: Logger Context

Loggers MUST support attaching persistent context that appears on all messages:

```gleam
let logger = log.new("api")
  |> log.with_context([#("service", "user-service"), #("version", "1.2.3")])

// All logs from this logger include service and version
logger |> log.info("starting")
```

#### FR-10: Scoped Context

Support temporary context for request/operation scope:

```gleam
log.with_scope([#("request_id", id)], fn() {
  // All logs in this block include request_id
  log.info("processing")
  do_work()
  log.info("complete")
})
```

**Note:** Scoped context requires target-specific implementation (process dictionary on Erlang, AsyncLocalStorage or explicit threading on JS).

### 4.4 Output Formats

#### FR-11: Human-Readable Format

Default console output format:

```
2024-12-26T10:30:45.123Z | INFO  | myapp.http | Request complete | method=POST path=/api/users duration_ms=42
```

Components:

- ISO 8601 timestamp with milliseconds
- Level (padded for alignment)
- Logger name
- Message
- Metadata as key=value pairs

#### FR-12: JSON Format

```json
{
  "timestamp": "2024-12-26T10:30:45.123Z",
  "level": "info",
  "logger": "myapp.http",
  "message": "Request complete",
  "method": "POST",
  "path": "/api/users",
  "duration_ms": "42"
}
```

#### FR-13: Custom Formats

Users MUST be able to provide custom formatters:

```gleam
type Formatter = fn(LogRecord) -> String
```

### 4.5 File Handling

#### FR-14: File Output

The file handler MUST:

- Create parent directories if they don't exist
- Handle file open/write errors gracefully (emit to stderr, don't crash)
- Buffer writes for performance (configurable flush interval)
- Support immediate flush for ERROR and FATAL levels

#### FR-15: Log Rotation

Support rotation strategies:

|Strategy      |Description                            |
|--------------|---------------------------------------|
|**Size-based**|Rotate when file exceeds N bytes       |
|**Time-based**|Rotate daily/hourly                    |
|**Combined**  |Rotate on size OR time, whichever first|

Rotation behavior:

- Rename current file with timestamp suffix
- Keep configurable number of old files
- Optionally compress rotated files (P2)

### 4.6 Performance

#### FR-16: Async Output

Provide non-blocking output option:

- On Erlang: Log records sent to handler actor
- On JavaScript: Batch writes with microtask scheduling

```gleam
handler.console(handler.ConsoleConfig(async: True))
```

#### FR-17: Sampling

For high-volume scenarios, support probabilistic sampling:

```gleam
log.configure([
  log.sampling(log.SampleConfig(
    level: log.Debug,
    rate: 0.1,  // Log 10% of debug messages
  )),
])
```

-----

## 5. Non-Functional Requirements

### 5.1 Performance

|Metric               |Requirement|
|---------------------|-----------|
|Filtered log call    |< 1μs      |
|Sync console write   |< 100μs    |
|Async log enqueue    |< 10μs     |
|Memory per log record|< 1KB      |

### 5.2 Reliability

- Handler errors MUST be isolated (one handler failing doesn't affect others)
- File handler MUST handle disk full gracefully
- Async handlers MUST have bounded queues with configurable overflow behavior (drop oldest, drop newest, block)

### 5.3 Compatibility

|Requirement  |Details|
|-------------|-------|
|Gleam version|>= 1.0 |
|Erlang/OTP   |>= 26  |
|Node.js      |>= 18  |
|Deno         |>= 1.40|
|Bun          |>= 1.0 |

### 5.4 Dependencies

Minimize external dependencies:

- `gleam_stdlib` - Required
- `gleam_erlang` - For Erlang target features
- `gleam_javascript` - For JS target features
- `simplifile` - For file operations (or implement minimal FFI)
- `gleam_json` - For JSON formatter only

-----

## 6. API Design Principles

### 6.1 Progressive Disclosure

```gleam
// Level 1: Just works
log.info("hello")

// Level 2: Add metadata
log.info("hello", [#("user", "alice")])

// Level 3: Named logger
let logger = log.new("myapp")
logger |> log.info("hello")

// Level 4: Full configuration
log.configure([...])
```

### 6.2 Explicit Over Magic

- No automatic context capture
- No implicit global state mutation
- Configuration is explicit and inspectable

### 6.3 Composition Over Configuration

```gleam
// Prefer this:
let logger = log.new("api")
  |> log.with_context([...])
  |> log.with_handler(my_handler)

// Over monolithic config objects
```

### 6.4 Type Safety

- Log levels are a custom type, not strings or integers
- Handlers implement a well-defined interface
- Metadata keys and values are typed (String, String)

-----

## 7. Module Structure

```
src/
├── birch.gleam               # Main public API
├── birch/
│   ├── level.gleam           # LogLevel type and comparisons
│   ├── record.gleam          # LogRecord type
│   ├── logger.gleam          # Logger type and operations
│   ├── handler.gleam         # Handler interface
│   ├── handler/
│   │   ├── console.gleam     # Console/TTY output
│   │   ├── file.gleam        # File output with rotation
│   │   ├── json.gleam        # JSON formatter
│   │   └── null.gleam        # No-op handler
│   ├── formatter.gleam       # Formatting utilities
│   ├── config.gleam          # Configuration types
│   └── internal/
│       ├── platform.gleam    # Cross-platform FFI declarations
│       └── ...
├── birch_ffi.erl             # Erlang FFI implementation
└── birch_ffi.mjs             # JavaScript FFI implementation
```

-----

## 8. Open Questions

### 8.1 Naming

**Decision:** The library is named `birch` after birch trees, whose white bark gleams in the light. This provides a memorable, nature-themed name while avoiding the reserved `gleam_*` prefix.

### 8.2 Global State

How should the default logger be managed?

|Option|Approach                                 |Trade-offs               |
|------|-----------------------------------------|-------------------------|
|**A** |Process dictionary (Erlang) / global (JS)|Convenient, less explicit|
|**B** |Explicit logger passing everywhere       |Pure, but verbose        |
|**C** |Hybrid: global default + explicit option |Flexible, some complexity|

**Recommendation:** Option C - most practical for real usage

### 8.3 Erlang Logger Integration

Should we:

- **A)** Build entirely independent logging
- **B)** Use Erlang's :logger as backend on BEAM
- **C)** Provide optional :logger backend

**Recommendation:** Option C - allows integration with Erlang/Elixir ecosystems when desired

### 8.4 Scoped Context on JavaScript

JavaScript lacks Erlang's process dictionary. Options:

- **A)** Explicit context passing (verbose but pure)
- **B)** AsyncLocalStorage (Node.js 16+, not universal)
- **C)** Zone.js style patching (complex, fragile)
- **D)** No scoped context on JS target (feature gap)

**Recommendation:** Option A as default, Option B as opt-in for Node.js

-----

## 9. Implementation Phases

### Phase 1: Core (MVP)

- [ ] LogLevel type and filtering
- [ ] LogRecord type
- [ ] Basic Logger with context
- [ ] Console handler (sync, colored)
- [ ] Human-readable formatter
- [ ] Global default logger
- [ ] Works on Erlang and JavaScript

**Deliverable:** Usable for basic application logging

### Phase 2: Production Features

- [ ] JSON formatter
- [ ] File handler
- [ ] Size-based rotation
- [ ] Async console handler
- [ ] Multiple handlers per logger
- [ ] Runtime level changes

**Deliverable:** Suitable for production deployments

### Phase 3: Advanced Features

- [ ] Time-based rotation
- [ ] Sampling/rate limiting
- [ ] Erlang :logger backend
- [ ] Scoped context (with JS AsyncLocalStorage option)
- [ ] Handler error callbacks
- [ ] Compression of rotated files

**Deliverable:** Feature parity with mature logging libraries

-----

## 10. Testing Strategy

### 10.1 Unit Tests

- Level comparison and filtering
- Record formatting
- Metadata merging
- Rotation logic (with mock filesystem)

### 10.2 Integration Tests

- Console output capture and verification
- File writes and rotation
- Cross-target behavior parity

### 10.3 Performance Tests

- Benchmark filtered vs unfiltered calls
- Throughput under load
- Memory usage over time

### 10.4 Property Tests

- Arbitrary metadata combinations
- Concurrent logging correctness
- Rotation under various conditions

-----

## 11. Documentation Plan

### 11.1 README

- Quick start (< 30 seconds to first log)
- Common patterns
- Configuration reference

### 11.2 Hexdocs

- Complete API documentation
- Module-level guides
- Examples for each handler

### 11.3 Guides

- "Logging in Wisp applications"
- "Production logging setup"
- "Writing custom handlers"
- "Migrating from glimt"

-----

## 12. Success Metrics

|Metric       |Target                             |Measurement         |
|-------------|-----------------------------------|--------------------|
|Adoption     |500+ downloads in first month      |Hex.pm stats        |
|Compatibility|Zero issues from target differences|GitHub issues       |
|Performance  |Meets NFR benchmarks               |Automated benchmarks|
|Satisfaction |Positive community feedback        |Discord, GitHub     |

-----

## Appendix A: Comparison with glimt

|Feature                   |glimt   |This Library|
|--------------------------|--------|------------|
|Erlang target             |Yes     |Yes         |
|JavaScript target         |No      |Yes         |
|Log levels                |Yes     |Yes         |
|Structured metadata       |Yes     |Yes         |
|Context propagation       |Yes     |Yes         |
|JSON output               |Yes     |Yes         |
|Console output            |Yes     |Yes         |
|File output               |No      |Yes         |
|Log rotation              |No      |Yes         |
|Async handlers            |Yes     |Yes         |
|Sampling                  |No      |Yes         |
|Erlang :logger integration|Yes     |Yes (optional)|
|Maintained                |No (2023)|Yes        |
|Modern Gleam              |No      |Yes         |

-----

## Appendix B: Example Usage

### Basic Application

```gleam
import birch as log

pub fn main() {
  log.info("Application starting")

  case connect_database() {
    Ok(conn) -> {
      log.info_m("Database connected", [#("host", conn.host)])
      run_server(conn)
    }
    Error(e) -> {
      log.error_m("Database connection failed", [#("error", string.inspect(e))])
    }
  }
}
```

### Web Application with Request Context

```gleam
import birch as log
import birch/handler/json

pub fn main() {
  log.configure([
    log.config_level(level.Info),
    log.config_handlers([json.handler()]),
  ])

  wisp.start(router, config)
}

fn handle_request(req: Request) -> Response {
  let request_id = generate_id()
  let logger = log.new("http")
    |> log.with_context([
      #("request_id", request_id),
      #("method", req.method),
      #("path", req.path),
    ])

  logger |> log.logger_info("Request received", [])

  let response = process_request(req, logger)

  logger |> log.logger_info_m("Request complete", [
    #("status", int.to_string(response.status)),
  ])

  response
}
```

### Library with Optional Logging

```gleam
// In library code
import birch as log

const logger = log.silent("mylib.internal")

pub fn do_something() {
  logger |> log.logger_debug("Starting operation", [])
  // ... work ...
  logger |> log.logger_debug("Operation complete", [])
}

// Consumer controls visibility via level configuration
```
