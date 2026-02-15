# birch - logs that gleam ✨🪵✨

A logging library for Gleam with cross-platform support.

The name "birch" comes from birch trees, whose white bark gleams in the light.

[![Package Version](https://img.shields.io/hexpm/v/birch)](https://hex.pm/packages/birch)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/birch/)

> [!IMPORTANT]
> birch is not yet 1.0. This means:
> 
> - the API is unstable
> - features and APIs may be removed in minor releases
> - quality should not be considered production-ready
>
> Before 1.0 we are exploring a broad set of features and
> stabilizing them at different rates, so 1.0 may ship with only a subset of what you see here.
> Once we reach 1.0, stable features will be ready for broad use.
>
> We welcome usage and feedback in
> the meantime! We will do our best to minimize breaking changes regardless.

## Features

- **Cross-platform**: Works on both Erlang and JavaScript targets
- **Zero-configuration startup**: Just import and start logging
- **Structured logging**: Key-value metadata on every log message
- **Multiple handlers**: Console, file, JSON, async, or custom handlers
- **Log rotation**: Size-based and time-based rotation for file handlers
- **Color support**: Colored output for TTY terminals
- **Lazy evaluation**: Avoid expensive string formatting when logs are filtered
- **Scoped context**: Request-scoped metadata that propagates automatically
- **Sampling**: Probabilistic sampling and rate limiting for high-volume scenarios

## Quick Start

```gleam
import birch as log

pub fn main() {
  // Simple logging
  log.info("Application starting")
  log.debug("Debug message")
  log.error("Something went wrong")

  // With metadata
  log.info_m("User logged in", [#("user_id", "123"), #("ip", "192.168.1.1")])
}
```

## Installation

Add `birch` to your `gleam.toml`:

```toml
[dependencies]
birch = ">= 0.1.0"
```

## Global Configuration

Configure the default logger with custom settings:

```gleam
import birch as log
import birch/level
import birch/handler/console
import birch/handler/json

pub fn main() {
  // Configure with multiple options
  log.configure([
    log.config_level(level.Debug),
    log.config_handlers([console.handler(), json.handler()]),
    log.config_context([#("app", "myapp"), #("env", "production")]),
  ])

  // All logs now include the context and go to both handlers
  log.info("Server starting")
}
```

### Runtime Level Changes

Change the log level at runtime without reconfiguring everything:

```gleam
import birch as log
import birch/level

// Enable debug logging for troubleshooting
log.set_level(level.Debug)

// Later, reduce verbosity
log.set_level(level.Warn)

// Check current level
let current = log.get_level()
```

## Named Loggers

Create named loggers for different components:

```gleam
import birch as log

pub fn main() {
  let db_logger = log.new("myapp.database")
  let http_logger = log.new("myapp.http")

  db_logger |> log.logger_info("Connected to database", [])
  http_logger |> log.logger_info("Server started on port 8080", [])
}
```

## Logger Context

Add persistent context to a logger:

```gleam
import birch as log

pub fn handle_request(request_id: String) {
  let logger = log.new("myapp.http")
    |> log.with_context([
      #("request_id", request_id),
      #("service", "api"),
    ])

  // All logs from this logger include the context
  logger |> log.logger_info("Processing request", [])
  logger |> log.logger_info("Request complete", [#("status", "200")])
}
```

## Scoped Context

Automatically attach metadata to all logs within a scope:

```gleam
import birch as log

pub fn handle_request(request_id: String) {
  log.with_scope([#("request_id", request_id)], fn() {
    // All logs in this block include request_id automatically
    log.info("Processing request")
    do_work()  // Logs in nested functions also include request_id
    log.info("Request complete")
  })
}
```

Scopes can be nested, with inner scopes adding to outer scope context:

```gleam
log.with_scope([#("request_id", "123")], fn() {
  log.info("Start")  // request_id=123

  log.with_scope([#("step", "validation")], fn() {
    log.info("Validating")  // request_id=123 step=validation
  })

  log.info("Done")  // request_id=123
})
```

### Platform Support

- **Erlang**: Uses process dictionary. Each process has isolated context.
- **Node.js**: Uses AsyncLocalStorage. Context propagates across async operations.
- **Other JS runtimes**: Falls back to stack-based storage.

Check availability with `log.is_scoped_context_available()`.

## Log Levels

Six log levels are supported, from least to most severe:

| Level   | Use Case                                |
|---------|----------------------------------------|
| `Trace` | Very detailed diagnostic information   |
| `Debug` | Debugging information during development |
| `Info`  | Normal operational messages (default)  |
| `Warn`  | Warning conditions that might need attention |
| `Error` | Error conditions that should be addressed |
| `Fatal` | Critical errors preventing continuation |

Set the minimum level for a logger:

```gleam
import birch as log
import birch/level

let logger = log.new("myapp")
  |> log.with_level(level.Debug)  // Log Debug and above
```

## Handlers

### Console Handler

The default handler outputs to stdout with colors:

```gleam
import birch/handler/console

let handler = console.handler()
// or with configuration
let handler = console.handler_with_config(console.ConsoleConfig(
  color: True,
  target: handler.Stdout,
))
```

### JSON Handler

For log aggregation systems:

```gleam
import birch/handler/json

let handler = json.handler()
```

Output:
```json
{"timestamp":"2024-12-26T10:30:45.123Z","level":"info","logger":"myapp","message":"Request complete","method":"POST","path":"/api/users"}
```

#### Custom JSON Format

Use the builder pattern to customize JSON output:

```gleam
import birch/handler/json
import gleam/json as j

let custom_handler =
  json.standard_builder()
  |> json.add_custom(fn(_record) {
    [
      #("service", j.string("my-app")),
      #("version", j.string("1.0.0")),
    ]
  })
  |> json.build()
  |> json.handler_with_formatter()
```

### File Handler

Write to files with optional rotation:

```gleam
import birch/handler/file

// Size-based rotation
let handler = file.handler(file.FileConfig(
  path: "/var/log/myapp.log",
  rotation: file.SizeRotation(max_bytes: 10_000_000, max_files: 5),
))

// Time-based rotation (daily)
let handler = file.handler(file.FileConfig(
  path: "/var/log/myapp.log",
  rotation: file.TimeRotation(interval: file.Daily, max_files: 7),
))

// Combined rotation (size OR time)
let handler = file.handler(file.FileConfig(
  path: "/var/log/myapp.log",
  rotation: file.CombinedRotation(
    max_bytes: 50_000_000,
    interval: file.Daily,
    max_files: 10,
  ),
))
```

### Async Handler

Wrap any handler for non-blocking logging:

```gleam
import birch/handler/async
import birch/handler/console

// Make console logging async
let async_console =
  console.handler()
  |> async.make_async(async.default_config())

// With custom configuration
let config =
  async.config()
  |> async.with_queue_size(5000)
  |> async.with_flush_interval(50)
  |> async.with_overflow(async.DropOldest)

let handler = async.make_async(console.handler(), config)

// Before shutdown, ensure all logs are written
async.flush()
```

### Null Handler

For testing or disabling logging:

```gleam
import birch/handler

let handler = handler.null()
```

## Custom Handlers

Create custom handlers with the handler interface:

```gleam
import birch/handler
import birch/formatter

let my_handler = handler.new(
  name: "custom",
  write: fn(message) {
    // Send to external service, etc.
  },
  format: formatter.human_readable,
)
```

### Error Callbacks

Handle errors from handlers without crashing:

```gleam
import birch/handler
import birch/handler/file

let handler =
  file.handler(config)
  |> handler.with_error_callback(fn(err) {
    io.println("Handler " <> err.handler_name <> " failed: " <> err.error)
  })
```

## Lazy Evaluation

Avoid expensive operations when logs are filtered:

```gleam
import birch as log

// The closure is only called if debug level is enabled
log.debug_lazy(fn() {
  "Expensive debug info: " <> compute_debug_info()
})
```

## Error Result Helpers

Log errors with automatic metadata extraction:

```gleam
import birch as log

case file.read("config.json") {
  Ok(content) -> parse_config(content)
  Error(_) as result -> {
    // Automatically includes error value in metadata
    log.error_result("Failed to read config file", result)
    use_defaults()
  }
}

// With additional metadata
log.error_result_m("Database query failed", result, [
  #("query", "SELECT * FROM users"),
  #("table", "users"),
])
```

## Sampling

For high-volume logging, sample messages probabilistically:

```gleam
import birch as log
import birch/level
import birch/sampling

// Log only 10% of debug messages
log.configure([
  log.config_sampling(sampling.config(level.Debug, 0.1)),
])

// Debug messages above the threshold are always logged
// Messages at or below Debug level are sampled at 10%
```

## Testing Support

### Custom Time Providers

Use deterministic timestamps in tests:

```gleam
import birch as log

let test_logger =
  log.new("test")
  |> log.with_time_provider(fn() { "2024-01-01T00:00:00.000Z" })
```

### Caller ID Capture

Track which process/thread created each log:

```gleam
import birch as log

let logger =
  log.new("myapp.worker")
  |> log.with_caller_id_capture()

// Log records will include:
// - Erlang: PID like "<0.123.0>"
// - JavaScript: "main", "pid-N", or "worker-N"
```

## Output Formats

### Human-Readable (default)

```
2024-12-26T10:30:45.123Z | INFO  | myapp.http | Request complete | method=POST path=/api/users
```

### JSON

```json
{"timestamp":"2024-12-26T10:30:45.123Z","level":"info","logger":"myapp.http","message":"Request complete","method":"POST","path":"/api/users"}
```

## Library Authors

For library code, create silent loggers that consumers can configure:

```gleam
// In your library
import birch as log

const logger = log.silent("mylib.internal")

pub fn do_something() {
  logger |> log.logger_debug("Starting operation", [])
  // ...
}
```

Consumers control logging by adding handlers to the logger.

## Comparison with Other Logging Libraries

Several logging libraries exist in the Gleam ecosystem. Here's how they compare:

| Feature | birch | [glight](https://hexdocs.pm/glight/) | [glogg](https://hexdocs.pm/glogg/) | [palabres](https://hexdocs.pm/palabres/) |
|---------|-------|--------|-------|----------|
| Erlang target | ✅ | ✅ | ✅ | ✅ |
| JavaScript target | ✅ | ❌ | ✅ | ✅ |
| Console output | ✅ | ✅ | ❌ | ✅ |
| File output | ✅ | ✅ | ❌ | ❌ |
| JSON output | ✅ | ✅ | ✅ | ✅ |
| File rotation | ✅ | ❌ | ❌ | ❌ |
| Colored output | ✅ | ✅ | ❌ | ✅ |
| Structured metadata | ✅ | ✅ | ✅ | ✅ |
| Typed metadata values | ❌ | ❌ | ✅ | ✅ |
| Named loggers | ✅ | ❌ | ❌ | ❌ |
| Logger context | ✅ | ✅ | ✅ | ❌ |
| Scoped context | ✅ | ❌ | ❌ | ❌ |
| Lazy evaluation | ✅ | ❌ | ❌ | ❌ |
| Custom handlers | ✅ | ❌ | ❌ | ❌ |
| Sampling | ✅ | ❌ | ❌ | ❌ |
| Stacktrace capture | ❌ | ❌ | ✅ | ❌ |
| Erlang logger integration | ✅ | ✅ | ❌ | ❌ |
| Wisp integration | ❌ | ❌ | ❌ | ✅ |
| Zero-config startup | ✅ | ❌ | ❌ | ✅ |

### When to Choose Each Library

- **birch**: Applications needing file rotation, scoped context propagation, lazy evaluation, custom handlers, or Erlang logger integration with cross-platform support.
- **glight**: Erlang-only applications that want a minimal wrapper around Erlang's standard logger module.
- **glogg**: Applications requiring typed metadata fields (Int, Float, Bool, Duration) or stacktrace capture.
- **palabres**: Wisp web applications that benefit from built-in middleware integration.

## Development

See [DEV.md](DEV.md) for development setup, testing, and contribution guidelines.

## License

MIT License - see [LICENSE](LICENSE) for details.
