# gleam_log

A modern, production-ready logging library for Gleam.

[![Package Version](https://img.shields.io/hexpm/v/gleam_log)](https://hex.pm/packages/gleam_log)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gleam_log/)

## Features

- **Cross-platform**: Works on both Erlang and JavaScript targets
- **Zero-configuration startup**: Just import and start logging
- **Structured logging**: Key-value metadata on every log message
- **Multiple handlers**: Console, file, JSON, or custom handlers
- **Log rotation**: Size-based rotation for file handlers
- **Color support**: Colored output for TTY terminals
- **Lazy evaluation**: Avoid expensive string formatting when logs are filtered

## Quick Start

```gleam
import gleam_log as log

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

Add `gleam_log` to your `gleam.toml`:

```toml
[dependencies]
gleam_log = ">= 0.1.0"
```

## Named Loggers

Create named loggers for different components:

```gleam
import gleam_log as log

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
import gleam_log as log

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
import gleam_log as log

let logger = log.new("myapp")
  |> log.with_level(log.Debug)  // Log Debug and above
```

## Handlers

### Console Handler

The default handler outputs to stdout with colors:

```gleam
import gleam_log/handler/console

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
import gleam_log/handler/json

let handler = json.handler()
```

Output:
```json
{"timestamp":"2024-12-26T10:30:45.123Z","level":"info","logger":"myapp","message":"Request complete","method":"POST","path":"/api/users"}
```

### File Handler

Write to files with optional rotation:

```gleam
import gleam_log/handler/file

let handler = file.handler(file.FileConfig(
  path: "/var/log/myapp.log",
  rotation: file.SizeRotation(max_bytes: 10_000_000, max_files: 5),
))
```

### Null Handler

For testing or disabling logging:

```gleam
import gleam_log/handler

let handler = handler.null()
```

## Custom Handlers

Create custom handlers with the handler interface:

```gleam
import gleam_log/handler
import gleam_log/formatter

let my_handler = handler.new(
  name: "custom",
  write: fn(message) {
    // Send to external service, etc.
  },
  format: formatter.human_readable,
)
```

## Lazy Evaluation

Avoid expensive operations when logs are filtered:

```gleam
import gleam_log as log

// The closure is only called if debug level is enabled
log.debug_lazy(fn() {
  "Expensive debug info: " <> compute_debug_info()
})
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
import gleam_log as log

const logger = log.silent("mylib.internal")

pub fn do_something() {
  logger |> log.logger_debug("Starting operation", [])
  // ...
}
```

Consumers control logging by adding handlers to the logger.

## Development

```bash
# Build for default target (Erlang)
gleam build

# Build for JavaScript
gleam build --target javascript

# Run tests on Erlang
gleam test

# Run tests on JavaScript
gleam test --target javascript

# Check formatting
gleam format --check src test

# Format code
gleam format src test

# Generate docs
gleam docs build
```

### Testing

This project uses:
- **gleeunit** - Standard test runner for Gleam
- **qcheck** - Property-based testing for more thorough test coverage

Unit tests are in `test/gleam_log_test.gleam` and property tests are in `test/property_test.gleam`.

### CI/CD

GitHub Actions runs on every push and PR:
- Tests on both Erlang and JavaScript targets
- Format checking
- Documentation build

### Code Coverage

Note: Gleam currently has limited support for code coverage tools. Since Gleam compiles to Erlang source (rather than abstract format), integration with Erlang's `cover` tool is challenging. We rely on comprehensive unit and property tests instead.

## License

MIT License - see [LICENSE](LICENSE) for details.
