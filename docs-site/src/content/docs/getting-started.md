---
title: Getting Started
description: Install birch and start logging in minutes
---

## Installation

Add `birch` to your `gleam.toml`:

```toml
[dependencies]
birch = ">= 0.1.0"
```

Then download dependencies:

```bash
gleam deps download
```

## Quick Start

Import birch and start logging immediately:

```gleam
import birch as log

pub fn main() {
  log.info("Application starting")
  log.debug("Debug message")
  log.error("Something went wrong")
}
```

## Structured Logging

Attach metadata to your log messages using the `_m` suffix functions:

```gleam
import birch as log

pub fn handle_request(user_id: String, path: String) {
  log.info_m("Request received", [
    #("user_id", user_id),
    #("path", path),
  ])

  // ... handle request ...

  log.info_m("Request complete", [
    #("user_id", user_id),
    #("status", "200"),
    #("duration_ms", "45"),
  ])
}
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

Add persistent context that's included in all logs from a logger:

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

## Log Levels

Six log levels are supported, from least to most severe:

| Level   | Use Case                                        |
|---------|-------------------------------------------------|
| `Trace` | Very detailed diagnostic information            |
| `Debug` | Debugging information during development        |
| `Info`  | Normal operational messages (default)           |
| `Warn`  | Warning conditions that might need attention    |
| `Error` | Error conditions that should be addressed       |
| `Fatal` | Critical errors preventing continuation         |

Set the minimum level for a logger:

```gleam
import birch as log
import birch/level

let logger = log.new("myapp")
  |> log.with_level(level.Debug)  // Log Debug and above
```

## Global Configuration

Configure the default logger with custom settings:

```gleam
import birch as log
import birch/level
import birch/handler/console
import birch/handler/json

pub fn main() {
  log.configure([
    log.config_level(level.Debug),
    log.config_handlers([console.handler(), json.handler()]),
    log.config_context([#("app", "myapp"), #("env", "production")]),
  ])

  // All logs now include the context and go to both handlers
  log.info("Server starting")
}
```

## Runtime Level Changes

Change the log level at runtime without reconfiguring everything:

```gleam
import birch as log
import birch/level

// Enable debug logging for troubleshooting
log.set_level(level.Debug)

// Later, reduce verbosity
log.set_level(level.Warn)
```

## Next Steps

- Learn about [handlers](/handlers/) for console, JSON, file output, and more
- Browse the [API reference](https://hexdocs.pm/birch/) for complete documentation
