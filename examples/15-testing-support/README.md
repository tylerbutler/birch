# 15 Testing Support

This example demonstrates utilities for testing logging behavior.

## What You'll Learn

- Using `with_time_provider()` for deterministic timestamps
- Using `with_caller_id_capture()` for debugging
- Using `handler.null()` for silent testing

## Running the Example

```bash
gleam run                         # Erlang/BEAM
gleam run --target javascript     # JavaScript
```

## Fixed Timestamps

Use `with_time_provider()` for deterministic test output:

```gleam
import birch as log

let test_logger =
  log.new("test")
  |> log.with_time_provider(fn() { "2024-01-01T00:00:00.000Z" })

// All logs have the fixed timestamp
test_logger |> log.logger_info("Test message", [])
```

## Caller ID Capture

Track which process/thread is logging:

```gleam
let debug_logger =
  log.new("debug")
  |> log.with_caller_id_capture()

// Log output includes caller_id metadata
// Erlang: "<0.123.0>"
// JavaScript: "main" or "pid-N"
```

## Null Handler

Completely silence logging in tests:

```gleam
import birch/handler

log.configure([
  log.config_handlers([handler.null()]),
])

// No output at all
log.info("This is silenced")
```

## Testing Patterns

### Pattern 1: Capture Handler

Create a handler that stores logs for assertions:

```gleam
// In test setup
let captured = []  // Use a mutable reference in real code
let capture_handler = handler.new(
  name: "capture",
  write: fn(msg) { /* append to captured */ },
  format: formatter.simple,
)
```

### Pattern 2: Test-Specific Logger

Create loggers for specific test scenarios:

```gleam
fn test_logger() -> Logger {
  log.new("test")
  |> log.with_time_provider(fn() { "FIXED" })
  |> log.with_context([#("test", "true")])
}
```

### Pattern 3: Silent Testing

Prevent log noise in test output:

```gleam
pub fn setup() {
  log.configure([log.config_handlers([handler.null()])])
}

pub fn teardown() {
  log.reset_config()
}
```

## Next Steps

- [08-custom-handlers](../08-custom-handlers/) - Build custom test handlers
- [04-named-loggers](../04-named-loggers/) - Logger configuration
