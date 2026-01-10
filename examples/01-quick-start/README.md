# 01 Quick Start

This example demonstrates zero-configuration logging with birch.

## What You'll Learn

- Importing birch as `log`
- Basic logging functions: `info()`, `debug()`, `warn()`, `error()`
- Default console output with colors
- Default log level filtering (Info)

## Running the Example

### Erlang/BEAM (default)

```bash
gleam run
```

### JavaScript

```bash
gleam run --target javascript
```

## Key Concepts

### Zero Configuration

birch works immediately without any setup. Just import and start logging:

```gleam
import birch as log

pub fn main() {
  log.info("Application starting")
}
```

### Default Log Level

The default log level is `Info`, which means:

- `trace()` and `debug()` messages are filtered out
- `info()`, `warn()`, `error()`, and `fatal()` messages are shown

This is intentional - debug messages are typically too verbose for production.

### Console Output

By default, birch outputs to the console with:

- Timestamps in ISO 8601 format
- Color-coded log levels (when running in a TTY)
- Logger name (defaults to "app")
- Your message

Example output:

```
2024-01-15T10:30:00.000Z | INFO  | app | Application starting
2024-01-15T10:30:00.001Z | WARN  | app | Something might be wrong
2024-01-15T10:30:00.002Z | ERROR | app | An error occurred
```

## Code Walkthrough

```gleam
import birch as log

pub fn main() {
  // This will appear - Info level and above are shown
  log.info("Application starting")

  // This won't appear - Debug is below the default Info level
  log.debug("This debug message is filtered")

  // These will appear
  log.warn("Something might be wrong")
  log.error("An error occurred")
  log.fatal("Critical failure")
}
```

## Next Steps

- [02-log-levels](../02-log-levels/) - Learn about all log levels and filtering
- [03-metadata](../03-metadata/) - Add structured data to your logs
- [04-named-loggers](../04-named-loggers/) - Organize logs by component
