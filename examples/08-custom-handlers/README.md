# 08 Custom Handlers

This example demonstrates creating custom log handlers.

## What You'll Learn

- The handler interface
- Creating a handler with `handler.new()`
- Custom formatters
- Handler patterns (memory capture, HTTP, filtering)

## Running the Example

```bash
gleam run                         # Erlang/BEAM
gleam run --target javascript     # JavaScript
```

## Handler Interface

A handler needs three things:

1. **Name** - Identifier for the handler
2. **Write function** - `fn(String) -> Nil` that outputs the message
3. **Format function** - `fn(LogRecord) -> String` that formats the record

```gleam
import birch/handler
import birch/formatter

let my_handler = handler.new(
  name: "my-handler",
  write: fn(message) { io.println(message) },
  format: formatter.human_readable,
)
```

## Custom Formatter

Create a custom format:

```gleam
import birch/record.{type LogRecord}
import birch/level

fn my_format(record: LogRecord) -> String {
  level.to_string(record.level)
  <> ": "
  <> record.message
}

let handler = handler.new(
  name: "simple",
  write: fn(msg) { io.println(msg) },
  format: my_format,
)
```

## Common Patterns

### Memory Capture Handler

Capture logs in memory for testing or buffering:

```gleam
// See the example code for a full implementation
let capture_handler = create_capture_handler()
```

### HTTP Handler Skeleton

Send logs to a remote service:

```gleam
// Placeholder - real implementation would use HTTP client
let http_handler = handler.new(
  name: "http:https://logs.example.com",
  write: fn(message) {
    // Send to HTTP endpoint
  },
  format: json.format_json,
)
```

### Filtering Handler

Only handle specific log levels:

```gleam
import birch/handler

let errors_only =
  my_handler
  |> handler.with_min_level(level.Err)
```

## Next Steps

- [09-global-config](../09-global-config/) - Configure multiple handlers globally
- [17-handler-errors](../17-handler-errors/) - Handle errors in custom handlers
