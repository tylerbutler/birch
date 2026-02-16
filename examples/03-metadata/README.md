# 03 Metadata

This example demonstrates structured logging with key-value metadata.

## What You'll Learn

- Adding metadata using named loggers (`logger.info`, `logger.debug`, etc.)
- Metadata format: `List(#(String, String))`
- How metadata appears in logs
- Common metadata patterns

## Running the Example

```bash
gleam run                         # Erlang/BEAM
gleam run --target javascript     # JavaScript
```

## Adding Metadata

Use named loggers with the `birch/logger` module to add metadata:

```gleam
import birch as log
import birch/logger

// Simple message (no metadata)
log.info("User logged in")

// Message with metadata via named logger
let lgr = log.new("app")
lgr |> logger.info("User logged in", [
  #("user_id", "12345"),
  #("ip", "192.168.1.1"),
])
```

## Metadata Format

Metadata is a list of key-value tuples where both keys and values are strings:

```gleam
type Metadata = List(#(String, String))
```

This design ensures cross-platform compatibility and consistent serialization.

## Output Format

In the default console format, metadata appears after the message:

```
2024-01-15T10:30:00.000Z | INFO  | app | User logged in | user_id=12345, ip=192.168.1.1
```

In JSON format (see [06-json-handler](../06-json-handler/)), metadata becomes JSON fields:

```json
{"timestamp":"2024-01-15T10:30:00.000Z","level":"info","logger":"app","message":"User logged in","user_id":"12345","ip":"192.168.1.1"}
```

## Common Metadata Patterns

### Request Tracking

```gleam
let lgr = log.new("app")
lgr |> logger.info("Processing request", [
  #("request_id", request_id),
  #("method", "POST"),
  #("path", "/api/users"),
])
```

### Performance Metrics

```gleam
lgr |> logger.info("Query completed", [
  #("query", "SELECT * FROM users"),
  #("duration_ms", int.to_string(duration)),
  #("rows", int.to_string(row_count)),
])
```

### Error Context

```gleam
lgr |> logger.error("Failed to process payment", [
  #("order_id", order_id),
  #("amount", float.to_string(amount)),
  #("error_code", code),
])
```

## Next Steps

- [04-named-loggers](../04-named-loggers/) - Organize logs by component
- [06-json-handler](../06-json-handler/) - JSON output for log aggregation
