# 17 Handler Errors

This example demonstrates error handling strategies for logging handlers.

## What You'll Learn

- Using `with_error_callback()` for handler-level errors
- Using `config_on_error()` for global error handling
- Graceful degradation strategies
- Building resilient logging pipelines

## Running the Example

```bash
gleam run                         # Erlang/BEAM
gleam run --target javascript     # JavaScript
```

## Handler Error Callbacks

Attach error callbacks to individual handlers:

```gleam
import birch as log
import birch/handler

let my_handler =
  handler.new(
    name: "risky",
    write: fn(msg) { /* might fail */ },
    format: formatter.simple,
  )
  |> handler.with_error_callback(fn(error) {
    // Log to stderr, notify monitoring, etc.
    io.println_error("Handler failed: " <> error)
  })
```

## Global Error Handling

Configure error handling for all handlers:

```gleam
log.configure([
  log.config_on_error(fn(handler_name, error) {
    // Called when any handler fails
    io.println_error(handler_name <> " error: " <> error)
  }),
])
```

## Graceful Degradation

### Pattern 1: Fallback Handler

When primary handler fails, fall back to console:

```gleam
fn create_resilient_handler() {
  let primary = file.handler("app.log", file.SizeRotation(10_000_000, 5))
  let fallback = console.handler()

  handler.new(
    name: "resilient",
    write: fn(msg) {
      case try_write_primary(primary, msg) {
        Ok(_) -> Nil
        Error(_) -> write_fallback(fallback, msg)
      }
    },
    format: formatter.human_readable,
  )
}
```

### Pattern 2: Silent Degradation

Continue even if logging fails:

```gleam
let safe_handler =
  my_handler
  |> handler.with_error_callback(fn(_error) {
    // Silently ignore errors - logging shouldn't crash the app
    Nil
  })
```

### Pattern 3: Error Aggregation

Collect and report errors periodically:

```gleam
// Pseudocode - actual implementation depends on your needs
let errors = []

let monitoring_handler =
  my_handler
  |> handler.with_error_callback(fn(error) {
    // Append to errors list
    // Periodically send to monitoring system
  })
```

## Error Types

Common handler errors include:

- **File errors**: Disk full, permission denied, file locked
- **Network errors**: Connection refused, timeout (for remote handlers)
- **Format errors**: Invalid data for JSON encoding

## Best Practices

1. **Always have a fallback**: Console output rarely fails
2. **Don't log the error with the same handler**: Avoid infinite loops
3. **Rate limit error reporting**: Don't spam error notifications
4. **Consider async error handling**: Don't block logging on error callbacks

## Next Steps

- [08-custom-handlers](../08-custom-handlers/) - Build custom handlers
- [13-async-handler](../13-async-handler/) - Async error handling patterns
