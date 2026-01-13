# 12 Scoped Context

This example demonstrates request-scoped metadata propagation.

## What You'll Learn

- Using `with_scope()` for temporary context
- Nested scopes
- Platform availability checking
- Request tracing patterns

## Running the Example

```bash
gleam run                         # Erlang/BEAM
gleam run --target javascript     # JavaScript (Node.js for full support)
```

## The Pattern

Scoped context lets you add metadata that automatically applies to all logs within a block, without threading context through function calls:

```gleam
import birch as log

pub fn handle_request(request_id: String) {
  log.with_scope([#("request_id", request_id)], fn() {
    // All logs in this block include request_id
    log.info("Processing request")
    validate_input()
    process_data()
    log.info("Request complete")
  })
}
```

## Nested Scopes

Scopes can be nested, with inner scopes adding to outer context:

```gleam
log.with_scope([#("request_id", "123")], fn() {
  log.info("Processing")  // request_id=123

  log.with_scope([#("step", "validation")], fn() {
    log.info("Validating")  // request_id=123, step=validation
  })

  log.info("Done")  // request_id=123 (step is gone)
})
```

## Platform Support

| Platform | Support | Mechanism |
|----------|---------|-----------|
| Erlang/BEAM | Full | Process dictionary |
| Node.js | Full | AsyncLocalStorage |
| Deno/Bun | Limited | Stack-based (may not propagate to async) |
| Browser | Limited | Stack-based |

Check availability:

```gleam
if log.is_scoped_context_available() {
  log.info("Full scoped context support")
} else {
  log.warn("Limited scoped context - may not propagate to async")
}
```

## Getting Current Context

```gleam
let context = log.get_scope_context()
```

## Use Cases

- **Request tracing:** Add request ID to all logs in a request handler
- **User context:** Track user ID across operations
- **Correlation IDs:** Link related operations across services
- **Transaction context:** Track database transaction metadata

## Next Steps

- [04-named-loggers](../04-named-loggers/) - Permanent context with named loggers
- [13-async-handler](../13-async-handler/) - Non-blocking logging
