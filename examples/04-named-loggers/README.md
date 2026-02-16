# 04 Named Loggers

This example demonstrates using named loggers to organize logs by component.

## What You'll Learn

- Creating named loggers with `log.new()`
- Using `birch/logger` module functions for named loggers
- Adding persistent context with `with_context()`
- Logger naming conventions

## Running the Example

```bash
gleam run                         # Erlang/BEAM
gleam run --target javascript     # JavaScript
```

## Creating Named Loggers

Use `log.new()` to create a named logger, then use `birch/logger` module functions:

```gleam
import birch as log
import birch/logger

let db_logger = log.new("myapp.database")
let http_logger = log.new("myapp.http")

// Use birch/logger module functions
db_logger |> logger.info("Connected", [])
http_logger |> logger.info("Server started", [])
```

## Logger-specific Functions

Named loggers use the `birch/logger` module:

| Default Logger | Named Logger |
|----------------|--------------|
| `log.info()` | `logger.info()` |
| `log.debug()` | `logger.debug()` |
| `log.error()` | `logger.error()` |
| etc. | etc. |

## Persistent Context

Add context that's included in all messages from a logger:

```gleam
let lgr =
  log.new("myapp.worker")
  |> log.with_context([#("worker_id", "worker-1")])

// All messages include worker_id
lgr |> logger.info("Processing", [])  // includes worker_id=worker-1
lgr |> logger.info("Complete", [])     // includes worker_id=worker-1
```

## Naming Conventions

Use dot notation for hierarchical names:

```
myapp                    # Root application logger
myapp.database           # Database component
myapp.database.pool      # Connection pool
myapp.http               # HTTP server
myapp.http.middleware    # HTTP middleware
```

This makes logs easy to filter and organize in log aggregation tools.

## Output Example

```
2024-01-15T10:30:00.000Z | INFO  | myapp.database | Connected to PostgreSQL | host=localhost
2024-01-15T10:30:00.001Z | INFO  | myapp.http | Server listening | port=8080
2024-01-15T10:30:00.500Z | DEBUG | myapp.database.pool | Connection acquired | pool_size=5
```

## Next Steps

- [05-console-handler](../05-console-handler/) - Customize console output
- [09-global-config](../09-global-config/) - Configure all loggers at once
- [12-scoped-context](../12-scoped-context/) - Request-scoped context
