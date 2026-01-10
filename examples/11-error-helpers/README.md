# 11 Error Helpers

This example demonstrates convenient error logging with Result types.

## What You'll Learn

- Using `error_result()` and `fatal_result()`
- Automatic error value extraction
- Metadata variants `_m`
- Logger-specific variants

## Running the Example

```bash
gleam run                         # Erlang/BEAM
gleam run --target javascript     # JavaScript
```

## The Pattern

Instead of manually extracting error values:

```gleam
case file.read("config.json") {
  Ok(content) -> parse_config(content)
  Error(err) -> {
    log.error_m("Failed to read config", [#("error", string.inspect(err))])
    use_defaults()
  }
}
```

Use `error_result()` for cleaner code:

```gleam
case file.read("config.json") {
  Ok(content) -> parse_config(content)
  Error(_) as result -> {
    log.error_result("Failed to read config", result)
    use_defaults()
  }
}
```

## Available Functions

| Function | Description |
|----------|-------------|
| `error_result(message, result)` | Log error with Result |
| `error_result_m(message, result, metadata)` | With additional metadata |
| `fatal_result(message, result)` | Log fatal with Result |
| `fatal_result_m(message, result, metadata)` | With additional metadata |
| `logger_error_result(logger, message, result, metadata)` | For named loggers |
| `logger_fatal_result(logger, message, result, metadata)` | For named loggers |

## How It Works

When you pass an `Error(value)`, the value is automatically:
1. Converted to a string using `string.inspect()`
2. Added to metadata under the `"error"` key

When you pass an `Ok(value)`, the log still happens but without error metadata.

## Output Example

```
2024-01-15T10:30:00.000Z | ERROR | app | Failed to read config | error=Enoent
```

## Next Steps

- [12-scoped-context](../12-scoped-context/) - Request-scoped metadata
- [17-handler-errors](../17-handler-errors/) - Handle errors in handlers
