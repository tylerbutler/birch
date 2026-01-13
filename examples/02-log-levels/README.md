# 02 Log Levels

This example demonstrates birch's six log levels and how filtering works.

## What You'll Learn

- All six log levels: Trace, Debug, Info, Warn, Err, Fatal
- How level filtering works
- Parsing levels from strings with `level.from_string()`
- Changing log level at runtime with `set_level()`

## Running the Example

```bash
gleam run                         # Erlang/BEAM
gleam run --target javascript     # JavaScript
```

## The Six Log Levels

Levels are ordered from least to most severe:

| Level | Use Case |
|-------|----------|
| `Trace` | Very detailed diagnostic info (function entry/exit, variable values) |
| `Debug` | Debugging information useful during development |
| `Info` | Normal operational messages (startup, shutdown, milestones) |
| `Warn` | Warning conditions that might need attention |
| `Err` | Error conditions that should be addressed |
| `Fatal` | Critical errors that prevent the system from continuing |

**Note:** `Err` is used instead of `Error` to avoid conflict with Gleam's `Result` type.

## Level Filtering

When you set a log level, all messages at that level **and above** are shown:

```
set_level(Info)  ->  Shows: Info, Warn, Err, Fatal
set_level(Debug) ->  Shows: Debug, Info, Warn, Err, Fatal
set_level(Trace) ->  Shows: Everything
set_level(Warn)  ->  Shows: Warn, Err, Fatal
```

The default level is `Info`.

## Parsing Levels from Strings

Useful for configuration from environment variables or config files:

```gleam
import birch/level

level.from_string("debug")   // Ok(Debug)
level.from_string("DEBUG")   // Ok(Debug) - case insensitive
level.from_string("warning") // Ok(Warn) - "warning" also works
level.from_string("invalid") // Error(Nil)
```

## Changing Level at Runtime

```gleam
import birch as log
import birch/level

// Enable debug logging for troubleshooting
log.set_level(level.Debug)

// Later, reduce verbosity
log.set_level(level.Warn)
```

## Next Steps

- [03-metadata](../03-metadata/) - Add structured data to your logs
- [04-named-loggers](../04-named-loggers/) - Organize logs by component
