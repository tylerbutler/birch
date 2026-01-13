# 09 Global Config

This example demonstrates application-wide logging configuration.

## What You'll Learn

- Using `configure()` to set global options
- Configuration options: level, handlers, context
- Runtime level changes with `set_level()`
- Resetting configuration with `reset_config()`

## Running the Example

```bash
gleam run                         # Erlang/BEAM
gleam run --target javascript     # JavaScript
```

## Basic Configuration

Use `configure()` with a list of options:

```gleam
import birch as log
import birch/level
import birch/handler/console
import birch/handler/json

log.configure([
  log.config_level(level.Debug),
  log.config_handlers([console.handler(), json.handler()]),
  log.config_context([#("app", "myapp"), #("version", "1.0.0")]),
])
```

## Configuration Options

| Option | Description |
|--------|-------------|
| `config_level(level)` | Set minimum log level |
| `config_handlers([...])` | Set list of handlers |
| `config_context([...])` | Set default metadata for all logs |
| `config_on_error(fn)` | Set global error callback |
| `config_sampling(config)` | Configure log sampling |

## Runtime Level Changes

Change the log level without reconfiguring everything:

```gleam
// Enable debug logging for troubleshooting
log.set_level(level.Debug)

// Later, reduce verbosity
log.set_level(level.Warn)
```

## Getting Current Configuration

```gleam
let config = log.get_config()
let current_level = log.get_level()
```

## Resetting Configuration

Return to default settings:

```gleam
log.reset_config()
```

Defaults are:
- Level: Info
- Handlers: Console with colors
- Context: Empty

## Next Steps

- [10-lazy-evaluation](../10-lazy-evaluation/) - Performance optimization
- [14-sampling](../14-sampling/) - Configure log sampling
