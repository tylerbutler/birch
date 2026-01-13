# 05 Console Handler

This example demonstrates configuring the console handler.

## What You'll Learn

- Console handler with default settings
- Enabling/disabling colors
- Choosing output target (stdout vs stderr)
- TTY detection

## Running the Example

```bash
gleam run                         # Erlang/BEAM
gleam run --target javascript     # JavaScript
```

## Default Console Handler

The simplest way to use the console handler:

```gleam
import birch as log
import birch/handler/console

log.configure([
  log.config_handlers([console.handler()]),
])
```

This gives you:
- Colors enabled (if running in a TTY)
- Output to stdout
- Human-readable format

## Custom Configuration

Use `console.handler_with_config()` for more control:

```gleam
import birch/handler/console
import birch/handler

// No colors, output to stderr
let my_handler = console.handler_with_config(
  console.ConsoleConfig(color: False, target: handler.Stderr)
)
```

## Output Targets

| Target | Description |
|--------|-------------|
| `Stdout` | Standard output (default) |
| `Stderr` | Standard error |
| `StdoutWithStderr` | Errors to stderr, others to stdout |

## Color Support

Colors are automatically enabled when:
1. `color: True` in the config
2. Output is to a TTY (terminal)

When piping output to a file or another program, colors are automatically disabled.

## Next Steps

- [06-json-handler](../06-json-handler/) - JSON output for log aggregation
- [07-file-handler](../07-file-handler/) - File output with rotation
