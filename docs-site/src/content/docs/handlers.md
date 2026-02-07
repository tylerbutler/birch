---
title: Handlers
description: Output handlers for console, JSON, file, and more
---

Handlers control where and how log messages are written. birch provides several built-in handlers and supports custom handlers.

## Console Handler

The default handler outputs to stdout with colors:

```gleam
import birch/handler/console

let handler = console.handler()
```

Configure output target and colors:

```gleam
import birch/handler
import birch/handler/console

let handler = console.handler_with_config(console.ConsoleConfig(
  color: True,
  target: handler.Stderr,  // or handler.Stdout
))
```

## JSON Handler

For log aggregation systems like Elasticsearch, Datadog, or Splunk:

```gleam
import birch/handler/json

let handler = json.handler()
```

Output:
```json
{"timestamp":"2024-12-26T10:30:45.123Z","level":"info","logger":"myapp","message":"Request complete","method":"POST","path":"/api/users"}
```

### Custom JSON Format

Use the builder pattern to customize JSON output:

```gleam
import birch/handler/json
import gleam/json as j

let custom_handler =
  json.standard_builder()
  |> json.add_custom(fn(_record) {
    [
      #("service", j.string("my-app")),
      #("version", j.string("1.0.0")),
    ]
  })
  |> json.build()
  |> json.handler_with_formatter()
```

## File Handler

Write to files with optional rotation:

### Size-Based Rotation

Rotate when files reach a size limit:

```gleam
import birch/handler/file

let handler = file.handler(file.FileConfig(
  path: "/var/log/myapp.log",
  rotation: file.SizeRotation(max_bytes: 10_000_000, max_files: 5),
))
```

### Time-Based Rotation

Rotate on a schedule:

```gleam
import birch/handler/file

let handler = file.handler(file.FileConfig(
  path: "/var/log/myapp.log",
  rotation: file.TimeRotation(interval: file.Daily, max_files: 7),
))
```

### Combined Rotation

Rotate based on size OR time:

```gleam
import birch/handler/file

let handler = file.handler(file.FileConfig(
  path: "/var/log/myapp.log",
  rotation: file.CombinedRotation(
    max_bytes: 50_000_000,
    interval: file.Daily,
    max_files: 10,
  ),
))
```

## Async Handler

Wrap any handler for non-blocking logging:

```gleam
import birch/handler/async
import birch/handler/console

let async_console =
  console.handler()
  |> async.make_async(async.default_config())
```

Configure queue size and overflow behavior:

```gleam
import birch/handler/async

let config =
  async.config()
  |> async.with_queue_size(5000)
  |> async.with_flush_interval(50)
  |> async.with_overflow(async.DropOldest)

let handler = async.make_async(console.handler(), config)

// Before shutdown, ensure all logs are written
async.flush()
```

## Null Handler

For testing or disabling logging:

```gleam
import birch/handler

let handler = handler.null()
```

## Custom Handlers

Create custom handlers with the handler interface:

```gleam
import birch/handler
import birch/formatter

let my_handler = handler.new(
  name: "custom",
  write: fn(message) {
    // Send to external service, database, etc.
  },
  format: formatter.human_readable,
)
```

### Error Callbacks

Handle errors from handlers without crashing:

```gleam
import birch/handler
import birch/handler/file

let handler =
  file.handler(config)
  |> handler.with_error_callback(fn(err) {
    io.println("Handler " <> err.handler_name <> " failed: " <> err.error)
  })
```

## Multiple Handlers

Send logs to multiple destinations:

```gleam
import birch as log
import birch/handler/console
import birch/handler/json
import birch/handler/file

pub fn main() {
  log.configure([
    log.config_handlers([
      console.handler(),           // Colored console output
      json.handler(),              // JSON to stdout for log aggregation
      file.handler(file_config),   // File with rotation
    ]),
  ])
}
```

## Platform Support

| Handler | Erlang | Node.js | Deno | Bun | Browser |
|---------|--------|---------|------|-----|---------|
| Console | Yes    | Yes     | Yes  | Yes | Yes     |
| JSON    | Yes    | Yes     | Yes  | Yes | Yes     |
| File    | Yes    | Yes     | Yes  | Yes | No      |
| Async   | Yes    | No      | No   | No  | No      |
| Null    | Yes    | Yes     | Yes  | Yes | Yes     |

The async handler requires OTP and is only available on the Erlang target. File handlers work on server-side JavaScript runtimes but not in browsers.

## Next Steps

- See the [API reference](https://hexdocs.pm/birch/) for complete handler documentation
- Check out the [examples](https://github.com/tylerbutler/birch/tree/main/examples) for more usage patterns
