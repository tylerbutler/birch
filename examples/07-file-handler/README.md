# 07 File Handler

This example demonstrates file output with rotation strategies.

## What You'll Learn

- Basic file handler
- Size-based rotation
- Time-based rotation
- Combined rotation strategies
- Compressed rotation

## Running the Example

```bash
gleam run                         # Erlang/BEAM
gleam run --target javascript     # JavaScript
```

## Basic File Handler

```gleam
import birch as log
import birch/handler/file

let handler = file.handler(file.FileConfig(
  path: "/var/log/myapp.log",
  rotation: file.NoRotation,
))

log.configure([log.config_handlers([handler])])
```

## Rotation Strategies

### No Rotation

File grows indefinitely:

```gleam
file.NoRotation
```

### Size-Based Rotation

Rotate when file exceeds a size limit:

```gleam
file.SizeRotation(
  max_bytes: 10_000_000,  // 10 MB
  max_files: 5,           // Keep 5 old files
)
```

Creates: `app.log`, `app.log.1`, `app.log.2`, ...

### Size-Based with Compression

Compress old log files to save space:

```gleam
file.SizeRotationCompressed(
  max_bytes: 10_000_000,
  max_files: 5,
  compress: True,
)
```

Creates: `app.log`, `app.log.1.gz`, `app.log.2.gz`, ...

### Time-Based Rotation

Rotate based on time intervals:

```gleam
file.TimeRotation(
  interval: file.Daily,   // Or file.Hourly
  max_files: 7,           // Keep 7 days
)
```

Creates: `app.log`, `app.log.2024-01-14`, `app.log.2024-01-13`, ...

### Combined Rotation

Rotate on size OR time, whichever comes first:

```gleam
file.CombinedRotation(
  max_bytes: 100_000_000,  // 100 MB
  interval: file.Daily,
  max_files: 30,
)
```

## Custom Formatter

Use JSON format for file logs:

```gleam
import birch/handler/file
import birch/handler/json

let handler = file.handler_with_formatter(
  file.FileConfig(path: "/var/log/app.log", rotation: file.NoRotation),
  json.format_json,
)
```

## Next Steps

- [08-custom-handlers](../08-custom-handlers/) - Create your own handlers
- [13-async-handler](../13-async-handler/) - Non-blocking file logging
