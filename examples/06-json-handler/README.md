# 06 JSON Handler

This example demonstrates JSON output for log aggregation systems.

## What You'll Learn

- Default JSON handler
- Builder pattern for custom JSON format
- Adding custom fields (service, version, environment)
- JSON output structure

## Running the Example

```bash
gleam run                         # Erlang/BEAM
gleam run --target javascript     # JavaScript
```

## Default JSON Handler

The simplest way to get JSON output:

```gleam
import birch as log
import birch/handler/json

log.configure([
  log.config_handlers([json.handler()]),
])

log.info("Server started")
```

Output:
```json
{"timestamp":"2024-01-15T10:30:00.000Z","level":"info","logger":"app","message":"Server started"}
```

## Builder Pattern

Use the builder pattern to customize JSON output:

```gleam
import birch/handler/json
import gleam/json as gjson

let custom_handler =
  json.standard_builder()
  |> json.add_custom(fn(_record) {
    [
      #("service", gjson.string("my-api")),
      #("version", gjson.string("1.0.0")),
    ]
  })
  |> json.build()
  |> json.handler_with_formatter()
```

Output:
```json
{"timestamp":"...","level":"info","logger":"app","message":"Server started","service":"my-api","version":"1.0.0"}
```

## Builder Functions

| Function | Adds Field |
|----------|------------|
| `add_timestamp()` | `"timestamp": "2024-01-15T10:30:00.000Z"` |
| `add_level()` | `"level": "info"` |
| `add_logger()` | `"logger": "myapp"` |
| `add_message()` | `"message": "Your message"` |
| `add_metadata()` | All metadata key-value pairs |
| `add_custom(fn)` | Any custom fields you define |

## Starting Points

- `json.builder()` - Empty builder, add exactly what you need
- `json.standard_builder()` - Pre-configured with all standard fields

## Log Aggregation

JSON output works well with:
- Elasticsearch / OpenSearch
- Datadog
- Splunk
- CloudWatch Logs
- Any JSON-compatible log aggregator

## Next Steps

- [07-file-handler](../07-file-handler/) - File output with rotation
- [08-custom-handlers](../08-custom-handlers/) - Create your own handlers
