//// JSON handler for structured log output.
////
//// Outputs log records as JSON objects, suitable for log aggregation systems.

import gleam/json
import gleam/list
import gleam_log/handler.{type Handler}
import gleam_log/internal/platform
import gleam_log/level
import gleam_log/record.{type LogRecord}

/// Create a JSON console handler.
/// Outputs JSON-formatted log records to stdout.
pub fn handler() -> Handler {
  handler.new(name: "json", write: platform.write_stdout, format: format_json)
}

/// Create a JSON handler that writes to stderr.
pub fn handler_stderr() -> Handler {
  handler.new(
    name: "json_stderr",
    write: platform.write_stderr,
    format: format_json,
  )
}

/// Format a log record as a JSON string.
pub fn format_json(record: LogRecord) -> String {
  // Build the base JSON object
  let base_fields = [
    #("timestamp", json.string(record.timestamp)),
    #("level", json.string(level.to_string_lowercase(record.level))),
    #("logger", json.string(record.logger_name)),
    #("message", json.string(record.message)),
  ]

  // Add metadata fields
  let metadata_fields =
    list.map(record.metadata, fn(pair) {
      let #(key, value) = pair
      #(key, json.string(value))
    })

  // Combine and encode
  list.append(base_fields, metadata_fields)
  |> json.object
  |> json.to_string
}
