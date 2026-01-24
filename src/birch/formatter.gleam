//// Log formatting utilities.
////
//// Formatters transform LogRecords into strings for output.

import birch/level
import birch/record.{type LogRecord}
import gleam/list
import gleam/string

/// A formatter is a function that converts a LogRecord to a string.
pub type Formatter =
  fn(LogRecord) -> String

/// Format a log record in a human-readable format.
///
/// Output format:
/// ```
/// 2024-12-26T10:30:45.123Z | INFO  | myapp.http | Request complete | key=value
/// ```
pub fn human_readable(record: LogRecord) -> String {
  let level_str = level.to_string(record.level) |> pad_level
  let metadata_str = format_metadata(record.metadata)

  case metadata_str {
    "" ->
      record.timestamp
      <> " | "
      <> level_str
      <> " | "
      <> record.logger_name
      <> " | "
      <> record.message
    _ ->
      record.timestamp
      <> " | "
      <> level_str
      <> " | "
      <> record.logger_name
      <> " | "
      <> record.message
      <> " | "
      <> metadata_str
  }
}

/// Format a log record as a simple message with level prefix.
///
/// Output format:
/// ```
/// [INFO] Request complete
/// ```
pub fn simple(record: LogRecord) -> String {
  "[" <> level.to_string(record.level) <> "] " <> record.message
}

/// Pad a level string to 5 characters for alignment.
/// Uses pattern matching on known log level strings for efficiency.
pub fn pad_level(level_str: String) -> String {
  case level_str {
    "TRACE" | "DEBUG" | "ERROR" | "FATAL" -> level_str
    "INFO" | "WARN" -> level_str <> " "
    _ -> level_str
  }
}

/// Format metadata as key=value pairs separated by spaces.
pub fn format_metadata(metadata: record.Metadata) -> String {
  metadata
  |> list.map(fn(pair) {
    let #(key, value) = pair
    key <> "=" <> escape_value(value)
  })
  |> string.join(" ")
}

/// Escape a value if it contains spaces or special characters.
fn escape_value(value: String) -> String {
  case string.contains(value, " ") || string.contains(value, "=") {
    True -> "\"" <> value <> "\""
    False -> value
  }
}
