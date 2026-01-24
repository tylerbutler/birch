//// Log formatting utilities.
////
//// Formatters transform LogRecords into strings for output.

import birch/level
import birch/record.{type LogRecord}
import gleam/list
import gleam/string

/// Format metadata, excluding internal keys (prefixed with _).
/// Internal keys are used by the logging system for features like
/// semantic log styles and grouping, and should not be shown to users.
pub fn format_metadata_visible(metadata: record.Metadata) -> String {
  metadata
  |> list.filter(fn(pair) { !string.starts_with(pair.0, "_") })
  |> format_metadata()
}

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

  let metadata_part = case metadata_str {
    "" -> ""
    m -> " | " <> m
  }

  record.timestamp
  <> " | "
  <> level_str
  <> " | "
  <> record.logger_name
  <> " | "
  <> record.message
  <> metadata_part
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

/// Format metadata with specific keys bolded.
/// Keys in the bold_keys list will be wrapped in ANSI bold codes.
pub fn format_metadata_with_bold(
  metadata: record.Metadata,
  bold_keys: List(String),
  use_color: Bool,
) -> String {
  metadata
  |> list.map(fn(pair) {
    let #(key, value) = pair
    let formatted_kv = key <> "=" <> escape_value(value)
    case use_color && list.contains(bold_keys, key) {
      True -> "\u{001b}[1m" <> formatted_kv <> "\u{001b}[22m"
      False -> formatted_kv
    }
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
