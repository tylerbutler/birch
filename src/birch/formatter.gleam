//// Log formatting utilities.
////
//// Formatters transform LogRecords into strings for output.

import birch/internal/platform
import birch/level
import birch/record.{type LogRecord}
import gleam/int
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

/// Format metadata with specific keys highlighted.
/// Keys in the highlight_keys list will be styled with bold and a unique
/// hash-based color for visual distinction.
pub fn format_metadata_with_bold(
  metadata: record.Metadata,
  highlight_keys: List(String),
  use_color: Bool,
) -> String {
  metadata
  |> list.map(fn(pair) {
    let #(key, value) = pair
    let formatted_kv = key <> "=" <> escape_value(value)
    case use_color && list.contains(highlight_keys, key) {
      True -> {
        let color = hash_color(key)
        let bold = "\u{001b}[1m"
        let reset = "\u{001b}[0m"
        bold <> color <> formatted_kv <> reset
      }
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

/// Get a color based on a simple hash of the input string.
/// Uses 256-color palette if terminal supports it, otherwise falls back to 6 basic colors.
fn hash_color(text: String) -> String {
  let hash =
    text
    |> string.to_utf_codepoints
    |> list.fold(0, fn(acc, cp) { acc + string.utf_codepoint_to_int(cp) })

  let color_depth = platform.get_color_depth()

  case color_depth >= 256 {
    True -> {
      // Use 256-color palette - pick from a range of nice, readable colors
      let color_index = { hash % 180 } + 38
      "\u{001b}[38;5;" <> int.to_string(color_index) <> "m"
    }
    False -> {
      // Fall back to basic 6-color palette
      case hash % 6 {
        0 -> "\u{001b}[36m"
        // cyan
        1 -> "\u{001b}[32m"
        // green
        2 -> "\u{001b}[33m"
        // yellow
        3 -> "\u{001b}[35m"
        // magenta
        4 -> "\u{001b}[34m"
        // blue
        _ -> "\u{001b}[31m"
        // red
      }
    }
  }
}
