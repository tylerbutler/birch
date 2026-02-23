//// Log formatting utilities.
////
//// Formatters transform LogRecords into strings for output.

import birch/internal/ansi
import birch/internal/platform
import birch/level
import birch/record.{type LogRecord}
import gleam/int
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
  format_metadata_colored(metadata, False)
}

/// Format metadata with color support.
/// Each key gets a unique hash-based color when use_color is True.
pub fn format_metadata_colored(
  metadata: record.Metadata,
  use_color: Bool,
) -> String {
  metadata
  |> list.map(fn(pair) {
    let #(key, value) = pair
    let value_str = record.metadata_value_to_string(value)
    let formatted_kv = key <> "=" <> escape_value(value_str)
    case use_color {
      True -> {
        let color = hash_color(key)
        color <> formatted_kv <> ansi.reset
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
pub fn hash_color(text: String) -> String {
  let hash =
    text
    |> string.to_utf_codepoints
    |> list.fold(0, fn(acc, cp) { acc + string.utf_codepoint_to_int(cp) })

  let color_depth = platform.get_color_depth()

  case color_depth >= 256 {
    True -> {
      let color_index = { hash % 180 } + 38
      "\u{001b}[38;5;" <> int.to_string(color_index) <> "m"
    }
    False -> {
      case hash % 6 {
        0 -> ansi.cyan
        1 -> ansi.green
        2 -> ansi.yellow
        3 -> ansi.magenta
        4 -> ansi.blue
        _ -> ansi.red
      }
    }
  }
}
