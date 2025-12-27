//// Log levels for controlling which messages are emitted.
////
//// Levels are ordered by severity: TRACE < DEBUG < INFO < WARN < ERROR < FATAL
////
//// Messages at or above the configured threshold are logged; those below are filtered.

import gleam/int
import gleam/order.{type Order}
import gleam/string

/// Log level representing the severity of a log message.
///
/// Levels are ordered from least to most severe:
/// - `Trace`: Very detailed diagnostic information
/// - `Debug`: Debugging information useful during development
/// - `Info`: Normal operational messages (default threshold)
/// - `Warn`: Warning conditions that might need attention
/// - `Err`: Error conditions that should be addressed
/// - `Fatal`: Critical errors that prevent the system from continuing
///
/// Note: `Err` is used instead of `Error` to avoid conflict with the
/// `Result` type's `Error` constructor.
pub type Level {
  Trace
  Debug
  Info
  Warn
  Err
  Fatal
}

/// Convert a log level to its integer representation.
/// Lower numbers are less severe.
pub fn to_int(level: Level) -> Int {
  case level {
    Trace -> 0
    Debug -> 1
    Info -> 2
    Warn -> 3
    Err -> 4
    Fatal -> 5
  }
}

/// Parse a string into a log level.
/// Case-insensitive. Returns Error for unrecognized strings.
pub fn from_string(s: String) -> Result(Level, Nil) {
  case string.lowercase(s) {
    "trace" -> Ok(Trace)
    "debug" -> Ok(Debug)
    "info" -> Ok(Info)
    "warn" | "warning" -> Ok(Warn)
    "error" | "err" -> Ok(Err)
    "fatal" | "critical" -> Ok(Fatal)
    _ -> Error(Nil)
  }
}

/// Convert a log level to its string representation.
pub fn to_string(level: Level) -> String {
  case level {
    Trace -> "TRACE"
    Debug -> "DEBUG"
    Info -> "INFO"
    Warn -> "WARN"
    Err -> "ERROR"
    Fatal -> "FATAL"
  }
}

/// Convert a log level to a lowercase string (for JSON output).
pub fn to_string_lowercase(level: Level) -> String {
  case level {
    Trace -> "trace"
    Debug -> "debug"
    Info -> "info"
    Warn -> "warn"
    Err -> "error"
    Fatal -> "fatal"
  }
}

/// Compare two log levels.
/// Returns the ordering relationship between them.
pub fn compare(a: Level, b: Level) -> Order {
  int.compare(to_int(a), to_int(b))
}

/// Check if the first level is at least as severe as the second.
/// Used for filtering: `should_log(message_level, threshold)`.
pub fn gte(a: Level, b: Level) -> Bool {
  to_int(a) >= to_int(b)
}

/// Check if the first level is more severe than the second.
pub fn gt(a: Level, b: Level) -> Bool {
  to_int(a) > to_int(b)
}

/// Check if the first level is less severe than the second.
pub fn lt(a: Level, b: Level) -> Bool {
  to_int(a) < to_int(b)
}

/// Check if the first level is at most as severe as the second.
pub fn lte(a: Level, b: Level) -> Bool {
  to_int(a) <= to_int(b)
}

/// Get the default log level (Info).
pub fn default() -> Level {
  Info
}
