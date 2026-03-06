//// Log levels for controlling which messages are emitted.
////
//// Levels are ordered by severity, following RFC 5424 (syslog) semantics
//// with an additional Trace level below Debug:
////
//// TRACE < DEBUG < INFO < NOTICE < WARN < ERR < CRITICAL < ALERT < FATAL
////
//// Messages at or above the configured threshold are logged; those below are filtered.

import gleam/int
import gleam/order.{type Order}
import gleam/string

/// Log level representing the severity of a log message.
///
/// Levels are ordered from least to most severe, following RFC 5424
/// (syslog) severity levels with an additional Trace level:
/// - `Trace`: Very detailed diagnostic information (no RFC 5424 equivalent)
/// - `Debug`: Debugging information useful during development
/// - `Info`: Normal operational messages (default threshold)
/// - `Notice`: Normal but significant conditions
/// - `Warn`: Warning conditions that might need attention
/// - `Err`: Error conditions that should be addressed
/// - `Critical`: Critical conditions — a subsystem has failed
/// - `Alert`: Action must be taken immediately
/// - `Fatal`: System is unusable (maps to RFC 5424 emergency)
///
/// Note: `Err` is used instead of `Error` to avoid conflict with the
/// `Result` type's `Error` constructor.
pub type Level {
  Trace
  Debug
  Info
  Notice
  Warn
  Err
  Critical
  Alert
  Fatal
}

/// Convert a log level to its integer representation.
/// Lower numbers are less severe.
pub fn to_int(level: Level) -> Int {
  case level {
    Trace -> 0
    Debug -> 1
    Info -> 2
    Notice -> 3
    Warn -> 4
    Err -> 5
    Critical -> 6
    Alert -> 7
    Fatal -> 8
  }
}

/// Parse a string into a log level.
/// Case-insensitive. Returns Error for unrecognized strings.
pub fn from_string(s: String) -> Result(Level, Nil) {
  case string.lowercase(s) {
    "trace" -> Ok(Trace)
    "debug" -> Ok(Debug)
    "info" -> Ok(Info)
    "notice" -> Ok(Notice)
    "warn" | "warning" -> Ok(Warn)
    "error" | "err" -> Ok(Err)
    "critical" -> Ok(Critical)
    "alert" -> Ok(Alert)
    "fatal" | "emergency" -> Ok(Fatal)
    _ -> Error(Nil)
  }
}

/// Convert a log level to its string representation.
pub fn to_string(level: Level) -> String {
  case level {
    Trace -> "TRACE"
    Debug -> "DEBUG"
    Info -> "INFO"
    Notice -> "NOTICE"
    Warn -> "WARN"
    Err -> "ERROR"
    Critical -> "CRITICAL"
    Alert -> "ALERT"
    Fatal -> "FATAL"
  }
}

/// Convert a log level to a lowercase string (for JSON output).
pub fn to_string_lowercase(level: Level) -> String {
  case level {
    Trace -> "trace"
    Debug -> "debug"
    Info -> "info"
    Notice -> "notice"
    Warn -> "warn"
    Err -> "error"
    Critical -> "critical"
    Alert -> "alert"
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
