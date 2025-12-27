//// Log record representation.
////
//// A LogRecord captures all information about a single log event:
//// timestamp, level, logger name, message, and metadata.

import gleam/list
import gleam_log/level.{type Level}

/// Metadata is a list of key-value pairs attached to a log record.
/// Keys and values are both strings for simplicity and cross-target compatibility.
pub type Metadata =
  List(#(String, String))

/// A log record representing a single log event.
pub type LogRecord {
  LogRecord(
    /// When the log event occurred (ISO 8601 timestamp)
    timestamp: String,
    /// Severity level of the log event
    level: Level,
    /// Name of the logger that produced this record
    logger_name: String,
    /// The log message
    message: String,
    /// Key-value metadata attached to this record
    metadata: Metadata,
  )
}

/// Create a new log record with the given parameters.
pub fn new(
  timestamp timestamp: String,
  level level: Level,
  logger_name logger_name: String,
  message message: String,
  metadata metadata: Metadata,
) -> LogRecord {
  LogRecord(
    timestamp: timestamp,
    level: level,
    logger_name: logger_name,
    message: message,
    metadata: metadata,
  )
}

/// Create a log record with empty metadata.
pub fn new_simple(
  timestamp timestamp: String,
  level level: Level,
  logger_name logger_name: String,
  message message: String,
) -> LogRecord {
  LogRecord(
    timestamp: timestamp,
    level: level,
    logger_name: logger_name,
    message: message,
    metadata: [],
  )
}

/// Add metadata to a log record.
/// New metadata is prepended, allowing later entries to shadow earlier ones.
pub fn with_metadata(record: LogRecord, metadata: Metadata) -> LogRecord {
  LogRecord(..record, metadata: list.append(metadata, record.metadata))
}

/// Get a metadata value by key.
pub fn get_metadata(record: LogRecord, key: String) -> Result(String, Nil) {
  list.find_map(record.metadata, fn(pair) {
    case pair {
      #(k, v) if k == key -> Ok(v)
      _ -> Error(Nil)
    }
  })
}
