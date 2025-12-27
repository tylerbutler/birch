//// Handler interface for log output destinations.
////
//// Handlers receive log records and write them to various destinations
//// (console, files, external services, etc.).

import gleam/list
import gleam_log/formatter
import gleam_log/level.{type Level}
import gleam_log/record.{type LogRecord}

/// Output target for console handlers.
pub type OutputTarget {
  /// Write to standard output
  Stdout
  /// Write to standard error
  Stderr
  /// Write errors (level >= Error) to stderr, others to stdout
  StdoutWithStderr
}

/// A handler that processes log records.
pub opaque type Handler {
  Handler(
    /// Name of the handler for identification
    name: String,
    /// Minimum level for this handler (optional override)
    min_level: Result(Level, Nil),
    /// The function that writes log records
    write: fn(LogRecord) -> Nil,
    /// Formatter to use
    format: formatter.Formatter,
  )
}

/// Create a new handler with a custom write function.
pub fn new(
  name name: String,
  write write: fn(String) -> Nil,
  format format: formatter.Formatter,
) -> Handler {
  Handler(name: name, min_level: Error(Nil), write: fn(record) {
    write(format(record))
  }, format: format)
}

/// Create a handler with a minimum level filter.
pub fn with_min_level(handler: Handler, level: Level) -> Handler {
  Handler(..handler, min_level: Ok(level))
}

/// Get the name of a handler.
pub fn name(handler: Handler) -> String {
  handler.name
}

/// Check if a handler should process a log record at the given level.
pub fn should_handle(handler: Handler, record_level: Level) -> Bool {
  case handler.min_level {
    Ok(min) -> level.gte(record_level, min)
    Error(Nil) -> True
  }
}

/// Write a log record to a handler.
pub fn handle(handler: Handler, record: LogRecord) -> Nil {
  case should_handle(handler, record.level) {
    True -> handler.write(record)
    False -> Nil
  }
}

/// Write a log record to multiple handlers.
pub fn handle_all(handlers: List(Handler), record: LogRecord) -> Nil {
  list.each(handlers, fn(h) { handle(h, record) })
}

/// Create a null handler that discards all logs.
/// Useful for testing or explicitly disabling logging.
pub fn null() -> Handler {
  Handler(name: "null", min_level: Error(Nil), write: fn(_) { Nil }, format: fn(
    _,
  ) { "" })
}
