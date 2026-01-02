//// Handler interface for log output destinations.
////
//// Handlers receive log records and write them to various destinations
//// (console, files, external services, etc.).

import birch/formatter
import birch/internal/platform
import birch/level.{type Level}
import birch/record.{type LogRecord}
import gleam/list
import gleam/option.{type Option, None, Some}

/// Output target for console handlers.
pub type OutputTarget {
  /// Write to standard output
  Stdout
  /// Write to standard error
  Stderr
  /// Write errors (level >= Error) to stderr, others to stdout
  StdoutWithStderr
}

/// Error details passed to error callbacks when a handler fails.
pub type HandlerError {
  HandlerError(
    /// Name of the handler that failed
    handler_name: String,
    /// Error message describing what went wrong
    error: String,
    /// The log record that was being processed when the error occurred
    record: LogRecord,
  )
}

/// Callback function invoked when a handler encounters an error.
pub type ErrorCallback =
  fn(HandlerError) -> Nil

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
    /// Optional callback invoked when the handler encounters an error
    error_callback: Option(ErrorCallback),
  )
}

/// Create a new handler with a custom write function.
pub fn new(
  name name: String,
  write write: fn(String) -> Nil,
  format format: formatter.Formatter,
) -> Handler {
  Handler(
    name: name,
    min_level: Error(Nil),
    write: fn(record) { write(format(record)) },
    format: format,
    error_callback: None,
  )
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
///
/// If the handler has an error callback attached and the write fails,
/// the error callback will be invoked with details about the failure.
/// Handler failures never crash the application.
pub fn handle(handler: Handler, record: LogRecord) -> Nil {
  case should_handle(handler, record.level) {
    True -> {
      // Use safe_call to catch any errors from the write function
      case platform.safe_call(fn() { handler.write(record) }) {
        Ok(Nil) -> Nil
        Error(error_msg) -> {
          // If there's an error callback, invoke it
          case handler.error_callback {
            Some(callback) -> {
              let err =
                HandlerError(
                  handler_name: handler.name,
                  error: error_msg,
                  record: record,
                )
              callback(err)
            }
            None -> Nil
          }
        }
      }
    }
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
  Handler(
    name: "null",
    min_level: Error(Nil),
    write: fn(_) { Nil },
    format: fn(_) { "" },
    error_callback: None,
  )
}

/// Create a handler with a raw write function that receives the full LogRecord.
/// This is used by async handlers that need access to the full record.
pub fn new_with_record_write(
  name name: String,
  write write: fn(LogRecord) -> Nil,
) -> Handler {
  Handler(
    name: name,
    min_level: Error(Nil),
    write: write,
    format: fn(_) { "" },
    error_callback: None,
  )
}

/// Attach an error callback to a handler.
///
/// The callback will be invoked when the handler encounters an error
/// during write operations. This allows for monitoring and alerting
/// on handler failures without crashing the application.
///
/// Example:
/// ```gleam
/// let handler =
///   console.handler()
///   |> handler.with_error_callback(fn(err) {
///     io.println("Handler failed: " <> err.error)
///   })
/// ```
pub fn with_error_callback(handler: Handler, callback: ErrorCallback) -> Handler {
  Handler(..handler, error_callback: Some(callback))
}

/// Get the error callback from a handler, if one is set.
pub fn get_error_callback(handler: Handler) -> Option(ErrorCallback) {
  handler.error_callback
}
