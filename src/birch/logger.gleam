//// Logger type and operations.
////
//// A Logger is a named logging context with an associated level, handlers,
//// and persistent metadata.

import birch/handler.{type Handler}
import birch/handler/console
import birch/internal/time
import birch/level.{type Level}
import birch/record.{type Metadata}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import gleam/time/timestamp.{type Timestamp}

// Re-import platform for non-timestamp operations
import birch/internal/platform

/// A function that formats a timestamp into a string.
/// Used for custom timestamp formatting in log output.
pub type TimestampFormatter =
  fn(Timestamp) -> String

/// A function that provides timestamps.
/// Used for testing with deterministic timestamps.
pub type TimeProvider =
  fn() -> String

/// A logger instance with configuration and context.
pub opaque type Logger {
  Logger(
    /// Name of the logger (e.g., "myapp.database")
    name: String,
    /// Minimum level for this logger
    min_level: Level,
    /// Handlers that receive log records
    handlers: List(Handler),
    /// Persistent context metadata
    context: Metadata,
    /// Optional custom time provider (defaults to using gleam_time)
    time_provider: Option(TimeProvider),
    /// Optional custom timestamp formatter (defaults to ISO 8601)
    timestamp_formatter: Option(TimestampFormatter),
    /// Whether to capture caller process/thread ID on each log call
    capture_caller_id: Bool,
  )
}

/// Create a new logger with the given name.
/// Uses default level (Info) and console handler.
pub fn new(name: String) -> Logger {
  Logger(
    name: name,
    min_level: level.Info,
    handlers: [console.handler()],
    context: [],
    time_provider: None,
    timestamp_formatter: None,
    capture_caller_id: False,
  )
}

/// Create a logger with no handlers (silent by default).
/// Useful for library loggers that consumers can configure.
pub fn silent(name: String) -> Logger {
  Logger(
    name: name,
    min_level: level.Info,
    handlers: [],
    context: [],
    time_provider: None,
    timestamp_formatter: None,
    capture_caller_id: False,
  )
}

/// Get the name of a logger.
pub fn name(logger: Logger) -> String {
  logger.name
}

/// Set the minimum level for a logger.
pub fn with_level(logger: Logger, min_level: Level) -> Logger {
  Logger(..logger, min_level: min_level)
}

/// Get the minimum level of a logger.
pub fn get_level(logger: Logger) -> Level {
  logger.min_level
}

/// Add a handler to a logger.
pub fn with_handler(logger: Logger, handler: Handler) -> Logger {
  Logger(..logger, handlers: [handler, ..logger.handlers])
}

/// Replace all handlers on a logger.
pub fn with_handlers(logger: Logger, handlers: List(Handler)) -> Logger {
  Logger(..logger, handlers: handlers)
}

/// Get the handlers attached to a logger.
pub fn get_handlers(logger: Logger) -> List(Handler) {
  logger.handlers
}

/// Add persistent context metadata to a logger.
/// This metadata will be included in all log records from this logger.
pub fn with_context(logger: Logger, context: Metadata) -> Logger {
  Logger(..logger, context: list.append(context, logger.context))
}

/// Get the context metadata of a logger.
pub fn get_context(logger: Logger) -> Metadata {
  logger.context
}

/// Set a custom time provider for a logger.
///
/// This is primarily useful for testing, allowing deterministic timestamps
/// in test output.
///
/// ## Example
///
/// ```gleam
/// // For testing - fixed timestamp
/// let test_logger =
///   logger.new("test")
///   |> logger.with_time_provider(fn() { "2024-01-01T00:00:00.000Z" })
///
/// // For testing - incrementing counter
/// let counter = atomic.new(0)
/// let test_logger =
///   logger.new("test")
///   |> logger.with_time_provider(fn() {
///     let n = atomic.add(counter, 1)
///     "T" <> int.to_string(n)
///   })
/// ```
pub fn with_time_provider(logger: Logger, provider: TimeProvider) -> Logger {
  Logger(..logger, time_provider: Some(provider))
}

/// Clear the custom time provider, reverting to the default platform timestamp.
pub fn without_time_provider(logger: Logger) -> Logger {
  Logger(..logger, time_provider: None)
}

/// Set a custom timestamp formatter for a logger.
///
/// The formatter receives a `Timestamp` from `gleam_time` and returns a string.
/// By default, loggers use ISO 8601 format (e.g., "2024-12-26T10:30:45.123Z").
///
/// ## Example
///
/// ```gleam
/// import birch/logger
/// import gleam/int
/// import gleam/time/timestamp
/// import gleam/time/calendar
///
/// // Unix seconds format
/// let logger =
///   logger.new("myapp")
///   |> logger.with_custom_timestamp(fn(ts) {
///     let #(seconds, _nanos) = timestamp.to_unix_seconds_and_nanoseconds(ts)
///     int.to_string(seconds)
///   })
///
/// // Date only format
/// let logger =
///   logger.new("myapp")
///   |> logger.with_custom_timestamp(fn(ts) {
///     let #(date, _time) = timestamp.to_calendar(ts, calendar.utc_offset)
///     int.to_string(date.year) <> "-" <>
///     int.to_string(date.month |> month_to_int) <> "-" <>
///     int.to_string(date.day)
///   })
/// ```
pub fn with_custom_timestamp(
  logger: Logger,
  formatter: TimestampFormatter,
) -> Logger {
  Logger(..logger, timestamp_formatter: Some(formatter))
}

/// Clear the custom timestamp formatter, reverting to the default ISO 8601 format.
pub fn without_custom_timestamp(logger: Logger) -> Logger {
  Logger(..logger, timestamp_formatter: None)
}

/// Enable caller ID capture for this logger.
///
/// When enabled, log records will include the process/thread ID of the caller.
/// This is useful for debugging concurrent applications.
///
/// - On Erlang: Captures the PID (e.g., "<0.123.0>")
/// - On JavaScript: Captures "main", "pid-N", or "worker-N"
///
/// Note: This has a small performance cost (~1μs per log call) due to the
/// FFI call to get the process/thread ID.
///
/// ## Example
///
/// ```gleam
/// let logger =
///   logger.new("myapp")
///   |> logger.with_caller_id_capture()
/// ```
pub fn with_caller_id_capture(logger: Logger) -> Logger {
  Logger(..logger, capture_caller_id: True)
}

/// Disable caller ID capture for this logger.
pub fn without_caller_id_capture(logger: Logger) -> Logger {
  Logger(..logger, capture_caller_id: False)
}

/// Check if caller ID capture is enabled for this logger.
pub fn is_caller_id_capture_enabled(logger: Logger) -> Bool {
  logger.capture_caller_id
}

/// Get the current timestamp using the logger's time provider or formatter.
/// Falls back to ISO 8601 format if no custom provider or formatter is set.
fn get_timestamp(logger: Logger) -> String {
  case logger.time_provider {
    // Custom time provider takes precedence (for testing)
    Some(provider) -> provider()
    // Otherwise, use the timestamp formatter or default to ISO 8601
    None -> {
      let ts = time.now()
      case logger.timestamp_formatter {
        Some(formatter) -> formatter(ts)
        None -> time.to_iso8601(ts)
      }
    }
  }
}

/// Get the caller ID if capture is enabled.
fn get_optional_caller_id(logger: Logger) -> Option(String) {
  case logger.capture_caller_id {
    True -> Some(platform.get_caller_id())
    False -> None
  }
}

/// Check if a log level should be logged by this logger.
pub fn should_log(logger: Logger, log_level: Level) -> Bool {
  level.gte(log_level, logger.min_level)
}

/// Merge metadata from call, scope, and logger context.
/// Priority: call metadata > scope context > logger context.
fn merge_metadata(logger: Logger, call_metadata: Metadata) -> Metadata {
  let scope_context = platform.get_scope_context()
  list.append(call_metadata, list.append(scope_context, logger.context))
}

/// Create a log record and dispatch it to handlers.
fn emit_record(
  logger: Logger,
  log_level: Level,
  message: String,
  metadata: Metadata,
) -> Nil {
  let merged_metadata = merge_metadata(logger, metadata)
  let base_record =
    record.new(
      timestamp: get_timestamp(logger),
      level: log_level,
      logger_name: logger.name,
      message: message,
      metadata: merged_metadata,
    )
  let final_record = case get_optional_caller_id(logger) {
    Some(caller_id) -> record.with_caller_id(base_record, caller_id)
    None -> base_record
  }
  handler.handle_all(logger.handlers, final_record)
}

/// Log a message at the specified level.
///
/// Metadata is merged with the following priority (first wins):
/// 1. Call metadata (passed to this function)
/// 2. Scope context (from with_scope)
/// 3. Logger context (from with_context)
pub fn log(
  logger: Logger,
  log_level: Level,
  message: String,
  metadata: Metadata,
) -> Nil {
  case should_log(logger, log_level) {
    False -> Nil
    True -> emit_record(logger, log_level, message, metadata)
  }
}

/// Log a message using lazy evaluation.
/// The message function is only called if the level is enabled.
///
/// Metadata is merged with the following priority (first wins):
/// 1. Call metadata (passed to this function)
/// 2. Scope context (from with_scope)
/// 3. Logger context (from with_context)
pub fn log_lazy(
  logger: Logger,
  log_level: Level,
  message_fn: fn() -> String,
  metadata: Metadata,
) -> Nil {
  case should_log(logger, log_level) {
    False -> Nil
    True -> emit_record(logger, log_level, message_fn(), metadata)
  }
}

// Convenience functions for each log level

/// Log a trace message.
pub fn trace(logger: Logger, message: String, metadata: Metadata) -> Nil {
  log(logger, level.Trace, message, metadata)
}

/// Log a trace message with lazy evaluation.
pub fn trace_lazy(
  logger: Logger,
  message_fn: fn() -> String,
  metadata: Metadata,
) -> Nil {
  log_lazy(logger, level.Trace, message_fn, metadata)
}

/// Log a debug message.
pub fn debug(logger: Logger, message: String, metadata: Metadata) -> Nil {
  log(logger, level.Debug, message, metadata)
}

/// Log a debug message with lazy evaluation.
pub fn debug_lazy(
  logger: Logger,
  message_fn: fn() -> String,
  metadata: Metadata,
) -> Nil {
  log_lazy(logger, level.Debug, message_fn, metadata)
}

/// Log an info message.
pub fn info(logger: Logger, message: String, metadata: Metadata) -> Nil {
  log(logger, level.Info, message, metadata)
}

/// Log an info message with lazy evaluation.
pub fn info_lazy(
  logger: Logger,
  message_fn: fn() -> String,
  metadata: Metadata,
) -> Nil {
  log_lazy(logger, level.Info, message_fn, metadata)
}

/// Log a warning message.
pub fn warn(logger: Logger, message: String, metadata: Metadata) -> Nil {
  log(logger, level.Warn, message, metadata)
}

/// Log a warning message with lazy evaluation.
pub fn warn_lazy(
  logger: Logger,
  message_fn: fn() -> String,
  metadata: Metadata,
) -> Nil {
  log_lazy(logger, level.Warn, message_fn, metadata)
}

/// Log an error message.
pub fn error(logger: Logger, message: String, metadata: Metadata) -> Nil {
  log(logger, level.Err, message, metadata)
}

/// Log an error message with lazy evaluation.
pub fn error_lazy(
  logger: Logger,
  message_fn: fn() -> String,
  metadata: Metadata,
) -> Nil {
  log_lazy(logger, level.Err, message_fn, metadata)
}

/// Log a fatal message.
pub fn fatal(logger: Logger, message: String, metadata: Metadata) -> Nil {
  log(logger, level.Fatal, message, metadata)
}

/// Log a fatal message with lazy evaluation.
pub fn fatal_lazy(
  logger: Logger,
  message_fn: fn() -> String,
  metadata: Metadata,
) -> Nil {
  log_lazy(logger, level.Fatal, message_fn, metadata)
}

// ============================================================================
// Error Result Convenience Functions
// ============================================================================

/// Log an error message with an associated Result.
///
/// If the result is an Error, the error value is automatically included
/// in the metadata under the "error" key using `string.inspect`.
///
/// ## Example
///
/// ```gleam
/// case database.connect() {
///   Ok(conn) -> use_connection(conn)
///   Error(_) as result -> {
///     logger |> log.error_result("Database connection failed", result, [])
///   }
/// }
/// ```
pub fn error_result(
  logger: Logger,
  message: String,
  result: Result(a, e),
  metadata: Metadata,
) -> Nil {
  let error_metadata = extract_error_metadata(result)
  log(logger, level.Err, message, list.append(error_metadata, metadata))
}

/// Log a fatal message with an associated Result.
///
/// If the result is an Error, the error value is automatically included
/// in the metadata under the "error" key using `string.inspect`.
///
/// ## Example
///
/// ```gleam
/// case critical_init() {
///   Ok(state) -> run(state)
///   Error(_) as result -> {
///     logger |> log.fatal_result("Cannot start application", result, [])
///     panic as "Critical initialization failed"
///   }
/// }
/// ```
pub fn fatal_result(
  logger: Logger,
  message: String,
  result: Result(a, e),
  metadata: Metadata,
) -> Nil {
  let error_metadata = extract_error_metadata(result)
  log(logger, level.Fatal, message, list.append(error_metadata, metadata))
}

/// Extract error metadata from a Result.
/// Returns empty list for Ok, or [#("error", inspected_value)] for Error.
fn extract_error_metadata(result: Result(a, e)) -> Metadata {
  case result {
    Ok(_) -> []
    Error(e) -> [#("error", string.inspect(e))]
  }
}
