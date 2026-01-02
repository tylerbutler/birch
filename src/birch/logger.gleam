//// Logger type and operations.
////
//// A Logger is a named logging context with an associated level, handlers,
//// and persistent metadata.

import birch/handler.{type Handler}
import birch/handler/console
import birch/internal/platform
import birch/level.{type Level}
import birch/record.{type Metadata}
import gleam/list

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
  )
}

/// Create a new logger with the given name.
/// Uses default level (Info) and console handler.
pub fn new(name: String) -> Logger {
  Logger(
    name: name,
    min_level: level.default(),
    handlers: [console.handler()],
    context: [],
  )
}

/// Create a logger with no handlers (silent by default).
/// Useful for library loggers that consumers can configure.
pub fn silent(name: String) -> Logger {
  Logger(name: name, min_level: level.default(), handlers: [], context: [])
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

/// Check if a log level should be logged by this logger.
pub fn should_log(logger: Logger, log_level: Level) -> Bool {
  level.gte(log_level, logger.min_level)
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
    True -> {
      // Merge metadata: call > scope > logger (first in list = highest priority)
      let scope_context = platform.get_scope_context()
      let merged_metadata =
        list.append(metadata, list.append(scope_context, logger.context))
      let record =
        record.new(
          timestamp: platform.timestamp_iso8601(),
          level: log_level,
          logger_name: logger.name,
          message: message,
          metadata: merged_metadata,
        )
      handler.handle_all(logger.handlers, record)
    }
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
    True -> {
      // Merge metadata: call > scope > logger (first in list = highest priority)
      let scope_context = platform.get_scope_context()
      let merged_metadata =
        list.append(metadata, list.append(scope_context, logger.context))
      let record =
        record.new(
          timestamp: platform.timestamp_iso8601(),
          level: log_level,
          logger_name: logger.name,
          message: message_fn(),
          metadata: merged_metadata,
        )
      handler.handle_all(logger.handlers, record)
    }
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
