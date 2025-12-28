//// A modern, production-ready logging library for Gleam.
////
//// ## Quick Start
////
//// ```gleam
//// import gleam_log as log
////
//// pub fn main() {
////   log.info("Application starting")
////   log.debug("Debug info", [#("key", "value")])
//// }
//// ```
////
//// ## Named Loggers
////
//// ```gleam
//// let logger = log.new("myapp.database")
//// logger |> log.logger_info("Connected", [])
//// ```
////
//// ## Configuration
////
//// ```gleam
//// import gleam_log as log
//// import gleam_log/level
//// import gleam_log/handler/console
////
//// log.configure([
////   log.config_level(level.Debug),
////   log.config_handlers([console.handler()]),
//// ])
//// ```

import gleam/list
import gleam_log/config.{type ConfigOption, type GlobalConfig}
import gleam_log/handler.{type Handler}
import gleam_log/handler/console
import gleam_log/internal/platform
import gleam_log/level.{type Level}
import gleam_log/logger.{type Logger}
import gleam_log/record.{type Metadata}

// Re-export types for convenience
pub type LogLevel =
  Level

pub type LogHandler =
  Handler

pub type LogMetadata =
  Metadata

/// Global configuration for the default logger.
/// Re-exported from config module for convenience.
pub type Config =
  GlobalConfig

// ============================================================================
// Global Configuration API
// ============================================================================

/// Configure the global logging settings.
///
/// Example:
/// ```gleam
/// import gleam_log as log
/// import gleam_log/level
/// import gleam_log/handler/console
///
/// log.configure([
///   log.config_level(level.Debug),
///   log.config_handlers([console.handler()]),
///   log.config_context([#("app", "myapp")]),
/// ])
/// ```
pub fn configure(options: List(ConfigOption)) -> Nil {
  let current = get_config()
  let new_config = config.apply_options(current, options)
  platform.set_global_config(new_config)
}

/// Get the current global configuration.
/// Returns the configured settings, or defaults if not configured.
pub fn get_config() -> GlobalConfig {
  case platform.get_global_config() {
    Ok(cfg) -> cfg
    Error(Nil) -> default_config()
  }
}

/// Reset the global configuration to defaults.
pub fn reset_config() -> Nil {
  platform.clear_global_config()
}

// ============================================================================
// Runtime Level Changes
// ============================================================================

/// Set the global log level at runtime.
///
/// This changes the log level for all new log operations immediately.
/// Other configuration (handlers, context) is preserved.
///
/// Example:
/// ```gleam
/// import gleam_log as log
/// import gleam_log/level
///
/// // Enable debug logging for troubleshooting
/// log.set_level(level.Debug)
///
/// // Later, reduce verbosity
/// log.set_level(level.Warn)
/// ```
pub fn set_level(lvl: Level) -> Nil {
  let current = get_config()
  let new_config = config.with_level(current, lvl)
  platform.set_global_config(new_config)
}

/// Get the current global log level.
///
/// Returns the configured log level, or Info if not configured.
pub fn get_level() -> Level {
  config.get_level(get_config())
}

/// Create a configuration option to set the global log level.
pub fn config_level(lvl: Level) -> ConfigOption {
  config.level(lvl)
}

/// Create a configuration option to set the global handlers.
pub fn config_handlers(handlers: List(Handler)) -> ConfigOption {
  config.handlers(handlers)
}

/// Create a configuration option to set the default context metadata.
pub fn config_context(ctx: Metadata) -> ConfigOption {
  config.context(ctx)
}

/// Default configuration: Info level, console handler, no context.
pub fn default_config() -> GlobalConfig {
  config.GlobalConfig(
    level: level.Info,
    handlers: [console.handler()],
    context: [],
  )
}

// ============================================================================
// Default Logger Operations
// ============================================================================

/// The default logger used by module-level logging functions.
/// This logger reads its configuration from the global config.
fn default_logger() -> Logger {
  let cfg = get_config()
  logger.new("app")
  |> logger.with_level(cfg.level)
  |> logger.with_handlers(cfg.handlers)
  |> logger.with_context(cfg.context)
}

/// Create a new named logger.
///
/// The logger inherits the global configuration (level, handlers, context).
/// Named loggers allow you to organize logs by component:
/// ```gleam
/// let db_logger = log.new("myapp.database")
/// let http_logger = log.new("myapp.http")
/// ```
pub fn new(name: String) -> Logger {
  let cfg = get_config()
  logger.new(name)
  |> logger.with_level(cfg.level)
  |> logger.with_handlers(cfg.handlers)
  |> logger.with_context(cfg.context)
}

/// Create a silent logger (no handlers).
/// Useful for library code where the consumer controls logging.
pub fn silent(name: String) -> Logger {
  logger.silent(name)
}

/// Add context metadata to a logger.
/// This metadata will be included in all subsequent log messages.
pub fn with_context(logger: Logger, context: Metadata) -> Logger {
  logger.with_context(logger, context)
}

/// Set the minimum log level for a logger.
pub fn with_level(logger: Logger, min_level: Level) -> Logger {
  logger.with_level(logger, min_level)
}

/// Add a handler to a logger.
pub fn with_handler(lgr: Logger, handler: Handler) -> Logger {
  logger.with_handler(lgr, handler)
}

// ============================================================================
// Logger-specific Logging Functions
// ============================================================================

/// Log a message at the specified level using a specific logger.
pub fn logger_log(
  lgr: Logger,
  log_level: Level,
  message: String,
  metadata: Metadata,
) -> Nil {
  logger.log(lgr, log_level, message, metadata)
}

/// Log a trace message using a specific logger.
pub fn logger_trace(lgr: Logger, message: String, metadata: Metadata) -> Nil {
  logger.trace(lgr, message, metadata)
}

/// Log a debug message using a specific logger.
pub fn logger_debug(lgr: Logger, message: String, metadata: Metadata) -> Nil {
  logger.debug(lgr, message, metadata)
}

/// Log an info message using a specific logger.
pub fn logger_info(lgr: Logger, message: String, metadata: Metadata) -> Nil {
  logger.info(lgr, message, metadata)
}

/// Log a warning message using a specific logger.
pub fn logger_warn(lgr: Logger, message: String, metadata: Metadata) -> Nil {
  logger.warn(lgr, message, metadata)
}

/// Log an error message using a specific logger.
pub fn logger_error(lgr: Logger, message: String, metadata: Metadata) -> Nil {
  logger.error(lgr, message, metadata)
}

/// Log a fatal message using a specific logger.
pub fn logger_fatal(lgr: Logger, message: String, metadata: Metadata) -> Nil {
  logger.fatal(lgr, message, metadata)
}

// ============================================================================
// Simple Module-Level Logging Functions
// ============================================================================

/// Log a trace message using the default logger.
pub fn trace(message: String) -> Nil {
  logger.trace(default_logger(), message, [])
}

/// Log a trace message with metadata using the default logger.
pub fn trace_m(message: String, metadata: Metadata) -> Nil {
  logger.trace(default_logger(), message, metadata)
}

/// Log a debug message using the default logger.
pub fn debug(message: String) -> Nil {
  logger.debug(default_logger(), message, [])
}

/// Log a debug message with metadata using the default logger.
pub fn debug_m(message: String, metadata: Metadata) -> Nil {
  logger.debug(default_logger(), message, metadata)
}

/// Log an info message using the default logger.
pub fn info(message: String) -> Nil {
  logger.info(default_logger(), message, [])
}

/// Log an info message with metadata using the default logger.
pub fn info_m(message: String, metadata: Metadata) -> Nil {
  logger.info(default_logger(), message, metadata)
}

/// Log a warning message using the default logger.
pub fn warn(message: String) -> Nil {
  logger.warn(default_logger(), message, [])
}

/// Log a warning message with metadata using the default logger.
pub fn warn_m(message: String, metadata: Metadata) -> Nil {
  logger.warn(default_logger(), message, metadata)
}

/// Log an error message using the default logger.
pub fn error(message: String) -> Nil {
  logger.error(default_logger(), message, [])
}

/// Log an error message with metadata using the default logger.
pub fn error_m(message: String, metadata: Metadata) -> Nil {
  logger.error(default_logger(), message, metadata)
}

/// Log a fatal message using the default logger.
pub fn fatal(message: String) -> Nil {
  logger.fatal(default_logger(), message, [])
}

/// Log a fatal message with metadata using the default logger.
pub fn fatal_m(message: String, metadata: Metadata) -> Nil {
  logger.fatal(default_logger(), message, metadata)
}

// ============================================================================
// Lazy Evaluation Variants
// ============================================================================

/// Log a debug message with lazy evaluation using the default logger.
/// The message function is only called if debug level is enabled.
pub fn debug_lazy(message_fn: fn() -> String) -> Nil {
  logger.debug_lazy(default_logger(), message_fn, [])
}

/// Log an info message with lazy evaluation using the default logger.
pub fn info_lazy(message_fn: fn() -> String) -> Nil {
  logger.info_lazy(default_logger(), message_fn, [])
}

// ============================================================================
// Utility Functions
// ============================================================================

/// Get the current timestamp in ISO 8601 format.
/// Useful for custom formatting or external logging.
pub fn timestamp() -> String {
  platform.timestamp_iso8601()
}

/// Parse a string into a log level.
/// Case-insensitive. Returns Error for unrecognized strings.
pub fn level_from_string(s: String) -> Result(Level, Nil) {
  level.from_string(s)
}

/// Convert a log level to its string representation.
pub fn level_to_string(lvl: Level) -> String {
  level.to_string(lvl)
}
