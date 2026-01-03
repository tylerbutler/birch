//// A logging library for Gleam with cross-platform support.
////
//// The name "birch" comes from birch trees, whose white bark gleams in the light.
////
//// ## Quick Start
////
//// ```gleam
//// import birch as log
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
//// import birch as log
////
//// let logger = log.new("myapp.database")
//// logger |> log.logger_info("Connected", [])
//// ```
////
//// ## Configuration
////
//// ```gleam
//// import birch as log
//// import birch/level
//// import birch/handler/console
////
//// log.configure([
////   log.config_level(level.Debug),
////   log.config_handlers([console.handler()]),
//// ])
//// ```

import birch/config.{type ConfigOption, type GlobalConfig, type SampleConfig}
import birch/handler.{type ErrorCallback, type Handler}
import birch/handler/console
import birch/internal/platform
import birch/level.{type Level}
import birch/logger.{type Logger}
import birch/record.{type Metadata}
import birch/sampling
import birch/scope
import gleam/option.{None}

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
/// import birch as log
/// import birch/level
/// import birch/handler/console
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
  config.set_global_config(new_config)
}

/// Get the current global configuration.
/// Returns the configured settings, or defaults if not configured.
pub fn get_config() -> GlobalConfig {
  case config.get_global_config() {
    Ok(cfg) -> cfg
    Error(Nil) -> default_config()
  }
}

/// Reset the global configuration to defaults.
pub fn reset_config() -> Nil {
  config.clear_global_config()
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
/// import birch as log
/// import birch/level
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
  config.set_global_config(new_config)
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

/// Create a configuration option to set the global error callback.
///
/// This callback is invoked when any handler encounters an error.
/// It's useful for monitoring and alerting on handler failures.
///
/// Example:
/// ```gleam
/// import birch as log
///
/// log.configure([
///   log.config_on_error(fn(err) {
///     io.println("Handler " <> err.handler_name <> " failed: " <> err.error)
///   }),
/// ])
/// ```
pub fn config_on_error(callback: ErrorCallback) -> ConfigOption {
  config.on_error(callback)
}

/// Create a configuration option to set sampling.
///
/// Example:
/// ```gleam
/// import birch as log
/// import birch/level
/// import birch/sampling
///
/// // Log only 10% of debug messages
/// log.configure([
///   log.config_sampling(sampling.config(level.Debug, 0.1)),
/// ])
/// ```
pub fn config_sampling(sample_config: SampleConfig) -> ConfigOption {
  config.sampling(sample_config)
}

/// Default configuration: Info level, console handler, no context, no error callback, no sampling.
pub fn default_config() -> GlobalConfig {
  config.GlobalConfig(
    level: level.Info,
    handlers: [console.handler()],
    context: [],
    on_error: None,
    sampling: Error(Nil),
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

/// Set a custom time provider for a logger.
///
/// This is primarily useful for testing, allowing deterministic timestamps.
///
/// ## Example
///
/// ```gleam
/// import birch as log
///
/// // For testing - fixed timestamp
/// let test_logger =
///   log.new("test")
///   |> log.with_time_provider(fn() { "2024-01-01T00:00:00.000Z" })
/// ```
pub fn with_time_provider(lgr: Logger, provider: fn() -> String) -> Logger {
  logger.with_time_provider(lgr, provider)
}

/// Clear the custom time provider, reverting to the default platform timestamp.
pub fn without_time_provider(lgr: Logger) -> Logger {
  logger.without_time_provider(lgr)
}

/// Enable caller ID capture for a logger.
///
/// When enabled, log records will include the process/thread ID of the caller.
/// This is useful for debugging concurrent applications.
///
/// - On Erlang: Captures the PID (e.g., "<0.123.0>")
/// - On JavaScript: Captures "main", "pid-N", or "worker-N"
///
/// ## Example
///
/// ```gleam
/// import birch as log
///
/// // Enable caller ID capture for debugging concurrent code
/// let logger =
///   log.new("myapp.worker")
///   |> log.with_caller_id_capture()
/// ```
pub fn with_caller_id_capture(lgr: Logger) -> Logger {
  logger.with_caller_id_capture(lgr)
}

/// Disable caller ID capture for a logger.
pub fn without_caller_id_capture(lgr: Logger) -> Logger {
  logger.without_caller_id_capture(lgr)
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

/// Check if a log should be sampled based on global config.
fn should_sample(log_level: Level) -> Bool {
  let cfg = get_config()
  sampling.should_sample_with_config(cfg.sampling, log_level)
}

/// Log a trace message using the default logger.
pub fn trace(message: String) -> Nil {
  case should_sample(level.Trace) {
    False -> Nil
    True -> logger.trace(default_logger(), message, [])
  }
}

/// Log a trace message with metadata using the default logger.
pub fn trace_m(message: String, metadata: Metadata) -> Nil {
  case should_sample(level.Trace) {
    False -> Nil
    True -> logger.trace(default_logger(), message, metadata)
  }
}

/// Log a debug message using the default logger.
pub fn debug(message: String) -> Nil {
  case should_sample(level.Debug) {
    False -> Nil
    True -> logger.debug(default_logger(), message, [])
  }
}

/// Log a debug message with metadata using the default logger.
pub fn debug_m(message: String, metadata: Metadata) -> Nil {
  case should_sample(level.Debug) {
    False -> Nil
    True -> logger.debug(default_logger(), message, metadata)
  }
}

/// Log an info message using the default logger.
pub fn info(message: String) -> Nil {
  case should_sample(level.Info) {
    False -> Nil
    True -> logger.info(default_logger(), message, [])
  }
}

/// Log an info message with metadata using the default logger.
pub fn info_m(message: String, metadata: Metadata) -> Nil {
  case should_sample(level.Info) {
    False -> Nil
    True -> logger.info(default_logger(), message, metadata)
  }
}

/// Log a warning message using the default logger.
pub fn warn(message: String) -> Nil {
  case should_sample(level.Warn) {
    False -> Nil
    True -> logger.warn(default_logger(), message, [])
  }
}

/// Log a warning message with metadata using the default logger.
pub fn warn_m(message: String, metadata: Metadata) -> Nil {
  case should_sample(level.Warn) {
    False -> Nil
    True -> logger.warn(default_logger(), message, metadata)
  }
}

/// Log an error message using the default logger.
pub fn error(message: String) -> Nil {
  case should_sample(level.Err) {
    False -> Nil
    True -> logger.error(default_logger(), message, [])
  }
}

/// Log an error message with metadata using the default logger.
pub fn error_m(message: String, metadata: Metadata) -> Nil {
  case should_sample(level.Err) {
    False -> Nil
    True -> logger.error(default_logger(), message, metadata)
  }
}

/// Log a fatal message using the default logger.
pub fn fatal(message: String) -> Nil {
  case should_sample(level.Fatal) {
    False -> Nil
    True -> logger.fatal(default_logger(), message, [])
  }
}

/// Log a fatal message with metadata using the default logger.
pub fn fatal_m(message: String, metadata: Metadata) -> Nil {
  case should_sample(level.Fatal) {
    False -> Nil
    True -> logger.fatal(default_logger(), message, metadata)
  }
}

// ============================================================================
// Lazy Evaluation Variants
// ============================================================================

/// Log a debug message with lazy evaluation using the default logger.
/// The message function is only called if debug level is enabled and sampled.
pub fn debug_lazy(message_fn: fn() -> String) -> Nil {
  case should_sample(level.Debug) {
    False -> Nil
    True -> logger.debug_lazy(default_logger(), message_fn, [])
  }
}

/// Log an info message with lazy evaluation using the default logger.
pub fn info_lazy(message_fn: fn() -> String) -> Nil {
  case should_sample(level.Info) {
    False -> Nil
    True -> logger.info_lazy(default_logger(), message_fn, [])
  }
}

// ============================================================================
// Error Result Convenience Functions
// ============================================================================

/// Log an error message with an associated Result using the default logger.
///
/// If the result is an Error, the error value is automatically included
/// in the metadata under the "error" key.
///
/// ## Example
///
/// ```gleam
/// import birch as log
///
/// case file.read("config.json") {
///   Ok(content) -> parse_config(content)
///   Error(_) as result -> {
///     log.error_result("Failed to read config file", result)
///     use_defaults()
///   }
/// }
/// ```
pub fn error_result(message: String, result: Result(a, e)) -> Nil {
  case should_sample(level.Err) {
    False -> Nil
    True -> logger.error_result(default_logger(), message, result, [])
  }
}

/// Log an error message with an associated Result and metadata.
pub fn error_result_m(
  message: String,
  result: Result(a, e),
  metadata: Metadata,
) -> Nil {
  case should_sample(level.Err) {
    False -> Nil
    True -> logger.error_result(default_logger(), message, result, metadata)
  }
}

/// Log a fatal message with an associated Result using the default logger.
///
/// If the result is an Error, the error value is automatically included
/// in the metadata under the "error" key.
pub fn fatal_result(message: String, result: Result(a, e)) -> Nil {
  case should_sample(level.Fatal) {
    False -> Nil
    True -> logger.fatal_result(default_logger(), message, result, [])
  }
}

/// Log a fatal message with an associated Result and metadata.
pub fn fatal_result_m(
  message: String,
  result: Result(a, e),
  metadata: Metadata,
) -> Nil {
  case should_sample(level.Fatal) {
    False -> Nil
    True -> logger.fatal_result(default_logger(), message, result, metadata)
  }
}

/// Log an error message with an associated Result using a specific logger.
pub fn logger_error_result(
  lgr: Logger,
  message: String,
  result: Result(a, e),
  metadata: Metadata,
) -> Nil {
  logger.error_result(lgr, message, result, metadata)
}

/// Log a fatal message with an associated Result using a specific logger.
pub fn logger_fatal_result(
  lgr: Logger,
  message: String,
  result: Result(a, e),
  metadata: Metadata,
) -> Nil {
  logger.fatal_result(lgr, message, result, metadata)
}

// ============================================================================
// Scoped Context
// ============================================================================

/// Execute a function with the given context applied.
///
/// All logs made within the scope (directly or through nested calls)
/// will include the scoped context metadata.
///
/// Scopes can be nested, with inner scopes adding to (and potentially
/// shadowing) the outer scope's context.
///
/// ## Example
///
/// ```gleam
/// import birch as log
///
/// pub fn handle_request(request_id: String) {
///   log.with_scope([#("request_id", request_id)], fn() {
///     // All logs in this block include request_id
///     log.info("processing request")
///     do_work()
///     log.info("request complete")
///   })
/// }
/// ```
///
/// ## Platform Support
///
/// - **Erlang:** Uses the process dictionary. Each process has its own scope.
/// - **JavaScript (Node.js):** Uses AsyncLocalStorage for async context propagation.
/// - **JavaScript (Other):** Falls back to simple storage; may not propagate to async operations.
pub fn with_scope(context: Metadata, work: fn() -> a) -> a {
  scope.with_scope(context, work)
}

/// Get the current scope context.
///
/// Returns the metadata from all active scopes, with inner scope values
/// taking precedence (appearing first in the list).
///
/// Returns an empty list if called outside of any scope.
pub fn get_scope_context() -> Metadata {
  scope.get_context()
}

/// Check if scoped context is available on the current platform.
///
/// - On Erlang: Always returns `True` (uses process dictionary)
/// - On Node.js: Returns `True` (uses AsyncLocalStorage)
/// - On other JavaScript runtimes: Returns `False`
///
/// When not available, `with_scope` still works but context may not
/// propagate to nested async operations.
pub fn is_scoped_context_available() -> Bool {
  scope.is_available()
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
