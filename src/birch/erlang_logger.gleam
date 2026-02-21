//// Erlang :logger integration for birch.
////
//// This module provides integration with Erlang's built-in :logger
//// system, allowing birch to:
////
//// 1. **Forward to :logger**: Create a birch handler that sends birch records
////    to Erlang's :logger system (useful for integrating with OTP applications)
////
//// 2. **Format for :logger**: Install birch as a **formatter** on an existing
////    :logger handler, so that OTP/Erlang log events are formatted using
////    birch's formatting pipeline. This is the idiomatic OTP way — the :logger
////    handler controls output (console, file, etc.) while birch controls formatting.
////
//// **Note**: These features are only available on the Erlang target. On JavaScript,
//// the functions will return errors indicating the feature is unavailable.
////
//// ## Example: Forward logs to Erlang :logger
////
//// ```gleam
//// import birch as log
//// import birch/erlang_logger
////
//// pub fn main() {
////   // Configure birch to forward to Erlang's :logger
////   log.configure([
////     log.config_handlers([erlang_logger.forward_to_logger()]),
////   ])
////
////   // Logs will now go to Erlang's :logger system
////   log.info("Hello from Gleam!")
//// }
//// ```
////
//// ## Example: Format OTP logs with birch
////
//// ```gleam
//// import birch/erlang_logger
//// import birch/formatter
////
//// pub fn main() {
////   // Install birch as the formatter on the default :logger handler
////   case erlang_logger.install_formatter() {
////     Ok(Nil) -> {
////       // OTP log messages are now formatted by birch's human_readable formatter
////     }
////     Error(reason) -> {
////       // Handle error (e.g., on JavaScript target)
////     }
////   }
//// }
//// ```

import birch/formatter
import birch/handler.{type Handler}
import birch/level.{type Level}
import birch/record

// ============================================================================
// Erlang Log Level Type
// ============================================================================

/// Erlang :logger log levels.
///
/// Erlang uses syslog-style levels which are more granular than birch levels.
/// These are mapped to/from birch levels as follows:
///
/// | Erlang Level | Gleam Level |
/// |--------------|-------------|
/// | emergency    | Fatal       |
/// | alert        | Fatal       |
/// | critical     | Fatal       |
/// | error        | Err         |
/// | warning      | Warn        |
/// | notice       | Info        |
/// | info         | Info        |
/// | debug        | Debug/Trace |
pub type ErlangLevel {
  ErlangEmergency
  ErlangAlert
  ErlangCritical
  ErlangError
  ErlangWarning
  ErlangNotice
  ErlangInfo
  ErlangDebug
}

// ============================================================================
// Level Conversion Functions
// ============================================================================

/// Convert a Gleam log level to an Erlang :logger level.
pub fn gleam_level_to_erlang(gleam_level: Level) -> ErlangLevel {
  case gleam_level {
    level.Trace -> ErlangDebug
    level.Debug -> ErlangDebug
    level.Info -> ErlangInfo
    level.Warn -> ErlangWarning
    level.Err -> ErlangError
    level.Fatal -> ErlangEmergency
  }
}

/// Convert an Erlang :logger level to a Gleam log level.
pub fn erlang_level_to_gleam(erlang_level: ErlangLevel) -> Level {
  case erlang_level {
    ErlangDebug -> level.Debug
    ErlangInfo -> level.Info
    ErlangNotice -> level.Info
    ErlangWarning -> level.Warn
    ErlangError -> level.Err
    ErlangCritical -> level.Fatal
    ErlangAlert -> level.Fatal
    ErlangEmergency -> level.Fatal
  }
}

// ============================================================================
// Forward to :logger Handler
// ============================================================================

/// Create a handler that forwards birch records to Erlang's :logger system.
///
/// This allows birch to integrate with existing OTP logging infrastructure,
/// including any :logger handlers already configured (default handler, file handlers, etc.).
///
/// On JavaScript, this creates a handler that writes to console instead,
/// since :logger is not available.
///
/// ## Example
///
/// ```gleam
/// import birch as log
/// import birch/erlang_logger
/// import birch/handler/console
///
/// pub fn main() {
///   // Use both console and :logger output
///   log.configure([
///     log.config_handlers([
///       console.handler(),
///       erlang_logger.forward_to_logger(),
///     ]),
///   ])
/// }
/// ```
pub fn forward_to_logger() -> Handler {
  handler.new(
    name: "erlang:logger",
    write: forward_write,
    format: formatter.human_readable,
  )
}

/// Create a handler that forwards to :logger with a custom formatter.
pub fn forward_to_logger_with_formatter(format: formatter.Formatter) -> Handler {
  handler.new(name: "erlang:logger", write: forward_write, format: format)
}

fn forward_write(message: String) -> Nil {
  do_logger_log(ErlangInfo, message)
}

// ============================================================================
// Forward to :logger with Level (raw handler)
// ============================================================================

/// Create a handler that forwards birch records to :logger with proper
/// level mapping (not just the formatted message).
///
/// This handler passes the full LogRecord to :logger, preserving the log level
/// and metadata. Use this when you want :logger to receive structured log data.
pub fn forward_to_logger_raw() -> Handler {
  handler.new_with_record_write(name: "erlang:logger:raw", write: fn(r) {
    let erlang_level = gleam_level_to_erlang(r.level)
    let message = formatter.human_readable(r)
    do_logger_log(erlang_level, message)
  })
}

// ============================================================================
// Install birch as :logger Formatter
// ============================================================================

/// The default handler ID for the BEAM's built-in :logger handler.
pub const default_handler_id = "default"

/// Type for the format callback function passed to the Erlang formatter.
///
/// The Erlang `format/2` callback extracts raw data from the :logger event
/// (timestamp, level, logger name, message, metadata) and calls this function
/// to produce the formatted output string.
pub type FormatCallback =
  fn(String, Level, String, String, record.Metadata) -> String

/// Install birch as the formatter on the default :logger handler,
/// using birch's `human_readable` formatter.
///
/// This is the recommended way to integrate birch with OTP's logging system.
/// The :logger handler continues to control output (console, file, etc.),
/// while birch controls how log messages are formatted.
///
/// Returns `Ok(Nil)` on success, or `Error(reason)` if installation fails.
/// On JavaScript, always returns `Error(...)` since :logger is not available.
///
/// ## Example
///
/// ```gleam
/// import birch/erlang_logger
///
/// pub fn main() {
///   case erlang_logger.install_formatter() {
///     Ok(Nil) -> {
///       // OTP logs are now formatted by birch
///     }
///     Error(reason) -> {
///       io.println("Failed: " <> reason)
///     }
///   }
/// }
/// ```
pub fn install_formatter() -> Result(Nil, String) {
  install_formatter_on(default_handler_id, formatter.human_readable)
}

/// Install birch as the formatter on the default :logger handler
/// with a custom birch formatter.
///
/// ## Example
///
/// ```gleam
/// import birch/erlang_logger
/// import birch/formatter
///
/// // Use the simple formatter for OTP logs
/// erlang_logger.install_formatter_with(formatter.simple)
/// ```
pub fn install_formatter_with(
  format: formatter.Formatter,
) -> Result(Nil, String) {
  install_formatter_on(default_handler_id, format)
}

/// Install birch as the formatter on a specific :logger handler.
///
/// Use this when you have multiple :logger handlers and want birch
/// formatting on a specific one.
///
/// ## Example
///
/// ```gleam
/// import birch/erlang_logger
/// import birch/formatter
///
/// // Install on a custom handler
/// erlang_logger.install_formatter_on("my_file_handler", formatter.human_readable)
/// ```
pub fn install_formatter_on(
  handler_id: String,
  format: formatter.Formatter,
) -> Result(Nil, String) {
  do_install_formatter(handler_id, make_format_callback(format))
}

/// Remove birch as the formatter from the default :logger handler,
/// restoring OTP's default formatter.
///
/// Returns `Ok(Nil)` on success, or `Error(reason)` if removal fails.
pub fn remove_formatter() -> Result(Nil, String) {
  remove_formatter_from(default_handler_id)
}

/// Remove birch as the formatter from a specific :logger handler,
/// restoring OTP's default formatter.
pub fn remove_formatter_from(handler_id: String) -> Result(Nil, String) {
  do_remove_formatter(handler_id)
}

/// Build the format callback that the Erlang formatter will invoke.
fn make_format_callback(format: formatter.Formatter) -> FormatCallback {
  fn(
    timestamp: String,
    lvl: Level,
    logger_name: String,
    message: String,
    metadata: record.Metadata,
  ) -> String {
    record.new(
      timestamp: timestamp,
      level: lvl,
      logger_name: logger_name,
      message: message,
      metadata: metadata,
    )
    |> format
  }
}

// ============================================================================
// Deprecated: Legacy Handler-Based API
// ============================================================================

/// The handler ID previously used when installing birch as a :logger handler.
@deprecated("Use install_formatter() instead of install_logger_handler()")
pub const legacy_handler_id = "birch"

/// Install birch as an Erlang :logger handler.
@deprecated("Use install_formatter() instead")
pub fn install_logger_handler() -> Result(Nil, String) {
  install_formatter()
}

/// Install birch as an Erlang :logger handler with a custom handler ID.
@deprecated("Use install_formatter_on() instead")
pub fn install_logger_handler_with_id(handler_id: String) -> Result(Nil, String) {
  install_formatter_on(handler_id, formatter.human_readable)
}

/// Uninstall the birch :logger handler.
@deprecated("Use remove_formatter() instead")
pub fn uninstall_logger_handler() -> Result(Nil, String) {
  remove_formatter()
}

/// Uninstall a birch :logger handler with a specific ID.
@deprecated("Use remove_formatter_from() instead")
pub fn uninstall_logger_handler_with_id(
  handler_id: String,
) -> Result(Nil, String) {
  remove_formatter_from(handler_id)
}

// ============================================================================
// FFI Declarations
// ============================================================================

/// Log a message to Erlang's :logger at the specified level.
@external(erlang, "birch_erlang_logger_ffi", "logger_log")
@external(javascript, "../birch_erlang_logger_ffi.mjs", "logger_log")
fn do_logger_log(level: ErlangLevel, message: String) -> Nil

/// Install birch as a :logger formatter on the specified handler.
@external(erlang, "birch_erlang_logger_ffi", "install_formatter")
@external(javascript, "../birch_erlang_logger_ffi.mjs", "install_formatter")
fn do_install_formatter(
  handler_id: String,
  format_fn: FormatCallback,
) -> Result(Nil, String)

/// Remove birch formatter from a :logger handler, restoring defaults.
@external(erlang, "birch_erlang_logger_ffi", "remove_formatter")
@external(javascript, "../birch_erlang_logger_ffi.mjs", "remove_formatter")
fn do_remove_formatter(handler_id: String) -> Result(Nil, String)
