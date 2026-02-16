//// Erlang :logger integration for birch.
////
//// This module provides optional integration with Erlang's built-in :logger
//// system, allowing birch to:
////
//// 1. **Forward to :logger**: Create a handler that sends birch records
////    to Erlang's :logger system (useful for integrating with OTP applications)
////
//// 2. **Receive from :logger**: Install birch as an Erlang :logger handler
////    to receive logs from OTP applications and process them with birch handlers
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
//// ## Example: Receive logs from OTP applications
////
//// ```gleam
//// import birch/erlang_logger
////
//// pub fn main() {
////   // Install birch as an Erlang :logger handler
////   case erlang_logger.install_logger_handler() {
////     Ok(Nil) -> {
////       // Logs from OTP applications will now be processed by birch
////     }
////     Error(reason) -> {
////       // Handle error (e.g., on JavaScript target)
////     }
////   }
//// }
//// ```

import birch/formatter
import birch/handler.{type Handler}
import birch/handler/console
import birch/level.{type Level}
import birch/record.{type Metadata}
import gleam/option.{type Option}

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

/// Convert an Erlang level to its atom representation (for FFI).
pub fn erlang_level_to_atom(erlang_level: ErlangLevel) -> String {
  case erlang_level {
    ErlangEmergency -> "emergency"
    ErlangAlert -> "alert"
    ErlangCritical -> "critical"
    ErlangError -> "error"
    ErlangWarning -> "warning"
    ErlangNotice -> "notice"
    ErlangInfo -> "info"
    ErlangDebug -> "debug"
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

// Forward a log message to Erlang's :logger (via FFI)
fn forward_write(message: String) -> Nil {
  do_logger_log(ErlangInfo, message)
}

// ============================================================================
// Forward to :logger with Level (raw handler)
// ============================================================================

/// Create a handler that forwards birch records to :logger with proper
/// level mapping and structured metadata.
///
/// This handler passes structured data to :logger, preserving the log level,
/// logger name, metadata, and caller ID. The birch_logger_formatter installed
/// on the default handler will then apply birch's formatting.
pub fn forward_to_logger_raw() -> Handler {
  handler.new_with_record_write(name: "erlang:logger:raw", write: fn(record) {
    let erlang_level = gleam_level_to_erlang(record.level)
    do_logger_log_structured(
      erlang_level,
      record.message,
      record.logger_name,
      record.metadata,
      record.caller_id,
    )
  })
}

// ============================================================================
// Formatter Setup for :logger's Default Handler
// ============================================================================

/// Configure the BEAM logger's default handler to use birch formatting.
///
/// This installs birch's simple-style formatter on `:logger`'s default handler,
/// so both birch logs and OTP/library logs get birch-style formatting
/// (pipe-delimited with colors, timestamps, and metadata).
///
/// On JavaScript, returns an error since `:logger` is not available.
///
/// ## Example
///
/// ```gleam
/// import birch/erlang_logger
///
/// pub fn main() {
///   // Explicitly set up birch formatting on :logger
///   let assert Ok(Nil) = erlang_logger.setup()
/// }
/// ```
pub fn setup() -> Result(Nil, String) {
  setup_with_config(console.default_config())
}

/// Configure the BEAM logger's default handler with custom formatting.
///
/// Accepts a `ConsoleConfig` to control the style (simple/fancy),
/// level formatter, colors, timestamps, etc.
///
/// ## Example
///
/// ```gleam
/// import birch/erlang_logger
/// import birch/handler/console
///
/// pub fn main() {
///   // Use fancy style with icons for all logs
///   let assert Ok(Nil) =
///     erlang_logger.setup_with_config(console.default_fancy_config())
/// }
/// ```
pub fn setup_with_config(config: console.ConsoleConfig) -> Result(Nil, String) {
  let format_fn = console.build_format_fn(config)
  do_configure_formatter(format_fn)
}

/// Ensure the birch formatter is configured on the default :logger handler.
///
/// This is idempotent — it configures the formatter on first call and
/// no-ops on subsequent calls. Called automatically when birch's default
/// configuration is used on the Erlang target.
pub fn ensure_formatter_configured() -> Nil {
  case do_is_formatter_configured() {
    True -> Nil
    False -> {
      let _ = setup()
      Nil
    }
  }
}

// ============================================================================
// Install birch as :logger Handler
// ============================================================================

/// Default handler ID used when installing birch as a :logger handler.
pub const default_handler_id = "birch"

/// Install birch as an Erlang :logger handler.
///
/// Once installed, logs from OTP applications (and Erlang code using :logger)
/// will be processed by birch's configured handlers.
///
/// Returns `Ok(Nil)` on success, or `Error(reason)` if installation fails.
/// On JavaScript, always returns `Error("erlang:logger is not available on JavaScript target")`.
///
/// ## Example
///
/// ```gleam
/// import birch/erlang_logger
///
/// pub fn main() {
///   case erlang_logger.install_logger_handler() {
///     Ok(Nil) -> io.println("Installed birch as :logger handler")
///     Error(reason) -> io.println("Failed: " <> reason)
///   }
/// }
/// ```
pub fn install_logger_handler() -> Result(Nil, String) {
  install_logger_handler_with_id(default_handler_id)
}

/// Install birch as an Erlang :logger handler with a custom handler ID.
///
/// This is useful if you want to install multiple instances or avoid conflicts
/// with other handlers.
pub fn install_logger_handler_with_id(handler_id: String) -> Result(Nil, String) {
  do_install_logger_handler(handler_id)
}

/// Uninstall the birch :logger handler.
///
/// Removes the handler installed by `install_logger_handler()`.
/// Returns `Ok(Nil)` on success, or `Error(reason)` if removal fails.
pub fn uninstall_logger_handler() -> Result(Nil, String) {
  uninstall_logger_handler_with_id(default_handler_id)
}

/// Uninstall a birch :logger handler with a specific ID.
pub fn uninstall_logger_handler_with_id(
  handler_id: String,
) -> Result(Nil, String) {
  do_uninstall_logger_handler(handler_id)
}

// ============================================================================
// FFI Declarations
// ============================================================================

/// Log a message to Erlang's :logger at the specified level.
@external(erlang, "birch_erlang_logger_ffi", "logger_log")
@external(javascript, "../birch_erlang_logger_ffi.mjs", "logger_log")
fn do_logger_log(level: ErlangLevel, message: String) -> Nil

/// Log structured data to Erlang's :logger, preserving birch metadata.
@external(erlang, "birch_erlang_logger_ffi", "logger_log_structured")
@external(javascript, "../birch_erlang_logger_ffi.mjs", "logger_log_structured")
fn do_logger_log_structured(
  level: ErlangLevel,
  message: String,
  logger_name: String,
  metadata: Metadata,
  caller_id: Option(String),
) -> Nil

/// Configure the default :logger handler to use birch's formatter.
@external(erlang, "birch_erlang_logger_ffi", "configure_default_handler_formatter")
@external(javascript, "../birch_erlang_logger_ffi.mjs", "configure_default_handler_formatter")
fn do_configure_formatter(format_fn: formatter.Formatter) -> Result(Nil, String)

/// Check if the birch formatter is already configured.
@external(erlang, "birch_erlang_logger_ffi", "is_formatter_configured")
@external(javascript, "../birch_erlang_logger_ffi.mjs", "is_formatter_configured")
fn do_is_formatter_configured() -> Bool

/// Install birch as a :logger handler.
@external(erlang, "birch_erlang_logger_ffi", "install_handler")
@external(javascript, "../birch_erlang_logger_ffi.mjs", "install_handler")
fn do_install_logger_handler(handler_id: String) -> Result(Nil, String)

/// Uninstall a :logger handler.
@external(erlang, "birch_erlang_logger_ffi", "uninstall_handler")
@external(javascript, "../birch_erlang_logger_ffi.mjs", "uninstall_handler")
fn do_uninstall_logger_handler(handler_id: String) -> Result(Nil, String)
