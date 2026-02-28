//// Erlang :logger integration for birch.
////
//// This module provides integration with Erlang's built-in :logger system.
////
//// ## Architecture
////
//// On BEAM, birch sends LogRecords **directly** to `:logger` — no intermediate
//// birch Handler is needed. The birch formatter installed on `:logger`'s default
//// handler receives the intact LogRecord and formats it. This is the idiomatic
//// OTP approach: `:logger` controls output routing and overload protection,
//// while birch controls formatting.
////
//// The flow:
//// ```
//// birch log.info("hello")
////   → logger:log(info, msg, #{birch_log_record => LogRecord})
////     → :logger default handler
////       → birch formatter callback
////         → FormatFn(LogRecord) → formatted string → console/file
//// ```
////
//// OTP/library log events (without birch metadata) are also handled: the
//// formatter builds a LogRecord from `:logger` event fields and formats
//// structured reports using their `report_cb` callbacks when available.
////
//// ## Setup
////
//// Birch automatically installs its formatter on the `:logger` default handler
//// when the default configuration is used. You can also set it up explicitly:
////
//// ```gleam
//// import birch/erlang_logger
////
//// pub fn main() {
////   let assert Ok(Nil) = erlang_logger.setup()
////   // All logs (birch + OTP) now use birch formatting
//// }
//// ```
////
//// ## Customization
////
//// ```gleam
//// import birch/erlang_logger
//// import birch/handler/console
////
//// // Use fancy style with icons
//// let assert Ok(Nil) =
////   erlang_logger.setup_with_config(console.default_fancy_config())
//// ```

import birch/formatter
import birch/handler.{type Handler}
import birch/handler/console
import birch/level.{type Level}
import birch/record
import gleam/option

// ============================================================================
// Erlang Log Level Type
// ============================================================================

/// Erlang :logger log levels (RFC 5424 syslog severity levels).
///
/// Birch levels map 1:1 to Erlang levels (except Trace, which shares
/// debug with Debug):
///
/// | Erlang Level | Gleam Level |
/// |--------------|-------------|
/// | emergency    | Fatal       |
/// | alert        | Alert       |
/// | critical     | Critical    |
/// | error        | Err         |
/// | warning      | Warn        |
/// | notice       | Notice      |
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
///
/// The mapping follows RFC 5424 semantics with a 1:1 correspondence
/// for all levels except Trace (which has no RFC 5424 equivalent and
/// maps to debug):
///
/// | Gleam Level | Erlang Level |
/// |-------------|--------------|
/// | Trace       | debug        |
/// | Debug       | debug        |
/// | Info        | info         |
/// | Notice      | notice       |
/// | Warn        | warning      |
/// | Err         | error        |
/// | Critical    | critical     |
/// | Alert       | alert        |
/// | Fatal       | emergency    |
pub fn gleam_level_to_erlang(gleam_level: Level) -> ErlangLevel {
  case gleam_level {
    level.Trace -> ErlangDebug
    level.Debug -> ErlangDebug
    level.Info -> ErlangInfo
    level.Notice -> ErlangNotice
    level.Warn -> ErlangWarning
    level.Err -> ErlangError
    level.Critical -> ErlangCritical
    level.Alert -> ErlangAlert
    level.Fatal -> ErlangEmergency
  }
}

/// Convert an Erlang :logger level to a Gleam log level.
///
/// This is a clean 1:1 reverse mapping. The only lossy direction is
/// Erlang debug → Gleam Debug (Trace is not distinguishable from Debug
/// on the Erlang side).
pub fn erlang_level_to_gleam(erlang_level: ErlangLevel) -> Level {
  case erlang_level {
    ErlangDebug -> level.Debug
    ErlangInfo -> level.Info
    ErlangNotice -> level.Notice
    ErlangWarning -> level.Warn
    ErlangError -> level.Err
    ErlangCritical -> level.Critical
    ErlangAlert -> level.Alert
    ErlangEmergency -> level.Fatal
  }
}

// ============================================================================
// Direct Emission to :logger (primary BEAM integration)
// ============================================================================

/// Emit a LogRecord directly to the BEAM's :logger system.
///
/// This is the primary integration point on BEAM. The entire LogRecord is
/// passed through `:logger` metadata so the birch formatter can use it
/// directly without decomposing/recomposing fields.
///
/// Called automatically by `logger.emit_record()` on the Erlang target.
/// On JavaScript, this is a no-op (birch handlers handle output directly).
pub fn emit(record: record.LogRecord) -> Nil {
  let erlang_level = gleam_level_to_erlang(record.level)
  do_emit_to_logger(erlang_level, record)
}

/// Check if the birch formatter is initialized on the default :logger handler.
/// Uses a persistent_term cache for fast repeated checks.
pub fn is_initialized() -> Bool {
  do_ensure_initialized()
}

// ============================================================================
// Formatter Setup for :logger's Default Handler
// ============================================================================

/// Configure the BEAM logger's default handler to use birch formatting.
///
/// This installs birch's formatter on `:logger`'s default handler,
/// so both birch logs and OTP/library logs get birch-style formatting
/// (pipe-delimited with timestamps, levels, and metadata).
///
/// On JavaScript, returns an error since `:logger` is not available.
///
/// ## Example
///
/// ```gleam
/// import birch/erlang_logger
///
/// pub fn main() {
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
///   let assert Ok(Nil) =
///     erlang_logger.setup_with_config(console.default_fancy_config())
/// }
/// ```
pub fn setup_with_config(config: console.ConsoleConfig) -> Result(Nil, String) {
  let format_fn = console.build_format_fn(config)
  install_formatter_on(default_handler_id, format_fn)
}

/// Ensure the birch formatter is configured on the default :logger handler.
///
/// This is idempotent — it configures the formatter on first call and
/// no-ops on subsequent calls. Called automatically when birch's default
/// configuration is used on the Erlang target.
pub fn ensure_formatter_configured() -> Nil {
  case do_ensure_initialized() {
    True -> Nil
    False -> {
      let _ = setup()
      Nil
    }
  }
}

// ============================================================================
// Install birch as :logger Formatter
// ============================================================================

/// The default handler ID for the BEAM's built-in :logger handler.
pub const default_handler_id = "default"

/// Install birch as the formatter on the default :logger handler,
/// using birch's `human_readable` formatter.
///
/// This is the recommended way to integrate birch with OTP's logging system.
/// The :logger handler continues to control output (console, file, etc.),
/// while birch controls how log messages are formatted.
///
/// The installed formatter handles both birch-originated and OTP log events:
/// - **Birch logs**: Detected via `birch_log_record` in metadata, the intact
///   LogRecord is passed directly to the format function.
/// - **OTP logs**: A LogRecord is built from `:logger` event fields.
///   Structured reports use their `report_cb` callback for human-readable output.
///
/// Returns `Ok(Nil)` on success, or `Error(reason)` if installation fails.
/// On JavaScript, always returns `Error(...)` since :logger is not available.
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
/// // Use the simple formatter for all logs
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
  do_install_formatter(handler_id, format)
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

// ============================================================================
// Deprecated: Forward handlers (no longer needed)
// ============================================================================

/// Create a handler that forwards birch records to the BEAM's :logger.
///
/// **Deprecated**: On BEAM, birch now sends LogRecords directly to `:logger`
/// without needing a handler wrapper. This function is retained for backwards
/// compatibility but the handler is redundant — logs go through `:logger`
/// automatically.
@deprecated("No longer needed — birch sends to :logger directly on BEAM. Remove this handler.")
pub fn forward_to_beam() -> Handler {
  handler.new_with_record_write(name: "erlang:logger", write: fn(r) {
    let erlang_level = gleam_level_to_erlang(r.level)
    do_logger_log_structured(
      erlang_level,
      r.message,
      r.logger_name,
      r.metadata,
      r.caller_id,
    )
  })
}

/// Create a handler that forwards birch records to Erlang's :logger system.
@deprecated("No longer needed — birch sends to :logger directly on BEAM")
pub fn forward_to_logger() -> Handler {
  handler.new(
    name: "erlang:logger",
    write: forward_write,
    format: formatter.human_readable,
  )
}

/// Create a handler that forwards to :logger with a custom formatter.
@deprecated("No longer needed — birch sends to :logger directly on BEAM")
pub fn forward_to_logger_with_formatter(format: formatter.Formatter) -> Handler {
  handler.new(name: "erlang:logger", write: forward_write, format: format)
}

fn forward_write(message: String) -> Nil {
  do_logger_log(ErlangInfo, message)
}

/// Create a handler that forwards birch records to :logger.
@deprecated("No longer needed — birch sends to :logger directly on BEAM")
pub fn forward_to_logger_raw() -> Handler {
  handler.new_with_record_write(name: "erlang:logger", write: fn(r) {
    let erlang_level = gleam_level_to_erlang(r.level)
    do_logger_log_structured(
      erlang_level,
      r.message,
      r.logger_name,
      r.metadata,
      r.caller_id,
    )
  })
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
// Deprecated: FormatCallback type
// ============================================================================

/// Type for the format callback function.
///
/// **Deprecated**: The formatter now receives a `Formatter` function
/// (i.e., `fn(LogRecord) -> String`) directly.
@deprecated("Use formatter.Formatter (fn(LogRecord) -> String) instead")
pub type FormatCallback =
  fn(String, Level, String, String, record.Metadata) -> String

// ============================================================================
// FFI Declarations
// ============================================================================

/// Emit a LogRecord directly to Erlang's :logger.
/// The entire LogRecord is passed through :logger metadata.
@external(erlang, "birch_erlang_logger_ffi", "emit_to_logger")
@external(javascript, "../birch_erlang_logger_ffi.mjs", "emit_to_logger")
fn do_emit_to_logger(level: ErlangLevel, record: record.LogRecord) -> Nil

/// Check if birch formatter is initialized (fast persistent_term check).
@external(erlang, "birch_erlang_logger_ffi", "ensure_initialized")
@external(javascript, "../birch_erlang_logger_ffi.mjs", "ensure_initialized")
fn do_ensure_initialized() -> Bool

/// Log a pre-formatted message to Erlang's :logger (legacy).
@external(erlang, "birch_erlang_logger_ffi", "logger_log")
@external(javascript, "../birch_erlang_logger_ffi.mjs", "logger_log")
fn do_logger_log(level: ErlangLevel, message: String) -> Nil

/// Log a message with structured birch metadata (legacy).
@external(erlang, "birch_erlang_logger_ffi", "logger_log_structured")
@external(javascript, "../birch_erlang_logger_ffi.mjs", "logger_log_structured")
fn do_logger_log_structured(
  level: ErlangLevel,
  message: String,
  logger_name: String,
  metadata: record.Metadata,
  caller_id: option.Option(String),
) -> Nil

/// Install birch as a :logger formatter on the specified handler.
@external(erlang, "birch_erlang_logger_ffi", "install_formatter")
@external(javascript, "../birch_erlang_logger_ffi.mjs", "install_formatter")
fn do_install_formatter(
  handler_id: String,
  format_fn: formatter.Formatter,
) -> Result(Nil, String)

/// Remove birch formatter from a :logger handler, restoring defaults.
@external(erlang, "birch_erlang_logger_ffi", "remove_formatter")
@external(javascript, "../birch_erlang_logger_ffi.mjs", "remove_formatter")
fn do_remove_formatter(handler_id: String) -> Result(Nil, String)
