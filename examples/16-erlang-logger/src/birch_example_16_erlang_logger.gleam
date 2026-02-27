//// Erlang Logger Integration Example
////
//// Demonstrates integrating birch with Erlang's built-in :logger system.
//// This example is BEAM only - it uses Erlang-specific features.

import birch as log
import birch/erlang_logger
import birch/formatter
import birch/handler.{type Handler}

pub fn main() {
  log.info("=== Erlang Logger Integration Demo ===")

  // Demo 1: Forward to Erlang logger
  demo_forward_to_logger()

  // Demo 2: Install birch as :logger formatter
  demo_install_formatter()

  log.reset_config()
  log.info("Demo complete")
}

/// Demonstrate forwarding birch logs to Erlang :logger.
fn demo_forward_to_logger() {
  log.info("--- Forward to Erlang Logger ---")

  // Create handler that forwards to Erlang's logger
  let handler = erlang_logger.forward_to_logger()

  // Configure birch to use this handler
  log.configure([log.config_handlers([handler])])

  // These logs go through Erlang's logger system
  log.info("This message goes to Erlang logger")
  log.warn("Warnings are mapped to Erlang warning level")
  log.error("Errors are mapped to Erlang error level")

  // Reset for next demo
  log.reset_config()
  log.info("Forwarding demo complete")
}

/// Demonstrate installing birch as a :logger formatter.
///
/// This is the idiomatic OTP approach: the :logger handler controls output
/// (console, file, etc.) while birch controls how messages are formatted.
fn demo_install_formatter() {
  log.info("--- Install birch as :logger Formatter ---")

  // Install birch as the formatter on the default :logger handler
  case erlang_logger.install_formatter() {
    Ok(Nil) -> {
      log.info("Birch is now formatting OTP :logger output")
      log.info("OTP and library logs will use birch's human_readable format")
    }
    Error(reason) -> {
      log.warn("Could not install formatter: " <> reason)
    }
  }

  // You can also use a custom formatter
  case erlang_logger.install_formatter_with(formatter.simple) {
    Ok(Nil) -> {
      log.info("Now using birch's simple formatter for OTP logs")
    }
    Error(_) -> Nil
  }

  // Remove birch formatter, restoring OTP defaults
  let _ = erlang_logger.remove_formatter()
  log.info("Formatter removed, OTP defaults restored")
}

/// Create a handler that forwards to Erlang logger.
/// Useful for OTP application integration.
pub fn create_erlang_handler() -> Handler {
  erlang_logger.forward_to_logger()
}

/// Install birch as a formatter for Erlang's :logger system.
pub fn setup_erlang_integration() -> Result(Nil, String) {
  erlang_logger.install_formatter()
}

/// Remove birch from Erlang's :logger system.
pub fn teardown_erlang_integration() -> Result(Nil, String) {
  erlang_logger.remove_formatter()
}
