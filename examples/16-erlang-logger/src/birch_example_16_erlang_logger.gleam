//// Erlang Logger Integration Example
////
//// Demonstrates integrating birch with Erlang's built-in :logger system.
//// This example is BEAM only - it uses Erlang-specific features.

import birch as log
import birch/erlang_logger
import birch/handler

pub fn main() {
  log.info("=== Erlang Logger Integration Demo ===", [])

  // Demo 1: Forward to Erlang logger
  demo_forward_to_logger()

  // Demo 2: Install as logger handler
  demo_install_handler()

  log.reset_config()
  log.info("Demo complete", [])
}

/// Demonstrate forwarding birch logs to Erlang :logger.
fn demo_forward_to_logger() {
  log.info("--- Forward to Erlang Logger ---", [])

  // Create handler that forwards to Erlang's logger
  let handler = erlang_logger.forward_to_logger()

  // Configure birch to use this handler
  log.configure([log.config_handlers([handler])])

  // These logs go through Erlang's logger system
  log.info("This message goes to Erlang logger", [])
  log.warn("Warnings are mapped to Erlang warning level", [])
  log.error("Errors are mapped to Erlang error level", [])

  // Reset for next demo
  log.reset_config()
  log.info("Forwarding demo complete", [])
}

/// Demonstrate installing birch as an Erlang logger handler.
fn demo_install_handler() {
  log.info("--- Install as Logger Handler ---", [])

  // Install birch to receive logs from Erlang's logger
  let _ = erlang_logger.install_logger_handler()

  log.info("Birch is now installed as an Erlang logger handler", [])
  log.info("OTP and library logs would now be formatted by birch", [])

  // Uninstall when done
  let _ = erlang_logger.uninstall_logger_handler()

  log.info("Handler uninstalled", [])
}

/// Create a handler that forwards to Erlang logger.
/// Useful for OTP application integration.
pub fn create_erlang_handler() -> handler.Handler {
  erlang_logger.forward_to_logger()
}

/// Install birch as a handler for Erlang's logger system.
pub fn setup_erlang_integration() -> Result(Nil, String) {
  erlang_logger.install_logger_handler()
}

/// Remove birch from Erlang's logger system.
pub fn teardown_erlang_integration() -> Result(Nil, String) {
  erlang_logger.uninstall_logger_handler()
}
