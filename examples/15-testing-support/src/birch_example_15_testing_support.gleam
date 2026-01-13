//// Testing Support Example
////
//// Demonstrates utilities for testing logging behavior.

import birch as log
import birch/handler
import birch/logger.{type Logger}

pub fn main() {
  log.info("=== Testing Support Demo ===")

  // Fixed timestamps
  demo_fixed_timestamp()

  // Caller ID capture
  demo_caller_id()

  // Null handler
  demo_null_handler()

  log.reset_config()
  log.info("Demo complete")
}

/// Demonstrate fixed timestamps for testing.
fn demo_fixed_timestamp() {
  log.info("--- Fixed Timestamps ---")

  let test_logger =
    log.new("test")
    |> log.with_time_provider(fn() { "2024-01-01T00:00:00.000Z" })

  test_logger |> log.logger_info("This has a fixed timestamp", [])
  test_logger |> log.logger_info("Same timestamp for this one", [])

  // Reset to normal timestamps
  let normal_logger = test_logger |> log.without_time_provider()
  normal_logger |> log.logger_info("Back to normal timestamps", [])
}

/// Demonstrate caller ID capture.
fn demo_caller_id() {
  log.info("--- Caller ID Capture ---")

  let debug_logger =
    log.new("debug")
    |> log.with_caller_id_capture()

  debug_logger
  |> log.logger_info("Check the caller_id in metadata", [#("extra", "value")])

  // Disable caller ID capture
  let normal_logger = debug_logger |> log.without_caller_id_capture()
  normal_logger |> log.logger_info("No caller_id here", [])
}

/// Demonstrate null handler for silent testing.
fn demo_null_handler() {
  log.info("--- Null Handler ---")

  // Save current config
  log.info("About to configure null handler...")

  // Configure with null handler
  log.configure([log.config_handlers([handler.null()])])

  // These logs are silenced
  log.info("This message is silenced")
  log.error("Even errors are silenced")

  // Restore default config
  log.reset_config()
  log.info("Logging restored after null handler demo")
}

/// Create a logger for testing with deterministic output.
pub fn test_logger(name: String) -> Logger {
  log.new(name)
  |> log.with_time_provider(fn() { "TEST-TIMESTAMP" })
  |> log.with_context([#("test", "true")])
}

/// Create a logger with caller ID for debugging concurrent code.
pub fn debug_logger(name: String) -> Logger {
  log.new(name)
  |> log.with_caller_id_capture()
}

/// Silence all logging (call reset_config() to restore).
pub fn silence_logging() -> Nil {
  log.configure([log.config_handlers([handler.null()])])
}
