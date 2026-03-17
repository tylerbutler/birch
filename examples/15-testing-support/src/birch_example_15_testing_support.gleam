//// Testing Support Example
////
//// Demonstrates utilities for testing logging behavior.

import birch
import birch/handler
import birch/log.{type Logger}
import birch/meta

pub fn main() {
  birch.info("=== Testing Support Demo ===")

  // Fixed timestamps
  demo_fixed_timestamp()

  // Caller ID capture
  demo_caller_id()

  // Null handler
  demo_null_handler()

  birch.reset_config()
  birch.info("Demo complete")
}

/// Demonstrate fixed timestamps for testing.
fn demo_fixed_timestamp() {
  birch.info("--- Fixed Timestamps ---")

  let test_logger =
    birch.new("test")
    |> birch.with_time_provider(fn() { "2024-01-01T00:00:00.000Z" })

  test_logger |> log.info("This has a fixed timestamp", [])
  test_logger |> log.info("Same timestamp for this one", [])

  // Reset to normal timestamps
  let normal_logger = test_logger |> birch.without_time_provider()
  normal_logger |> log.info("Back to normal timestamps", [])
}

/// Demonstrate caller ID capture.
fn demo_caller_id() {
  birch.info("--- Caller ID Capture ---")

  let debug_logger =
    birch.new("debug")
    |> birch.with_caller_id_capture()

  debug_logger
  |> log.info("Check the caller_id in metadata", [meta.string("extra", "value")])

  // Disable caller ID capture
  let normal_logger = debug_logger |> birch.without_caller_id_capture()
  normal_logger |> log.info("No caller_id here", [])
}

/// Demonstrate null handler for silent testing.
fn demo_null_handler() {
  birch.info("--- Null Handler ---")

  // Save current config
  birch.info("About to configure null handler...")

  // Configure with null handler
  birch.configure([birch.config_handlers([handler.null()])])

  // These logs are silenced
  birch.info("This message is silenced")
  birch.error("Even errors are silenced")

  // Restore default config
  birch.reset_config()
  birch.info("Logging restored after null handler demo")
}

/// Create a logger for testing with deterministic output.
pub fn test_logger(name: String) -> Logger {
  birch.new(name)
  |> birch.with_time_provider(fn() { "TEST-TIMESTAMP" })
  |> birch.with_context([meta.string("test", "true")])
}

/// Create a logger with caller ID for debugging concurrent code.
pub fn debug_logger(name: String) -> Logger {
  birch.new(name)
  |> birch.with_caller_id_capture()
}

/// Silence all logging (call reset_config() to restore).
pub fn silence_logging() -> Nil {
  birch.configure([birch.config_handlers([handler.null()])])
}
