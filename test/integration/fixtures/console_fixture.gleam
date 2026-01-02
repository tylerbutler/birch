//// Integration test fixture for console handler output.
////
//// This fixture exercises the console handler with various log levels.
//// The test harness captures stdout/stderr and verifies:
//// - All log levels produce output
//// - ANSI color codes are present (when TTY)
//// - Message content is correct
//// - Format is consistent

import gleam_log
import gleam_log/handler/console
import gleam_log/level

pub fn main() {
  // Configure with debug level to capture all messages
  gleam_log.configure([
    gleam_log.config_level(level.Debug),
    gleam_log.config_handlers([console.handler()]),
  ])

  // Log at each level with identifiable messages
  gleam_log.debug("Debug message for testing")
  gleam_log.info("Info message for testing")
  gleam_log.warn("Warn message for testing")
  gleam_log.error("Error message for testing")

  // Test with metadata
  gleam_log.info_m("Message with metadata", [
    #("request_id", "test-123"),
    #("user", "integration-test"),
  ])

  // Reset config for clean state
  gleam_log.reset_config()
}
