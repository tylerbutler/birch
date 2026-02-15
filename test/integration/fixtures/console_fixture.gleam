//// Integration test fixture for console handler output.
////
//// This fixture exercises the console handler with various log levels.
//// The test harness captures stdout/stderr and verifies:
//// - All log levels produce output
//// - ANSI color codes are present (when TTY)
//// - Message content is correct
//// - Format is consistent

import birch as log
import birch/handler/console
import birch/level

pub fn main() {
  // Configure with debug level to capture all messages
  log.configure([
    log.config_level(level.Debug),
    log.config_handlers([console.handler()]),
  ])

  // Log at each level with identifiable messages
  log.debug("Debug message for testing", [])
  log.info("Info message for testing", [])
  log.warn("Warn message for testing", [])
  log.error("Error message for testing", [])

  // Test with metadata
  log.info("Message with metadata", [
    #("request_id", "test-123"),
    #("user", "integration-test"),
  ])

  // Reset config for clean state
  log.reset_config()
}
