//// Integration test fixture for console handler output.
////
//// This fixture exercises the console handler with various log levels.
//// The test harness captures stdout/stderr and verifies:
//// - All log levels produce output
//// - ANSI color codes are present (when TTY)
//// - Message content is correct
//// - Format is consistent

import birch
import birch/handler/console
import birch/level
import birch/log
import birch/meta

pub fn main() {
  // Configure with debug level to capture all messages
  birch.configure([
    birch.config_level(level.Debug),
    birch.config_handlers([console.handler()]),
  ])

  // Log at each level with identifiable messages
  birch.debug("Debug message for testing")
  birch.info("Info message for testing")
  birch.warn("Warn message for testing")
  birch.error("Error message for testing")

  // Test with metadata
  birch.new("console-fixture")
  |> birch.with_handler(console.handler())
  |> birch.with_level(level.Debug)
  |> log.info("Message with metadata", [
    meta.string("request_id", "test-123"),
    meta.string("user", "integration-test"),
  ])

  // Reset config for clean state
  birch.reset_config()
}
