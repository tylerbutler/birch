//// Integration test fixture for JSON handler output.
////
//// This fixture exercises the JSON handler.
//// The test harness captures stdout and verifies:
//// - Output is valid JSON
//// - Required fields are present (timestamp, level, logger, message)
//// - Metadata is included correctly
//// - Each line is a separate JSON object

import gleam_log
import gleam_log/handler/json
import gleam_log/level

pub fn main() {
  // Configure with JSON handler
  gleam_log.configure([
    gleam_log.config_level(level.Debug),
    gleam_log.config_handlers([json.handler()]),
  ])

  // Log at various levels
  gleam_log.debug("JSON debug message")
  gleam_log.info("JSON info message")
  gleam_log.warn("JSON warn message")
  gleam_log.error("JSON error message")

  // Log with metadata - should appear as additional JSON fields
  gleam_log.info_m("JSON with metadata", [
    #("transaction_id", "txn-456"),
    #("amount", "100.50"),
  ])

  // Reset config for clean state
  gleam_log.reset_config()
}
