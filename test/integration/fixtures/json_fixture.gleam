//// Integration test fixture for JSON handler output.
////
//// This fixture exercises the JSON handler.
//// The test harness captures stdout and verifies:
//// - Output is valid JSON
//// - Required fields are present (timestamp, level, logger, message)
//// - Metadata is included correctly
//// - Each line is a separate JSON object

import birch as log
import birch/handler/json
import birch/level
import birch/logger
import birch/meta

pub fn main() {
  // Configure with JSON handler
  log.configure([
    log.config_level(level.Debug),
    log.config_handlers([json.handler()]),
  ])

  // Log at various levels
  log.debug("JSON debug message")
  log.info("JSON info message")
  log.warn("JSON warn message")
  log.error("JSON error message")

  // Log with metadata - should appear as additional JSON fields
  log.new("json-fixture")
  |> log.with_handler(json.handler())
  |> log.with_level(level.Debug)
  |> logger.info("JSON with metadata", [
    meta.string("transaction_id", "txn-456"),
    meta.string("amount", "100.50"),
  ])

  // Reset config for clean state
  log.reset_config()
}
