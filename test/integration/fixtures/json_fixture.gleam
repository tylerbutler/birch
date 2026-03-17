//// Integration test fixture for JSON handler output.
////
//// This fixture exercises the JSON handler.
//// The test harness captures stdout and verifies:
//// - Output is valid JSON
//// - Required fields are present (timestamp, level, logger, message)
//// - Metadata is included correctly
//// - Each line is a separate JSON object

import birch
import birch/handler/json
import birch/level
import birch/log
import birch/meta

pub fn main() {
  // Configure with JSON handler
  birch.configure([
    birch.config_level(level.Debug),
    birch.config_handlers([json.handler()]),
  ])

  // Log at various levels
  birch.debug("JSON debug message")
  birch.info("JSON info message")
  birch.warn("JSON warn message")
  birch.error("JSON error message")

  // Log with metadata - should appear as additional JSON fields
  birch.new("json-fixture")
  |> birch.with_handler(json.handler())
  |> birch.with_level(level.Debug)
  |> log.info("JSON with metadata", [
    meta.string("transaction_id", "txn-456"),
    meta.string("amount", "100.50"),
  ])

  // Reset config for clean state
  birch.reset_config()
}
