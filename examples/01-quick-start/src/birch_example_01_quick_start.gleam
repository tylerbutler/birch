//// Quick Start Example
////
//// This example demonstrates zero-configuration logging with birch.
//// No setup required - just import and start logging.

import birch as log

pub fn main() {
  // Info level and above are shown by default
  log.info("Application starting", [])

  // Debug is filtered out by default (below Info threshold)
  log.debug("This debug message won't appear", [])

  // Simulate some application work
  log.info("Processing data...", [])

  // Warnings for conditions that might need attention
  log.warn("Cache miss - fetching from source", [])

  // Errors for problems that should be addressed
  log.error("Failed to connect to backup server", [])

  // Fatal for critical issues
  log.fatal("Unrecoverable error - shutting down", [])

  log.info("Application finished", [])
}

/// A simple function that logs during execution.
/// Shows how logging integrates naturally into your code.
pub fn process_item(item: String) -> Result(String, String) {
  log.info("Processing item: " <> item, [])

  case item {
    "" -> {
      log.error("Empty item received", [])
      Error("Item cannot be empty")
    }
    _ -> {
      log.debug("Item processed successfully", [])
      Ok("Processed: " <> item)
    }
  }
}
