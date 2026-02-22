//// Error Helpers Example
////
//// Demonstrates convenient error logging with Result types.

import birch as log
import birch/logger
import birch/meta
import simplifile

pub fn main() {
  log.info("=== Error Helpers Demo ===")

  // Basic error_result
  demo_error_result()

  // With metadata
  demo_error_result_with_metadata()

  // Fatal result
  demo_fatal_result()

  // Named logger variant
  demo_logger_error_result()

  log.reset_config()
  log.info("Demo complete")
}

/// Demonstrate basic error_result usage.
fn demo_error_result() {
  log.info("--- Basic error_result ---")

  // Simulate a file operation that fails
  let result = simplifile.read("/nonexistent/file.txt")

  case result {
    Ok(content) -> log.info("File content: " <> content)
    Error(_) -> {
      // The error value is automatically extracted and logged
      log.error_result("Failed to read file", result)
    }
  }
}

/// Demonstrate error_result with additional metadata.
fn demo_error_result_with_metadata() {
  log.info("--- error_result with metadata ---")

  let path = "/another/missing/file.txt"
  let result = simplifile.read(path)

  let lgr = log.new("app")
  case result {
    Ok(_) -> Nil
    Error(_) -> {
      // Add context about what we were trying to do
      logger.error_result(lgr, "Configuration load failed", result, [
        meta.string("path", path),
        meta.string("fallback", "using_defaults"),
      ])
    }
  }
}

/// Demonstrate fatal_result for critical errors.
fn demo_fatal_result() {
  log.info("--- fatal_result ---")

  // Simulate a critical failure
  let result: Result(String, String) = Error("Database connection lost")

  log.fatal_result("Critical system failure", result)
}

/// Demonstrate logger.error_result for named loggers.
fn demo_logger_error_result() {
  log.info("--- logger.error_result ---")

  let db_logger =
    log.new("myapp.database")
    |> log.with_context([meta.string("component", "database")])

  // Simulate a database error
  let result: Result(Int, String) = Error("Connection timeout after 30s")

  logger.error_result(db_logger, "Query failed", result, [
    meta.string("query", "SELECT * FROM users"),
    meta.string("timeout_ms", "30000"),
  ])
}

/// Example of using error_result in a real function.
pub fn load_config(path: String) -> Result(String, String) {
  let lgr = log.new("app")
  case simplifile.read(path) {
    Ok(content) -> {
      logger.info(lgr, "Configuration loaded", [meta.string("path", path)])
      Ok(content)
    }
    Error(_) as result -> {
      logger.error_result(lgr, "Failed to load configuration", result, [
        meta.string("path", path),
      ])
      Error("Failed to load configuration from " <> path)
    }
  }
}
