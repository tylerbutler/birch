//// Error Helpers Example
////
//// Demonstrates convenient error logging with Result types.

import birch as log
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

  case result {
    Ok(_) -> Nil
    Error(_) -> {
      // Add context about what we were trying to do
      log.error_result_m("Configuration load failed", result, [
        #("path", path),
        #("fallback", "using_defaults"),
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

/// Demonstrate logger_error_result for named loggers.
fn demo_logger_error_result() {
  log.info("--- logger_error_result ---")

  let db_logger =
    log.new("myapp.database")
    |> log.with_context([#("component", "database")])

  // Simulate a database error
  let result: Result(Int, String) = Error("Connection timeout after 30s")

  log.logger_error_result(db_logger, "Query failed", result, [
    #("query", "SELECT * FROM users"),
    #("timeout_ms", "30000"),
  ])
}

/// Example of using error_result in a real function.
pub fn load_config(path: String) -> Result(String, String) {
  case simplifile.read(path) {
    Ok(content) -> {
      log.info_m("Configuration loaded", [#("path", path)])
      Ok(content)
    }
    Error(_) as result -> {
      log.error_result_m("Failed to load configuration", result, [
        #("path", path),
      ])
      Error("Failed to load configuration from " <> path)
    }
  }
}
