//// Named Loggers Example
////
//// Demonstrates using named loggers to organize logs by component.

import birch as log
import birch/level
import birch/logger.{type Logger}

pub fn main() {
  // Enable debug to see more output
  log.set_level(level.Debug)

  log.info("=== Named Loggers Demo ===")

  // Create component loggers
  demo_component_loggers()

  // Logger with persistent context
  demo_logger_context()

  // Simulate a multi-component application
  demo_application()

  // Reset to default
  log.reset_config()
}

/// Create loggers for different components.
fn demo_component_loggers() {
  log.info("--- Component Loggers ---")

  let db_logger = log.new("myapp.database")
  let http_logger = log.new("myapp.http")
  let cache_logger = log.new("myapp.cache")

  db_logger |> logger.info("Database connection established", [])
  http_logger |> logger.info("HTTP server starting", [])
  cache_logger |> logger.debug("Cache initialized", [])
}

/// Logger with persistent context.
fn demo_logger_context() {
  log.info("--- Logger Context ---")

  // Create a logger with persistent context
  let worker_logger =
    log.new("myapp.worker")
    |> log.with_context([#("worker_id", "worker-1"), #("queue", "high-priority")])

  // All messages include the context
  worker_logger |> logger.info("Worker started", [])
  worker_logger |> logger.debug("Polling for jobs", [])
  worker_logger
  |> logger.info("Job completed", [#("job_id", "job-123"), #("duration_ms", "42")])
}

/// Simulate a multi-component application.
fn demo_application() {
  log.info("--- Application Simulation ---")

  // Create component modules
  let db = create_database_logger()
  let http = create_http_logger()

  // Simulate startup
  db |> logger.info("Connecting to database", [])
  db |> logger.info("Connection pool initialized", [#("pool_size", "10")])
  http |> logger.info("Starting HTTP server", [#("port", "8080")])
  http |> logger.info("Server ready", [])

  // Simulate a request
  http |> logger.debug("Request received", [#("method", "GET"), #("path", "/api/users")])
  db |> logger.debug("Executing query", [#("query", "SELECT * FROM users")])
  db |> logger.debug("Query completed", [#("rows", "25")])
  http |> logger.info("Response sent", [#("status", "200"), #("duration_ms", "15")])
}

/// Create a database logger for a module.
pub fn create_database_logger() -> Logger {
  log.new("myapp.database")
  |> log.with_context([#("component", "database")])
}

/// Create an HTTP logger for a module.
pub fn create_http_logger() -> Logger {
  log.new("myapp.http")
  |> log.with_context([#("component", "http")])
}

/// Example of a function that takes a logger.
/// This pattern allows callers to control logging behavior.
pub fn process_with_logger(lgr: Logger, data: String) -> String {
  lgr |> logger.debug("Processing data", [#("length", "5")])
  let result = "processed: " <> data
  lgr |> logger.debug("Processing complete", [])
  result
}
