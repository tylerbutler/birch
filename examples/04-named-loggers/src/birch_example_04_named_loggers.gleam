//// Named Loggers Example
////
//// Demonstrates using named loggers to organize logs by component.

import birch
import birch/level
import birch/log.{type Logger}
import birch/meta

pub fn main() {
  // Enable debug to see more output
  birch.set_level(level.Debug)

  birch.info("=== Named Loggers Demo ===")

  // Create component loggers
  demo_component_loggers()

  // Logger with persistent context
  demo_logger_context()

  // Simulate a multi-component application
  demo_application()

  // Reset to default
  birch.reset_config()
}

/// Create loggers for different components.
fn demo_component_loggers() {
  birch.info("--- Component Loggers ---")

  let db_logger = birch.new("myapp.database")
  let http_logger = birch.new("myapp.http")
  let cache_logger = birch.new("myapp.cache")

  db_logger |> log.info("Database connection established", [])
  http_logger |> log.info("HTTP server starting", [])
  cache_logger |> log.debug("Cache initialized", [])
}

/// Logger with persistent context.
fn demo_logger_context() {
  birch.info("--- Logger Context ---")

  // Create a logger with persistent context
  let worker_logger =
    birch.new("myapp.worker")
    |> birch.with_context([meta.string("worker_id", "worker-1"), meta.string("queue", "high-priority")])

  // All messages include the context
  worker_logger |> log.info("Worker started", [])
  worker_logger |> log.debug("Polling for jobs", [])
  worker_logger
  |> log.info("Job completed", [meta.string("job_id", "job-123"), meta.string("duration_ms", "42")])
}

/// Simulate a multi-component application.
fn demo_application() {
  birch.info("--- Application Simulation ---")

  // Create component modules
  let db = create_database_logger()
  let http = create_http_logger()

  // Simulate startup
  db |> log.info("Connecting to database", [])
  db |> log.info("Connection pool initialized", [meta.string("pool_size", "10")])
  http |> log.info("Starting HTTP server", [meta.string("port", "8080")])
  http |> log.info("Server ready", [])

  // Simulate a request
  http |> log.debug("Request received", [meta.string("method", "GET"), meta.string("path", "/api/users")])
  db |> log.debug("Executing query", [meta.string("query", "SELECT * FROM users")])
  db |> log.debug("Query completed", [meta.string("rows", "25")])
  http |> log.info("Response sent", [meta.string("status", "200"), meta.string("duration_ms", "15")])
}

/// Create a database logger for a module.
pub fn create_database_logger() -> Logger {
  birch.new("myapp.database")
  |> birch.with_context([meta.string("component", "database")])
}

/// Create an HTTP logger for a module.
pub fn create_http_logger() -> Logger {
  birch.new("myapp.http")
  |> birch.with_context([meta.string("component", "http")])
}

/// Example of a function that takes a logger.
/// This pattern allows callers to control logging behavior.
pub fn process_with_logger(lgr: Logger, data: String) -> String {
  lgr |> log.debug("Processing data", [meta.string("length", "5")])
  let result = "processed: " <> data
  lgr |> log.debug("Processing complete", [])
  result
}
