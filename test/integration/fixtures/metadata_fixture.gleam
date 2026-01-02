//// Integration test fixture for metadata and context.
////
//// This fixture exercises context and metadata handling.
//// The test harness captures output and verifies:
//// - Global context is applied to all logs
//// - Scoped context works correctly
//// - Metadata merges properly
//// - Named loggers include their context

import gleam_log
import gleam_log/handler/json
import gleam_log/level

pub fn main() {
  // Configure with JSON handler for easy parsing, and global context
  gleam_log.configure([
    gleam_log.config_level(level.Debug),
    gleam_log.config_handlers([json.handler()]),
    gleam_log.config_context([#("service", "test-service"), #("env", "test")]),
  ])

  // Basic log - should include global context
  gleam_log.info("Basic message with global context")

  // Log with additional metadata - should merge with global context
  gleam_log.info_m("Message with extra metadata", [#("request_id", "req-789")])

  // Test scoped context
  gleam_log.with_scope([#("scope_id", "scope-abc")], fn() {
    gleam_log.info("Inside scoped context")

    // Nested scope
    gleam_log.with_scope([#("nested", "true")], fn() {
      gleam_log.info("Inside nested scope")
      Nil
    })

    Nil
  })

  // After scope - should not have scope context
  gleam_log.info("After scoped context")

  // Named logger with its own context
  let db_logger =
    gleam_log.new("myapp.database")
    |> gleam_log.with_context([#("component", "database")])

  gleam_log.logger_info(db_logger, "Database log", [#("query", "SELECT *")])

  // Reset config for clean state
  gleam_log.reset_config()
}
