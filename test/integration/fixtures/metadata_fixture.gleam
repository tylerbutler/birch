//// Integration test fixture for metadata and context.
////
//// This fixture exercises context and metadata handling.
//// The test harness captures output and verifies:
//// - Global context is applied to all logs
//// - Scoped context works correctly
//// - Metadata merges properly
//// - Named loggers include their context

import birch as log
import birch/handler/json
import birch/level

pub fn main() {
  // Configure with JSON handler for easy parsing, and global context
  log.configure([
    log.config_level(level.Debug),
    log.config_handlers([json.handler()]),
    log.config_context([#("service", "test-service"), #("env", "test")]),
  ])

  // Basic log - should include global context
  log.info("Basic message with global context")

  // Log with additional metadata - should merge with global context
  log.info_m("Message with extra metadata", [#("request_id", "req-789")])

  // Test scoped context
  log.with_scope([#("scope_id", "scope-abc")], fn() {
    log.info("Inside scoped context")

    // Nested scope
    log.with_scope([#("nested", "true")], fn() {
      log.info("Inside nested scope")
      Nil
    })

    Nil
  })

  // After scope - should not have scope context
  log.info("After scoped context")

  // Named logger with its own context
  let db_logger =
    log.new("myapp.database")
    |> log.with_context([#("component", "database")])

  log.logger_info(db_logger, "Database log", [#("query", "SELECT *")])

  // Reset config for clean state
  log.reset_config()
}
