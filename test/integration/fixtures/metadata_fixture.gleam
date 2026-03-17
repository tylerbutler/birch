//// Integration test fixture for metadata and context.
////
//// This fixture exercises context and metadata handling.
//// The test harness captures output and verifies:
//// - Global context is applied to all logs
//// - Scoped context works correctly
//// - Metadata merges properly
//// - Named loggers include their context

import birch
import birch/handler/json
import birch/level
import birch/log
import birch/meta

pub fn main() {
  // Configure with JSON handler for easy parsing, and global context
  birch.configure([
    birch.config_level(level.Debug),
    birch.config_handlers([json.handler()]),
    birch.config_context([
      meta.string("service", "test-service"),
      meta.string("env", "test"),
    ]),
  ])

  // Basic log - should include global context
  birch.info("Basic message with global context")

  // Log with additional metadata - should merge with global context
  birch.new("metadata-fixture")
  |> birch.with_handler(json.handler())
  |> birch.with_level(level.Debug)
  |> log.info("Message with extra metadata", [
    meta.string("request_id", "req-789"),
  ])

  // Test scoped context
  birch.with_scope([meta.string("scope_id", "scope-abc")], fn() {
    birch.info("Inside scoped context")

    // Nested scope
    birch.with_scope([meta.string("nested", "true")], fn() {
      birch.info("Inside nested scope")
      Nil
    })

    Nil
  })

  // After scope - should not have scope context
  birch.info("After scoped context")

  // Named logger with its own context
  let db_logger =
    birch.new("myapp.database")
    |> birch.with_context([meta.string("component", "database")])
    |> birch.with_handler(json.handler())

  db_logger |> log.info("Database log", [meta.string("query", "SELECT *")])

  // Reset config for clean state
  birch.reset_config()
}
