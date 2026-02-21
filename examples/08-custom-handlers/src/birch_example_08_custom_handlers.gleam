//// Custom Handlers Example
////
//// Demonstrates creating custom log handlers.

import birch as log
import birch/formatter
import birch/handler.{type Handler}
import birch/level
import birch/record.{type LogRecord}
import gleam/io
import gleam/string

pub fn main() {
  log.info("=== Custom Handlers Demo ===", [])

  // Simple custom handler
  demo_simple_handler()

  // Custom formatter
  demo_custom_format()

  // Multiple handlers
  demo_multiple_handlers()

  // Reset to defaults
  log.reset_config()
  log.info("Demo complete", [])
}

/// Demonstrate a simple custom handler.
fn demo_simple_handler() {
  log.info("--- Simple Custom Handler ---", [])

  let simple_handler =
    handler.new(
      name: "simple",
      write: fn(message) { io.println("[CUSTOM] " <> message) },
      format: formatter.simple,
    )

  log.configure([log.config_handlers([simple_handler])])
  log.info("This uses a simple custom handler", [])
}

/// Demonstrate a custom formatter.
fn demo_custom_format() {
  log.info("--- Custom Formatter ---", [])

  let custom_handler =
    handler.new(
      name: "custom-format",
      write: fn(message) { io.println(message) },
      format: emoji_format,
    )

  log.configure([log.config_handlers([custom_handler])])
  log.debug("Debug message", [])
  log.info("Info message", [])
  log.warn("Warning message", [])
  log.error("Error message", [])
}

/// Demonstrate using multiple handlers.
fn demo_multiple_handlers() {
  log.info("--- Multiple Handlers ---", [])

  // A handler that prefixes messages
  let prefix_handler =
    handler.new(
      name: "prefix",
      write: fn(message) { io.println("[PREFIX] " <> message) },
      format: formatter.simple,
    )

  // A handler that uppercases messages
  let upper_handler =
    handler.new(
      name: "upper",
      write: fn(message) { io.println(string.uppercase(message)) },
      format: formatter.simple,
    )

  log.configure([log.config_handlers([prefix_handler, upper_handler])])
  log.info("This goes to both handlers", [])
}

/// Custom formatter that uses emoji for log levels.
fn emoji_format(record: LogRecord) -> String {
  let emoji = case record.level {
    level.Trace -> "🔍"
    level.Debug -> "🐛"
    level.Info -> "ℹ️"
    level.Warn -> "⚠️"
    level.Err -> "❌"
    level.Fatal -> "💀"
  }

  emoji <> " " <> record.message
}

/// Create a handler with a minimum level filter.
/// Only logs at or above the specified level will be handled.
pub fn create_filtered_handler(name: String, min_level: level.Level) -> Handler {
  handler.new(
    name: name,
    write: fn(message) { io.println(message) },
    format: formatter.human_readable,
  )
  |> handler.with_min_level(min_level)
}

/// Create a simple prefix handler.
pub fn create_prefix_handler(name: String, prefix: String) -> Handler {
  handler.new(
    name: name,
    write: fn(message) { io.println(prefix <> message) },
    format: formatter.simple,
  )
}
