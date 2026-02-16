//// Global Config Example
////
//// Demonstrates application-wide logging configuration.

import birch as log
import birch/handler/console
import birch/handler/json
import birch/level
import birch/logger

pub fn main() {
  log.info("=== Global Config Demo ===")

  // Basic configuration
  demo_basic_config()

  // Multiple handlers
  demo_multiple_handlers()

  // Global context
  demo_global_context()

  // Runtime level changes
  demo_runtime_level_changes()

  // Reset to defaults
  log.reset_config()
  log.info("Demo complete - config reset to defaults")
}

/// Demonstrate basic configuration.
fn demo_basic_config() {
  log.info("--- Basic Configuration ---")

  // Configure with debug level
  log.configure([log.config_level(level.Debug)])

  log.info("Log level set to Debug")
  log.debug("Now debug messages appear")

  // Check current level
  let current = log.get_level()
  log.info("Current level: " <> level.to_string(current))
}

/// Demonstrate multiple handlers.
fn demo_multiple_handlers() {
  log.info("--- Multiple Handlers ---")

  // Configure with both console and JSON handlers
  log.configure([
    log.config_level(level.Info),
    log.config_handlers([console.handler(), json.handler()]),
  ])

  let lgr = log.new("app")
  log.info("This goes to both console and JSON handlers")
  logger.info(lgr, "With metadata", [#("key", "value")])
}

/// Demonstrate global context.
fn demo_global_context() {
  log.info("--- Global Context ---")

  // Set global context that appears in all logs
  log.configure([
    log.config_handlers([console.handler()]),
    log.config_context([#("app", "myapp"), #("version", "1.0.0")]),
  ])

  let lgr = log.new("app")
  log.info("This includes global context")
  logger.info(lgr, "Plus additional metadata", [#("request_id", "123")])
}

/// Demonstrate runtime level changes.
fn demo_runtime_level_changes() {
  log.info("--- Runtime Level Changes ---")

  log.configure([
    log.config_handlers([console.handler()]),
    log.config_level(level.Info),
  ])

  log.info("Starting at Info level")
  log.debug("This debug is filtered")

  // Enable debug logging
  log.set_level(level.Debug)
  log.info("Level changed to Debug")
  log.debug("Now debug appears")

  // Reduce to Warn only
  log.set_level(level.Warn)
  log.info("This info is now filtered")
  log.warn("Only warn and above appear")

  // Back to Info
  log.set_level(level.Info)
  log.info("Back to Info level")
}

/// Configure logging for a production environment.
pub fn configure_production() -> Nil {
  log.configure([
    log.config_level(level.Info),
    log.config_handlers([json.handler()]),
    log.config_context([#("env", "production")]),
  ])
}

/// Configure logging for development.
pub fn configure_development() -> Nil {
  log.configure([
    log.config_level(level.Debug),
    log.config_handlers([console.handler()]),
    log.config_context([#("env", "development")]),
  ])
}
