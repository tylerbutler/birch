//// Global Config Example
////
//// Demonstrates application-wide logging configuration.

import birch
import birch/handler/console
import birch/handler/json
import birch/level
import birch/log
import birch/meta

pub fn main() {
  birch.info("=== Global Config Demo ===")

  // Basic configuration
  demo_basic_config()

  // Multiple handlers
  demo_multiple_handlers()

  // Global context
  demo_global_context()

  // Runtime level changes
  demo_runtime_level_changes()

  // Reset to defaults
  birch.reset_config()
  birch.info("Demo complete - config reset to defaults")
}

/// Demonstrate basic configuration.
fn demo_basic_config() {
  birch.info("--- Basic Configuration ---")

  // Configure with debug level
  birch.configure([birch.config_level(level.Debug)])

  birch.info("Log level set to Debug")
  birch.debug("Now debug messages appear")

  // Check current level
  let current = birch.get_level()
  birch.info("Current level: " <> level.to_string(current))
}

/// Demonstrate multiple handlers.
fn demo_multiple_handlers() {
  birch.info("--- Multiple Handlers ---")

  // Configure with both console and JSON handlers
  birch.configure([
    birch.config_level(level.Info),
    birch.config_handlers([console.handler(), json.handler()]),
  ])

  let lgr = birch.new("app")
  birch.info("This goes to both console and JSON handlers")
  log.info(lgr, "With metadata", [meta.string("key", "value")])
}

/// Demonstrate global context.
fn demo_global_context() {
  birch.info("--- Global Context ---")

  // Set global context that appears in all logs
  birch.configure([
    birch.config_handlers([console.handler()]),
    birch.config_context([meta.string("app", "myapp"), meta.string("version", "1.0.0")]),
  ])

  let lgr = birch.new("app")
  birch.info("This includes global context")
  log.info(lgr, "Plus additional metadata", [meta.string("request_id", "123")])
}

/// Demonstrate runtime level changes.
fn demo_runtime_level_changes() {
  birch.info("--- Runtime Level Changes ---")

  birch.configure([
    birch.config_handlers([console.handler()]),
    birch.config_level(level.Info),
  ])

  birch.info("Starting at Info level")
  birch.debug("This debug is filtered")

  // Enable debug logging
  birch.set_level(level.Debug)
  birch.info("Level changed to Debug")
  birch.debug("Now debug appears")

  // Reduce to Warn only
  birch.set_level(level.Warn)
  birch.info("This info is now filtered")
  birch.warn("Only warn and above appear")

  // Back to Info
  birch.set_level(level.Info)
  birch.info("Back to Info level")
}

/// Configure logging for a production environment.
pub fn configure_production() -> Nil {
  birch.configure([
    birch.config_level(level.Info),
    birch.config_handlers([json.handler()]),
    birch.config_context([meta.string("env", "production")]),
  ])
}

/// Configure logging for development.
pub fn configure_development() -> Nil {
  birch.configure([
    birch.config_level(level.Debug),
    birch.config_handlers([console.handler()]),
    birch.config_context([meta.string("env", "development")]),
  ])
}
