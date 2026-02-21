//// Log Levels Example
////
//// Demonstrates birch's six log levels and how filtering works.

import birch as log
import birch/level

pub fn main() {
  log.info("=== Log Levels Demo ===", [])

  // Show all six levels (some will be filtered at default Info level)
  demo_all_levels()

  // Demonstrate runtime level changes
  demo_level_changes()

  // Demonstrate parsing levels from strings
  demo_level_parsing()
}

/// Demonstrate all six log levels.
fn demo_all_levels() {
  log.info("--- All Six Levels (at default Info level) ---", [])

  // These won't appear at default Info level
  log.trace("TRACE: Very detailed diagnostic info", [])
  log.debug("DEBUG: Debugging information", [])

  // These will appear
  log.info("INFO: Normal operational message", [])
  log.warn("WARN: Warning condition", [])
  log.error("ERROR: Error condition", [])
  log.fatal("FATAL: Critical error", [])
}

/// Demonstrate changing log level at runtime.
fn demo_level_changes() {
  log.info("--- Level Changes at Runtime ---", [])

  // Enable debug logging
  log.set_level(level.Debug)
  log.info("Log level set to Debug", [])
  log.debug("This debug message now appears", [])
  log.trace("But trace is still filtered", [])

  // Enable trace logging (everything)
  log.set_level(level.Trace)
  log.trace("Now trace messages appear too", [])

  // Set to warn only (less verbose)
  log.set_level(level.Warn)
  log.info("This info message is now filtered", [])
  log.warn("Only warn and above appear now", [])

  // Reset to default
  log.set_level(level.Info)
  log.info("Back to default Info level", [])
}

/// Demonstrate parsing log levels from strings.
fn demo_level_parsing() {
  log.info("--- Parsing Levels from Strings ---", [])

  // Parse various level strings
  parse_and_log("debug")
  parse_and_log("DEBUG")
  parse_and_log("warning")
  parse_and_log("error")
  parse_and_log("invalid")
}

fn parse_and_log(s: String) {
  case level.from_string(s) {
    Ok(lvl) -> log.info("Parsed '" <> s <> "' as " <> level.to_string(lvl), [])
    Error(Nil) -> log.warn("Failed to parse '" <> s <> "' as a level", [])
  }
}

/// Get the current log level.
pub fn current_level() -> level.Level {
  log.get_level()
}

/// Check if a level would be logged at the current threshold.
pub fn would_log(check_level: level.Level) -> Bool {
  level.gte(check_level, current_level())
}
