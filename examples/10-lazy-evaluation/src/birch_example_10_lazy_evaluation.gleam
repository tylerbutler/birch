//// Lazy Evaluation Example
////
//// Demonstrates lazy evaluation for performance optimization.

import birch as log
import birch/level
import gleam/int
import gleam/list
import gleam/string

pub fn main() {
  log.info("=== Lazy Evaluation Demo ===", [])

  // Show the problem with eager evaluation
  demo_eager_evaluation()

  // Show lazy evaluation solution
  demo_lazy_evaluation()

  // Comparison at different log levels
  demo_level_comparison()

  log.reset_config()
  log.info("Demo complete", [])
}

/// Demonstrate the problem with eager evaluation.
fn demo_eager_evaluation() {
  log.info("--- Eager Evaluation (Problem) ---", [])

  // Set level to Info (debug filtered)
  log.configure([log.config_level(level.Info)])

  log.info("Level is Info - debug messages are filtered", [])

  // This ALWAYS evaluates, even though debug is filtered!
  // In real code, this could be an expensive operation
  log.debug("Eager: " <> simulate_expensive_operation(), [])

  log.info("Notice: expensive_operation() was called even though debug was filtered", [])
}

/// Demonstrate lazy evaluation solution.
fn demo_lazy_evaluation() {
  log.info("--- Lazy Evaluation (Solution) ---", [])

  // Set level to Info (debug filtered)
  log.configure([log.config_level(level.Info)])

  log.info("Level is Info - debug messages are filtered", [])

  // This is only evaluated if debug level is enabled
  log.debug_lazy(fn() { "Lazy: " <> simulate_expensive_operation() })

  log.info("With lazy evaluation, expensive_operation() was NOT called", [])
}

/// Compare behavior at different log levels.
fn demo_level_comparison() {
  log.info("--- Level Comparison ---", [])

  // At Debug level, lazy function IS called
  log.configure([log.config_level(level.Debug)])
  log.info("Level is Debug - lazy function will be called", [])
  log.debug_lazy(fn() { "This lazy message IS evaluated: " <> int.to_string(42) })

  // At Warn level, lazy function is NOT called
  log.configure([log.config_level(level.Warn)])
  log.warn("Level is Warn - lazy debug function will NOT be called", [])
  log.debug_lazy(fn() {
    // This code never runs!
    "This never evaluates: " <> simulate_expensive_operation()
  })

  // Same with info_lazy
  log.info_lazy(fn() {
    // This code never runs either!
    "This never evaluates: " <> simulate_expensive_operation()
  })

  log.warn("Neither lazy debug nor lazy info were evaluated", [])
}

/// Simulate an expensive operation.
/// In real code, this could be:
/// - Serializing a large data structure
/// - Computing a complex value
/// - Making a network call for additional info
fn simulate_expensive_operation() -> String {
  // Just for demonstration
  "expensive_result"
}

/// Example: Debug logging with large data serialization.
/// Use lazy evaluation when serializing lists, records, etc.
pub fn log_large_data(items: List(String)) -> Nil {
  // Bad: Always serializes the list
  // log.debug("Items: " <> string.inspect(items, []))

  // Good: Only serializes if debug is enabled
  log.debug_lazy(fn() {
    "Items (" <> int.to_string(list.length(items)) <> "): " <> string.join(items, ", ")
  })
}
