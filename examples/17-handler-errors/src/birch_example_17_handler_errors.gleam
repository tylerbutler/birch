//// Handler Errors Example
////
//// Demonstrates error handling strategies for logging handlers.

import birch as log
import birch/formatter
import birch/handler
import birch/handler/console

pub fn main() {
  log.info("=== Handler Errors Demo ===", [])

  // Demo 1: Error callback
  demo_error_callback()

  // Demo 2: Global error handling
  demo_global_error_handling()

  // Demo 3: Graceful degradation
  demo_graceful_degradation()

  log.reset_config()
  log.info("Demo complete", [])
}

/// Demonstrate handler-level error callbacks.
fn demo_error_callback() {
  log.info("--- Handler Error Callback ---", [])

  // Create a handler with an error callback
  let handler_with_callback =
    console.handler()
    |> handler.with_error_callback(fn(err: handler.HandlerError) {
      // This would be called if the handler fails
      // In practice, console rarely fails
      // err contains handler_name, error message, and the log record
      let handler.HandlerError(handler_name: name, error: msg, ..) = err
      log.error("Handler " <> name <> " failed: " <> msg, [])
    })

  log.configure([log.config_handlers([handler_with_callback])])

  log.info("Handler with error callback configured", [])
  log.info("If this handler fails, the callback will be invoked", [])

  log.reset_config()
}

/// Demonstrate global error handling configuration.
fn demo_global_error_handling() {
  log.info("--- Global Error Handling ---", [])

  // Configure global error handling
  log.configure([
    log.config_on_error(fn(err: handler.HandlerError) {
      // Called when any handler fails
      // Note: be careful not to create infinite loops here
      let handler.HandlerError(handler_name: name, error: msg, ..) = err
      log.error("Error in " <> name <> ": " <> msg, [])
    }),
  ])

  log.info("Global error handler configured", [])
  log.info("Any handler failure will trigger the callback", [])

  log.reset_config()
}

/// Demonstrate graceful degradation patterns.
fn demo_graceful_degradation() {
  log.info("--- Graceful Degradation ---", [])

  // Create a "safe" handler that silently handles errors
  let safe_handler = create_safe_handler()

  log.configure([log.config_handlers([safe_handler])])

  log.info("Using safe handler - errors are silently handled", [])
  log.info("Application continues even if logging fails", [])

  log.reset_config()
}

/// Create a handler with silent error handling.
pub fn create_safe_handler() -> handler.Handler {
  console.handler()
  |> handler.with_error_callback(fn(_err: handler.HandlerError) {
    // Silently ignore errors
    // Logging failures shouldn't crash the application
    Nil
  })
}

/// Create a handler that reports errors to a monitoring callback.
pub fn create_monitored_handler(
  on_error: fn(handler.HandlerError) -> Nil,
) -> handler.Handler {
  console.handler()
  |> handler.with_error_callback(on_error)
}

/// Example of creating a resilient handler with fallback behavior.
/// In real usage, you might fall back to a simpler handler.
pub fn create_resilient_handler() -> handler.Handler {
  // Primary handler with error callback
  let primary =
    console.handler()
    |> handler.with_error_callback(fn(err: handler.HandlerError) {
      // In a real implementation, you might:
      // 1. Switch to a fallback handler
      // 2. Buffer messages for retry
      // 3. Alert monitoring systems
      let handler.HandlerError(error: msg, ..) = err
      log.error("Primary handler failed: " <> msg, [])
    })

  primary
}

/// Create a custom handler that can fail (for demonstration).
pub fn create_failing_handler() -> handler.Handler {
  handler.new(
    name: "failing",
    write: fn(_msg) {
      // This handler always "succeeds" but demonstrates the pattern
      // In reality, this might be a network call that fails
      Nil
    },
    format: formatter.simple,
  )
  |> handler.with_error_callback(fn(err: handler.HandlerError) {
    // Handle the failure gracefully
    let handler.HandlerError(error: msg, ..) = err
    log.error("Failing handler error: " <> msg, [])
  })
}
