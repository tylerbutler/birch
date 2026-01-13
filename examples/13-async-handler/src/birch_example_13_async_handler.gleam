//// Async Handler Example
////
//// Demonstrates non-blocking logging with async handlers.

import birch as log
import birch/handler/async
import birch/handler/console

pub fn main() {
  log.info("=== Async Handler Demo ===")

  // Basic async handler
  demo_basic_async()

  // Custom configuration
  demo_custom_config()

  // Important: flush before exit
  demo_flush()

  log.reset_config()
  log.info("Demo complete")
}

/// Demonstrate basic async handler.
fn demo_basic_async() {
  log.info("--- Basic Async Handler ---")

  // Wrap console handler with async
  let async_console =
    console.handler()
    |> async.make_async(async.default_config())

  log.configure([log.config_handlers([async_console])])

  // These logs are queued and written asynchronously
  log.info("This is logged asynchronously")
  log.info("The main thread continues immediately")
  log.info("Logs are written in the background")

  // Wait for logs to be written
  async.flush()
}

/// Demonstrate custom async configuration.
fn demo_custom_config() {
  log.info("--- Custom Async Configuration ---")

  // Configure async behavior
  let config =
    async.config()
    |> async.with_queue_size(2000)
    // Allow 2000 pending messages
    |> async.with_flush_interval(50)
    // Flush every 50ms
    |> async.with_overflow(async.DropOldest)
  // Drop old messages if full

  let async_handler =
    console.handler()
    |> async.make_async(config)

  log.configure([log.config_handlers([async_handler])])

  log.info("Using custom async configuration")
  log.info("Queue size: 2000, flush interval: 50ms")

  async.flush()
}

/// Demonstrate proper flush before shutdown.
fn demo_flush() {
  log.info("--- Flush Before Shutdown ---")

  let async_handler =
    console.handler()
    |> async.make_async(async.default_config())

  log.configure([log.config_handlers([async_handler])])

  // Log some messages
  log.info("Message 1")
  log.info("Message 2")
  log.info("Message 3")

  // IMPORTANT: Always flush before exit
  // Without this, messages might be lost
  async.flush()

  log.info("All messages flushed successfully")
}

/// Create an async file handler for production.
pub fn create_async_file_handler() -> log.LogHandler {
  // In real code, you'd use a file handler here
  let base_handler = console.handler()

  let config =
    async.config()
    |> async.with_queue_size(10_000)
    |> async.with_flush_interval(100)
    |> async.with_overflow(async.DropOldest)

  async.make_async(base_handler, config)
}
