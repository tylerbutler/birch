//// Async handler for non-blocking log output.
////
//// This module provides asynchronous logging capabilities where log records
//// are queued and written by a background process/task, preventing I/O from
//// blocking application logic.
////
//// ## Erlang Target
////
//// On Erlang, a separate process is spawned that receives log records via
//// message passing. This provides true non-blocking behavior with natural
//// backpressure via the process mailbox.
////
//// ## JavaScript Target
////
//// On JavaScript, records are queued in memory and processed via
//// setTimeout/setImmediate batching. This allows the main thread to continue
//// while logs are written asynchronously.
////
//// ## Usage
////
//// ```gleam
//// import birch/handler/async
//// import birch/handler/console
////
//// // Wrap any handler to make it async
//// let async_console =
////   console.handler()
////   |> async.make_async(async.default_config())
////
//// // Use with custom configuration
//// let config =
////   async.config()
////   |> async.with_queue_size(5000)
////   |> async.with_flush_interval(50)
////   |> async.with_overflow(async.Block)
////
//// let handler = async.make_async(console.handler(), config)
////
//// // Before shutdown, ensure all logs are written
//// async.flush()
//// ```

import birch/handler.{type Handler}
import birch/internal/platform
import birch/record.{type LogRecord}

/// Behavior when the async queue is full.
pub type OverflowBehavior {
  /// Drop the oldest message in the queue to make room
  DropOldest
  /// Drop the newest message (the one being added)
  DropNewest
  /// Block until there is room in the queue (use with caution)
  Block
}

/// Configuration for async handlers.
pub type AsyncConfig {
  AsyncConfig(
    /// Maximum number of pending messages in the queue
    queue_size: Int,
    /// Flush interval in milliseconds (for batch writes)
    flush_interval_ms: Int,
    /// What to do when the queue is full
    overflow: OverflowBehavior,
  )
}

/// Create an empty async configuration builder.
pub fn config() -> AsyncConfig {
  AsyncConfig(queue_size: 1000, flush_interval_ms: 100, overflow: DropOldest)
}

/// Get the default async configuration.
///
/// - queue_size: 1000 messages
/// - flush_interval_ms: 100ms
/// - overflow: DropOldest
pub fn default_config() -> AsyncConfig {
  config()
}

/// Set the maximum queue size.
pub fn with_queue_size(config: AsyncConfig, size: Int) -> AsyncConfig {
  AsyncConfig(..config, queue_size: size)
}

/// Set the flush interval in milliseconds.
pub fn with_flush_interval(config: AsyncConfig, interval_ms: Int) -> AsyncConfig {
  AsyncConfig(..config, flush_interval_ms: interval_ms)
}

/// Set the overflow behavior.
pub fn with_overflow(
  config: AsyncConfig,
  overflow: OverflowBehavior,
) -> AsyncConfig {
  AsyncConfig(..config, overflow: overflow)
}

/// Wrap a handler to make it asynchronous.
///
/// Log records sent to the returned handler are queued and processed
/// by a background process (Erlang) or via setTimeout batching (JavaScript).
///
/// The original handler's write function is called from the background
/// process/task, not the calling code.
pub fn make_async(base_handler: Handler, async_config: AsyncConfig) -> Handler {
  let base_name = handler.name(base_handler)
  let async_name = "async:" <> base_name

  // Create the callback that will be called by the async worker
  // This callback calls the base handler with each record
  let callback = fn(record: LogRecord) { handler.handle(base_handler, record) }

  // Start the async writer with the callback
  let writer_id =
    platform.start_async_writer(
      async_name,
      callback,
      async_config.queue_size,
      async_config.flush_interval_ms,
      overflow_to_int(async_config.overflow),
    )

  // Create a handler that sends records to the async writer
  handler.new_with_record_write(name: async_name, write: fn(record: LogRecord) {
    platform.async_send(writer_id, record)
  })
}

/// Convert overflow behavior to integer for FFI.
fn overflow_to_int(overflow: OverflowBehavior) -> Int {
  case overflow {
    DropOldest -> 0
    DropNewest -> 1
    Block -> 2
  }
}

/// Flush all pending log messages.
///
/// This function blocks until all queued messages have been written.
/// Use this before application shutdown to ensure no logs are lost.
pub fn flush() -> Nil {
  platform.flush_async_writers()
}

/// Flush a specific async handler by name.
pub fn flush_handler(name: String) -> Nil {
  platform.flush_async_writer(name)
}
