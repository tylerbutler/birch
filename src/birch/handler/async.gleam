//// Async handler for non-blocking log output.
////
//// This module provides asynchronous logging capabilities where log records
//// are queued and written by a background process/task, preventing I/O from
//// blocking application logic.
////
//// ## Erlang Target (OTP Actor Pattern)
////
//// On Erlang, this uses a proper OTP actor with gleam_otp Subjects for message
//// passing. This follows the glimt pattern, providing:
////
//// - True non-blocking behavior with natural backpressure via actor mailbox
//// - Clean integration with OTP supervision trees
//// - Proper flush semantics with synchronous confirmation
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
import birch/record.{type LogRecord}

// Erlang-only imports
@target(erlang)
import birch/internal/async_actor.{type AsyncActor, type Message}

@target(erlang)
import gleam/dict.{type Dict}

@target(erlang)
import gleam/erlang/process.{type Subject}

// JavaScript-only imports
@target(javascript)
import birch/internal/platform

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

/// Convert overflow behavior to integer for FFI.
fn overflow_to_int(overflow: OverflowBehavior) -> Int {
  case overflow {
    DropOldest -> 0
    DropNewest -> 1
    Block -> 2
  }
}

// ============================================================================
// Erlang Implementation (OTP Actor Pattern)
// ============================================================================

@target(erlang)
/// Registry of active async actors (Erlang only).
/// This uses a mutable reference stored via process dictionary for simplicity.
type ActorRegistry =
  Dict(String, AsyncActor)

@target(erlang)
/// Get or create the actor registry.
fn get_actor_registry() -> ActorRegistry {
  get_actor_registry_ffi()
}

@target(erlang)
@external(erlang, "birch_ffi", "get_actor_registry")
fn get_actor_registry_ffi() -> ActorRegistry

@target(erlang)
fn set_actor_registry(registry: ActorRegistry) -> Nil {
  set_actor_registry_ffi(registry)
}

@target(erlang)
@external(erlang, "birch_ffi", "set_actor_registry")
fn set_actor_registry_ffi(registry: ActorRegistry) -> Nil

@target(erlang)
/// Wrap a handler to make it asynchronous (Erlang implementation).
///
/// Uses an OTP actor with gleam_otp Subject for message passing.
/// This follows the glimt pattern for async logging.
pub fn make_async(base_handler: Handler, async_config: AsyncConfig) -> Handler {
  let base_name = handler.name(base_handler)
  let async_name = "async:" <> base_name

  // Start the OTP actor
  let assert Ok(actor) =
    async_actor.start(
      base_handler,
      async_config.queue_size,
      overflow_to_int(async_config.overflow),
    )

  // Register the actor for later flush/shutdown
  let registry = get_actor_registry()
  let new_registry = dict.insert(registry, async_name, actor)
  set_actor_registry(new_registry)

  // Create a handler that sends records to the actor
  handler.new_with_record_write(name: async_name, write: fn(record: LogRecord) {
    async_actor.send(actor, record)
  })
}

@target(erlang)
/// Flush all pending log messages (Erlang implementation).
///
/// This function blocks until all queued messages have been written.
/// Use this before application shutdown to ensure no logs are lost.
pub fn flush() -> Nil {
  let registry = get_actor_registry()
  dict.each(registry, fn(_name, actor) { async_actor.flush(actor) })
}

@target(erlang)
/// Flush a specific async handler by name (Erlang implementation).
pub fn flush_handler(name: String) -> Nil {
  let registry = get_actor_registry()
  case dict.get(registry, name) {
    Ok(actor) -> async_actor.flush(actor)
    Error(Nil) -> Nil
  }
}

@target(erlang)
/// Shutdown all async handlers gracefully (Erlang only).
///
/// This flushes pending records and stops all async actors.
/// Call this during application shutdown.
pub fn shutdown_all() -> Nil {
  let registry = get_actor_registry()
  dict.each(registry, fn(_name, actor) { async_actor.shutdown(actor) })
  set_actor_registry(dict.new())
}

@target(erlang)
/// Shutdown a specific async handler (Erlang only).
pub fn shutdown_handler(name: String) -> Nil {
  let registry = get_actor_registry()
  case dict.get(registry, name) {
    Ok(actor) -> {
      async_actor.shutdown(actor)
      let new_registry = dict.delete(registry, name)
      set_actor_registry(new_registry)
    }
    Error(Nil) -> Nil
  }
}

@target(erlang)
/// Get the OTP Subject for an async handler (Erlang only).
///
/// This allows advanced users to interact directly with the actor,
/// for example to add it to a supervision tree.
pub fn get_subject(name: String) -> Result(Subject(Message), Nil) {
  let registry = get_actor_registry()
  case dict.get(registry, name) {
    Ok(actor) -> Ok(async_actor.subject(actor))
    Error(Nil) -> Error(Nil)
  }
}

// ============================================================================
// JavaScript Implementation (FFI-based)
// ============================================================================

@target(javascript)
/// Wrap a handler to make it asynchronous (JavaScript implementation).
///
/// Uses setTimeout batching for async writes.
pub fn make_async(base_handler: Handler, async_config: AsyncConfig) -> Handler {
  let base_name = handler.name(base_handler)
  let async_name = "async:" <> base_name

  // Create the callback that will be called by the async worker
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

@target(javascript)
/// Flush all pending log messages (JavaScript implementation).
pub fn flush() -> Nil {
  platform.flush_async_writers()
}

@target(javascript)
/// Flush a specific async handler by name (JavaScript implementation).
pub fn flush_handler(name: String) -> Nil {
  platform.flush_async_writer(name)
}
