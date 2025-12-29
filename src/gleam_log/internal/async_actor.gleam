//// OTP Actor-based async handler implementation (Erlang only).
////
//// This module provides a proper OTP actor using gleam_otp for async log handling
//// on the Erlang target. It follows the glimt pattern of using Subjects for
//// message passing, providing:
////
//// - True non-blocking log writes
//// - Natural backpressure via actor mailbox
//// - Clean supervision integration
//// - Proper flush semantics
////
//// On JavaScript, this module is not used - the platform FFI handles async there.

import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam_log/handler.{type Handler}
import gleam_log/record.{type LogRecord}

/// Messages that can be sent to the async handler actor.
pub type Message {
  /// Log a record asynchronously
  Log(LogRecord)
  /// Flush all pending records and reply when done
  Flush(reply_to: Subject(Nil))
  /// Shutdown the actor gracefully
  Shutdown
}

/// Internal state of the async handler actor.
pub type State {
  State(
    /// The underlying handler to write to
    handler: Handler,
    /// Pending records (for batching if needed)
    pending: List(LogRecord),
    /// Maximum queue size before applying overflow behavior
    max_queue_size: Int,
    /// Overflow behavior (0=DropOldest, 1=DropNewest, 2=Block)
    overflow: Int,
  )
}

/// Start result containing the actor's Subject.
pub type AsyncActor {
  AsyncActor(
    /// Subject for sending messages to the actor
    subject: Subject(Message),
    /// Name of the async handler
    name: String,
  )
}

/// Error that can occur when starting the async actor.
pub type StartError {
  /// Actor failed to start
  ActorStartError(actor.StartError)
}

/// Start an async handler actor that wraps the given handler.
///
/// Returns a Subject that can be used to send log records to the actor.
/// The actor processes records asynchronously in a separate Erlang process.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(async_actor) =
///   async_actor.start(
///     console.handler(),
///     async_actor.default_config(),
///   )
///
/// // Send a log record (non-blocking)
/// async_actor.send(async_actor, record)
///
/// // Flush before shutdown
/// async_actor.flush(async_actor)
/// ```
pub fn start(
  handler: Handler,
  max_queue_size: Int,
  overflow: Int,
) -> Result(AsyncActor, StartError) {
  let handler_name = handler.name(handler)
  let async_name = "async:" <> handler_name

  let initial_state =
    State(
      handler: handler,
      pending: [],
      max_queue_size: max_queue_size,
      overflow: overflow,
    )

  case actor.start(initial_state, handle_message) {
    Ok(subject) -> Ok(AsyncActor(subject: subject, name: async_name))
    Error(err) -> Error(ActorStartError(err))
  }
}

/// Handle incoming messages to the async actor.
fn handle_message(
  message: Message,
  state: State,
) -> actor.Next(Message, State) {
  case message {
    Log(record) -> {
      // Apply overflow handling if queue is full
      let new_pending = case list.length(state.pending) >= state.max_queue_size {
        True -> handle_overflow(record, state.pending, state.overflow)
        False -> [record, ..state.pending]
      }

      // Process the record immediately (write to handler)
      // For simplicity, we write each record as it comes in
      // This provides natural backpressure via the actor mailbox
      case new_pending {
        [latest, ..rest] -> {
          handler.handle(state.handler, latest)
          actor.continue(State(..state, pending: rest))
        }
        [] -> actor.continue(state)
      }
    }

    Flush(reply_to) -> {
      // Write all pending records
      flush_pending(state.handler, state.pending)
      // Reply to indicate flush is complete
      process.send(reply_to, Nil)
      actor.continue(State(..state, pending: []))
    }

    Shutdown -> {
      // Flush remaining records before stopping
      flush_pending(state.handler, state.pending)
      actor.Stop(process.Normal)
    }
  }
}

/// Handle queue overflow based on configured behavior.
fn handle_overflow(
  record: LogRecord,
  pending: List(LogRecord),
  overflow: Int,
) -> List(LogRecord) {
  case overflow {
    // DropOldest - remove from end (oldest), add new to front
    0 -> {
      let trimmed = list.take(pending, list.length(pending) - 1)
      [record, ..trimmed]
    }
    // DropNewest - don't add the new record
    1 -> pending
    // Block - not really blocking, just add (mailbox provides backpressure)
    2 -> [record, ..pending]
    // Default: drop newest
    _ -> pending
  }
}

/// Flush all pending records to the handler.
fn flush_pending(handler: Handler, pending: List(LogRecord)) -> Nil {
  // Pending is stored newest-first, so reverse before writing
  pending
  |> list.reverse
  |> list.each(fn(record) { handler.handle(handler, record) })
}

/// Send a log record to the async actor (non-blocking).
pub fn send(actor: AsyncActor, record: LogRecord) -> Nil {
  process.send(actor.subject, Log(record))
}

/// Flush all pending records and wait for completion.
///
/// This function blocks until all queued records have been written.
/// Use before application shutdown to ensure no logs are lost.
pub fn flush(actor: AsyncActor) -> Nil {
  // Create a subject to receive the flush confirmation
  let reply_subject = process.new_subject()
  process.send(actor.subject, Flush(reply_subject))

  // Wait for the flush to complete (with timeout)
  case process.receive(reply_subject, 5000) {
    Ok(Nil) -> Nil
    Error(Nil) -> Nil
  }
}

/// Shutdown the async actor gracefully.
///
/// This flushes any pending records before stopping the actor.
pub fn shutdown(actor: AsyncActor) -> Nil {
  process.send(actor.subject, Shutdown)
}

/// Get the name of the async actor.
pub fn name(actor: AsyncActor) -> String {
  actor.name
}

/// Get the Subject for direct message sending (advanced use).
pub fn subject(actor: AsyncActor) -> Subject(Message) {
  actor.subject
}
