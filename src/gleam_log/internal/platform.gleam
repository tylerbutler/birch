//// Platform-specific operations.
////
//// This module provides cross-platform abstractions for operations that
//// differ between Erlang and JavaScript targets.

import gleam_log/record.{type LogRecord}

/// Get the current timestamp in ISO 8601 format with milliseconds.
/// Returns a string like "2024-12-26T10:30:45.123Z"
@external(erlang, "gleam_log_ffi", "timestamp_iso8601")
@external(javascript, "../../gleam_log_ffi.mjs", "timestamp_iso8601")
pub fn timestamp_iso8601() -> String

/// Write a string to stdout.
@external(erlang, "gleam_log_ffi", "write_stdout")
@external(javascript, "../../gleam_log_ffi.mjs", "write_stdout")
pub fn write_stdout(message: String) -> Nil

/// Write a string to stderr.
@external(erlang, "gleam_log_ffi", "write_stderr")
@external(javascript, "../../gleam_log_ffi.mjs", "write_stderr")
pub fn write_stderr(message: String) -> Nil

/// Check if stdout is a TTY (for color support detection).
@external(erlang, "gleam_log_ffi", "is_stdout_tty")
@external(javascript, "../../gleam_log_ffi.mjs", "is_stdout_tty")
pub fn is_stdout_tty() -> Bool

// ============================================================================
// Async Handler FFI
// ============================================================================

/// Opaque type representing an async writer ID.
/// On Erlang this is a PID, on JavaScript it's a unique identifier.
pub type AsyncWriterId

/// Start an async writer process/task.
///
/// - name: Identifier for this writer (used for flush_handler)
/// - callback: Function called for each log record
/// - queue_size: Maximum queue size
/// - flush_interval_ms: Batch flush interval
/// - overflow: Overflow behavior (0=DropOldest, 1=DropNewest, 2=Block)
///
/// Returns an opaque writer ID used for async_send.
@external(erlang, "gleam_log_ffi", "start_async_writer")
@external(javascript, "../../gleam_log_ffi.mjs", "start_async_writer")
pub fn start_async_writer(
  name: String,
  callback: fn(LogRecord) -> Nil,
  queue_size: Int,
  flush_interval_ms: Int,
  overflow: Int,
) -> AsyncWriterId

/// Send a log record to an async writer.
@external(erlang, "gleam_log_ffi", "async_send")
@external(javascript, "../../gleam_log_ffi.mjs", "async_send")
pub fn async_send(writer_id: AsyncWriterId, record: LogRecord) -> Nil

/// Flush all async writers, waiting for pending messages.
@external(erlang, "gleam_log_ffi", "flush_async_writers")
@external(javascript, "../../gleam_log_ffi.mjs", "flush_async_writers")
pub fn flush_async_writers() -> Nil

/// Flush a specific async writer by name.
@external(erlang, "gleam_log_ffi", "flush_async_writer")
@external(javascript, "../../gleam_log_ffi.mjs", "flush_async_writer")
pub fn flush_async_writer(name: String) -> Nil
