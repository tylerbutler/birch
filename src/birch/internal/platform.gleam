//// Platform-specific operations.
////
//// This module provides cross-platform abstractions for operations that
//// differ between Erlang and JavaScript targets.

import birch/record.{type LogRecord, type Metadata}

/// Get the current timestamp in ISO 8601 format with milliseconds.
/// Returns a string like "2024-12-26T10:30:45.123Z"
@external(erlang, "birch_ffi", "timestamp_iso8601")
@external(javascript, "../../birch_ffi.mjs", "timestamp_iso8601")
pub fn timestamp_iso8601() -> String

/// Write a string to stdout.
@external(erlang, "birch_ffi", "write_stdout")
@external(javascript, "../../birch_ffi.mjs", "write_stdout")
pub fn write_stdout(message: String) -> Nil

/// Write a string to stderr.
@external(erlang, "birch_ffi", "write_stderr")
@external(javascript, "../../birch_ffi.mjs", "write_stderr")
pub fn write_stderr(message: String) -> Nil

/// Check if stdout is a TTY (for color support detection).
@external(erlang, "birch_ffi", "is_stdout_tty")
@external(javascript, "../../birch_ffi.mjs", "is_stdout_tty")
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
@external(erlang, "birch_ffi", "start_async_writer")
@external(javascript, "../../birch_ffi.mjs", "start_async_writer")
pub fn start_async_writer(
  name: String,
  callback: fn(LogRecord) -> Nil,
  queue_size: Int,
  flush_interval_ms: Int,
  overflow: Int,
) -> AsyncWriterId

/// Send a log record to an async writer.
@external(erlang, "birch_ffi", "async_send")
@external(javascript, "../../birch_ffi.mjs", "async_send")
pub fn async_send(writer_id: AsyncWriterId, record: LogRecord) -> Nil

/// Flush all async writers, waiting for pending messages.
@external(erlang, "birch_ffi", "flush_async_writers")
@external(javascript, "../../birch_ffi.mjs", "flush_async_writers")
pub fn flush_async_writers() -> Nil

/// Flush a specific async writer by name.
@external(erlang, "birch_ffi", "flush_async_writer")
@external(javascript, "../../birch_ffi.mjs", "flush_async_writer")
pub fn flush_async_writer(name: String) -> Nil

// ============================================================================
// File Compression FFI
// ============================================================================

/// Compress a file using gzip and write to the destination path.
/// The source file is read, compressed, and written to dest_path.
/// Returns Ok(Nil) on success, Error(String) with error message on failure.
@external(erlang, "birch_ffi", "compress_file_gzip")
@external(javascript, "../../birch_ffi.mjs", "compress_file_gzip")
pub fn compress_file_gzip(
  source_path: String,
  dest_path: String,
) -> Result(Nil, String)

// ============================================================================
// Safe Call (Error Catching)
// ============================================================================

/// Safely call a function, catching any errors/exceptions.
/// Returns Ok(Nil) if the function succeeded, Error(message) if it failed.
@external(erlang, "birch_ffi", "safe_call")
@external(javascript, "../../birch_ffi.mjs", "safe_call")
pub fn safe_call(f: fn() -> Nil) -> Result(Nil, String)

// ============================================================================
// Scoped Context FFI
// ============================================================================

/// Get the current scope context.
/// Returns the metadata from all active scopes.
/// On Erlang: Uses process dictionary
/// On JavaScript: Uses AsyncLocalStorage (Node.js) or empty list (fallback)
@external(erlang, "birch_ffi", "get_scope_context")
@external(javascript, "../../birch_ffi.mjs", "get_scope_context")
pub fn get_scope_context() -> Metadata

/// Set the current scope context.
/// Replaces the entire scope context with the given metadata.
/// Note: On JavaScript with AsyncLocalStorage, this is only used for fallback.
@external(erlang, "birch_ffi", "set_scope_context")
@external(javascript, "../../birch_ffi.mjs", "set_scope_context")
pub fn set_scope_context(context: Metadata) -> Nil

/// Check if scoped context is available on the current platform.
/// Returns True on Erlang (process dictionary) and Node.js (AsyncLocalStorage).
/// Returns False on other JavaScript runtimes.
@external(erlang, "birch_ffi", "is_scope_context_available")
@external(javascript, "../../birch_ffi.mjs", "is_scope_context_available")
pub fn is_scope_context_available() -> Bool

// ============================================================================
// Sampling FFI
// ============================================================================

/// Generate a random float between 0.0 (inclusive) and 1.0 (exclusive).
/// Used for probabilistic sampling.
@external(erlang, "birch_ffi", "random_float")
@external(javascript, "../../birch_ffi.mjs", "random_float")
pub fn random_float() -> Float

/// Get the current time in milliseconds since epoch.
/// Used for token bucket rate limiting.
@external(erlang, "birch_ffi", "current_time_ms")
@external(javascript, "../../birch_ffi.mjs", "current_time_ms")
pub fn current_time_ms() -> Int
