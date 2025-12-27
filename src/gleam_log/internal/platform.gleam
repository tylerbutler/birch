//// Platform-specific operations.
////
//// This module provides cross-platform abstractions for operations that
//// differ between Erlang and JavaScript targets.

import gleam_log/config.{type GlobalConfig}

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
// Global Configuration Storage
// ============================================================================

/// Get the global configuration from platform-specific storage.
/// Returns Ok(config) if set, Error(Nil) if not configured.
@external(erlang, "gleam_log_ffi", "get_global_config")
@external(javascript, "../../gleam_log_ffi.mjs", "get_global_config")
pub fn get_global_config() -> Result(GlobalConfig, Nil)

/// Set the global configuration in platform-specific storage.
@external(erlang, "gleam_log_ffi", "set_global_config")
@external(javascript, "../../gleam_log_ffi.mjs", "set_global_config")
pub fn set_global_config(config: GlobalConfig) -> Nil

/// Clear the global configuration from platform-specific storage.
@external(erlang, "gleam_log_ffi", "clear_global_config")
@external(javascript, "../../gleam_log_ffi.mjs", "clear_global_config")
pub fn clear_global_config() -> Nil
