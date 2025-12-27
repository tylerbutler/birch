//// File handler for log output with rotation support.
////
//// Writes log messages to files with configurable rotation strategies.

import gleam/int
import gleam/list
import gleam/result
import gleam/string
import gleam_log/formatter
import gleam_log/handler.{type Handler}
import gleam_log/internal/platform
import gleam_log/record.{type LogRecord}
import simplifile

/// Rotation strategy for log files.
pub type Rotation {
  /// No rotation - file grows indefinitely
  NoRotation
  /// Rotate when file exceeds max_bytes, keep up to max_files old files
  SizeRotation(max_bytes: Int, max_files: Int)
}

/// File handler configuration.
pub type FileConfig {
  FileConfig(
    /// Path to the log file
    path: String,
    /// Rotation strategy
    rotation: Rotation,
  )
}

/// Create a file handler with the given configuration.
/// Uses human-readable format by default.
pub fn handler(config: FileConfig) -> Handler {
  handler_with_formatter(config, formatter.human_readable)
}

/// Create a file handler with a custom formatter.
pub fn handler_with_formatter(
  config: FileConfig,
  format: formatter.Formatter,
) -> Handler {
  // Ensure parent directory exists
  let _ = ensure_parent_dir(config.path)

  handler.new(
    name: "file:" <> config.path,
    write: fn(message) { write_to_file(config, message) },
    format: format,
  )
}

/// Write a message to the log file, handling rotation if needed.
fn write_to_file(config: FileConfig, message: String) -> Nil {
  // Check if we need to rotate first
  case config.rotation {
    NoRotation -> Nil
    SizeRotation(max_bytes, max_files) -> {
      case should_rotate(config.path, max_bytes) {
        True -> rotate_file(config.path, max_files)
        False -> Nil
      }
    }
  }

  // Append message to file
  let content = message <> "\n"
  case simplifile.append(config.path, content) {
    Ok(Nil) -> Nil
    Error(e) -> {
      // Log to stderr on failure, but don't crash
      platform.write_stderr(
        "gleam_log: failed to write to "
        <> config.path
        <> ": "
        <> simplifile.describe_error(e),
      )
    }
  }
}

/// Check if a file should be rotated based on size.
fn should_rotate(path: String, max_bytes: Int) -> Bool {
  case file_size(path) {
    Ok(size) -> size >= max_bytes
    Error(_) -> False
  }
}

/// Get the size of a file in bytes.
fn file_size(path: String) -> Result(Int, Nil) {
  simplifile.file_info(path)
  |> result.map(fn(info) { info.size })
  |> result.replace_error(Nil)
}

/// Rotate log files.
/// myapp.log -> myapp.log.1 -> myapp.log.2 -> ... -> deleted
fn rotate_file(path: String, max_files: Int) -> Nil {
  // Delete the oldest file if it exists
  let oldest = path <> "." <> int.to_string(max_files)
  let _ = simplifile.delete(oldest)

  // Shift all existing rotated files
  shift_files(path, max_files - 1)

  // Rename current file to .1
  let _ = simplifile.rename(path, path <> ".1")

  Nil
}

/// Shift rotated files: .N -> .N+1 for N from max down to 1
fn shift_files(base_path: String, n: Int) -> Nil {
  case n < 1 {
    True -> Nil
    False -> {
      let from = base_path <> "." <> int.to_string(n)
      let to = base_path <> "." <> int.to_string(n + 1)
      let _ = simplifile.rename(from, to)
      shift_files(base_path, n - 1)
    }
  }
}

/// Ensure the parent directory of a path exists.
fn ensure_parent_dir(path: String) -> Result(Nil, Nil) {
  case get_parent_dir(path) {
    "" -> Ok(Nil)
    parent ->
      simplifile.create_directory_all(parent)
      |> result.replace_error(Nil)
  }
}

/// Get the parent directory of a path.
fn get_parent_dir(path: String) -> String {
  case string.split(path, "/") {
    [] -> ""
    parts -> {
      let without_last = list.take(parts, list.length(parts) - 1)
      string.join(without_last, "/")
    }
  }
}
