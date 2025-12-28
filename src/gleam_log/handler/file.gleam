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

/// Time interval for time-based rotation.
pub type TimeInterval {
  /// Rotate every hour
  Hourly
  /// Rotate daily at midnight
  Daily
}

/// Rotation strategy for log files.
pub type Rotation {
  /// No rotation - file grows indefinitely
  NoRotation
  /// Rotate when file exceeds max_bytes, keep up to max_files old files
  SizeRotation(max_bytes: Int, max_files: Int)
  /// Rotate based on time interval, keep up to max_files old files
  TimeRotation(interval: TimeInterval, max_files: Int)
  /// Rotate on size OR time, whichever comes first
  CombinedRotation(max_bytes: Int, interval: TimeInterval, max_files: Int)
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
      case should_rotate_by_size(config.path, max_bytes) {
        True -> rotate_file(config.path, max_files)
        False -> Nil
      }
    }
    TimeRotation(interval, max_files) -> {
      case should_rotate_by_time(config.path, interval) {
        True -> rotate_file_with_timestamp(config.path, max_files, interval)
        False -> Nil
      }
    }
    CombinedRotation(max_bytes, interval, max_files) -> {
      // Rotate on size OR time, whichever triggers first
      let rotate_for_size = should_rotate_by_size(config.path, max_bytes)
      let rotate_for_time = should_rotate_by_time(config.path, interval)
      case rotate_for_size || rotate_for_time {
        True -> rotate_file_with_timestamp(config.path, max_files, interval)
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
fn should_rotate_by_size(path: String, max_bytes: Int) -> Bool {
  case file_size(path) {
    Ok(size) -> size >= max_bytes
    Error(_) -> False
  }
}

/// Check if a file should be rotated based on time.
/// Uses a marker file to track the last rotation period.
fn should_rotate_by_time(path: String, interval: TimeInterval) -> Bool {
  let marker_path = path <> ".rotation"
  let current_period = format_rotation_timestamp(interval)

  case simplifile.read(marker_path) {
    Ok(last_period) -> {
      // Rotate if we're in a different period
      string.trim(last_period) != current_period
    }
    Error(_) -> {
      // No marker file means either first run or file was deleted
      // Check if the log file exists - if so, we should rotate
      case simplifile.is_file(path) {
        Ok(True) -> True
        _ -> {
          // No log file yet, create marker for current period
          let _ = simplifile.write(marker_path, current_period)
          False
        }
      }
    }
  }
}

/// Get the size of a file in bytes.
fn file_size(path: String) -> Result(Int, Nil) {
  simplifile.file_info(path)
  |> result.map(fn(info) { info.size })
  |> result.replace_error(Nil)
}

/// Rotate log files using numeric suffixes.
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

/// Rotate log files using timestamp-based suffixes for time-based rotation.
/// myapp.log -> myapp.log.2024-12-26T14 (hourly) or myapp.log.2024-12-26 (daily)
fn rotate_file_with_timestamp(
  path: String,
  max_files: Int,
  interval: TimeInterval,
) -> Nil {
  let marker_path = path <> ".rotation"
  let current_period = format_rotation_timestamp(interval)

  // Get the old period for naming the rotated file
  let old_period = case simplifile.read(marker_path) {
    Ok(content) -> string.trim(content)
    Error(_) -> format_rotation_timestamp(interval)
  }

  // Rename current file with timestamp suffix
  let rotated_path = path <> "." <> old_period
  let _ = simplifile.rename(path, rotated_path)

  // Update marker file with current period
  let _ = simplifile.write(marker_path, current_period)

  // Clean up old files if we exceed max_files
  cleanup_old_rotated_files(path, max_files)

  Nil
}

/// Clean up old rotated files, keeping only the most recent max_files.
fn cleanup_old_rotated_files(base_path: String, max_files: Int) -> Nil {
  let dir = get_parent_dir(base_path)
  let filename = get_filename(base_path)

  case simplifile.read_directory(case dir {
    "" -> "."
    d -> d
  }) {
    Ok(files) -> {
      // Find all rotated files for this log (files starting with base name followed by a dot)
      let rotated_files =
        files
        |> list.filter(fn(f) {
          string.starts_with(f, filename <> ".")
          && !string.ends_with(f, ".rotation")
        })
        |> list.sort(string.compare)
        |> list.reverse

      // Delete files beyond max_files
      case list.length(rotated_files) > max_files {
        True -> {
          let files_to_delete = list.drop(rotated_files, max_files)
          list.each(files_to_delete, fn(f) {
            let full_path = case dir {
              "" -> f
              d -> d <> "/" <> f
            }
            let _ = simplifile.delete(full_path)
            Nil
          })
        }
        False -> Nil
      }
    }
    Error(_) -> Nil
  }
}

/// Get the filename from a path.
fn get_filename(path: String) -> String {
  case string.split(path, "/") {
    [] -> path
    parts -> {
      case list.last(parts) {
        Ok(name) -> name
        Error(_) -> path
      }
    }
  }
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

// ============================================================================
// Time-Based Rotation Helpers
// ============================================================================

/// Get the number of hours in a time interval.
pub fn interval_to_hours(interval: TimeInterval) -> Int {
  case interval {
    Hourly -> 1
    Daily -> 24
  }
}

/// Format a rotation timestamp based on the interval.
/// - Hourly: "2024-12-26T14" (includes hour)
/// - Daily: "2024-12-26" (date only)
pub fn format_rotation_timestamp(interval: TimeInterval) -> String {
  let timestamp = platform.timestamp_iso8601()
  case interval {
    Hourly -> {
      // Take first 13 chars: "2024-12-26T14"
      string.slice(timestamp, 0, 13)
    }
    Daily -> {
      // Take first 10 chars: "2024-12-26"
      string.slice(timestamp, 0, 10)
    }
  }
}
