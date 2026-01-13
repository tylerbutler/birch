//// File Handler Example
////
//// Demonstrates file output with various rotation strategies.

import birch as log
import birch/handler/file
import birch/handler/json
import simplifile

pub fn main() {
  log.info("=== File Handler Demo ===")

  // Create a temp directory for demo files
  let demo_dir = "/tmp/birch-demo"
  let _ = simplifile.create_directory_all(demo_dir)

  // Demo various rotation strategies
  demo_no_rotation(demo_dir)
  demo_size_rotation(demo_dir)
  demo_time_rotation(demo_dir)
  demo_combined_rotation(demo_dir)
  demo_json_file(demo_dir)

  // Clean up
  let _ = simplifile.delete_all([demo_dir])

  // Reset to defaults
  log.reset_config()
  log.info("Demo complete")
}

/// Demonstrate file handler without rotation.
fn demo_no_rotation(dir: String) {
  log.info("--- No Rotation ---")

  let handler =
    file.handler(file.FileConfig(
      path: dir <> "/no-rotation.log",
      rotation: file.NoRotation,
    ))

  log.configure([log.config_handlers([handler])])
  log.info("This goes to a file with no rotation")
  log.info("File will grow indefinitely")
}

/// Demonstrate size-based rotation.
fn demo_size_rotation(dir: String) {
  log.info("--- Size Rotation ---")

  let handler =
    file.handler(file.FileConfig(
      path: dir <> "/size-rotation.log",
      rotation: file.SizeRotation(
        max_bytes: 1024,
        // 1 KB for demo
        max_files: 3,
      ),
    ))

  log.configure([log.config_handlers([handler])])
  log.info("This file rotates when it exceeds 1KB")
  log.info("Old files: size-rotation.log.1, size-rotation.log.2, etc.")
}

/// Demonstrate time-based rotation.
fn demo_time_rotation(dir: String) {
  log.info("--- Time Rotation ---")

  let handler =
    file.handler(file.FileConfig(
      path: dir <> "/time-rotation.log",
      rotation: file.TimeRotation(interval: file.Daily, max_files: 7),
    ))

  log.configure([log.config_handlers([handler])])
  log.info("This file rotates daily")
  log.info("Old files have date suffixes: time-rotation.log.2024-01-14")
}

/// Demonstrate combined rotation.
fn demo_combined_rotation(dir: String) {
  log.info("--- Combined Rotation ---")

  let handler =
    file.handler(file.FileConfig(
      path: dir <> "/combined.log",
      rotation: file.CombinedRotation(
        max_bytes: 10_000_000,
        // 10 MB
        interval: file.Daily,
        max_files: 30,
      ),
    ))

  log.configure([log.config_handlers([handler])])
  log.info("This file rotates on size OR time")
  log.info("Whichever condition is met first triggers rotation")
}

/// Demonstrate JSON format for file logs.
fn demo_json_file(dir: String) {
  log.info("--- JSON File Format ---")

  let handler =
    file.handler_with_formatter(
      file.FileConfig(
        path: dir <> "/json.log",
        rotation: file.SizeRotation(max_bytes: 10_000_000, max_files: 5),
      ),
      json.format_json,
    )

  log.configure([log.config_handlers([handler])])
  log.info("This file contains JSON-formatted logs")
  log.info_m("Great for log aggregation", [#("key", "value")])
}

/// Create a production-ready file handler.
pub fn create_production_file_handler(log_path: String) -> log.LogHandler {
  file.handler_with_formatter(
    file.FileConfig(
      path: log_path,
      rotation: file.CombinedRotation(
        max_bytes: 100_000_000,
        // 100 MB
        interval: file.Daily,
        max_files: 30,
        // Keep 30 days
      ),
    ),
    json.format_json,
  )
}
