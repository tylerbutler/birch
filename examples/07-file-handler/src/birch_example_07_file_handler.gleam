//// File Handler Example
////
//// Demonstrates file output with various rotation strategies.

import birch
import birch/handler.{type Handler}
import birch/handler/file
import birch/handler/json
import birch/log
import birch/meta
import simplifile

pub fn main() {
  birch.info("=== File Handler Demo ===")

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
  birch.reset_config()
  birch.info("Demo complete")
}

/// Demonstrate file handler without rotation.
fn demo_no_rotation(dir: String) {
  birch.info("--- No Rotation ---")

  let handler =
    file.handler(file.FileConfig(
      path: dir <> "/no-rotation.log",
      rotation: file.NoRotation,
    ))

  birch.configure([birch.config_handlers([handler])])
  birch.info("This goes to a file with no rotation")
  birch.info("File will grow indefinitely")
}

/// Demonstrate size-based rotation.
fn demo_size_rotation(dir: String) {
  birch.info("--- Size Rotation ---")

  let handler =
    file.handler(file.FileConfig(
      path: dir <> "/size-rotation.log",
      rotation: file.SizeRotation(
        max_bytes: 1024,
        // 1 KB for demo
        max_files: 3,
        compress: False,
      ),
    ))

  birch.configure([birch.config_handlers([handler])])
  birch.info("This file rotates when it exceeds 1KB")
  birch.info("Old files: size-rotation.birch.1, size-rotation.birch.2, etc.")
}

/// Demonstrate time-based rotation.
fn demo_time_rotation(dir: String) {
  birch.info("--- Time Rotation ---")

  let handler =
    file.handler(file.FileConfig(
      path: dir <> "/time-rotation.log",
      rotation: file.TimeRotation(interval: file.Daily, max_files: 7),
    ))

  birch.configure([birch.config_handlers([handler])])
  birch.info("This file rotates daily")
  birch.info("Old files have date suffixes: time-rotation.birch.2024-01-14")
}

/// Demonstrate combined rotation.
fn demo_combined_rotation(dir: String) {
  birch.info("--- Combined Rotation ---")

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

  birch.configure([birch.config_handlers([handler])])
  birch.info("This file rotates on size OR time")
  birch.info("Whichever condition is met first triggers rotation")
}

/// Demonstrate JSON format for file logs.
fn demo_json_file(dir: String) {
  birch.info("--- JSON File Format ---")

  let handler =
    file.handler_with_formatter(
      file.FileConfig(
        path: dir <> "/json.log",
        rotation: file.SizeRotation(
          max_bytes: 10_000_000,
          max_files: 5,
          compress: False,
        ),
      ),
      json.format_json,
    )

  birch.configure([birch.config_handlers([handler])])
  let lgr = birch.new("app")
  birch.info("This file contains JSON-formatted logs")
  log.info(lgr, "Great for log aggregation", [meta.string("key", "value")])
}

/// Create a production-ready file handler.
pub fn create_production_file_handler(log_path: String) -> Handler {
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
