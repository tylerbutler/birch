//// Erlang Logger Integration Example
////
//// Demonstrates integrating birch with Erlang's built-in :logger system.
//// This example is BEAM only - it uses Erlang-specific features.
////
//// On BEAM, birch sends LogRecords directly to :logger — no handler wrapper
//// is needed. The birch formatter installed on :logger's default handler
//// formats both birch logs and OTP logs consistently.

import birch as log
import birch/erlang_logger
import birch/formatter

pub fn main() {
  log.info("=== Erlang Logger Integration Demo ===")

  // Demo 1: Direct logging (birch → :logger automatically on BEAM)
  demo_direct_logging()

  // Demo 2: Install birch as :logger formatter
  demo_install_formatter()

  log.reset_config()
  log.info("Demo complete")
}

/// Demonstrate direct birch logging on BEAM.
///
/// On BEAM, birch sends LogRecords directly to :logger without needing
/// a handler wrapper. The formatter installed on :logger's default handler
/// formats the output using birch's formatting pipeline.
fn demo_direct_logging() {
  log.info("--- Direct Logging on BEAM ---")

  // These logs go through Erlang's :logger system automatically
  log.info("This message goes to :logger automatically")
  log.warn("Warnings are mapped to Erlang warning level")
  log.error("Errors are mapped to Erlang error level")

  log.info("Direct logging demo complete")
}

/// Demonstrate installing birch as a :logger formatter.
///
/// This is the idiomatic OTP approach: the :logger handler controls output
/// (console, file, etc.) while birch controls how messages are formatted.
fn demo_install_formatter() {
  log.info("--- Install birch as :logger Formatter ---")

  // Install birch as the formatter on the default :logger handler
  case erlang_logger.install_formatter() {
    Ok(Nil) -> {
      log.info("Birch is now formatting OTP :logger output")
      log.info("OTP and library logs will use birch's human_readable format")
    }
    Error(reason) -> {
      log.warn("Could not install formatter: " <> reason)
    }
  }

  // You can also use a custom formatter
  case erlang_logger.install_formatter_with(formatter.simple) {
    Ok(Nil) -> {
      log.info("Now using birch's simple formatter for OTP logs")
    }
    Error(_) -> Nil
  }

  // Remove birch formatter, restoring OTP defaults
  let _ = erlang_logger.remove_formatter()
  log.info("Formatter removed, OTP defaults restored")
}

/// Install birch as a formatter for Erlang's :logger system.
pub fn setup_erlang_integration() -> Result(Nil, String) {
  erlang_logger.install_formatter()
}

/// Remove birch from Erlang's :logger system.
pub fn teardown_erlang_integration() -> Result(Nil, String) {
  erlang_logger.remove_formatter()
}
