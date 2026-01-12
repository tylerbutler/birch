//// Console Handler Example
////
//// Demonstrates configuring the console handler for different scenarios.

import birch as log
import birch/handler
import birch/handler/console

pub fn main() {
  log.info("=== Console Handler Demo ===")

  // Default console handler
  demo_default_handler()

  // Handler without colors
  demo_no_colors()

  // Handler to stderr
  demo_stderr()

  // Reset to defaults
  log.reset_config()
  log.info("Demo complete")
}

/// Demonstrate the default console handler.
fn demo_default_handler() {
  log.info("--- Default Console Handler ---")

  // The default handler uses colors (if TTY) and outputs to stdout
  log.configure([log.config_handlers([console.handler()])])

  log.info("This uses the default console handler")
  log.warn("Warnings are shown in yellow (if colors enabled)")
  log.error("Errors are shown in red (if colors enabled)")
}

/// Demonstrate console handler without colors.
fn demo_no_colors() {
  log.info("--- Console Handler Without Colors ---")

  let no_color_handler =
    console.handler_with_config(console.ConsoleConfig(
      color: False,
      target: handler.Stdout,
    ))

  log.configure([log.config_handlers([no_color_handler])])

  log.info("This message has no color codes")
  log.warn("Even warnings have no colors")
  log.error("Errors are also plain text")
}

/// Demonstrate console handler to stderr.
fn demo_stderr() {
  log.info("--- Console Handler to stderr ---")

  let stderr_handler =
    console.handler_with_config(console.ConsoleConfig(
      color: True,
      target: handler.Stderr,
    ))

  log.configure([log.config_handlers([stderr_handler])])

  log.info("This goes to stderr")
  log.error("Errors also go to stderr")
}

/// Create a console handler with custom settings.
/// This function demonstrates the configuration options.
pub fn create_custom_console_handler(
  use_colors: Bool,
  use_stderr: Bool,
) -> handler.Handler {
  let target = case use_stderr {
    True -> handler.Stderr
    False -> handler.Stdout
  }

  console.handler_with_config(console.ConsoleConfig(
    color: use_colors,
    target: target,
  ))
}
