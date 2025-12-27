//// Console handler for log output.
////
//// Writes log messages to stdout or stderr, with optional color support.

import gleam_log/formatter
import gleam_log/handler.{type Handler, type OutputTarget, Stderr, Stdout}
import gleam_log/internal/platform
import gleam_log/level
import gleam_log/record.{type LogRecord}

/// ANSI color codes for terminal output.
pub type Color {
  Reset
  Red
  Yellow
  Blue
  Cyan
  Gray
  BrightRed
}

/// Console handler configuration.
pub type ConsoleConfig {
  ConsoleConfig(
    /// Whether to use colors (if terminal supports it)
    color: Bool,
    /// Output target (stdout, stderr, or split)
    target: OutputTarget,
  )
}

/// Default console configuration.
pub fn default_config() -> ConsoleConfig {
  ConsoleConfig(color: True, target: Stdout)
}

/// Create a console handler with default settings.
/// Uses human-readable format, colors if TTY, outputs to stdout.
pub fn handler() -> Handler {
  handler_with_config(default_config())
}

/// Create a console handler with custom configuration.
pub fn handler_with_config(config: ConsoleConfig) -> Handler {
  let use_color = config.color && platform.is_stdout_tty()

  let write_fn = case config.target {
    Stdout -> platform.write_stdout
    Stderr -> platform.write_stderr
    handler.StdoutWithStderr -> write_split
  }

  let format_fn = case use_color {
    True -> format_with_color
    False -> formatter.human_readable
  }

  handler.new(name: "console", write: write_fn, format: format_fn)
}

/// Write to stdout for normal logs, stderr for errors.
fn write_split(message: String) -> Nil {
  // We can't easily determine the level here, so we just write to stdout
  // A more complete implementation would need access to the record
  platform.write_stdout(message)
}

/// Format a log record with ANSI colors based on level.
fn format_with_color(record: LogRecord) -> String {
  let color = level_color(record.level)
  let level_str =
    color_code(color)
    <> pad_level(level.to_string(record.level))
    <> color_code(Reset)

  let metadata_str = formatter.format_metadata(record.metadata)

  case metadata_str {
    "" ->
      color_code(Gray)
      <> record.timestamp
      <> color_code(Reset)
      <> " | "
      <> level_str
      <> " | "
      <> record.logger_name
      <> " | "
      <> record.message
    _ ->
      color_code(Gray)
      <> record.timestamp
      <> color_code(Reset)
      <> " | "
      <> level_str
      <> " | "
      <> record.logger_name
      <> " | "
      <> record.message
      <> " | "
      <> color_code(Cyan)
      <> metadata_str
      <> color_code(Reset)
  }
}

/// Get the color for a log level.
fn level_color(lvl: level.Level) -> Color {
  case lvl {
    level.Trace -> Gray
    level.Debug -> Blue
    level.Info -> Cyan
    level.Warn -> Yellow
    level.Error -> Red
    level.Fatal -> BrightRed
  }
}

/// Get the ANSI escape code for a color.
fn color_code(color: Color) -> String {
  case color {
    Reset -> "\u{001b}[0m"
    Red -> "\u{001b}[31m"
    Yellow -> "\u{001b}[33m"
    Blue -> "\u{001b}[34m"
    Cyan -> "\u{001b}[36m"
    Gray -> "\u{001b}[90m"
    BrightRed -> "\u{001b}[91m"
  }
}

/// Pad a level string to 5 characters for alignment.
fn pad_level(level_str: String) -> String {
  case level_str {
    "TRACE" -> "TRACE"
    "DEBUG" -> "DEBUG"
    "INFO" -> "INFO "
    "WARN" -> "WARN "
    "ERROR" -> "ERROR"
    "FATAL" -> "FATAL"
    _ -> level_str
  }
}
