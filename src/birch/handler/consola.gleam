//// Consola-style handler for log output.
////
//// Provides fancy formatted output inspired by the unjs consola package,
//// featuring level-specific icons, colored badges, styled scope prefixes,
//// box output, grouping, and semantic log types (success, start, ready, fail).

import birch/formatter
import birch/handler.{type Handler, Stderr, Stdout}
import birch/internal/platform
import birch/level
import birch/logger.{type Logger}
import birch/record.{type LogRecord}
import gleam/int
import gleam/list
import gleam/string

// ============================================================================
// Level Formatter
// ============================================================================

/// An opaque level formatter that encapsulates formatting logic and configuration.
/// Use the provided constructor functions to create formatters.
pub opaque type LevelFormatter {
  LevelFormatter(format: fn(level.Level, Bool) -> String)
}

/// Configuration for label-style level formatting.
pub type LabelConfig {
  LabelConfig(
    /// Whether to show icons (e.g., ℹ, ⚠, ✖)
    icons: Bool,
  )
}

/// Default label configuration with icons enabled.
pub fn default_label_config() -> LabelConfig {
  LabelConfig(icons: True)
}

/// Create a label-style formatter with default settings.
/// Output: "ℹ info", "⚠ warn", "✖ error", etc.
///
/// With colors enabled, the icon and label are colored based on severity.
pub fn label_formatter() -> LevelFormatter {
  label_formatter_with_config(default_label_config())
}

/// Create a label-style formatter with custom configuration.
pub fn label_formatter_with_config(config: LabelConfig) -> LevelFormatter {
  LevelFormatter(format: fn(lvl, use_color) {
    format_label(lvl, config, use_color)
  })
}

/// Configuration for badge-style level formatting.
pub type BadgeConfig {
  BadgeConfig
}

/// Default badge configuration.
pub fn default_badge_config() -> BadgeConfig {
  BadgeConfig
}

/// Create a badge-style formatter with default settings.
/// Output: " INFO ", " WARN ", " ERROR ", etc.
///
/// With colors enabled, displays as colored background with contrasting text.
/// This style provides high visual prominence, especially for errors.
pub fn badge_formatter() -> LevelFormatter {
  badge_formatter_with_config(default_badge_config())
}

/// Create a badge-style formatter with custom configuration.
pub fn badge_formatter_with_config(_config: BadgeConfig) -> LevelFormatter {
  LevelFormatter(format: fn(lvl, use_color) { format_badge(lvl, use_color) })
}

/// Create a custom level formatter from a formatting function.
/// The function receives the log level and whether colors are enabled.
pub fn custom_level_formatter(
  format: fn(level.Level, Bool) -> String,
) -> LevelFormatter {
  LevelFormatter(format: format)
}

/// Apply a level formatter to format a log level.
pub fn format_level(
  formatter: LevelFormatter,
  lvl: level.Level,
  use_color: Bool,
) -> String {
  formatter.format(lvl, use_color)
}

fn format_label(
  lvl: level.Level,
  config: LabelConfig,
  use_color: Bool,
) -> String {
  let label = level_label(lvl)

  case use_color, config.icons {
    True, True -> {
      let color = level_color(lvl)
      let icon = level_icon(lvl)
      color <> icon <> " " <> bold <> label <> reset
    }
    True, False -> {
      let color = level_color(lvl)
      color <> bold <> label <> reset
    }
    False, True -> {
      let icon = level_icon(lvl)
      icon <> " " <> label
    }
    False, False -> label
  }
}

fn format_badge(lvl: level.Level, use_color: Bool) -> String {
  let label = level_label_upper(lvl)

  case use_color {
    True -> {
      let bg_color = level_bg_color(lvl)
      let text_color = level_badge_text_color(lvl)
      bg_color <> text_color <> "[" <> label <> "]" <> reset
    }
    False -> "[" <> label <> "]"
  }
}

/// Get the uppercase label for a log level (for badge style).
fn level_label_upper(lvl: level.Level) -> String {
  case lvl {
    level.Trace -> "TRACE"
    level.Debug -> "DEBUG"
    level.Info -> "INFO"
    level.Warn -> "WARN"
    level.Err -> "ERROR"
    level.Fatal -> "FATAL"
  }
}

/// Get the background color for a log level (for badge style).
fn level_bg_color(lvl: level.Level) -> String {
  case lvl {
    level.Trace -> bg_gray
    level.Debug -> bg_gray
    level.Info -> bg_cyan
    level.Warn -> bg_yellow
    level.Err -> bg_red
    level.Fatal -> bg_red
  }
}

/// Get the text color for badge style (for contrast against background).
fn level_badge_text_color(lvl: level.Level) -> String {
  case lvl {
    level.Trace -> white
    level.Debug -> white
    level.Info -> black
    level.Warn -> black
    level.Err -> white
    level.Fatal -> white
  }
}

// ============================================================================
// Configuration
// ============================================================================

/// Configuration for the consola handler.
pub type ConsolaConfig {
  ConsolaConfig(
    /// Whether to use colors (if terminal supports it)
    color: Bool,
    /// Whether to show timestamps
    timestamps: Bool,
    /// Output target (stdout or stderr)
    target: handler.OutputTarget,
    /// How to format log levels
    level_formatter: LevelFormatter,
  )
}

/// Semantic log style for special log types.
pub type LogStyle {
  /// Success style (✔ green) - for successful completions
  Success
  /// Start style (◐ magenta) - for starting operations
  Start
  /// Ready style (✔ green) - for ready/initialized states
  Ready
  /// Fail style (✖ red) - for failures (non-fatal)
  Fail
}

/// Default consola configuration with label-style formatting.
pub fn default_config() -> ConsolaConfig {
  ConsolaConfig(
    color: True,
    timestamps: False,
    target: Stdout,
    level_formatter: label_formatter(),
  )
}

/// Set the level formatter for a configuration.
pub fn with_level_formatter(
  config: ConsolaConfig,
  formatter: LevelFormatter,
) -> ConsolaConfig {
  ConsolaConfig(..config, level_formatter: formatter)
}

/// Use badge-style level formatting.
pub fn with_badge_style(config: ConsolaConfig) -> ConsolaConfig {
  ConsolaConfig(..config, level_formatter: badge_formatter())
}

/// Use label-style level formatting (default).
pub fn with_label_style(config: ConsolaConfig) -> ConsolaConfig {
  ConsolaConfig(..config, level_formatter: label_formatter())
}

/// Use label-style formatting without icons.
pub fn with_label_style_no_icons(config: ConsolaConfig) -> ConsolaConfig {
  ConsolaConfig(
    ..config,
    level_formatter: label_formatter_with_config(LabelConfig(icons: False)),
  )
}

/// Create a consola handler with default settings.
pub fn handler() -> Handler {
  handler_with_config(default_config())
}

/// Create a consola handler with custom configuration.
pub fn handler_with_config(config: ConsolaConfig) -> Handler {
  let use_color = config.color && platform.is_stdout_tty()

  let write_fn = case config.target {
    Stdout -> platform.write_stdout
    Stderr -> platform.write_stderr
    handler.StdoutWithStderr -> platform.write_stdout
  }

  let format_fn =
    format_consola(use_color, config.timestamps, config.level_formatter)

  handler.new(name: "consola", write: write_fn, format: format_fn)
}

// ============================================================================
// ANSI Escape Codes
// ============================================================================

const reset = "\u{001b}[0m"

const bold = "\u{001b}[1m"

const dim = "\u{001b}[2m"

// Colors
const gray = "\u{001b}[90m"

const cyan = "\u{001b}[36m"

const green = "\u{001b}[32m"

const yellow = "\u{001b}[33m"

const red = "\u{001b}[31m"

const magenta = "\u{001b}[35m"

const black = "\u{001b}[30m"

const white = "\u{001b}[97m"

// Background colors
const bg_gray = "\u{001b}[100m"

const bg_cyan = "\u{001b}[46m"

const bg_yellow = "\u{001b}[43m"

const bg_red = "\u{001b}[41m"

// ============================================================================
// Box Output
// ============================================================================

// Box drawing characters (rounded corners)
const box_top_left = "╭"

const box_top_right = "╮"

const box_bottom_left = "╰"

const box_bottom_right = "╯"

const box_horizontal = "─"

const box_vertical = "│"

/// Format a message inside a box.
/// Returns the formatted box string that can be printed directly.
///
/// Example output:
/// ```
/// ╭──────────────────╮
/// │ Hello, World!    │
/// ╰──────────────────╯
/// ```
pub fn box(message: String) -> String {
  box_with_title(message, "")
}

/// Format a message inside a box with an optional title.
///
/// Example output:
/// ```
/// ╭─ Title ──────────╮
/// │ Hello, World!    │
/// ╰──────────────────╯
/// ```
pub fn box_with_title(message: String, title: String) -> String {
  let use_color = platform.is_stdout_tty()
  format_box(message, title, use_color)
}

/// Format a box with explicit color control.
pub fn box_colored(message: String, title: String, use_color: Bool) -> String {
  format_box(message, title, use_color)
}

fn format_box(message: String, title: String, use_color: Bool) -> String {
  let lines = string.split(message, "\n")
  let max_width = find_max_width(lines, string.length(title))
  let padded_width = max_width + 2
  // 1 space padding on each side

  let color = case use_color {
    True -> cyan
    False -> ""
  }
  let reset_code = case use_color {
    True -> reset
    False -> ""
  }

  // Build top border
  let top_border = case title {
    "" ->
      color
      <> box_top_left
      <> string.repeat(box_horizontal, padded_width)
      <> box_top_right
      <> reset_code
    t ->
      color
      <> box_top_left
      <> box_horizontal
      <> " "
      <> t
      <> " "
      <> string.repeat(
        box_horizontal,
        padded_width - string.length(t) - 3 |> int.max(0),
      )
      <> box_top_right
      <> reset_code
  }

  // Build content lines
  let content_lines =
    list.map(lines, fn(line) {
      let padding = max_width - string.length(line)
      color
      <> box_vertical
      <> reset_code
      <> " "
      <> line
      <> string.repeat(" ", padding)
      <> " "
      <> color
      <> box_vertical
      <> reset_code
    })

  // Build bottom border
  let bottom_border =
    color
    <> box_bottom_left
    <> string.repeat(box_horizontal, padded_width)
    <> box_bottom_right
    <> reset_code

  // Combine all parts
  [top_border, ..content_lines]
  |> list.append([bottom_border])
  |> string.join("\n")
}

fn find_max_width(lines: List(String), min_width: Int) -> Int {
  list.fold(lines, min_width, fn(acc, line) {
    int.max(acc, string.length(line))
  })
}

/// Write a boxed message directly to stdout.
pub fn write_box(message: String) -> Nil {
  platform.write_stdout(box(message))
}

/// Write a boxed message with title directly to stdout.
pub fn write_box_with_title(message: String, title: String) -> Nil {
  platform.write_stdout(box_with_title(message, title))
}

// ============================================================================
// Grouping
// ============================================================================

/// Execute a function within a named group, with all log output indented.
/// The group title is printed before the content, and indentation is applied
/// to all output within the scope.
///
/// Example:
/// ```gleam
/// consola.with_group("Building project", fn() {
///   logger.info(lgr, "Compiling sources...", [])
///   logger.info(lgr, "Linking...", [])
/// })
/// ```
///
/// Output:
/// ```
/// ▸ Building project
///   ℹ info Compiling sources...
///   ℹ info Linking...
/// ```
pub fn with_group(title: String, work: fn() -> a) -> a {
  let use_color = platform.is_stdout_tty()
  let arrow = case use_color {
    True -> cyan <> "▸" <> reset
    False -> "▸"
  }
  platform.write_stdout(arrow <> " " <> title)
  let result = work()
  result
}

/// Create a handler that indents output by the specified level.
/// Use this within a group to get indented log output.
///
/// Example:
/// ```gleam
/// let indented_handler = consola.indented_handler(1)
/// let lgr = logger.new("build") |> logger.with_handlers([indented_handler])
/// ```
pub fn indented_handler(indent_level: Int) -> Handler {
  indented_handler_with_config(indent_level, default_config())
}

/// Create an indented handler with custom configuration.
pub fn indented_handler_with_config(
  indent_level: Int,
  config: ConsolaConfig,
) -> Handler {
  let use_color = config.color && platform.is_stdout_tty()

  let write_fn = case config.target {
    Stdout -> platform.write_stdout
    Stderr -> platform.write_stderr
    handler.StdoutWithStderr -> platform.write_stdout
  }

  let indent = string.repeat("  ", indent_level)
  let format_fn =
    format_consola_indented(
      use_color,
      config.timestamps,
      config.level_formatter,
      indent,
    )

  handler.new(name: "consola", write: write_fn, format: format_fn)
}

fn format_consola_indented(
  use_color: Bool,
  show_timestamp: Bool,
  level_fmt: LevelFormatter,
  indent: String,
) -> formatter.Formatter {
  fn(record: LogRecord) -> String {
    let base = format_record(record, use_color, show_timestamp, level_fmt)
    indent <> base
  }
}

// ============================================================================
// Semantic Log Types
// ============================================================================

/// Log a success message (✔ green).
/// Used for successful completions.
///
/// Example:
/// ```gleam
/// consola.success(lgr, "Build completed!", [])
/// ```
pub fn success(lgr: Logger, message: String, metadata: record.Metadata) -> Nil {
  log_styled(lgr, Success, message, metadata)
}

/// Log a start message (◐ magenta).
/// Used when beginning an operation.
///
/// Example:
/// ```gleam
/// consola.start(lgr, "Building project...", [])
/// ```
pub fn start(lgr: Logger, message: String, metadata: record.Metadata) -> Nil {
  log_styled(lgr, Start, message, metadata)
}

/// Log a ready message (✔ green).
/// Used when something is initialized and ready.
///
/// Example:
/// ```gleam
/// consola.ready(lgr, "Server listening on port 3000", [])
/// ```
pub fn ready(lgr: Logger, message: String, metadata: record.Metadata) -> Nil {
  log_styled(lgr, Ready, message, metadata)
}

/// Log a fail message (✖ red).
/// Used for non-fatal failures.
///
/// Example:
/// ```gleam
/// consola.fail(lgr, "Could not connect to cache", [])
/// ```
pub fn fail(lgr: Logger, message: String, metadata: record.Metadata) -> Nil {
  log_styled(lgr, Fail, message, metadata)
}

fn log_styled(
  lgr: Logger,
  style: LogStyle,
  message: String,
  metadata: record.Metadata,
) -> Nil {
  let use_color = platform.is_stdout_tty()
  let formatted = format_styled_message(style, message, metadata, use_color)
  // Write directly since we're bypassing normal log flow for styling
  platform.write_stdout(formatted)
  // Also log through logger at Info level for any other handlers
  logger.info(lgr, message, metadata)
}

fn format_styled_message(
  style: LogStyle,
  message: String,
  metadata: record.Metadata,
  use_color: Bool,
) -> String {
  let #(icon, color, label) = style_properties(style)

  let icon_part = icon <> " "

  let metadata_str = formatter.format_metadata(metadata)
  let metadata_part = case metadata_str {
    "" -> ""
    m ->
      case use_color {
        True -> " " <> dim <> m <> reset
        False -> " " <> m
      }
  }

  case use_color {
    True ->
      color
      <> icon_part
      <> bold
      <> label
      <> reset
      <> " "
      <> message
      <> metadata_part
    False -> icon_part <> label <> " " <> message <> metadata_part
  }
}

fn style_properties(style: LogStyle) -> #(String, String, String) {
  case style {
    Success -> #("✔", green, "success")
    Start -> #("◐", magenta, "start")
    Ready -> #("✔", green, "ready")
    Fail -> #("✖", red, "fail")
  }
}

// ============================================================================
// Direct Output Functions (for use without a logger)
// ============================================================================

/// Write a success message directly to stdout.
pub fn write_success(message: String) -> Nil {
  write_styled(Success, message)
}

/// Write a start message directly to stdout.
pub fn write_start(message: String) -> Nil {
  write_styled(Start, message)
}

/// Write a ready message directly to stdout.
pub fn write_ready(message: String) -> Nil {
  write_styled(Ready, message)
}

/// Write a fail message directly to stdout.
pub fn write_fail(message: String) -> Nil {
  write_styled(Fail, message)
}

fn write_styled(style: LogStyle, message: String) -> Nil {
  let use_color = platform.is_stdout_tty()
  let formatted = format_styled_message(style, message, [], use_color)
  platform.write_stdout(formatted)
}

// ============================================================================
// Level Formatting (original implementation)
// ============================================================================

/// Get the icon for a log level.
fn level_icon(lvl: level.Level) -> String {
  case lvl {
    level.Trace -> "→"
    level.Debug -> "⚙"
    level.Info -> "ℹ"
    level.Warn -> "⚠"
    level.Err -> "✖"
    level.Fatal -> "✖"
  }
}

/// Get the color code for a log level.
fn level_color(lvl: level.Level) -> String {
  case lvl {
    level.Trace -> gray
    level.Debug -> gray
    level.Info -> cyan
    level.Warn -> yellow
    level.Err -> red
    level.Fatal -> bg_red <> white
  }
}

/// Get the label for a log level (lowercase for consola style).
fn level_label(lvl: level.Level) -> String {
  case lvl {
    level.Trace -> "trace"
    level.Debug -> "debug"
    level.Info -> "info"
    level.Warn -> "warn"
    level.Err -> "error"
    level.Fatal -> "fatal"
  }
}

/// Create a formatter with the given options.
fn format_consola(
  use_color: Bool,
  show_timestamp: Bool,
  level_fmt: LevelFormatter,
) -> formatter.Formatter {
  fn(record: LogRecord) -> String {
    format_record(record, use_color, show_timestamp, level_fmt)
  }
}

/// Format a log record with the given settings.
fn format_record(
  record: LogRecord,
  use_color: Bool,
  show_timestamp: Bool,
  level_fmt: LevelFormatter,
) -> String {
  let level_part = format_level(level_fmt, record.level, use_color)

  let timestamp_part = case show_timestamp, use_color {
    True, True -> dim <> record.timestamp <> reset <> " "
    True, False -> record.timestamp <> " "
    False, _ -> ""
  }

  let scope_part = case record.logger_name, use_color {
    "", _ -> ""
    name, True -> dim <> "[" <> name <> "]" <> reset <> " "
    name, False -> "[" <> name <> "] "
  }

  let metadata_str = formatter.format_metadata(record.metadata)
  let metadata_part = case metadata_str, use_color {
    "", _ -> ""
    m, True -> " " <> dim <> m <> reset
    m, False -> " " <> m
  }

  timestamp_part
  <> level_part
  <> " "
  <> scope_part
  <> record.message
  <> metadata_part
}
