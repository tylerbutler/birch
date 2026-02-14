//// Console handler for log output.
////
//// Provides console output with multiple presentation styles:
//// - **Simple style**: Traditional pipe-delimited format
//// - **Fancy style**: Compact format with icons and styled scope prefixes
////
//// Also includes box output, grouping, and semantic log types (success, start, ready, fail).

import birch/formatter
import birch/handler.{type Handler, type OutputTarget, Stderr, Stdout}
import birch/internal/ansi
import birch/internal/platform
import birch/level_formatter.{type LevelFormatter}
import birch/record.{type LogRecord, type Metadata}
import gleam/int
import gleam/io
import gleam/list
import gleam/string

// ============================================================================
// Configuration
// ============================================================================

/// Console handler configuration.
pub type ConsoleConfig {
  ConsoleConfig(
    /// Whether to use colors (if terminal supports it)
    color: Bool,
    /// Whether to show timestamps
    timestamps: Bool,
    /// Output target (stdout, stderr, or split)
    target: OutputTarget,
    /// How to format log levels
    level_formatter: LevelFormatter,
    /// Presentation style
    style: ConsoleStyle,
    /// Whether to automatically indent based on scope depth
    auto_indent_from_scopes: Bool,
  )
}

/// Console presentation style.
pub type ConsoleStyle {
  /// Pipe-delimited format: "timestamp | LEVEL | logger | message"
  Simple
  /// Compact format with icons: "level [scope] message metadata"
  Fancy
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

/// Default console configuration with simple style.
pub fn default_config() -> ConsoleConfig {
  ConsoleConfig(
    color: True,
    timestamps: True,
    target: Stdout,
    level_formatter: level_formatter.simple_formatter(),
    style: Simple,
    auto_indent_from_scopes: False,
  )
}

/// Default fancy configuration with label-style formatting.
pub fn default_fancy_config() -> ConsoleConfig {
  ConsoleConfig(
    color: True,
    timestamps: False,
    target: Stdout,
    level_formatter: level_formatter.label_formatter(),
    style: Fancy,
    auto_indent_from_scopes: False,
  )
}

// ============================================================================
// Handler Creation
// ============================================================================

/// Create a console handler with default settings (simple style).
/// Uses human-readable format, colors if TTY, outputs to stdout.
pub fn handler() -> Handler {
  handler_with_config(default_config())
}

/// Create a fancy console handler with default fancy settings.
/// Uses icons and compact format, colors if TTY, outputs to stdout.
pub fn fancy_handler() -> Handler {
  handler_with_config(default_fancy_config())
}

/// Create a console handler with custom configuration.
pub fn handler_with_config(config: ConsoleConfig) -> Handler {
  let use_color = config.color && platform.is_stdout_tty()

  let write_fn = case config.target {
    Stdout -> io.println
    Stderr -> io.println_error
  }

  let format_fn = case config.style {
    Simple ->
      format_simple(
        use_color,
        config.timestamps,
        config.level_formatter,
        config.auto_indent_from_scopes,
      )
    Fancy ->
      format_fancy(
        use_color,
        config.timestamps,
        config.level_formatter,
        config.auto_indent_from_scopes,
      )
  }

  handler.new(name: "console", write: write_fn, format: format_fn)
}

// ============================================================================
// Simple Style Formatting
// ============================================================================

/// Create a simple formatter (pipe-delimited).
fn format_simple(
  use_color: Bool,
  show_timestamp: Bool,
  level_fmt: LevelFormatter,
  auto_indent: Bool,
) -> formatter.Formatter {
  fn(record: LogRecord) -> String {
    let base =
      format_record_simple(record, use_color, show_timestamp, level_fmt)
    case auto_indent {
      False -> base
      True -> {
        let depth = platform.get_scope_depth()
        string.repeat("  ", depth) <> base
      }
    }
  }
}

/// Format a log record in simple style.
fn format_record_simple(
  record: LogRecord,
  use_color: Bool,
  show_timestamp: Bool,
  level_fmt: LevelFormatter,
) -> String {
  let level_part =
    level_formatter.format_level_padded(level_fmt, record.level, use_color)

  let timestamp_part = case show_timestamp, use_color {
    True, True -> ansi.gray <> record.timestamp <> ansi.reset <> " | "
    True, False -> record.timestamp <> " | "
    False, _ -> ""
  }

  let metadata_str = format_metadata_visible(record.metadata, use_color)

  let metadata_part = case metadata_str {
    "" -> ""
    m ->
      " | "
      <> case use_color {
        True -> ansi.cyan <> m <> ansi.reset
        False -> m
      }
  }

  timestamp_part
  <> level_part
  <> " | "
  <> record.logger_name
  <> " | "
  <> record.message
  <> metadata_part
}

// ============================================================================
// Fancy Style Formatting
// ============================================================================

/// Create a fancy formatter (compact with icons).
fn format_fancy(
  use_color: Bool,
  show_timestamp: Bool,
  level_fmt: LevelFormatter,
  auto_indent: Bool,
) -> formatter.Formatter {
  fn(record: LogRecord) -> String {
    let base = format_record_fancy(record, use_color, show_timestamp, level_fmt)
    case auto_indent {
      False -> base
      True -> {
        let depth = platform.get_scope_depth()
        string.repeat("  ", depth) <> base
      }
    }
  }
}

/// Format a log record in fancy style.
fn format_record_fancy(
  record: LogRecord,
  use_color: Bool,
  show_timestamp: Bool,
  level_fmt: LevelFormatter,
) -> String {
  let dim = ansi.dim
  let reset = ansi.reset

  let level_part =
    level_formatter.format_level_padded(level_fmt, record.level, use_color)

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

  let metadata_str = format_metadata_visible(record.metadata, use_color)
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

// ============================================================================
// Metadata Formatting (filters internal keys)
// ============================================================================

/// Format metadata, excluding internal keys (prefixed with _).
fn format_metadata_visible(metadata: record.Metadata, use_color: Bool) -> String {
  let visible_metadata =
    list.filter(metadata, fn(pair) { !string.starts_with(pair.0, "_") })

  formatter.format_metadata_colored(visible_metadata, use_color)
}

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
    True -> ansi.cyan
    False -> ""
  }
  let reset_code = case use_color {
    True -> ansi.reset
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
  lines
  |> list.map(string.length)
  |> list.fold(min_width, int.max)
}

/// Write a boxed message directly to stdout.
pub fn write_box(message: String) -> Nil {
  io.println(box(message))
}

/// Write a boxed message with title directly to stdout.
pub fn write_box_with_title(message: String, title: String) -> Nil {
  io.println(box_with_title(message, title))
}

// ============================================================================
// Grouping
// ============================================================================

/// Execute a function within a named group, with all log output indented.
/// The group title is printed before the content, and indentation is applied
/// to all output within the scope.
///
/// The title is displayed in bold with a unique color based on its hash,
/// making it easy to distinguish different groups visually.
///
/// Example:
/// ```gleam
/// console.with_group("Building project", fn() {
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
  let #(arrow, styled_title) = case use_color {
    True -> {
      let color = formatter.hash_color(title)
      let reset = ansi.reset
      let bold = ansi.bold
      #(color <> "▸" <> reset, bold <> color <> title <> reset)
    }
    False -> #("▸", title)
  }
  io.println(arrow <> " " <> styled_title)
  work()
}

/// Create a handler that indents output by the specified level.
/// Use this within a group to get indented log output.
///
/// Example:
/// ```gleam
/// let indented_handler = console.indented_handler(1)
/// let lgr = logger.new("build") |> logger.with_handlers([indented_handler])
/// ```
pub fn indented_handler(indent_level: Int) -> Handler {
  indented_handler_with_config(indent_level, default_fancy_config())
}

/// Create an indented handler with custom configuration.
pub fn indented_handler_with_config(
  indent_level: Int,
  config: ConsoleConfig,
) -> Handler {
  let use_color = config.color && platform.is_stdout_tty()

  let write_fn = case config.target {
    Stdout -> io.println
    Stderr -> io.println_error
  }

  let indent = string.repeat("  ", indent_level)
  let format_fn =
    format_indented(
      use_color,
      config.timestamps,
      config.level_formatter,
      indent,
    )

  handler.new(name: "console", write: write_fn, format: format_fn)
}

fn format_indented(
  use_color: Bool,
  show_timestamp: Bool,
  level_fmt: LevelFormatter,
  indent: String,
) -> formatter.Formatter {
  fn(record: LogRecord) -> String {
    let base = format_record_fancy(record, use_color, show_timestamp, level_fmt)
    indent <> base
  }
}

// ============================================================================
// Semantic Log Types
// ============================================================================

/// Write a success message (✔ green) to stdout.
/// Used for successful completions.
///
/// Example:
/// ```gleam
/// console.success("Build completed!", [])
/// ```
pub fn success(message: String, metadata: Metadata) -> Nil {
  write_styled_with_metadata(Success, message, metadata)
}

/// Write a start message (◐ magenta) to stdout.
/// Used when beginning an operation.
///
/// Example:
/// ```gleam
/// console.start("Building project...", [])
/// ```
pub fn start(message: String, metadata: Metadata) -> Nil {
  write_styled_with_metadata(Start, message, metadata)
}

/// Write a ready message (✔ green) to stdout.
/// Used when something is initialized and ready.
///
/// Example:
/// ```gleam
/// console.ready("Server listening on port 3000", [#("port", "3000")])
/// ```
pub fn ready(message: String, metadata: Metadata) -> Nil {
  write_styled_with_metadata(Ready, message, metadata)
}

/// Write a fail message (✖ red) to stdout.
/// Used for non-fatal failures.
///
/// Example:
/// ```gleam
/// console.fail("Could not connect to cache", [#("host", "localhost")])
/// ```
pub fn fail(message: String, metadata: Metadata) -> Nil {
  write_styled_with_metadata(Fail, message, metadata)
}

fn write_styled_with_metadata(
  style: LogStyle,
  message: String,
  metadata: Metadata,
) -> Nil {
  let use_color = platform.is_stdout_tty()
  let formatted = format_styled_message(style, message, metadata, use_color)
  io.println(formatted)
}

fn format_styled_message(
  style: LogStyle,
  message: String,
  metadata: Metadata,
  use_color: Bool,
) -> String {
  let #(icon, color, label) = style_properties(style)

  let icon_part = icon <> " "

  let metadata_str = formatter.format_metadata_colored(metadata, use_color)
  let metadata_part = case metadata_str {
    "" -> ""
    m -> " " <> m
  }

  case use_color {
    True ->
      color
      <> icon_part
      <> ansi.bold
      <> label
      <> ansi.reset
      <> " "
      <> message
      <> metadata_part
    False -> icon_part <> label <> " " <> message <> metadata_part
  }
}

fn style_properties(style: LogStyle) -> #(String, String, String) {
  case style {
    Success -> #("✔", ansi.green, "success")
    Start -> #("◐", ansi.magenta, "start")
    Ready -> #("✔", ansi.green, "ready")
    Fail -> #("✖", ansi.red, "fail")
  }
}

// ============================================================================
// Direct Output Functions (for use without metadata)
// ============================================================================

/// Write a success message directly to stdout (without metadata).
pub fn write_success(message: String) -> Nil {
  success(message, [])
}

/// Write a start message directly to stdout (without metadata).
pub fn write_start(message: String) -> Nil {
  start(message, [])
}

/// Write a ready message directly to stdout (without metadata).
pub fn write_ready(message: String) -> Nil {
  ready(message, [])
}

/// Write a fail message directly to stdout (without metadata).
pub fn write_fail(message: String) -> Nil {
  fail(message, [])
}
