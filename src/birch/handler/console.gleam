//// Console handler for log output.
////
//// Provides console output with multiple presentation styles:
//// - **Simple style**: Traditional pipe-delimited format
//// - **Fancy style**: Compact format with icons and styled scope prefixes
////
//// Also includes box output, grouping, and semantic log types (success, start, ready, fail).

import birch/formatter
import birch/handler.{type Handler, type OutputTarget, Stderr, Stdout}
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
// Configuration Modifiers
// ============================================================================

/// Set the level formatter for a configuration.
pub fn with_level_formatter(
  config: ConsoleConfig,
  formatter: LevelFormatter,
) -> ConsoleConfig {
  ConsoleConfig(..config, level_formatter: formatter)
}

/// Use badge-style level formatting.
pub fn with_badge_style(config: ConsoleConfig) -> ConsoleConfig {
  ConsoleConfig(..config, level_formatter: level_formatter.badge_formatter())
}

/// Use label-style level formatting (icons + lowercase labels).
pub fn with_label_style(config: ConsoleConfig) -> ConsoleConfig {
  ConsoleConfig(..config, level_formatter: level_formatter.label_formatter())
}

/// Use label-style formatting without icons.
pub fn with_label_style_no_icons(config: ConsoleConfig) -> ConsoleConfig {
  ConsoleConfig(
    ..config,
    level_formatter: level_formatter.label_formatter_with_config(
      level_formatter.LabelConfig(icons: False),
    ),
  )
}

/// Use simple-style level formatting (uppercase labels only).
pub fn with_simple_style(config: ConsoleConfig) -> ConsoleConfig {
  ConsoleConfig(..config, level_formatter: level_formatter.simple_formatter())
}

/// Enable timestamps in output.
pub fn with_timestamps(config: ConsoleConfig) -> ConsoleConfig {
  ConsoleConfig(..config, timestamps: True)
}

/// Disable timestamps in output.
pub fn without_timestamps(config: ConsoleConfig) -> ConsoleConfig {
  ConsoleConfig(..config, timestamps: False)
}

/// Enable colors in output.
pub fn with_color(config: ConsoleConfig) -> ConsoleConfig {
  ConsoleConfig(..config, color: True)
}

/// Disable colors in output.
pub fn without_color(config: ConsoleConfig) -> ConsoleConfig {
  ConsoleConfig(..config, color: False)
}

/// Enable automatic indentation based on scope depth.
/// When enabled, logs will be automatically indented by 2 spaces per scope level.
pub fn with_auto_indent_from_scopes(config: ConsoleConfig) -> ConsoleConfig {
  ConsoleConfig(..config, auto_indent_from_scopes: True)
}

/// Disable automatic indentation from scopes.
pub fn without_auto_indent_from_scopes(config: ConsoleConfig) -> ConsoleConfig {
  ConsoleConfig(..config, auto_indent_from_scopes: False)
}

/// Set output target to stdout.
pub fn with_stdout(config: ConsoleConfig) -> ConsoleConfig {
  ConsoleConfig(..config, target: Stdout)
}

/// Set output target to stderr.
pub fn with_stderr(config: ConsoleConfig) -> ConsoleConfig {
  ConsoleConfig(..config, target: Stderr)
}

/// Set presentation style to Simple (pipe-delimited).
pub fn with_simple_presentation(config: ConsoleConfig) -> ConsoleConfig {
  ConsoleConfig(..config, style: Simple)
}

/// Set presentation style to Fancy (compact with icons).
pub fn with_fancy_presentation(config: ConsoleConfig) -> ConsoleConfig {
  ConsoleConfig(..config, style: Fancy)
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
    handler.StdoutWithStderr -> write_split
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

/// Write to stdout for normal logs, stderr for errors.
fn write_split(message: String) -> Nil {
  // We can't easily determine the level here, so we just write to stdout
  // A more complete implementation would need access to the record
  io.println(message)
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
    True, True ->
      level_formatter.ansi_gray()
      <> record.timestamp
      <> level_formatter.ansi_reset()
      <> " | "
    True, False -> record.timestamp <> " | "
    False, _ -> ""
  }

  let metadata_str = format_metadata_visible(record.metadata, use_color)

  let metadata_part = case metadata_str {
    "" -> ""
    m ->
      " | "
      <> case use_color {
        True -> level_formatter.ansi_cyan() <> m <> level_formatter.ansi_reset()
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
  let dim = level_formatter.ansi_dim()
  let reset = level_formatter.ansi_reset()

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
/// Bolds keys listed in _scope_highlight_keys if use_color is true.
fn format_metadata_visible(metadata: record.Metadata, use_color: Bool) -> String {
  // Extract the highlight keys from _scope_highlight_keys metadata
  let highlight_keys = case list.key_find(metadata, "_scope_highlight_keys") {
    Ok(keys_str) -> string.split(keys_str, ",")
    Error(_) -> []
  }

  // Filter out internal keys
  let visible_metadata =
    list.filter(metadata, fn(pair) { !string.starts_with(pair.0, "_") })

  // Format with bold highlighting for scope keys
  formatter.format_metadata_with_bold(
    visible_metadata,
    highlight_keys,
    use_color,
  )
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
    True -> level_formatter.ansi_cyan()
    False -> ""
  }
  let reset_code = case use_color {
    True -> level_formatter.ansi_reset()
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

/// Get a color based on a simple hash of the input string.
/// Uses 256-color palette if terminal supports it, otherwise falls back to 6 basic colors.
fn hash_color(text: String) -> String {
  let hash =
    text
    |> string.to_utf_codepoints
    |> list.fold(0, fn(acc, cp) { acc + string.utf_codepoint_to_int(cp) })

  let color_depth = platform.get_color_depth()

  case color_depth >= 256 {
    True -> {
      // Use 256-color palette - pick from a range of nice, readable colors
      // We use colors 38-218 (avoiding the very dark and very light ones)
      // These are the 6x6x6 color cube (16-231) minus the extremes
      let color_index = { hash % 180 } + 38
      "\u{001b}[38;5;" <> int.to_string(color_index) <> "m"
    }
    False -> {
      // Fall back to basic 6-color palette
      case hash % 6 {
        0 -> level_formatter.ansi_cyan()
        1 -> level_formatter.ansi_green()
        2 -> level_formatter.ansi_yellow()
        3 -> level_formatter.ansi_magenta()
        4 -> level_formatter.ansi_blue()
        _ -> level_formatter.ansi_red()
      }
    }
  }
}

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
      let color = hash_color(title)
      let reset = level_formatter.ansi_reset()
      let bold = level_formatter.ansi_bold()
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
    handler.StdoutWithStderr -> io.println
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

  let metadata_str = formatter.format_metadata(metadata)
  let metadata_part = case metadata_str {
    "" -> ""
    m ->
      case use_color {
        True ->
          " " <> level_formatter.ansi_dim() <> m <> level_formatter.ansi_reset()
        False -> " " <> m
      }
  }

  case use_color {
    True ->
      color
      <> icon_part
      <> level_formatter.ansi_bold()
      <> label
      <> level_formatter.ansi_reset()
      <> " "
      <> message
      <> metadata_part
    False -> icon_part <> label <> " " <> message <> metadata_part
  }
}

fn style_properties(style: LogStyle) -> #(String, String, String) {
  case style {
    Success -> #("✔", level_formatter.ansi_green(), "success")
    Start -> #("◐", level_formatter.ansi_magenta(), "start")
    Ready -> #("✔", level_formatter.ansi_green(), "ready")
    Fail -> #("✖", level_formatter.ansi_red(), "fail")
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
