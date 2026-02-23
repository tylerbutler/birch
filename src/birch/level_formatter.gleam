//// Level formatting utilities for log output.
////
//// This module provides customizable level formatters that control how log levels
//// are displayed. It supports multiple styles including labels with icons,
//// badge-style formatting, and custom formatters.
////
//// ## Available Styles
////
//// - **Label style**: `ℹ info`, `⚠ warn`, `✖ error` - icons with lowercase labels
//// - **Badge style**: `[INFO]`, `[WARN]`, `[ERROR]` - uppercase in brackets with bold foreground colors
//// - **Simple style**: `INFO`, `WARN`, `ERROR` - uppercase labels only
//// - **Custom**: Create your own formatting function

import birch/internal/ansi
import birch/level
import gleam/string

// ============================================================================
// Level Formatter Type
// ============================================================================

/// An opaque level formatter that encapsulates formatting logic and configuration.
/// Use the provided constructor functions to create formatters.
pub opaque type LevelFormatter {
  LevelFormatter(
    format: fn(level.Level, Bool) -> String,
    /// The target width for padding this formatter's output
    target_width: Int,
  )
}

// ============================================================================
// Label Style Formatter
// ============================================================================

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
  LevelFormatter(
    format: fn(lvl, use_color) { format_label(lvl, config, use_color) },
    // Label width: "trace" = 5, "error" = 5 (but icons add 2 chars: "ℹ info" = 6)
    target_width: case config.icons {
      True -> 6
      False -> 5
    },
  )
}

fn format_label(
  lvl: level.Level,
  config: LabelConfig,
  use_color: Bool,
) -> String {
  let label = level.to_string_lowercase(lvl)

  case use_color, config.icons {
    True, True -> {
      let color = level_color(lvl)
      let icon = level_icon(lvl)
      color <> icon <> " " <> ansi.bold <> label <> ansi.reset
    }
    True, False -> {
      let color = level_color(lvl)
      color <> ansi.bold <> label <> ansi.reset
    }
    False, True -> {
      let icon = level_icon(lvl)
      icon <> " " <> label
    }
    False, False -> label
  }
}

// ============================================================================
// Badge Style Formatter
// ============================================================================

/// Create a badge-style formatter.
/// Output: "[INFO]", "[WARN]", "[ERROR]", etc.
///
/// With colors enabled, displays with bold foreground colors based on severity.
/// This style provides high visual prominence, especially for errors.
pub fn badge_formatter() -> LevelFormatter {
  LevelFormatter(
    format: fn(lvl, use_color) { format_badge(lvl, use_color) },
    // Badge width: "[TRACE]" = 7, "[ERROR]" = 7
    target_width: 7,
  )
}

fn format_badge(lvl: level.Level, use_color: Bool) -> String {
  let label = level.to_string(lvl)

  case use_color {
    True -> {
      let color = level_color(lvl)
      color <> ansi.bold <> "[" <> label <> "]" <> ansi.reset
    }
    False -> "[" <> label <> "]"
  }
}

// ============================================================================
// Simple Style Formatter
// ============================================================================

/// Create a simple formatter that outputs just the uppercase level name.
/// Output: "TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"
///
/// With colors enabled, the level is colored based on severity.
/// This matches the style used by the original console handler.
pub fn simple_formatter() -> LevelFormatter {
  LevelFormatter(
    format: fn(lvl, use_color) { format_simple(lvl, use_color) },
    // Simple width: "TRACE" = 5, "ERROR" = 5
    target_width: 5,
  )
}

fn format_simple(lvl: level.Level, use_color: Bool) -> String {
  let label = level.to_string(lvl)

  case use_color {
    True -> {
      let color = simple_level_color(lvl)
      color <> label <> ansi.reset
    }
    False -> label
  }
}

// ============================================================================
// Custom Formatter
// ============================================================================

/// Create a custom level formatter from a formatting function.
/// The function receives the log level and whether colors are enabled.
///
/// You should also specify the target width for padding.
///
/// ## Example
///
/// ```gleam
/// let my_formatter = custom_level_formatter(fn(lvl, use_color) {
///   case lvl {
///     level.Info -> "INFO:"
///     level.Warn -> "WARNING:"
///     _ -> "LOG:"
///   }
/// }, 8)  // "WARNING:" is 8 characters
/// ```
pub fn custom_level_formatter(
  format: fn(level.Level, Bool) -> String,
  target_width: Int,
) -> LevelFormatter {
  LevelFormatter(format: format, target_width: target_width)
}

// ============================================================================
// Formatter Application
// ============================================================================

/// Apply a level formatter to format a log level.
pub fn format_level(
  formatter: LevelFormatter,
  lvl: level.Level,
  use_color: Bool,
) -> String {
  formatter.format(lvl, use_color)
}

/// Apply a level formatter and pad the result to the formatter's target width.
/// This is a convenience function for layout formatters.
pub fn format_level_padded(
  formatter: LevelFormatter,
  lvl: level.Level,
  use_color: Bool,
) -> String {
  formatter.format(lvl, use_color)
  |> pad_to_width(formatter.target_width)
}

/// Get the target width for a level formatter.
/// This is useful if you need to apply custom padding logic.
pub fn get_target_width(formatter: LevelFormatter) -> Int {
  formatter.target_width
}

// ============================================================================
// Level Properties (Icons, Colors, Labels)
// ============================================================================

/// Get the icon for a log level.
pub fn level_icon(lvl: level.Level) -> String {
  case lvl {
    level.Trace -> "→"
    level.Debug -> "⚙"
    level.Info -> "ℹ"
    level.Warn -> "⚠"
    level.Err -> "✖"
    level.Fatal -> "✖"
  }
}

/// Get the color code for a log level (label/badge/fancy style).
/// Debug uses gray to keep it visually subdued alongside Trace.
pub fn level_color(lvl: level.Level) -> String {
  case lvl {
    level.Trace -> ansi.gray
    level.Debug -> ansi.gray
    level.Info -> ansi.cyan
    level.Warn -> ansi.yellow
    level.Err -> ansi.red
    level.Fatal -> ansi.bright_red
  }
}

/// Get the color code for a log level (simple/uppercase style).
/// Debug uses blue to distinguish it from Trace since both share
/// the same uppercase "DEBUG"/"TRACE" text without icons.
fn simple_level_color(lvl: level.Level) -> String {
  case lvl {
    level.Trace -> ansi.gray
    level.Debug -> ansi.blue
    level.Info -> ansi.cyan
    level.Warn -> ansi.yellow
    level.Err -> ansi.red
    level.Fatal -> ansi.bright_red
  }
}

// ============================================================================
// Padding Helpers (for use by layout formatters)
// ============================================================================

/// Pad a formatted level string to a fixed width.
/// Use this in layout formatters to ensure consistent alignment.
///
/// The width should be chosen based on the longest possible level output
/// for the given formatter style:
/// - Simple style (uppercase): 5 characters ("TRACE", "DEBUG", "ERROR", "FATAL")
/// - Badge style: 7 characters ("[TRACE]", "[DEBUG]", "[ERROR]", "[FATAL]")
/// - Label style (lowercase): 5 characters ("trace", "debug", "error", "fatal")
///
/// This function intelligently handles both plain text and ANSI-formatted strings
/// by padding AFTER any ANSI reset codes to keep coloring clean.
pub fn pad_to_width(formatted: String, target_width: Int) -> String {
  // Calculate visual length - either by removing ANSI codes or using string length
  let visual_length = case string.contains(formatted, ansi.reset) {
    True -> calculate_visual_length(formatted)
    False -> string.length(formatted)
  }

  let padding_needed = target_width - visual_length
  case padding_needed > 0 {
    True -> formatted <> string.repeat(" ", padding_needed)
    False -> formatted
  }
}

/// Calculate the visual length of a string, excluding ANSI escape codes.
/// Uses a state machine to skip all escape sequences (ESC[...m).
fn calculate_visual_length(s: String) -> Int {
  s
  |> string.to_graphemes
  |> count_visible_chars(False, 0)
}

fn count_visible_chars(chars: List(String), in_escape: Bool, count: Int) -> Int {
  case chars {
    [] -> count
    ["\u{001b}", ..rest] -> count_visible_chars(rest, True, count)
    ["m", ..rest] if in_escape -> count_visible_chars(rest, False, count)
    [_, ..rest] if in_escape -> count_visible_chars(rest, True, count)
    [_, ..rest] -> count_visible_chars(rest, False, count + 1)
  }
}
