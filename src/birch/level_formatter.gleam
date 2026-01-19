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

import birch/level
import gleam/string

// ============================================================================
// ANSI Escape Codes
// ============================================================================

const reset = "\u{001b}[0m"

const bold = "\u{001b}[1m"

// Colors
const gray = "\u{001b}[90m"

const cyan = "\u{001b}[36m"

const green = "\u{001b}[32m"

const yellow = "\u{001b}[33m"

const red = "\u{001b}[31m"

const magenta = "\u{001b}[35m"

const blue = "\u{001b}[34m"

const bright_red = "\u{001b}[91m"

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

// ============================================================================
// Badge Style Formatter
// ============================================================================

/// Configuration for badge-style level formatting.
pub type BadgeConfig {
  BadgeConfig
}

/// Default badge configuration.
pub fn default_badge_config() -> BadgeConfig {
  BadgeConfig
}

/// Create a badge-style formatter with default settings.
/// Output: "[INFO]", "[WARN]", "[ERROR]", etc.
///
/// With colors enabled, displays with bold foreground colors based on severity.
/// This style provides high visual prominence, especially for errors.
pub fn badge_formatter() -> LevelFormatter {
  badge_formatter_with_config(default_badge_config())
}

/// Create a badge-style formatter with custom configuration.
pub fn badge_formatter_with_config(_config: BadgeConfig) -> LevelFormatter {
  LevelFormatter(
    format: fn(lvl, use_color) { format_badge(lvl, use_color) },
    // Badge width: "[TRACE]" = 7, "[ERROR]" = 7
    target_width: 7,
  )
}

fn format_badge(lvl: level.Level, use_color: Bool) -> String {
  let label = level_label_upper(lvl)

  case use_color {
    True -> {
      let color = level_color(lvl)
      color <> bold <> "[" <> label <> "]" <> reset
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
  let label = level_label_upper(lvl)

  case use_color {
    True -> {
      let color = simple_level_color(lvl)
      color <> label <> reset
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

/// Get the color code for a log level (fancy style).
pub fn level_color(lvl: level.Level) -> String {
  case lvl {
    level.Trace -> gray
    level.Debug -> gray
    level.Info -> cyan
    level.Warn -> yellow
    level.Err -> red
    level.Fatal -> bright_red
  }
}

/// Get the color code for a log level (simple style).
fn simple_level_color(lvl: level.Level) -> String {
  case lvl {
    level.Trace -> gray
    level.Debug -> blue
    level.Info -> cyan
    level.Warn -> yellow
    level.Err -> red
    level.Fatal -> bright_red
  }
}

/// Get the label for a log level (lowercase).
pub fn level_label(lvl: level.Level) -> String {
  case lvl {
    level.Trace -> "trace"
    level.Debug -> "debug"
    level.Info -> "info"
    level.Warn -> "warn"
    level.Err -> "error"
    level.Fatal -> "fatal"
  }
}

/// Get the uppercase label for a log level.
pub fn level_label_upper(lvl: level.Level) -> String {
  case lvl {
    level.Trace -> "TRACE"
    level.Debug -> "DEBUG"
    level.Info -> "INFO"
    level.Warn -> "WARN"
    level.Err -> "ERROR"
    level.Fatal -> "FATAL"
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
  // Check if the string contains ANSI codes by looking for reset code
  case string.contains(formatted, reset) {
    True -> {
      // Find the visual length by removing ANSI codes
      let visual_length = calculate_visual_length(formatted)
      let padding_needed = target_width - visual_length
      case padding_needed > 0 {
        True -> formatted <> string.repeat(" ", padding_needed)
        False -> formatted
      }
    }
    False -> {
      // Plain text - simple padding
      let padding_needed = target_width - string.length(formatted)
      case padding_needed > 0 {
        True -> formatted <> string.repeat(" ", padding_needed)
        False -> formatted
      }
    }
  }
}

/// Calculate the visual length of a string, excluding ANSI escape codes.
/// This is a simplified calculation that counts visible characters.
fn calculate_visual_length(s: String) -> Int {
  // For our formatters, we know the structure:
  // - Simple: "\u{001b}[XXm" + label + "\u{001b}[0m"
  // - Badge: "\u{001b}[XXm\u{001b}[1m[" + label + "]\u{001b}[0m"
  // - Label: "\u{001b}[XXm\u{001b}[1m" + label + "\u{001b}[0m"
  //
  // We can estimate by: total length - (ANSI overhead)
  // Each color code is typically 5-6 chars, reset is 4 chars, bold is 4 chars
  //
  // For now, use a simple heuristic: count characters that aren't part of escape sequences
  let without_reset = string.replace(s, reset, "")
  let without_bold = string.replace(without_reset, bold, "")

  // Remove color codes (they all start with \u{001b}[ and end with m)
  // This is approximate but works for our use case
  let chars = string.to_graphemes(without_bold)
  count_visible_chars(chars, False, 0)
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

// ============================================================================
// ANSI Code Accessors (for use by other modules)
// ============================================================================

/// Get the ANSI reset code.
pub fn ansi_reset() -> String {
  reset
}

/// Get the ANSI bold code.
pub fn ansi_bold() -> String {
  bold
}

/// Get the ANSI dim code.
pub fn ansi_dim() -> String {
  "\u{001b}[2m"
}

/// Get the ANSI gray color code.
pub fn ansi_gray() -> String {
  gray
}

/// Get the ANSI cyan color code.
pub fn ansi_cyan() -> String {
  cyan
}

/// Get the ANSI green color code.
pub fn ansi_green() -> String {
  green
}

/// Get the ANSI yellow color code.
pub fn ansi_yellow() -> String {
  yellow
}

/// Get the ANSI red color code.
pub fn ansi_red() -> String {
  red
}

/// Get the ANSI magenta color code.
pub fn ansi_magenta() -> String {
  magenta
}
