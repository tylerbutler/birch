//// Global configuration for the logging system.
////
//// This module provides types and functions for configuring the default
//// logger and application-wide logging settings.

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam_log/handler.{type ErrorCallback, type Handler}
import gleam_log/level.{type Level}
import gleam_log/record.{type Metadata}

/// Global configuration that affects the default logger and application-wide settings.
pub type GlobalConfig {
  GlobalConfig(
    /// Minimum log level for the default logger
    level: Level,
    /// Handlers to use for the default logger
    handlers: List(Handler),
    /// Default context metadata applied to all loggers
    context: Metadata,
    /// Optional global error callback for handler failures
    on_error: Option(ErrorCallback),
  )
}

/// Configuration options used in the builder pattern.
pub opaque type ConfigOption {
  LevelOption(Level)
  HandlersOption(List(Handler))
  ContextOption(Metadata)
  OnErrorOption(ErrorCallback)
}

/// Create a configuration option to set the log level.
pub fn level(lvl: Level) -> ConfigOption {
  LevelOption(lvl)
}

/// Create a configuration option to set the handlers.
pub fn handlers(h: List(Handler)) -> ConfigOption {
  HandlersOption(h)
}

/// Create a configuration option to set the default context.
pub fn context(ctx: Metadata) -> ConfigOption {
  ContextOption(ctx)
}

/// Create a configuration option to set the global error callback.
///
/// This callback is invoked when any handler encounters an error.
/// It's useful for monitoring and alerting on handler failures.
pub fn on_error(callback: ErrorCallback) -> ConfigOption {
  OnErrorOption(callback)
}

/// Returns the default global configuration with no handlers.
/// Note: Use gleam_log.default_config() to get defaults with console handler.
pub fn empty() -> GlobalConfig {
  GlobalConfig(level: level.Info, handlers: [], context: [], on_error: None)
}

/// Apply a list of configuration options to a GlobalConfig.
pub fn apply_options(
  config: GlobalConfig,
  options: List(ConfigOption),
) -> GlobalConfig {
  list.fold(options, config, apply_option)
}

/// Apply a single configuration option to a GlobalConfig.
fn apply_option(config: GlobalConfig, option: ConfigOption) -> GlobalConfig {
  case option {
    LevelOption(lvl) -> GlobalConfig(..config, level: lvl)
    HandlersOption(h) -> GlobalConfig(..config, handlers: h)
    ContextOption(ctx) -> GlobalConfig(..config, context: ctx)
    OnErrorOption(callback) -> GlobalConfig(..config, on_error: Some(callback))
  }
}

// ============================================================================
// Runtime Level Changes
// ============================================================================

/// Update only the log level in a GlobalConfig, preserving other settings.
pub fn with_level(config: GlobalConfig, lvl: Level) -> GlobalConfig {
  GlobalConfig(..config, level: lvl)
}

/// Get the log level from a GlobalConfig.
pub fn get_level(config: GlobalConfig) -> Level {
  config.level
}

/// Get the error callback from a GlobalConfig, if set.
pub fn get_on_error(config: GlobalConfig) -> Option(ErrorCallback) {
  config.on_error
}
