//// Global configuration for the logging system.
////
//// This module provides types and functions for configuring the default
//// logger and application-wide logging settings.

import gleam/list
import gleam_log/handler.{type Handler}
import gleam_log/level.{type Level}
import gleam_log/record.{type Metadata}
import gleam_log/sampling.{type SampleConfig}

/// Global configuration that affects the default logger and
/// application-wide settings.
pub type GlobalConfig {
  GlobalConfig(
    /// Minimum log level for the default logger
    level: Level,
    /// Handlers to use for the default logger
    handlers: List(Handler),
    /// Default context metadata applied to all loggers
    context: Metadata,
    /// Optional sampling configuration
    sampling: Result(SampleConfig, Nil),
  )
}

/// Configuration options used in the builder pattern.
pub opaque type ConfigOption {
  LevelOption(Level)
  HandlersOption(List(Handler))
  ContextOption(Metadata)
  SamplingOption(SampleConfig)
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

/// Create a configuration option to set sampling.
pub fn sampling(config: SampleConfig) -> ConfigOption {
  SamplingOption(config)
}

/// Returns the default global configuration with no handlers.
/// Note: Use gleam_log.default_config() to get defaults with console handler.
pub fn empty() -> GlobalConfig {
  GlobalConfig(
    level: level.Info,
    handlers: [],
    context: [],
    sampling: Error(Nil),
  )
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
    SamplingOption(s) -> GlobalConfig(..config, sampling: Ok(s))
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
