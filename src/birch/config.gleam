//// Global configuration for the logging system.
////
//// This module provides types and functions for configuring the default
//// logger and application-wide logging settings.

import birch/handler.{type ErrorCallback, type Handler}
import birch/level.{type Level}
import birch/record.{type Metadata}
import gleam/list
import gleam/option.{type Option, Some}

// ============================================================================
// Sampling Types (defined here to avoid circular imports)
// ============================================================================

/// Configuration for probabilistic sampling.
///
/// Logs at or below the configured level will be sampled at the specified rate.
/// Logs above the configured level are always logged (no sampling applied).
pub type SampleConfig {
  SampleConfig(
    /// Apply sampling to this level and below
    level: Level,
    /// Probability of logging (0.0 to 1.0)
    rate: Float,
  )
}

// ============================================================================
// Global Configuration
// ============================================================================

/// Global configuration that affects the default logger and
/// application-wide settings.
///
/// **Planned breaking change:** The `sampling` field will change from
/// `Result(SampleConfig, Nil)` to `Option(SampleConfig)` in a future release.
/// Migrate from `Ok(config)` / `Error(Nil)` to `Some(config)` / `None`.
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
    /// Optional sampling configuration
    sampling: Option(SampleConfig),
  )
}

/// Configuration options used in the builder pattern.
pub opaque type ConfigOption {
  LevelOption(Level)
  HandlersOption(List(Handler))
  ContextOption(Metadata)
  OnErrorOption(ErrorCallback)
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

/// Create a configuration option to set the global error callback.
///
/// This callback is invoked when any handler encounters an error.
/// It's useful for monitoring and alerting on handler failures.
pub fn on_error(callback: ErrorCallback) -> ConfigOption {
  OnErrorOption(callback)
}

/// Create a configuration option to set sampling.
pub fn sampling(config: SampleConfig) -> ConfigOption {
  SamplingOption(config)
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
    SamplingOption(s) -> GlobalConfig(..config, sampling: Some(s))
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

// ============================================================================
// Global Configuration Storage (FFI)
// ============================================================================

/// Get the global configuration from platform-specific storage.
/// Returns Ok(config) if set, Error(Nil) if not configured.
@external(erlang, "birch_ffi", "get_global_config")
@external(javascript, "../birch_ffi.mjs", "get_global_config")
pub fn get_global_config() -> Result(GlobalConfig, Nil)

/// Set the global configuration in platform-specific storage.
@external(erlang, "birch_ffi", "set_global_config")
@external(javascript, "../birch_ffi.mjs", "set_global_config")
pub fn set_global_config(config: GlobalConfig) -> Nil

/// Clear the global configuration from platform-specific storage.
@external(erlang, "birch_ffi", "clear_global_config")
@external(javascript, "../birch_ffi.mjs", "clear_global_config")
pub fn clear_global_config() -> Nil
