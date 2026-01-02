//// Sampling and rate limiting for high-volume logging scenarios.
////
//// This module provides probabilistic sampling and rate limiting to prevent
//// log flooding in high-throughput applications.
////
//// ## Sampling
////
//// Sampling allows you to log only a percentage of messages at certain levels:
////
//// ```gleam
//// import birch as log
//// import birch/level
//// import birch/sampling
////
//// // Log only 10% of debug messages
//// log.configure([
////   log.config_sampling(sampling.config(level.Debug, 0.1)),
//// ])
//// ```
////
//// ## Rate Limiting
////
//// Rate limiting uses a token bucket algorithm to limit logs per second:
////
//// ```gleam
//// import birch/sampling
////
//// // Allow max 100 logs/second with burst of 10
//// let config = sampling.rate_limit_config(100, 10)
//// ```

import birch/config.{type SampleConfig, SampleConfig}
import birch/internal/platform
import birch/level.{type Level}
import gleam/float
import gleam/int

// ============================================================================
// Sampling Configuration
// ============================================================================

// Re-export SampleConfig type from config module
pub type SampleConfigType =
  SampleConfig

/// Create a sampling configuration.
///
/// - `lvl`: Apply sampling to this level and all levels below it
/// - `rate`: Probability of logging (0.0 = never, 1.0 = always)
///
/// The rate is clamped to the valid range [0.0, 1.0].
pub fn config(lvl: Level, rate: Float) -> SampleConfig {
  SampleConfig(level: lvl, rate: clamp_rate(rate))
}

/// Clamp a rate to the valid range [0.0, 1.0].
fn clamp_rate(rate: Float) -> Float {
  case rate <. 0.0 {
    True -> 0.0
    False ->
      case rate >. 1.0 {
        True -> 1.0
        False -> rate
      }
  }
}

/// Check if a log at the given level should be sampled (logged).
///
/// Returns True if the log should be emitted, False if it should be dropped.
///
/// - Logs above the sample config level are always logged
/// - Logs at or below the sample config level are sampled probabilistically
pub fn should_sample(sample_config: SampleConfig, log_level: Level) -> Bool {
  // If log level is above the sampling threshold, always log
  case level.gt(log_level, sample_config.level) {
    True -> True
    False -> {
      // For levels at or below threshold, apply probabilistic sampling
      case sample_config.rate {
        r if r >=. 1.0 -> True
        r if r <=. 0.0 -> False
        rate -> {
          let random = platform.random_float()
          random <. rate
        }
      }
    }
  }
}

/// Check if a log should be sampled, given an optional SampleConfig.
///
/// If no config is provided (Error), always returns True (log everything).
pub fn should_sample_with_config(
  maybe_config: Result(SampleConfig, Nil),
  log_level: Level,
) -> Bool {
  case maybe_config {
    Error(Nil) -> True
    Ok(sample_config) -> should_sample(sample_config, log_level)
  }
}

// ============================================================================
// Rate Limiting Configuration
// ============================================================================

/// Configuration for rate limiting using a token bucket algorithm.
pub type RateLimitConfig {
  RateLimitConfig(
    /// Maximum logs per second
    max_per_second: Int,
    /// Burst size (maximum tokens that can accumulate)
    burst_size: Int,
  )
}

/// Create a rate limit configuration.
///
/// - `max_per_second`: Target rate limit (tokens added per second)
/// - `burst_size`: Maximum burst capacity (allows brief spikes above rate)
pub fn rate_limit_config(
  max_per_second: Int,
  burst_size: Int,
) -> RateLimitConfig {
  RateLimitConfig(max_per_second: max_per_second, burst_size: burst_size)
}

// ============================================================================
// Token Bucket Implementation
// ============================================================================

/// A token bucket for rate limiting.
///
/// The bucket starts with max_tokens and consumes one token per log.
/// Tokens are refilled over time based on the rate.
pub opaque type TokenBucket {
  TokenBucket(
    /// Maximum tokens the bucket can hold
    max_tokens: Int,
    /// Tokens added per second
    refill_rate: Int,
    /// Current number of tokens
    tokens: Float,
    /// Last refill timestamp (milliseconds)
    last_refill_ms: Int,
  )
}

/// Create a new token bucket.
///
/// - `max_tokens`: Maximum tokens the bucket can hold (burst capacity)
/// - `refill_rate`: Tokens added per second
pub fn new_token_bucket(max_tokens: Int, refill_rate: Int) -> TokenBucket {
  TokenBucket(
    max_tokens: max_tokens,
    refill_rate: refill_rate,
    tokens: int_to_float(max_tokens),
    last_refill_ms: platform.current_time_ms(),
  )
}

/// Get the maximum tokens for a bucket.
pub fn bucket_max_tokens(bucket: TokenBucket) -> Int {
  bucket.max_tokens
}

/// Get the refill rate (burst size) for a bucket.
pub fn bucket_burst_size(bucket: TokenBucket) -> Int {
  bucket.refill_rate
}

/// Try to consume a token from the bucket.
///
/// Returns a tuple of:
/// - Bool: True if token was consumed (log allowed), False if bucket empty
/// - TokenBucket: The updated bucket state
pub fn try_consume(bucket: TokenBucket) -> #(Bool, TokenBucket) {
  let now_ms = platform.current_time_ms()
  let refilled = refill_bucket(bucket, now_ms)

  case refilled.tokens >=. 1.0 {
    True -> {
      let new_bucket = TokenBucket(..refilled, tokens: refilled.tokens -. 1.0)
      #(True, new_bucket)
    }
    False -> #(False, refilled)
  }
}

/// Refill the bucket based on elapsed time.
fn refill_bucket(bucket: TokenBucket, now_ms: Int) -> TokenBucket {
  let elapsed_ms = now_ms - bucket.last_refill_ms
  case elapsed_ms > 0 {
    False -> bucket
    True -> {
      let elapsed_seconds = int_to_float(elapsed_ms) /. 1000.0
      let tokens_to_add = elapsed_seconds *. int_to_float(bucket.refill_rate)
      let new_tokens =
        float.min(
          bucket.tokens +. tokens_to_add,
          int_to_float(bucket.max_tokens),
        )
      TokenBucket(..bucket, tokens: new_tokens, last_refill_ms: now_ms)
    }
  }
}

/// Convert an Int to a Float.
fn int_to_float(n: Int) -> Float {
  int.to_float(n)
}
