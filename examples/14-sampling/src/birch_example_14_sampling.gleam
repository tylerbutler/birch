//// Sampling Example
////
//// Demonstrates log sampling and rate limiting.

import birch as log
import birch/config
import birch/level
import birch/sampling

pub fn main() {
  log.info("=== Sampling Demo ===", [])

  // Probabilistic sampling
  demo_probabilistic_sampling()

  // Token bucket rate limiting
  demo_token_bucket()

  log.reset_config()
  log.info("Demo complete", [])
}

/// Demonstrate probabilistic sampling.
fn demo_probabilistic_sampling() {
  log.info("--- Probabilistic Sampling ---", [])

  // Configure to sample 50% of debug messages
  log.configure([log.config_sampling(sampling.config(level.Debug, 0.5))])

  log.info("Configured 50% sampling for Debug level", [])
  log.info("Info and above are always logged", [])

  // Log several debug messages - about half should appear
  log.debug("Debug message 1 (may be sampled)", [])
  log.debug("Debug message 2 (may be sampled)", [])
  log.debug("Debug message 3 (may be sampled)", [])
  log.debug("Debug message 4 (may be sampled)", [])
  log.debug("Debug message 5 (may be sampled)", [])

  // These are always logged (above Debug level)
  log.info("Info is always logged", [])
  log.warn("Warn is always logged", [])

  // Reset sampling
  log.reset_config()
}

/// Demonstrate token bucket rate limiting.
fn demo_token_bucket() {
  log.info("--- Token Bucket Rate Limiting ---", [])

  // Create a token bucket: max 5 tokens, refills at 10/second
  let bucket = sampling.new_token_bucket(5, 10)

  log.info("Created token bucket: max=5, rate=10/sec", [])

  // Try to consume tokens
  simulate_token_bucket(bucket, 1)
}

fn simulate_token_bucket(bucket: sampling.TokenBucket, count: Int) -> Nil {
  case count > 8 {
    True -> Nil
    False -> {
      let #(allowed, new_bucket) = sampling.try_consume(bucket)
      case allowed {
        True -> log.info("Log " <> int_to_string(count) <> ": Allowed", [])
        False -> log.info("Log " <> int_to_string(count) <> ": Rate limited", [])
      }
      simulate_token_bucket(new_bucket, count + 1)
    }
  }
}

fn int_to_string(n: Int) -> String {
  case n {
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    _ -> "N"
  }
}

/// Create a sampling config for production.
/// Logs 1% of debug, all info and above.
pub fn production_sampling() -> config.SampleConfig {
  sampling.config(level.Debug, 0.01)
}

/// Create a sampling config for development.
/// Logs everything.
pub fn development_sampling() -> config.SampleConfig {
  sampling.config(level.Trace, 1.0)
}

/// Create a token bucket for high-volume logging.
/// Allows bursts of 100, sustained rate of 1000/second.
pub fn high_volume_bucket() -> sampling.TokenBucket {
  sampling.new_token_bucket(100, 1000)
}
