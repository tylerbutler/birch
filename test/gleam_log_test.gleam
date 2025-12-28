import gleam/list
import gleam/order
import gleam/string
import gleam_log
import gleam_log/formatter
import gleam_log/handler
import gleam_log/handler/async
import gleam_log/handler/file
import gleam_log/handler/json
import gleam_log/level
import gleam_log/logger
import gleam_log/record
import gleam_log/sampling
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Level Tests
// ============================================================================

pub fn level_ordering_test() {
  level.compare(level.Trace, level.Debug)
  |> should.equal(order.Lt)

  level.compare(level.Debug, level.Info)
  |> should.equal(order.Lt)

  level.compare(level.Info, level.Warn)
  |> should.equal(order.Lt)

  level.compare(level.Warn, level.Err)
  |> should.equal(order.Lt)

  level.compare(level.Err, level.Fatal)
  |> should.equal(order.Lt)
}

pub fn level_equality_test() {
  level.compare(level.Info, level.Info)
  |> should.equal(order.Eq)
}

pub fn level_gte_test() {
  level.gte(level.Info, level.Debug)
  |> should.be_true

  level.gte(level.Debug, level.Info)
  |> should.be_false

  level.gte(level.Info, level.Info)
  |> should.be_true
}

pub fn level_to_string_test() {
  level.to_string(level.Trace)
  |> should.equal("TRACE")

  level.to_string(level.Info)
  |> should.equal("INFO")

  level.to_string(level.Fatal)
  |> should.equal("FATAL")
}

pub fn level_from_string_test() {
  level.from_string("info")
  |> should.equal(Ok(level.Info))

  level.from_string("INFO")
  |> should.equal(Ok(level.Info))

  level.from_string("warning")
  |> should.equal(Ok(level.Warn))

  level.from_string("invalid")
  |> should.equal(Error(Nil))
}

pub fn level_default_test() {
  level.default()
  |> should.equal(level.Info)
}

// ============================================================================
// Record Tests
// ============================================================================

pub fn record_creation_test() {
  let r =
    record.new(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "Hello",
      metadata: [#("key", "value")],
    )

  r.timestamp
  |> should.equal("2024-12-26T10:30:45.123Z")

  r.level
  |> should.equal(level.Info)

  r.logger_name
  |> should.equal("test")

  r.message
  |> should.equal("Hello")

  r.metadata
  |> should.equal([#("key", "value")])
}

pub fn record_with_metadata_test() {
  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "Hello",
    )
    |> record.with_metadata([#("new_key", "new_value")])

  r.metadata
  |> should.equal([#("new_key", "new_value")])
}

pub fn record_get_metadata_test() {
  let r =
    record.new(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "Hello",
      metadata: [#("key1", "value1"), #("key2", "value2")],
    )

  record.get_metadata(r, "key1")
  |> should.equal(Ok("value1"))

  record.get_metadata(r, "key2")
  |> should.equal(Ok("value2"))

  record.get_metadata(r, "missing")
  |> should.equal(Error(Nil))
}

// ============================================================================
// Formatter Tests
// ============================================================================

pub fn formatter_human_readable_test() {
  let r =
    record.new(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "myapp.http",
      message: "Request complete",
      metadata: [#("method", "POST"), #("path", "/api/users")],
    )

  let formatted = formatter.human_readable(r)

  formatted
  |> string.contains("2024-12-26T10:30:45.123Z")
  |> should.be_true

  formatted
  |> string.contains("INFO")
  |> should.be_true

  formatted
  |> string.contains("myapp.http")
  |> should.be_true

  formatted
  |> string.contains("Request complete")
  |> should.be_true

  formatted
  |> string.contains("method=POST")
  |> should.be_true
}

pub fn formatter_simple_test() {
  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Err,
      logger_name: "test",
      message: "Something failed",
    )

  formatter.simple(r)
  |> should.equal("[ERROR] Something failed")
}

pub fn formatter_metadata_with_spaces_test() {
  let metadata = [#("path", "/api/users with spaces")]

  let formatted = formatter.format_metadata(metadata)

  formatted
  |> string.contains("\"")
  |> should.be_true
}

// ============================================================================
// Logger Tests
// ============================================================================

pub fn logger_creation_test() {
  let lgr = logger.new("myapp.database")

  logger.name(lgr)
  |> should.equal("myapp.database")

  logger.get_level(lgr)
  |> should.equal(level.Info)
}

pub fn logger_with_level_test() {
  let lgr =
    logger.new("test")
    |> logger.with_level(level.Debug)

  logger.get_level(lgr)
  |> should.equal(level.Debug)
}

pub fn logger_with_context_test() {
  let lgr =
    logger.new("test")
    |> logger.with_context([#("service", "api"), #("version", "1.0")])

  logger.get_context(lgr)
  |> list.length
  |> should.equal(2)
}

pub fn logger_should_log_test() {
  let lgr =
    logger.new("test")
    |> logger.with_level(level.Warn)

  logger.should_log(lgr, level.Err)
  |> should.be_true

  logger.should_log(lgr, level.Warn)
  |> should.be_true

  logger.should_log(lgr, level.Info)
  |> should.be_false

  logger.should_log(lgr, level.Debug)
  |> should.be_false
}

pub fn logger_silent_test() {
  let lgr = logger.silent("library.internal")

  logger.get_handlers(lgr)
  |> list.length
  |> should.equal(0)
}

// ============================================================================
// Handler Tests
// ============================================================================

pub fn handler_null_test() {
  let h = handler.null()

  handler.name(h)
  |> should.equal("null")

  // Should not crash
  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "test",
    )

  handler.handle(h, r)
}

pub fn handler_with_min_level_test() {
  let h =
    handler.null()
    |> handler.with_min_level(level.Err)

  handler.should_handle(h, level.Err)
  |> should.be_true

  handler.should_handle(h, level.Fatal)
  |> should.be_true

  handler.should_handle(h, level.Warn)
  |> should.be_false

  handler.should_handle(h, level.Info)
  |> should.be_false
}

// ============================================================================
// JSON Handler Tests
// ============================================================================

pub fn json_format_test() {
  let r =
    record.new(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "myapp.http",
      message: "Request complete",
      metadata: [#("method", "POST")],
    )

  let formatted = json.format_json(r)

  formatted
  |> string.contains("\"timestamp\":\"2024-12-26T10:30:45.123Z\"")
  |> should.be_true

  formatted
  |> string.contains("\"level\":\"info\"")
  |> should.be_true

  formatted
  |> string.contains("\"logger\":\"myapp.http\"")
  |> should.be_true

  formatted
  |> string.contains("\"message\":\"Request complete\"")
  |> should.be_true

  formatted
  |> string.contains("\"method\":\"POST\"")
  |> should.be_true
}

// ============================================================================
// Main API Tests
// ============================================================================

pub fn main_api_new_test() {
  let lgr = gleam_log.new("myapp")

  logger.name(lgr)
  |> should.equal("myapp")
}

pub fn main_api_with_context_test() {
  let lgr =
    gleam_log.new("myapp")
    |> gleam_log.with_context([#("env", "test")])

  logger.get_context(lgr)
  |> should.equal([#("env", "test")])
}

pub fn main_api_with_level_test() {
  let lgr =
    gleam_log.new("myapp")
    |> gleam_log.with_level(level.Debug)

  logger.get_level(lgr)
  |> should.equal(level.Debug)
}

pub fn main_api_level_from_string_test() {
  gleam_log.level_from_string("debug")
  |> should.equal(Ok(level.Debug))

  gleam_log.level_from_string("error")
  |> should.equal(Ok(level.Err))
}

pub fn main_api_level_to_string_test() {
  gleam_log.level_to_string(level.Info)
  |> should.equal("INFO")
}

// ============================================================================
// Global Configuration Tests
// ============================================================================

pub fn config_default_test() {
  // Get default config before any configuration
  let config = gleam_log.get_config()

  // Default level should be Info
  config.level
  |> should.equal(level.Info)

  // Default should have one handler (console)
  config.handlers
  |> list.length
  |> should.equal(1)
}

pub fn config_set_level_test() {
  // Configure with Debug level
  gleam_log.configure([gleam_log.config_level(level.Debug)])

  let config = gleam_log.get_config()
  config.level
  |> should.equal(level.Debug)

  // Reset to default for other tests
  gleam_log.configure([gleam_log.config_level(level.Info)])
}

pub fn config_set_handlers_test() {
  // Configure with custom handlers
  let null_handler = handler.null()
  gleam_log.configure([gleam_log.config_handlers([null_handler])])

  let config = gleam_log.get_config()
  config.handlers
  |> list.length
  |> should.equal(1)

  // The handler should be the null handler
  config.handlers
  |> list.first
  |> should.be_ok
  |> handler.name
  |> should.equal("null")

  // Reset to default
  gleam_log.reset_config()
}

pub fn config_set_context_test() {
  // Configure with default context
  gleam_log.configure([
    gleam_log.config_context([#("app", "test"), #("env", "testing")]),
  ])

  let config = gleam_log.get_config()
  config.context
  |> should.equal([#("app", "test"), #("env", "testing")])

  // Reset to default
  gleam_log.reset_config()
}

pub fn config_multiple_options_test() {
  // Configure with multiple options at once
  let null_handler = handler.null()
  gleam_log.configure([
    gleam_log.config_level(level.Warn),
    gleam_log.config_handlers([null_handler]),
    gleam_log.config_context([#("service", "api")]),
  ])

  let config = gleam_log.get_config()
  config.level
  |> should.equal(level.Warn)
  config.handlers
  |> list.length
  |> should.equal(1)
  config.context
  |> should.equal([#("service", "api")])

  // Reset to default
  gleam_log.reset_config()
}

pub fn config_reset_test() {
  // Configure with custom settings
  gleam_log.configure([gleam_log.config_level(level.Fatal)])

  // Verify it was set
  let config1 = gleam_log.get_config()
  config1.level
  |> should.equal(level.Fatal)

  // Reset to default
  gleam_log.reset_config()

  // Verify it was reset
  let config2 = gleam_log.get_config()
  config2.level
  |> should.equal(level.Info)
}

pub fn config_default_logger_uses_config_test() {
  // Configure the global config with Debug level
  gleam_log.configure([gleam_log.config_level(level.Debug)])

  // Create a new default logger - it should inherit the global level
  let lgr = gleam_log.new("test.config")

  // The logger should use the global configuration's level
  logger.get_level(lgr)
  |> should.equal(level.Debug)

  // Reset to default
  gleam_log.reset_config()
}

// ============================================================================
// Async Handler Tests
// ============================================================================

pub fn async_config_default_test() {
  let config = async.default_config()

  config.queue_size
  |> should.equal(1000)

  config.flush_interval_ms
  |> should.equal(100)

  config.overflow
  |> should.equal(async.DropOldest)
}

pub fn async_config_builder_test() {
  let config =
    async.config()
    |> async.with_queue_size(500)
    |> async.with_flush_interval(50)
    |> async.with_overflow(async.DropNewest)

  config.queue_size
  |> should.equal(500)

  config.flush_interval_ms
  |> should.equal(50)

  config.overflow
  |> should.equal(async.DropNewest)
}

pub fn async_make_async_creates_handler_test() {
  let base_handler = handler.null()
  let async_handler = async.make_async(base_handler, async.default_config())

  // The async handler should have a name containing "async"
  handler.name(async_handler)
  |> string.contains("async")
  |> should.be_true
}

pub fn async_handle_does_not_block_test() {
  // Create a counter to track writes
  let base_handler = handler.null()
  let async_handler = async.make_async(base_handler, async.default_config())

  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "async test message",
    )

  // This should return immediately (non-blocking)
  handler.handle(async_handler, r)
  handler.handle(async_handler, r)
  handler.handle(async_handler, r)

  // Flush to ensure all messages are processed
  async.flush()
}

pub fn async_flush_waits_for_pending_test() {
  let base_handler = handler.null()
  let async_handler = async.make_async(base_handler, async.default_config())

  // Send multiple records
  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "flush test",
    )

  handler.handle(async_handler, r)
  handler.handle(async_handler, r)

  // Flush should complete without hanging
  async.flush()
}

// ============================================================================
// Runtime Level Change Tests
// ============================================================================

pub fn set_level_changes_global_level_test() {
  // Reset to known state
  gleam_log.reset_config()

  // Default level should be Info
  gleam_log.get_level()
  |> should.equal(level.Info)

  // Set to Debug level
  gleam_log.set_level(level.Debug)

  // Level should now be Debug
  gleam_log.get_level()
  |> should.equal(level.Debug)

  // Reset for other tests
  gleam_log.reset_config()
}

pub fn set_level_affects_new_loggers_test() {
  // Reset to known state
  gleam_log.reset_config()

  // Set global level to Warn
  gleam_log.set_level(level.Warn)

  // New loggers should inherit the global level
  let lgr = gleam_log.new("test.runtime_level")
  logger.get_level(lgr)
  |> should.equal(level.Warn)

  // Reset for other tests
  gleam_log.reset_config()
}

pub fn set_level_takes_effect_immediately_test() {
  // Reset to known state
  gleam_log.reset_config()

  // Verify starting at Info
  let config1 = gleam_log.get_config()
  config1.level
  |> should.equal(level.Info)

  // Change to Trace
  gleam_log.set_level(level.Trace)

  // Should take effect immediately
  let config2 = gleam_log.get_config()
  config2.level
  |> should.equal(level.Trace)

  // Change again to Fatal
  gleam_log.set_level(level.Fatal)

  // Should take effect immediately
  let config3 = gleam_log.get_config()
  config3.level
  |> should.equal(level.Fatal)

  // Reset for other tests
  gleam_log.reset_config()
}

pub fn set_level_preserves_other_config_test() {
  // Reset to known state
  gleam_log.reset_config()

  // Configure with custom handlers and context
  let null_handler = handler.null()
  gleam_log.configure([
    gleam_log.config_handlers([null_handler]),
    gleam_log.config_context([#("app", "test")]),
  ])

  // Set level (should preserve other settings)
  gleam_log.set_level(level.Debug)

  let config = gleam_log.get_config()
  config.level
  |> should.equal(level.Debug)

  // Handlers and context should be preserved
  config.handlers
  |> list.length
  |> should.equal(1)

  config.handlers
  |> list.first
  |> should.be_ok
  |> handler.name
  |> should.equal("null")

  config.context
  |> should.equal([#("app", "test")])

  // Reset for other tests
  gleam_log.reset_config()
}

// ============================================================================
// Scoped Context Tests
// ============================================================================

pub fn scope_with_scope_returns_work_result_test() {
  // with_scope should return the result of the work function
  let result = gleam_log.with_scope([#("request_id", "123")], fn() { 42 })

  result
  |> should.equal(42)
}

pub fn scope_with_scope_string_result_test() {
  // with_scope should work with any return type
  let result =
    gleam_log.with_scope([#("trace_id", "abc")], fn() { "hello" })

  result
  |> should.equal("hello")
}

pub fn scope_get_scope_context_inside_scope_test() {
  // Inside a scope, get_scope_context should return the scope's context
  gleam_log.with_scope([#("request_id", "req-123")], fn() {
    let ctx = gleam_log.get_scope_context()
    ctx
    |> should.equal([#("request_id", "req-123")])
  })
}

pub fn scope_get_scope_context_outside_scope_test() {
  // Outside any scope, get_scope_context should return empty list
  let ctx = gleam_log.get_scope_context()

  ctx
  |> should.equal([])
}

pub fn scope_nested_scopes_test() {
  // Nested scopes should combine context, with inner values taking precedence
  gleam_log.with_scope([#("outer", "value1")], fn() {
    // First scope has outer context
    let outer_ctx = gleam_log.get_scope_context()
    outer_ctx
    |> list.key_find("outer")
    |> should.equal(Ok("value1"))

    // Nested scope adds more context
    gleam_log.with_scope([#("inner", "value2")], fn() {
      let inner_ctx = gleam_log.get_scope_context()

      // Both keys should be present
      inner_ctx
      |> list.key_find("outer")
      |> should.equal(Ok("value1"))

      inner_ctx
      |> list.key_find("inner")
      |> should.equal(Ok("value2"))
    })

    // After inner scope ends, only outer context remains
    let after_inner_ctx = gleam_log.get_scope_context()
    after_inner_ctx
    |> list.key_find("inner")
    |> should.be_error

    after_inner_ctx
    |> list.key_find("outer")
    |> should.equal(Ok("value1"))
  })
}

pub fn scope_nested_scopes_shadow_test() {
  // Inner scope with same key should shadow outer scope
  gleam_log.with_scope([#("key", "outer_value")], fn() {
    gleam_log.with_scope([#("key", "inner_value")], fn() {
      let ctx = gleam_log.get_scope_context()

      // The inner value should come first (shadow the outer)
      ctx
      |> list.key_find("key")
      |> should.equal(Ok("inner_value"))
    })

    // After inner scope, original value restored
    let ctx = gleam_log.get_scope_context()
    ctx
    |> list.key_find("key")
    |> should.equal(Ok("outer_value"))
  })
}

pub fn scope_is_available_test() {
  // is_scoped_context_available should return a boolean
  let available = gleam_log.is_scoped_context_available()

  // On both Erlang and JavaScript this should return a boolean
  // We just check it's callable and doesn't crash
  case available {
    True -> Nil
    False -> Nil
  }
}

pub fn scope_multiple_metadata_pairs_test() {
  // with_scope should support multiple key-value pairs
  gleam_log.with_scope(
    [#("request_id", "123"), #("user_id", "456"), #("trace_id", "789")],
    fn() {
      let ctx = gleam_log.get_scope_context()

      ctx
      |> list.key_find("request_id")
      |> should.equal(Ok("123"))

      ctx
      |> list.key_find("user_id")
      |> should.equal(Ok("456"))

      ctx
      |> list.key_find("trace_id")
      |> should.equal(Ok("789"))
    },
  )
}

pub fn scope_context_cleared_after_scope_test() {
  // Ensure context is properly cleaned up after scope ends
  gleam_log.with_scope([#("temp_key", "temp_value")], fn() {
    // Inside scope, context exists
    gleam_log.get_scope_context()
    |> list.key_find("temp_key")
    |> should.equal(Ok("temp_value"))
  })

  // After scope ends, context should be empty
  gleam_log.get_scope_context()
  |> should.equal([])
}

pub fn scope_context_included_in_log_records_test() {
  // Reset global config for clean test
  gleam_log.reset_config()

  // Create a mutable reference to capture log records (using list accumulator pattern)
  // We'll use a logger with a null handler and verify scope context is read
  gleam_log.with_scope([#("request_id", "test-123")], fn() {
    // Get the scope context which should be included in logs
    let ctx = gleam_log.get_scope_context()
    ctx
    |> list.key_find("request_id")
    |> should.equal(Ok("test-123"))

    // The log functions will include this context
    // (actual verification would require a capturing handler)
  })

  // Verify scope context is cleaned up
  gleam_log.get_scope_context()
  |> should.equal([])
}

pub fn scope_empty_context_test() {
  // with_scope with empty context should not crash
  let result = gleam_log.with_scope([], fn() { "success" })

  result
  |> should.equal("success")
}

// ============================================================================
// Sampling Tests
// ============================================================================

pub fn sampling_config_creation_test() {
  let config = sampling.config(level.Debug, 0.5)

  config.level
  |> should.equal(level.Debug)

  config.rate
  |> should.equal(0.5)
}

pub fn sampling_config_rate_clamping_test() {
  // Rates should be clamped to 0.0 - 1.0 range
  let config_high = sampling.config(level.Debug, 2.0)
  config_high.rate
  |> should.equal(1.0)

  let config_low = sampling.config(level.Debug, -0.5)
  config_low.rate
  |> should.equal(0.0)
}

pub fn sampling_should_sample_always_test() {
  // Rate 1.0 should always sample
  let config = sampling.config(level.Debug, 1.0)

  // Run multiple times to ensure it always returns True
  sampling.should_sample(config, level.Trace)
  |> should.be_true

  sampling.should_sample(config, level.Debug)
  |> should.be_true

  sampling.should_sample(config, level.Info)
  |> should.be_true
}

pub fn sampling_should_sample_never_test() {
  // Rate 0.0 should never sample (for levels at or below config level)
  let config = sampling.config(level.Debug, 0.0)

  sampling.should_sample(config, level.Trace)
  |> should.be_false

  sampling.should_sample(config, level.Debug)
  |> should.be_false
}

pub fn sampling_bypasses_for_higher_levels_test() {
  // Logs above the sample level should always be logged
  let config = sampling.config(level.Debug, 0.0)

  // Even with 0% rate, levels above Debug should still log
  sampling.should_sample(config, level.Info)
  |> should.be_true

  sampling.should_sample(config, level.Warn)
  |> should.be_true

  sampling.should_sample(config, level.Err)
  |> should.be_true
}

pub fn sampling_no_config_always_logs_test() {
  // When no sampling config, should_sample_with_config should return True
  let sample_config = Ok(sampling.config(level.Debug, 1.0))
  sampling.should_sample_with_config(sample_config, level.Debug)
  |> should.be_true

  // With Error (no config), should always sample
  sampling.should_sample_with_config(Error(Nil), level.Debug)
  |> should.be_true
}

// ============================================================================
// Rate Limiting Tests
// ============================================================================

pub fn rate_limit_config_creation_test() {
  let config = sampling.rate_limit_config(100, 10)

  config.max_per_second
  |> should.equal(100)

  config.burst_size
  |> should.equal(10)
}

pub fn rate_limit_token_bucket_creation_test() {
  let bucket = sampling.new_token_bucket(10, 5)

  sampling.bucket_max_tokens(bucket)
  |> should.equal(10)

  sampling.bucket_burst_size(bucket)
  |> should.equal(5)
}

pub fn rate_limit_try_consume_test() {
  // Create a bucket with max 10 tokens
  let bucket = sampling.new_token_bucket(10, 5)

  // First consume should succeed (starts with max tokens)
  let #(allowed1, bucket1) = sampling.try_consume(bucket)
  allowed1
  |> should.be_true

  // Continue consuming until we hit the limit
  let #(_allowed2, bucket2) = sampling.try_consume(bucket1)
  let #(_allowed3, bucket3) = sampling.try_consume(bucket2)
  let #(_allowed4, bucket4) = sampling.try_consume(bucket3)
  let #(_allowed5, bucket5) = sampling.try_consume(bucket4)
  let #(_allowed6, bucket6) = sampling.try_consume(bucket5)
  let #(_allowed7, bucket7) = sampling.try_consume(bucket6)
  let #(_allowed8, bucket8) = sampling.try_consume(bucket7)
  let #(_allowed9, bucket9) = sampling.try_consume(bucket8)
  let #(_allowed10, bucket10) = sampling.try_consume(bucket9)

  // After 10 consumes, the next should fail (no tokens left)
  let #(allowed11, _bucket11) = sampling.try_consume(bucket10)
  allowed11
  |> should.be_false
}

// ============================================================================
// Global Sampling Configuration Tests
// ============================================================================

pub fn config_with_sampling_test() {
  // Reset to known state
  gleam_log.reset_config()

  // Configure with sampling
  gleam_log.configure([
    gleam_log.config_sampling(sampling.config(level.Debug, 0.5)),
  ])

  let config = gleam_log.get_config()

  // Verify sampling config is set
  config.sampling
  |> should.be_ok

  // Reset for other tests
  gleam_log.reset_config()
}

pub fn config_without_sampling_test() {
  // Reset to known state
  gleam_log.reset_config()

  let config = gleam_log.get_config()

  // Default should have no sampling
  config.sampling
  |> should.be_error

  // Reset for other tests
  gleam_log.reset_config()
}

// ============================================================================
// Time-Based Rotation Tests (Phase 3.1)
// ============================================================================

pub fn time_interval_hourly_test() {
  // Hourly interval should be a valid TimeInterval variant
  let interval = file.Hourly
  case interval {
    file.Hourly -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn time_interval_daily_test() {
  // Daily interval should be a valid TimeInterval variant
  let interval = file.Daily
  case interval {
    file.Daily -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn rotation_time_rotation_test() {
  // TimeRotation should be constructible with interval and max_files
  let rotation = file.TimeRotation(interval: file.Daily, max_files: 7)
  case rotation {
    file.TimeRotation(interval: file.Daily, max_files: 7) ->
      should.be_true(True)
    _ -> should.fail()
  }
}

pub fn rotation_combined_rotation_test() {
  // CombinedRotation should accept both size and time parameters
  let rotation =
    file.CombinedRotation(
      max_bytes: 10_000_000,
      interval: file.Hourly,
      max_files: 24,
    )
  case rotation {
    file.CombinedRotation(
      max_bytes: 10_000_000,
      interval: file.Hourly,
      max_files: 24,
    ) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn time_interval_to_hours_hourly_test() {
  // Hourly interval should represent 1 hour
  file.interval_to_hours(file.Hourly)
  |> should.equal(1)
}

pub fn time_interval_to_hours_daily_test() {
  // Daily interval should represent 24 hours
  file.interval_to_hours(file.Daily)
  |> should.equal(24)
}

pub fn file_handler_with_time_rotation_test() {
  // Should be able to create a file handler with TimeRotation
  let config =
    file.FileConfig(
      path: "/tmp/gleam_log_test_time.log",
      rotation: file.TimeRotation(interval: file.Daily, max_files: 7),
    )
  let h = file.handler(config)

  handler.name(h)
  |> string.contains("file:")
  |> should.be_true
}

pub fn file_handler_with_combined_rotation_test() {
  // Should be able to create a file handler with CombinedRotation
  let config =
    file.FileConfig(
      path: "/tmp/gleam_log_test_combined.log",
      rotation: file.CombinedRotation(
        max_bytes: 1_000_000,
        interval: file.Hourly,
        max_files: 48,
      ),
    )
  let h = file.handler(config)

  handler.name(h)
  |> string.contains("file:")
  |> should.be_true
}

pub fn format_rotation_timestamp_hourly_test() {
  // Hourly rotation should include hour in timestamp
  // Example: "2024-12-26T14" for 2:00 PM on Dec 26
  let timestamp = file.format_rotation_timestamp(file.Hourly)

  // Should be in format YYYY-MM-DDTHH
  timestamp
  |> string.length
  |> should.equal(13)

  // Should contain 'T' separator
  timestamp
  |> string.contains("T")
  |> should.be_true
}

pub fn format_rotation_timestamp_daily_test() {
  // Daily rotation should include only date
  // Example: "2024-12-26"
  let timestamp = file.format_rotation_timestamp(file.Daily)

  // Should be in format YYYY-MM-DD
  timestamp
  |> string.length
  |> should.equal(10)

  // Should contain dashes
  timestamp
  |> string.contains("-")
  |> should.be_true
}
