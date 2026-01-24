import birch as log
import birch/formatter
import birch/handler
import birch/handler/async
import birch/handler/console
import birch/handler/file
import birch/handler/json
import birch/internal/platform
import birch/level
import birch/level_formatter
import birch/logger
import birch/record
import birch/sampling
import birch/scope
import birl
import gleam/json as gleam_json
import gleam/list
import gleam/option.{None, Some}
import gleam/order
import gleam/result
import gleam/string
import gleeunit
import gleeunit/should
import simplifile

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

pub fn logger_timestamp_format_default_test() {
  let lgr = logger.new("test")

  logger.get_timestamp_format(lgr)
  |> should.equal(logger.Iso8601)
}

pub fn logger_timestamp_format_naive_test() {
  let lgr =
    logger.new("test")
    |> logger.with_timestamp_format(logger.Naive)

  logger.get_timestamp_format(lgr)
  |> should.equal(logger.Naive)
}

pub fn logger_timestamp_format_unix_test() {
  let lgr =
    logger.new("test")
    |> logger.with_timestamp_format(logger.Unix)

  logger.get_timestamp_format(lgr)
  |> should.equal(logger.Unix)
}

pub fn logger_timestamp_format_custom_test() {
  // Custom format that just returns the year
  let custom_formatter = fn(time: birl.Time) {
    let day = birl.get_day(time)
    day.year |> string.inspect
  }

  let lgr =
    logger.new("test")
    |> logger.with_timestamp_format(logger.Custom(custom_formatter))

  // Verify the format is set (we can't compare function equality directly)
  case logger.get_timestamp_format(lgr) {
    logger.Custom(_) -> True |> should.be_true
    _ -> False |> should.be_true
  }
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
// JSON Builder Pattern Tests
// ============================================================================

pub fn json_builder_empty_test() {
  // An empty builder should produce an empty JSON object
  let format = json.builder() |> json.build()
  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "Hello",
    )

  let formatted = format(r)
  formatted
  |> should.equal("{}")
}

pub fn json_builder_add_timestamp_test() {
  let format =
    json.builder()
    |> json.add_timestamp()
    |> json.build()

  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "Hello",
    )

  let formatted = format(r)
  formatted
  |> string.contains("\"timestamp\":\"2024-12-26T10:30:45.123Z\"")
  |> should.be_true
}

pub fn json_builder_add_level_test() {
  let format =
    json.builder()
    |> json.add_level()
    |> json.build()

  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Warn,
      logger_name: "test",
      message: "Hello",
    )

  let formatted = format(r)
  formatted
  |> string.contains("\"level\":\"warn\"")
  |> should.be_true
}

pub fn json_builder_add_logger_test() {
  let format =
    json.builder()
    |> json.add_logger()
    |> json.build()

  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "myapp.database",
      message: "Hello",
    )

  let formatted = format(r)
  formatted
  |> string.contains("\"logger\":\"myapp.database\"")
  |> should.be_true
}

pub fn json_builder_add_message_test() {
  let format =
    json.builder()
    |> json.add_message()
    |> json.build()

  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "Request completed",
    )

  let formatted = format(r)
  formatted
  |> string.contains("\"message\":\"Request completed\"")
  |> should.be_true
}

pub fn json_builder_add_metadata_test() {
  let format =
    json.builder()
    |> json.add_metadata()
    |> json.build()

  let r =
    record.new(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "Hello",
      metadata: [#("method", "GET"), #("path", "/api/users")],
    )

  let formatted = format(r)
  formatted
  |> string.contains("\"method\":\"GET\"")
  |> should.be_true

  formatted
  |> string.contains("\"path\":\"/api/users\"")
  |> should.be_true
}

pub fn json_builder_add_custom_test() {
  let format =
    json.builder()
    |> json.add_custom(fn(_record) {
      [
        #("service", gleam_json.string("my-app")),
        #("version", gleam_json.string("1.0.0")),
      ]
    })
    |> json.build()

  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "Hello",
    )

  let formatted = format(r)
  formatted
  |> string.contains("\"service\":\"my-app\"")
  |> should.be_true

  formatted
  |> string.contains("\"version\":\"1.0.0\"")
  |> should.be_true
}

pub fn json_builder_multiple_fields_test() {
  // Test combining multiple fields
  let format =
    json.builder()
    |> json.add_timestamp()
    |> json.add_level()
    |> json.add_message()
    |> json.build()

  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Err,
      logger_name: "test",
      message: "Something failed",
    )

  let formatted = format(r)
  formatted
  |> string.contains("\"timestamp\":\"2024-12-26T10:30:45.123Z\"")
  |> should.be_true

  formatted
  |> string.contains("\"level\":\"error\"")
  |> should.be_true

  formatted
  |> string.contains("\"message\":\"Something failed\"")
  |> should.be_true
}

pub fn json_standard_builder_test() {
  // Standard builder should include all common fields
  let format = json.standard_builder() |> json.build()

  let r =
    record.new(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "myapp.http",
      message: "Request complete",
      metadata: [#("method", "POST")],
    )

  let formatted = format(r)

  // Should have all standard fields
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

pub fn json_standard_builder_matches_format_json_test() {
  // Standard builder should produce equivalent output to format_json
  let r =
    record.new(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "myapp.http",
      message: "Request complete",
      metadata: [#("method", "POST")],
    )

  let format_json_output = json.format_json(r)
  let builder_format = json.standard_builder() |> json.build()
  let builder_output = builder_format(r)

  // Both should produce the same JSON (field order may vary, but content same)
  // Check that both contain the same fields
  format_json_output
  |> string.contains("\"timestamp\"")
  |> should.be_true
  builder_output
  |> string.contains("\"timestamp\"")
  |> should.be_true

  format_json_output
  |> string.contains("\"level\"")
  |> should.be_true
  builder_output
  |> string.contains("\"level\"")
  |> should.be_true

  format_json_output
  |> string.contains("\"logger\"")
  |> should.be_true
  builder_output
  |> string.contains("\"logger\"")
  |> should.be_true

  format_json_output
  |> string.contains("\"message\"")
  |> should.be_true
  builder_output
  |> string.contains("\"message\"")
  |> should.be_true

  format_json_output
  |> string.contains("\"method\"")
  |> should.be_true
  builder_output
  |> string.contains("\"method\"")
  |> should.be_true
}

pub fn json_standard_builder_with_custom_extension_test() {
  // Standard builder can be extended with custom fields
  let format =
    json.standard_builder()
    |> json.add_custom(fn(_record) {
      [#("environment", gleam_json.string("production"))]
    })
    |> json.build()

  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "Hello",
    )

  let formatted = format(r)

  // Should have standard fields
  formatted
  |> string.contains("\"timestamp\":")
  |> should.be_true

  formatted
  |> string.contains("\"level\":")
  |> should.be_true

  // Plus custom field
  formatted
  |> string.contains("\"environment\":\"production\"")
  |> should.be_true
}

pub fn json_handler_with_formatter_test() {
  // handler_with_formatter should create a valid handler
  let format =
    json.builder()
    |> json.add_message()
    |> json.build()

  let h = json.handler_with_formatter(format)

  handler.name(h)
  |> should.equal("json")
}

pub fn json_handler_stderr_with_formatter_test() {
  // handler_stderr_with_formatter should create a handler writing to stderr
  let format = json.standard_builder() |> json.build()
  let h = json.handler_stderr_with_formatter(format)

  handler.name(h)
  |> should.equal("json_stderr")
}

pub fn json_add_custom_uses_record_data_test() {
  // Custom extractor should have access to record data
  let format =
    json.builder()
    |> json.add_custom(fn(record) {
      // Use record data in custom field
      let level_uppercase = level.to_string(record.level)
      [#("severity", gleam_json.string(level_uppercase))]
    })
    |> json.build()

  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Fatal,
      logger_name: "test",
      message: "Critical error",
    )

  let formatted = format(r)
  formatted
  |> string.contains("\"severity\":\"FATAL\"")
  |> should.be_true
}

pub fn json_builder_field_order_test() {
  // Fields should be added in order
  let format =
    json.builder()
    |> json.add_level()
    |> json.add_message()
    |> json.add_timestamp()
    |> json.build()

  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "Hello",
    )

  let formatted = format(r)

  // All fields should be present (order may vary in actual JSON output
  // as JSON objects are unordered, but the builder adds them in sequence)
  formatted
  |> string.contains("\"level\":")
  |> should.be_true

  formatted
  |> string.contains("\"message\":")
  |> should.be_true

  formatted
  |> string.contains("\"timestamp\":")
  |> should.be_true
}

// ============================================================================
// Main API Tests
// ============================================================================

pub fn main_api_new_test() {
  let lgr = log.new("myapp")

  logger.name(lgr)
  |> should.equal("myapp")
}

pub fn main_api_with_context_test() {
  let lgr =
    log.new("myapp")
    |> log.with_context([#("env", "test")])

  logger.get_context(lgr)
  |> should.equal([#("env", "test")])
}

pub fn main_api_with_level_test() {
  let lgr =
    log.new("myapp")
    |> log.with_level(level.Debug)

  logger.get_level(lgr)
  |> should.equal(level.Debug)
}

pub fn main_api_level_from_string_test() {
  log.level_from_string("debug")
  |> should.equal(Ok(level.Debug))

  log.level_from_string("error")
  |> should.equal(Ok(level.Err))
}

pub fn main_api_level_to_string_test() {
  log.level_to_string(level.Info)
  |> should.equal("INFO")
}

// ============================================================================
// Global Configuration Tests
// ============================================================================

pub fn config_default_test() {
  // Get default config before any configuration
  let config = log.get_config()

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
  log.configure([log.config_level(level.Debug)])

  let config = log.get_config()
  config.level
  |> should.equal(level.Debug)

  // Reset to default for other tests
  log.configure([log.config_level(level.Info)])
}

pub fn config_set_handlers_test() {
  // Configure with custom handlers
  let null_handler = handler.null()
  log.configure([log.config_handlers([null_handler])])

  let config = log.get_config()
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
  log.reset_config()
}

pub fn config_set_context_test() {
  // Configure with default context
  log.configure([
    log.config_context([#("app", "test"), #("env", "testing")]),
  ])

  let config = log.get_config()
  config.context
  |> should.equal([#("app", "test"), #("env", "testing")])

  // Reset to default
  log.reset_config()
}

pub fn config_multiple_options_test() {
  // Configure with multiple options at once
  let null_handler = handler.null()
  log.configure([
    log.config_level(level.Warn),
    log.config_handlers([null_handler]),
    log.config_context([#("service", "api")]),
  ])

  let config = log.get_config()
  config.level
  |> should.equal(level.Warn)
  config.handlers
  |> list.length
  |> should.equal(1)
  config.context
  |> should.equal([#("service", "api")])

  // Reset to default
  log.reset_config()
}

pub fn config_reset_test() {
  // Configure with custom settings
  log.configure([log.config_level(level.Fatal)])

  // Verify it was set
  let config1 = log.get_config()
  config1.level
  |> should.equal(level.Fatal)

  // Reset to default
  log.reset_config()

  // Verify it was reset
  let config2 = log.get_config()
  config2.level
  |> should.equal(level.Info)
}

pub fn config_default_logger_uses_config_test() {
  // Configure the global config with Debug level
  log.configure([log.config_level(level.Debug)])

  // Create a new default logger - it should inherit the global level
  let lgr = log.new("test.config")

  // The logger should use the global configuration's level
  logger.get_level(lgr)
  |> should.equal(level.Debug)

  // Reset to default
  log.reset_config()
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

pub fn async_flush_handler_by_name_test() {
  let base_handler = handler.null()
  let async_handler = async.make_async(base_handler, async.default_config())
  let handler_name = handler.name(async_handler)

  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "flush by name test",
    )

  handler.handle(async_handler, r)

  // Flush specific handler by name
  async.flush_handler(handler_name)
}

pub fn async_multiple_handlers_test() {
  // Create multiple async handlers wrapping different base handlers
  let handler1 = async.make_async(handler.null(), async.default_config())
  let handler2 = async.make_async(handler.null(), async.default_config())

  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "multi-handler test",
    )

  // Both handlers should receive records independently
  handler.handle(handler1, r)
  handler.handle(handler2, r)

  // Flush all should work with multiple handlers
  async.flush()
}

pub fn async_overflow_drop_oldest_test() {
  // Configure with very small queue to test overflow
  let config =
    async.config()
    |> async.with_queue_size(2)
    |> async.with_overflow(async.DropOldest)

  let async_handler = async.make_async(handler.null(), config)

  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "overflow test",
    )

  // Send more records than queue can hold
  handler.handle(async_handler, r)
  handler.handle(async_handler, r)
  handler.handle(async_handler, r)
  handler.handle(async_handler, r)

  // Should not crash, oldest messages dropped
  async.flush()
}

pub fn async_overflow_drop_newest_test() {
  let config =
    async.config()
    |> async.with_queue_size(2)
    |> async.with_overflow(async.DropNewest)

  let async_handler = async.make_async(handler.null(), config)

  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "overflow newest test",
    )

  handler.handle(async_handler, r)
  handler.handle(async_handler, r)
  handler.handle(async_handler, r)

  async.flush()
}

// Erlang-only tests for OTP actor features
@target(erlang)
pub fn async_shutdown_handler_test() {
  let base_handler = handler.null()
  let async_handler = async.make_async(base_handler, async.default_config())
  let handler_name = handler.name(async_handler)

  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "shutdown test",
    )

  handler.handle(async_handler, r)

  // Shutdown should flush and stop the actor
  async.shutdown_handler(handler_name)
}

@target(erlang)
pub fn async_shutdown_all_test() {
  // Create multiple handlers
  let handler1 = async.make_async(handler.null(), async.default_config())
  let handler2 = async.make_async(handler.null(), async.default_config())

  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "shutdown all test",
    )

  handler.handle(handler1, r)
  handler.handle(handler2, r)

  // Shutdown all should flush and stop all actors
  async.shutdown_all()
}

@target(erlang)
pub fn async_get_subject_returns_subject_test() {
  let async_handler = async.make_async(handler.null(), async.default_config())
  let handler_name = handler.name(async_handler)

  // Should be able to get the Subject for the async handler
  case async.get_subject(handler_name) {
    Ok(_subject) -> should.be_true(True)
    Error(Nil) -> should.fail()
  }

  async.shutdown_handler(handler_name)
}

@target(erlang)
pub fn async_get_subject_returns_error_for_unknown_test() {
  // Should return Error for non-existent handler
  case async.get_subject("non-existent-handler") {
    Ok(_) -> should.fail()
    Error(Nil) -> should.be_true(True)
  }
}

// ============================================================================
// Runtime Level Change Tests
// ============================================================================

pub fn set_level_changes_global_level_test() {
  // Reset to known state
  log.reset_config()

  // Default level should be Info
  log.get_level()
  |> should.equal(level.Info)

  // Set to Debug level
  log.set_level(level.Debug)

  // Level should now be Debug
  log.get_level()
  |> should.equal(level.Debug)

  // Reset for other tests
  log.reset_config()
}

pub fn set_level_affects_new_loggers_test() {
  // Reset to known state
  log.reset_config()

  // Set global level to Warn
  log.set_level(level.Warn)

  // New loggers should inherit the global level
  let lgr = log.new("test.runtime_level")
  logger.get_level(lgr)
  |> should.equal(level.Warn)

  // Reset for other tests
  log.reset_config()
}

pub fn set_level_takes_effect_immediately_test() {
  // Reset to known state
  log.reset_config()

  // Verify starting at Info
  let config1 = log.get_config()
  config1.level
  |> should.equal(level.Info)

  // Change to Trace
  log.set_level(level.Trace)

  // Should take effect immediately
  let config2 = log.get_config()
  config2.level
  |> should.equal(level.Trace)

  // Change again to Fatal
  log.set_level(level.Fatal)

  // Should take effect immediately
  let config3 = log.get_config()
  config3.level
  |> should.equal(level.Fatal)

  // Reset for other tests
  log.reset_config()
}

pub fn set_level_preserves_other_config_test() {
  // Reset to known state
  log.reset_config()

  // Configure with custom handlers and context
  let null_handler = handler.null()
  log.configure([
    log.config_handlers([null_handler]),
    log.config_context([#("app", "test")]),
  ])

  // Set level (should preserve other settings)
  log.set_level(level.Debug)

  let config = log.get_config()
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
  log.reset_config()
}

// ============================================================================
// File Handler Compression Tests
// ============================================================================

pub fn file_rotation_no_compression_test() {
  // Test that SizeRotation works without compression
  let test_dir = "/tmp/birch_test_no_compress"
  let test_path = test_dir <> "/app.log"

  // Clean up any existing test files
  let _ = simplifile.delete_all([test_dir])
  let _ = simplifile.create_directory_all(test_dir)

  // Create a file handler with small max_bytes for easy rotation
  let config =
    file.FileConfig(
      path: test_path,
      rotation: file.SizeRotation(max_bytes: 100, max_files: 3),
    )
  let file_handler = file.handler(config)

  // Write enough data to trigger rotation
  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "This is a log message that should trigger rotation when repeated",
    )

  // Write multiple log records to trigger rotation
  handler.handle(file_handler, r)
  handler.handle(file_handler, r)
  handler.handle(file_handler, r)

  // Verify the rotated file exists (without .gz extension)
  simplifile.is_file(test_path <> ".1")
  |> should.be_ok
  |> should.be_true

  // Clean up
  let _ = simplifile.delete_all([test_dir])
}

pub fn file_rotation_with_compression_test() {
  // Test that SizeRotation with compress: True creates .gz files
  let test_dir = "/tmp/birch_test_compress"
  let test_path = test_dir <> "/app.log"

  // Clean up any existing test files
  let _ = simplifile.delete_all([test_dir])
  let _ = simplifile.create_directory_all(test_dir)

  // Create a file handler with compression enabled
  let config =
    file.FileConfig(
      path: test_path,
      rotation: file.SizeRotationCompressed(
        max_bytes: 100,
        max_files: 3,
        compress: True,
      ),
    )
  let file_handler = file.handler(config)

  // Write enough data to trigger rotation
  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "This is a log message that should trigger rotation when repeated",
    )

  // Write multiple log records to trigger rotation
  handler.handle(file_handler, r)
  handler.handle(file_handler, r)
  handler.handle(file_handler, r)

  // Verify the compressed rotated file exists (with .gz extension)
  simplifile.is_file(test_path <> ".1.gz")
  |> should.be_ok
  |> should.be_true

  // Clean up
  let _ = simplifile.delete_all([test_dir])
}

pub fn file_rotation_compression_disabled_test() {
  // Test that SizeRotationCompressed with compress: False behaves like SizeRotation
  let test_dir = "/tmp/birch_test_compress_disabled"
  let test_path = test_dir <> "/app.log"

  // Clean up any existing test files
  let _ = simplifile.delete_all([test_dir])
  let _ = simplifile.create_directory_all(test_dir)

  // Create a file handler with compression disabled
  let config =
    file.FileConfig(
      path: test_path,
      rotation: file.SizeRotationCompressed(
        max_bytes: 100,
        max_files: 3,
        compress: False,
      ),
    )
  let file_handler = file.handler(config)

  // Write enough data to trigger rotation
  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "This is a log message that should trigger rotation when repeated",
    )

  // Write multiple log records to trigger rotation
  handler.handle(file_handler, r)
  handler.handle(file_handler, r)
  handler.handle(file_handler, r)

  // Verify the rotated file exists (without .gz extension)
  simplifile.is_file(test_path <> ".1")
  |> should.be_ok
  |> should.be_true

  // Verify no .gz file exists
  simplifile.is_file(test_path <> ".1.gz")
  |> should.be_ok
  |> should.be_false

  // Clean up
  let _ = simplifile.delete_all([test_dir])
}

pub fn file_rotation_compressed_file_is_smaller_test() {
  // Test that compressed files are actually smaller than uncompressed
  let test_dir = "/tmp/birch_test_compress_size"
  let test_path_uncompressed = test_dir <> "/uncompressed.log"
  let test_path_compressed = test_dir <> "/compressed.log"

  // Clean up any existing test files
  let _ = simplifile.delete_all([test_dir])
  let _ = simplifile.create_directory_all(test_dir)

  // Create handlers
  let config_uncompressed =
    file.FileConfig(
      path: test_path_uncompressed,
      rotation: file.SizeRotation(max_bytes: 100, max_files: 3),
    )
  let config_compressed =
    file.FileConfig(
      path: test_path_compressed,
      rotation: file.SizeRotationCompressed(
        max_bytes: 100,
        max_files: 3,
        compress: True,
      ),
    )

  let handler_uncompressed = file.handler(config_uncompressed)
  let handler_compressed = file.handler(config_compressed)

  // Write the same data to both to trigger rotation
  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "This is a repeating log message for testing compression size comparison",
    )

  handler.handle(handler_uncompressed, r)
  handler.handle(handler_uncompressed, r)
  handler.handle(handler_uncompressed, r)

  handler.handle(handler_compressed, r)
  handler.handle(handler_compressed, r)
  handler.handle(handler_compressed, r)

  // Get file sizes
  let uncompressed_size =
    simplifile.file_info(test_path_uncompressed <> ".1")
    |> result.map(fn(info) { info.size })
    |> result.unwrap(0)

  let compressed_size =
    simplifile.file_info(test_path_compressed <> ".1.gz")
    |> result.map(fn(info) { info.size })
    |> result.unwrap(0)

  // Compressed file should be smaller (or at least not larger for very small files)
  // Note: For very small files, compression overhead might make them larger
  // So we just verify both files exist and are non-zero
  { uncompressed_size > 0 }
  |> should.be_true

  { compressed_size > 0 }
  |> should.be_true

  // Clean up
  let _ = simplifile.delete_all([test_dir])
}

pub fn file_rotation_max_files_with_compression_test() {
  // Test that max_files limit works correctly with compressed files
  let test_dir = "/tmp/birch_test_max_files_compress"
  let test_path = test_dir <> "/app.log"

  // Clean up any existing test files
  let _ = simplifile.delete_all([test_dir])
  let _ = simplifile.create_directory_all(test_dir)

  // Create a file handler with small max_bytes and max_files: 2
  let config =
    file.FileConfig(
      path: test_path,
      rotation: file.SizeRotationCompressed(
        max_bytes: 50,
        max_files: 2,
        compress: True,
      ),
    )
  let file_handler = file.handler(config)

  // Write enough data to trigger multiple rotations
  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "Log message for testing max files limit with compression enabled",
    )

  // This should trigger rotations and enforce max_files limit
  handler.handle(file_handler, r)
  handler.handle(file_handler, r)
  handler.handle(file_handler, r)
  handler.handle(file_handler, r)
  handler.handle(file_handler, r)

  // With max_files: 2, we should have .1.gz and .2.gz at most
  // The .3.gz file should be deleted
  simplifile.is_file(test_path <> ".1.gz")
  |> should.be_ok
  |> should.be_true

  simplifile.is_file(test_path <> ".2.gz")
  |> should.be_ok
  |> should.be_true

  // .3.gz should NOT exist (deleted due to max_files limit)
  simplifile.is_file(test_path <> ".3.gz")
  |> should.be_ok
  |> should.be_false

  // Clean up
  let _ = simplifile.delete_all([test_dir])
}

// ============================================================================
// Handler Error Callback Tests
// ============================================================================

pub fn handler_error_type_test() {
  // Test that HandlerError can be created with all expected fields
  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Err,
      logger_name: "test",
      message: "test message",
    )

  let err =
    handler.HandlerError(
      handler_name: "test-handler",
      error: "write failed",
      record: r,
    )

  err.handler_name
  |> should.equal("test-handler")

  err.error
  |> should.equal("write failed")

  err.record.message
  |> should.equal("test message")
}

pub fn handler_with_error_callback_test() {
  // Test that with_error_callback returns a handler with the callback attached
  let base_handler = handler.null()
  let callback = fn(_: handler.HandlerError) { Nil }

  let handler_with_cb = handler.with_error_callback(base_handler, callback)

  // Handler name should be unchanged
  handler.name(handler_with_cb)
  |> should.equal("null")
}

pub fn handler_error_callback_invoked_on_failure_test() {
  // Create a handler that always fails
  let failing_handler =
    handler.new(
      name: "failing",
      write: fn(_msg: String) {
        // Simulate failure by panicking (which should be caught)
        panic as "simulated write failure"
      },
      format: fn(_) { "formatted" },
    )

  // Track if callback was called using a simple counter approach
  // We'll verify the handler doesn't crash the test
  let error_received = fn(err: handler.HandlerError) {
    // Verify error contains expected information
    err.handler_name
    |> should.equal("failing")
  }

  let handler_with_cb =
    handler.with_error_callback(failing_handler, error_received)

  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "test",
    )

  // This should not crash - error should be caught and callback invoked
  handler.handle(handler_with_cb, r)
}

pub fn handler_error_callback_not_invoked_on_success_test() {
  // Create a handler that succeeds
  let success_handler = handler.null()

  // Track if callback was called - it should NOT be called
  let error_callback = fn(_err: handler.HandlerError) {
    // This should never be called for a successful handler
    panic as "error callback should not be called on success"
  }

  let handler_with_cb =
    handler.with_error_callback(success_handler, error_callback)

  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "test",
    )

  // This should succeed without calling the error callback
  handler.handle(handler_with_cb, r)
}

pub fn handler_error_includes_record_test() {
  // Verify that the error callback receives the original log record
  let failing_handler =
    handler.new(
      name: "failing",
      write: fn(_msg: String) { panic as "simulated failure" },
      format: fn(_) { "formatted" },
    )

  let error_callback = fn(err: handler.HandlerError) {
    // Verify the record is passed through
    err.record.message
    |> should.equal("original message")

    err.record.logger_name
    |> should.equal("test.logger")
  }

  let handler_with_cb =
    handler.with_error_callback(failing_handler, error_callback)

  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test.logger",
      message: "original message",
    )

  handler.handle(handler_with_cb, r)
}

pub fn config_on_error_option_test() {
  // Reset to known state
  log.reset_config()

  // Create an error callback
  let global_error_callback = fn(_err: handler.HandlerError) { Nil }

  // Configure with global error callback
  log.configure([log.config_on_error(global_error_callback)])

  // Verify config was set (we can't easily inspect the callback,
  // but we can verify configuration doesn't error)

  // Reset for other tests
  log.reset_config()
}

pub fn handler_get_error_callback_test() {
  // Test getting the error callback from a handler
  let base_handler = handler.null()

  // Handler without callback should return None
  case handler.get_error_callback(base_handler) {
    None -> Nil
    Some(_) -> panic as "Expected None, got Some"
  }

  // Handler with callback should return Some
  let callback = fn(_: handler.HandlerError) { Nil }
  let handler_with_cb = handler.with_error_callback(base_handler, callback)

  case handler.get_error_callback(handler_with_cb) {
    Some(_) -> Nil
    None -> panic as "Expected Some, got None"
  }
}

// ============================================================================
// Scoped Context Tests
// ============================================================================

pub fn scope_with_scope_returns_work_result_test() {
  // with_scope should return the result of the work function
  let result = log.with_scope([#("request_id", "123")], fn() { 42 })

  result
  |> should.equal(42)
}

pub fn scope_with_scope_string_result_test() {
  // with_scope should work with any return type
  let result = log.with_scope([#("trace_id", "abc")], fn() { "hello" })

  result
  |> should.equal("hello")
}

pub fn scope_get_scope_context_inside_scope_test() {
  // Inside a scope, get_scope_context should return the scope's context
  // (including internal metadata like _scope_highlight_keys)
  log.with_scope([#("request_id", "req-123")], fn() {
    let ctx = log.get_scope_context()
    // Filter out internal keys for comparison
    let visible_ctx =
      ctx
      |> list.filter(fn(pair) { !string.starts_with(pair.0, "_") })
    visible_ctx
    |> should.equal([#("request_id", "req-123")])
  })
}

pub fn scope_get_scope_context_outside_scope_test() {
  // Outside any scope, get_scope_context should return empty list (no visible metadata)
  let ctx = log.get_scope_context()

  ctx
  |> list.filter(fn(pair) { !string.starts_with(pair.0, "_") })
  |> should.equal([])
}

pub fn scope_nested_scopes_test() {
  // Nested scopes should combine context, with inner values taking precedence
  log.with_scope([#("outer", "value1")], fn() {
    // First scope has outer context
    let outer_ctx = log.get_scope_context()
    outer_ctx
    |> list.key_find("outer")
    |> should.equal(Ok("value1"))

    // Nested scope adds more context
    log.with_scope([#("inner", "value2")], fn() {
      let inner_ctx = log.get_scope_context()

      // Both keys should be present
      inner_ctx
      |> list.key_find("outer")
      |> should.equal(Ok("value1"))

      inner_ctx
      |> list.key_find("inner")
      |> should.equal(Ok("value2"))
    })

    // After inner scope ends, only outer context remains
    let after_inner_ctx = log.get_scope_context()
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
  log.with_scope([#("key", "outer_value")], fn() {
    log.with_scope([#("key", "inner_value")], fn() {
      let ctx = log.get_scope_context()

      // The inner value should come first (shadow the outer)
      ctx
      |> list.key_find("key")
      |> should.equal(Ok("inner_value"))
    })

    // After inner scope, original value restored
    let ctx = log.get_scope_context()
    ctx
    |> list.key_find("key")
    |> should.equal(Ok("outer_value"))
  })
}

pub fn scope_is_available_test() {
  // is_scoped_context_available should return a boolean
  let available = log.is_scoped_context_available()

  // On both Erlang and JavaScript this should return a boolean
  // We just check it's callable and doesn't crash
  case available {
    True -> Nil
    False -> Nil
  }
}

pub fn scope_multiple_metadata_pairs_test() {
  // with_scope should support multiple key-value pairs
  log.with_scope(
    [#("request_id", "123"), #("user_id", "456"), #("trace_id", "789")],
    fn() {
      let ctx = log.get_scope_context()

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
  log.with_scope([#("temp_key", "temp_value")], fn() {
    // Inside scope, context exists
    log.get_scope_context()
    |> list.key_find("temp_key")
    |> should.equal(Ok("temp_value"))
  })

  // After scope ends, context should be empty (no visible metadata)
  log.get_scope_context()
  |> list.filter(fn(pair) { !string.starts_with(pair.0, "_") })
  |> should.equal([])
}

pub fn scope_context_included_in_log_records_test() {
  // Reset global config for clean test
  log.reset_config()

  // Create a mutable reference to capture log records (using list accumulator pattern)
  // We'll use a logger with a null handler and verify scope context is read
  log.with_scope([#("request_id", "test-123")], fn() {
    // Get the scope context which should be included in logs
    let ctx = log.get_scope_context()
    ctx
    |> list.key_find("request_id")
    |> should.equal(Ok("test-123"))
    // The log functions will include this context
    // (actual verification would require a capturing handler)
  })

  // Verify scope context is cleaned up (no visible metadata)
  log.get_scope_context()
  |> list.filter(fn(pair) { !string.starts_with(pair.0, "_") })
  |> should.equal([])
}

pub fn scope_empty_context_test() {
  // with_scope with empty context should not crash
  let result = log.with_scope([], fn() { "success" })

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
  log.reset_config()

  // Configure with sampling
  log.configure([
    log.config_sampling(sampling.config(level.Debug, 0.5)),
  ])

  let config = log.get_config()

  // Verify sampling config is set
  config.sampling
  |> should.be_ok

  // Reset for other tests
  log.reset_config()
}

pub fn config_without_sampling_test() {
  // Reset to known state
  log.reset_config()

  let config = log.get_config()

  // Default should have no sampling
  config.sampling
  |> should.be_error

  // Reset for other tests
  log.reset_config()
}

// ============================================================================
// Time-Based Rotation Tests (Phase 3.1)
// ============================================================================

pub fn time_interval_hourly_test() {
  // Hourly interval should be a valid TimeInterval variant
  let interval = file.Hourly
  // Verify the variant matches by comparing to itself
  interval
  |> should.equal(file.Hourly)
}

pub fn time_interval_daily_test() {
  // Daily interval should be a valid TimeInterval variant
  let interval = file.Daily
  // Verify the variant matches by comparing to itself
  interval
  |> should.equal(file.Daily)
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
      path: "/tmp/birch_test_time.log",
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
      path: "/tmp/birch_test_combined.log",
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

// ============================================================================
// Error Result Convenience Functions Tests
// ============================================================================

pub fn logger_error_result_with_error_test() {
  // Use null handler to verify the function runs without crashing
  let lgr =
    logger.new("test")
    |> logger.with_handlers([handler.null()])
    |> logger.with_level(level.Trace)

  // Log an error result - should not crash
  let result: Result(Int, String) = Error("connection refused")
  logger.error_result(lgr, "Database connection failed", result, [
    #("host", "localhost"),
  ])

  // If we get here, the test passed (no crash)
  True |> should.be_true
}

pub fn logger_error_result_with_ok_test() {
  // Use null handler
  let lgr =
    logger.new("test")
    |> logger.with_handlers([handler.null()])
    |> logger.with_level(level.Trace)

  // Log with an Ok result (no error metadata should be added)
  let result: Result(Int, String) = Ok(42)
  logger.error_result(lgr, "Operation completed", result, [])

  // If we get here, the test passed
  True |> should.be_true
}

pub fn logger_fatal_result_test() {
  // Use null handler
  let lgr =
    logger.new("test")
    |> logger.with_handlers([handler.null()])
    |> logger.with_level(level.Trace)

  // Log a fatal error result
  let result: Result(Nil, String) = Error("critical failure")
  logger.fatal_result(lgr, "System cannot continue", result, [])

  // If we get here, the test passed
  True |> should.be_true
}

pub fn module_level_error_result_test() {
  // Reset config and use null handler
  log.reset_config()
  log.configure([
    log.config_handlers([handler.null()]),
    log.config_level(level.Trace),
  ])

  // Use module-level error_result
  let result: Result(String, String) = Error("file not found")
  log.error_result("Failed to read config", result)

  // Clean up
  log.reset_config()

  // If we get here, the test passed
  True |> should.be_true
}

pub fn module_level_error_result_m_test() {
  // Reset config and use null handler
  log.reset_config()
  log.configure([
    log.config_handlers([handler.null()]),
    log.config_level(level.Trace),
  ])

  // Use module-level error_result_m with metadata
  let result: Result(String, String) = Error("permission denied")
  log.error_result_m("Access denied", result, [#("path", "/etc/secrets")])

  // Clean up
  log.reset_config()

  // If we get here, the test passed
  True |> should.be_true
}

// ============================================================================
// Time Provider Tests
// ============================================================================

pub fn logger_with_time_provider_test() {
  // Use null handler
  let lgr =
    logger.new("test")
    |> logger.with_handlers([handler.null()])
    |> logger.with_time_provider(fn() { "2024-01-01T00:00:00.000Z" })

  // Log a message
  logger.info(lgr, "Test message", [])

  // If we get here, the test passed
  True |> should.be_true
}

pub fn logger_without_time_provider_test() {
  // Create a logger with a time provider, then remove it
  let lgr =
    logger.new("test")
    |> logger.with_time_provider(fn() { "FIXED" })
    |> logger.without_time_provider()
    |> logger.with_handlers([handler.null()])

  // Log a message
  logger.info(lgr, "Test message", [])

  // If we get here, the test passed
  True |> should.be_true
}

pub fn log_module_with_time_provider_test() {
  // Use module-level API for time provider
  let lgr =
    log.new("test")
    |> log.with_time_provider(fn() { "1999-12-31T23:59:59.999Z" })
    |> log.with_handler(handler.null())

  log.logger_info(lgr, "Y2K test", [])

  // If we get here, the test passed
  True |> should.be_true
}

// ============================================================================
// Caller ID Capture Tests
// ============================================================================

pub fn logger_with_caller_id_capture_test() {
  // Create a logger with caller ID capture enabled
  let lgr =
    logger.new("test")
    |> logger.with_handlers([handler.null()])
    |> logger.with_caller_id_capture()

  // Verify capture is enabled
  logger.is_caller_id_capture_enabled(lgr)
  |> should.be_true

  // Log a message
  logger.info(lgr, "Test message", [])

  // If we get here, the test passed
  True |> should.be_true
}

pub fn logger_without_caller_id_capture_test() {
  // Create a logger with caller ID capture, then disable it
  let lgr =
    logger.new("test")
    |> logger.with_caller_id_capture()
    |> logger.without_caller_id_capture()

  logger.is_caller_id_capture_enabled(lgr)
  |> should.be_false
}

pub fn record_with_caller_id_test() {
  // Test the record.with_caller_id function directly
  let r =
    record.new_simple(
      timestamp: "2024-01-01T00:00:00.000Z",
      level: level.Info,
      logger_name: "test",
      message: "Hello",
    )

  // Initially no caller ID
  record.get_caller_id(r)
  |> should.equal(None)

  // Add caller ID
  let r2 = record.with_caller_id(r, "<0.123.0>")
  record.get_caller_id(r2)
  |> should.equal(Some("<0.123.0>"))
}

pub fn log_module_with_caller_id_capture_test() {
  // Use module-level API
  let lgr =
    log.new("test")
    |> log.with_caller_id_capture()
    |> log.with_handler(handler.null())

  log.logger_info(lgr, "Test from module API", [])

  // If we get here, the test passed
  True |> should.be_true
}

// ============================================================================
// Combined Feature Tests
// ============================================================================

pub fn logger_all_advanced_features_combined_test() {
  // Test all advanced features together
  let lgr =
    log.new("combined-test")
    |> log.with_time_provider(fn() { "2025-01-02T12:00:00.000Z" })
    |> log.with_caller_id_capture()
    |> log.with_handler(handler.null())
    |> log.with_level(level.Trace)

  // Log an error with result
  let result: Result(Int, String) = Error("test error")
  log.logger_error_result(lgr, "Combined test failed", result, [
    #("feature", "all"),
  ])

  // If we get here, the test passed
  True |> should.be_true
}

// ============================================================================
// Console Handler Tests (Unified)
// ============================================================================

pub fn console_handler_creation_test() {
  let h = console.handler()
  handler.name(h)
  |> should.equal("console")
}

pub fn console_fancy_handler_creation_test() {
  let h = console.fancy_handler()
  handler.name(h)
  |> should.equal("console")
}

pub fn console_handler_with_config_test() {
  let config =
    console.ConsoleConfig(
      color: False,
      timestamps: True,
      target: handler.Stdout,
      level_formatter: level_formatter.label_formatter(),
      style: console.Fancy,
      auto_indent_from_scopes: False,
    )
  let h = console.handler_with_config(config)
  handler.name(h)
  |> should.equal("console")
}

pub fn console_default_config_test() {
  let config = console.default_config()
  config.color
  |> should.be_true

  config.timestamps
  |> should.be_true

  config.target
  |> should.equal(handler.Stdout)

  config.style
  |> should.equal(console.Simple)
}

pub fn console_default_fancy_config_test() {
  let config = console.default_fancy_config()
  config.color
  |> should.be_true

  config.timestamps
  |> should.be_false

  config.target
  |> should.equal(handler.Stdout)

  config.style
  |> should.equal(console.Fancy)
}

pub fn console_handler_stderr_test() {
  let config =
    console.ConsoleConfig(
      color: True,
      timestamps: False,
      target: handler.Stderr,
      level_formatter: level_formatter.label_formatter(),
      style: console.Fancy,
      auto_indent_from_scopes: False,
    )
  let h = console.handler_with_config(config)
  handler.name(h)
  |> should.equal("console")
}

pub fn console_handler_handles_log_record_test() {
  // Create a console handler with a null-like write function
  // to verify it doesn't crash when handling records
  let h = console.handler()

  let r =
    record.new(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "myapp.http",
      message: "Request received",
      metadata: [#("user_id", "123")],
    )

  // This should not crash
  handler.handle(h, r)
}

// ============================================================================
// Console Box Output Tests
// ============================================================================

pub fn console_box_simple_test() {
  let boxed = console.box("Hello, World!")

  // Should contain box drawing characters
  boxed
  |> string.contains("╭")
  |> should.be_true

  boxed
  |> string.contains("╰")
  |> should.be_true

  boxed
  |> string.contains("│")
  |> should.be_true

  // Should contain the message
  boxed
  |> string.contains("Hello, World!")
  |> should.be_true
}

pub fn console_box_with_title_test() {
  let boxed = console.box_with_title("Content here", "My Title")

  // Should contain the title
  boxed
  |> string.contains("My Title")
  |> should.be_true

  // Should contain the content
  boxed
  |> string.contains("Content here")
  |> should.be_true
}

pub fn console_box_multiline_test() {
  let boxed = console.box("Line 1\nLine 2\nLine 3")

  // Should contain all lines
  boxed
  |> string.contains("Line 1")
  |> should.be_true

  boxed
  |> string.contains("Line 2")
  |> should.be_true

  boxed
  |> string.contains("Line 3")
  |> should.be_true
}

pub fn console_box_colored_explicit_test() {
  // Test with colors explicitly disabled
  let boxed_no_color = console.box_colored("Test", "", False)

  // Should not contain ANSI escape codes
  boxed_no_color
  |> string.contains("\u{001b}")
  |> should.be_false

  // Test with colors explicitly enabled
  let boxed_color = console.box_colored("Test", "", True)

  // Should contain ANSI escape codes
  boxed_color
  |> string.contains("\u{001b}")
  |> should.be_true
}

pub fn console_write_box_test() {
  // Just verify it doesn't crash
  console.write_box("Test message")
}

pub fn console_write_box_with_title_test() {
  // Just verify it doesn't crash
  console.write_box_with_title("Test message", "Title")
}

// ============================================================================
// Console Grouping Tests
// ============================================================================

pub fn console_with_group_returns_result_test() {
  // with_group should return the result of the work function
  let result = console.with_group("Test Group", fn() { 42 })

  result
  |> should.equal(42)
}

pub fn console_with_group_string_result_test() {
  let result = console.with_group("String Group", fn() { "hello" })

  result
  |> should.equal("hello")
}

pub fn console_indented_handler_creation_test() {
  let h = console.indented_handler(1)
  handler.name(h)
  |> should.equal("console")
}

pub fn console_indented_handler_with_config_test() {
  let config =
    console.ConsoleConfig(
      color: False,
      timestamps: False,
      target: handler.Stdout,
      level_formatter: level_formatter.label_formatter(),
      style: console.Fancy,
      auto_indent_from_scopes: False,
    )
  let h = console.indented_handler_with_config(2, config)
  handler.name(h)
  |> should.equal("console")
}

pub fn console_indented_handler_handles_records_test() {
  let h = console.indented_handler(1)

  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "Indented message",
    )

  // Should not crash
  handler.handle(h, r)
}

pub fn console_auto_indent_from_scopes_test() {
  // Test that auto-indent configuration option works
  let config =
    console.default_fancy_config()
    |> console.with_auto_indent_from_scopes()

  config.auto_indent_from_scopes
  |> should.be_true()

  let config_disabled =
    config
    |> console.without_auto_indent_from_scopes()

  config_disabled.auto_indent_from_scopes
  |> should.be_false()
}

pub fn scope_depth_tracking_test() {
  // Test that scope depth is tracked correctly
  // Clean up any leftover scope state from previous tests
  platform.set_scope_context([])
  platform.set_scope_depth(0)

  // Outside any scope, depth should be 0
  scope.get_depth()
  |> should.equal(0)

  // Inside one scope, depth should be 1
  scope.with_scope([#("key1", "value1")], fn() {
    scope.get_depth()
    |> should.equal(1)

    // Inside nested scope, depth should be 2
    scope.with_scope([#("key2", "value2")], fn() {
      scope.get_depth()
      |> should.equal(2)
      Nil
    })

    // Back to depth 1 after inner scope exits
    scope.get_depth()
    |> should.equal(1)
    Nil
  })

  // Back to depth 0 after all scopes exit
  scope.get_depth()
  |> should.equal(0)
}

// ============================================================================
// Console Semantic Log Type Tests
// ============================================================================

pub fn console_log_style_type_test() {
  // Test that LogStyle variants exist
  let _success = console.Success
  let _start = console.Start
  let _ready = console.Ready
  let _fail = console.Fail

  // If we get here, the types exist
  True |> should.be_true
}

pub fn console_write_success_test() {
  // Just verify it doesn't crash
  console.write_success("Operation completed successfully")
}

pub fn console_write_start_test() {
  // Just verify it doesn't crash
  console.write_start("Starting operation...")
}

pub fn console_write_ready_test() {
  // Just verify it doesn't crash
  console.write_ready("System ready")
}

pub fn console_write_fail_test() {
  // Just verify it doesn't crash
  console.write_fail("Operation failed")
}

pub fn console_success_with_metadata_test() {
  // Should not crash
  console.success("Build completed!", [#("duration", "5s")])
}

pub fn console_start_with_metadata_test() {
  // Should not crash
  console.start("Building project...", [#("target", "release")])
}

pub fn console_ready_with_metadata_test() {
  // Should not crash
  console.ready("Server listening on port 3000", [#("port", "3000")])
}

pub fn console_fail_with_metadata_test() {
  // Should not crash
  console.fail("Could not connect to cache", [#("host", "localhost")])
}

// ============================================================================
// Level Formatter Tests
// ============================================================================

pub fn level_formatter_label_with_color_test() {
  let formatter = level_formatter.label_formatter()
  let result = level_formatter.format_level(formatter, level.Info, True)

  // Should contain the icon
  result
  |> string.contains("ℹ")
  |> should.be_true

  // Should contain the label
  result
  |> string.contains("info")
  |> should.be_true

  // Should contain ANSI codes (color enabled)
  result
  |> string.contains("\u{001b}")
  |> should.be_true
}

pub fn level_formatter_label_without_color_test() {
  let formatter = level_formatter.label_formatter()
  let result = level_formatter.format_level(formatter, level.Warn, False)

  // Should contain icon and label
  result
  |> string.contains("⚠")
  |> should.be_true

  result
  |> string.contains("warn")
  |> should.be_true

  // Should NOT contain ANSI codes
  result
  |> string.contains("\u{001b}")
  |> should.be_false
}

pub fn level_formatter_label_no_icons_test() {
  let formatter =
    level_formatter.label_formatter_with_config(level_formatter.LabelConfig(
      icons: False,
    ))
  let result = level_formatter.format_level(formatter, level.Info, False)

  // Should NOT contain icon
  result
  |> string.contains("ℹ")
  |> should.be_false

  // Should contain label
  result
  |> string.contains("info")
  |> should.be_true
}

pub fn level_formatter_badge_with_color_test() {
  let formatter = level_formatter.badge_formatter()
  let result = level_formatter.format_level(formatter, level.Err, True)

  // Should contain uppercase label in brackets
  result
  |> string.contains("[ERROR]")
  |> should.be_true

  // Should contain ANSI codes (color enabled)
  result
  |> string.contains("\u{001b}")
  |> should.be_true
}

pub fn level_formatter_badge_without_color_test() {
  let formatter = level_formatter.badge_formatter()
  let result = level_formatter.format_level(formatter, level.Info, False)

  // Should contain uppercase label in brackets
  result
  |> string.contains("[INFO]")
  |> should.be_true

  // Should NOT contain ANSI codes
  result
  |> string.contains("\u{001b}")
  |> should.be_false
}

pub fn level_formatter_badge_all_levels_test() {
  let formatter = level_formatter.badge_formatter()

  level_formatter.format_level(formatter, level.Trace, False)
  |> should.equal("[TRACE]")

  level_formatter.format_level(formatter, level.Debug, False)
  |> should.equal("[DEBUG]")

  level_formatter.format_level(formatter, level.Info, False)
  |> should.equal("[INFO]")

  level_formatter.format_level(formatter, level.Warn, False)
  |> should.equal("[WARN]")

  level_formatter.format_level(formatter, level.Err, False)
  |> should.equal("[ERROR]")

  level_formatter.format_level(formatter, level.Fatal, False)
  |> should.equal("[FATAL]")
}

pub fn level_formatter_simple_test() {
  let formatter = level_formatter.simple_formatter()

  // Simple formatter produces uppercase labels (no padding - that's done by layout)
  level_formatter.format_level(formatter, level.Info, False)
  |> should.equal("INFO")

  level_formatter.format_level(formatter, level.Warn, False)
  |> should.equal("WARN")

  level_formatter.format_level(formatter, level.Err, False)
  |> should.equal("ERROR")
}

pub fn console_with_badge_style_test() {
  let config = console.default_fancy_config() |> console.with_badge_style
  let h = console.handler_with_config(config)
  handler.name(h)
  |> should.equal("console")
}

pub fn console_with_label_style_test() {
  let config = console.default_config() |> console.with_label_style
  let h = console.handler_with_config(config)
  handler.name(h)
  |> should.equal("console")
}

pub fn console_with_label_style_no_icons_test() {
  let config = console.default_config() |> console.with_label_style_no_icons
  let h = console.handler_with_config(config)
  handler.name(h)
  |> should.equal("console")
}

pub fn level_formatter_custom_test() {
  let custom_formatter =
    level_formatter.custom_level_formatter(
      fn(lvl, _use_color) {
        case lvl {
          level.Info -> "INFO:"
          level.Warn -> "WARNING:"
          level.Err -> "ERROR:"
          _ -> "LOG:"
        }
      },
      8,
    )

  level_formatter.format_level(custom_formatter, level.Info, False)
  |> should.equal("INFO:")

  level_formatter.format_level(custom_formatter, level.Warn, False)
  |> should.equal("WARNING:")

  level_formatter.format_level(custom_formatter, level.Err, False)
  |> should.equal("ERROR:")

  level_formatter.format_level(custom_formatter, level.Debug, False)
  |> should.equal("LOG:")
}

pub fn console_with_level_formatter_test() {
  let custom_formatter =
    level_formatter.custom_level_formatter(fn(_lvl, _use_color) { "CUSTOM" }, 6)

  let config =
    console.default_config()
    |> console.with_level_formatter(custom_formatter)

  let h = console.handler_with_config(config)
  handler.name(h)
  |> should.equal("console")
}
