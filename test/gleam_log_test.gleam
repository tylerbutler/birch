import gleam/list
import gleam/order
import gleam/string
import gleam_log
import gleam_log/formatter
import gleam_log/handler
import gleam_log/handler/json
import gleam_log/level
import gleam_log/logger
import gleam_log/record
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

  level.compare(level.Warn, level.Error)
  |> should.equal(order.Lt)

  level.compare(level.Error, level.Fatal)
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
      level: level.Error,
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

  logger.should_log(lgr, level.Error)
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
    |> handler.with_min_level(level.Error)

  handler.should_handle(h, level.Error)
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
    |> gleam_log.with_level(gleam_log.debug_level)

  logger.get_level(lgr)
  |> should.equal(level.Debug)
}

pub fn main_api_level_from_string_test() {
  gleam_log.level_from_string("debug")
  |> should.equal(Ok(gleam_log.debug_level))

  gleam_log.level_from_string("error")
  |> should.equal(Ok(gleam_log.error_level))
}

pub fn main_api_level_to_string_test() {
  gleam_log.level_to_string(gleam_log.info_level)
  |> should.equal("INFO")
}
