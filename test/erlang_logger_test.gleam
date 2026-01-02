//// Tests for Erlang :logger integration.
////
//// These tests cover the Erlang :logger backend. Some tests are
//// platform-specific and will behave differently on Erlang vs JavaScript:
////
//// - **Level conversion tests**: Pure Gleam, work on both platforms
//// - **Forward handler tests**: Work on both (uses console.log on JS)
//// - **Install/uninstall tests**: Only succeed on Erlang, expect errors on JS

import birch as log
import birch/erlang_logger
import birch/handler
import birch/level
import birch/logger
import birch/record
import gleam/list
import gleeunit/should

// ============================================================================
// Forward to :logger Handler Tests
// (These work on both Erlang and JavaScript targets)
// ============================================================================

pub fn forward_to_logger_creates_handler_test() {
  // Creating a handler that forwards to :logger should work
  let h = erlang_logger.forward_to_logger()

  // The handler should have a descriptive name
  handler.name(h)
  |> should.equal("erlang:logger")
}

pub fn forward_to_logger_handler_can_handle_records_test() {
  // The forward handler should not crash when handling records
  let h = erlang_logger.forward_to_logger()

  let r =
    record.new(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test.erlang_logger",
      message: "Test message for :logger",
      metadata: [#("key", "value")],
    )

  // Should not crash
  handler.handle(h, r)
}

pub fn forward_to_logger_with_all_levels_test() {
  // Test that all log levels can be forwarded without error
  let h = erlang_logger.forward_to_logger()

  let levels = [
    level.Trace,
    level.Debug,
    level.Info,
    level.Warn,
    level.Err,
    level.Fatal,
  ]

  list.each(levels, fn(lvl) {
    let r =
      record.new(
        timestamp: "2024-12-26T10:30:45.123Z",
        level: lvl,
        logger_name: "test.levels",
        message: "Testing level " <> level.to_string(lvl),
        metadata: [],
      )
    handler.handle(h, r)
  })
}

// ============================================================================
// Install :logger Handler Tests
// (These tests are platform-specific - succeed on Erlang, error on JavaScript)
// ============================================================================

/// Helper to check if we're running on Erlang.
/// Uses the install result as a platform indicator.
fn is_erlang_target() -> Bool {
  // Try to install a test handler - if it succeeds, we're on Erlang
  let result =
    erlang_logger.install_logger_handler_with_id("birch_platform_check")
  case result {
    Ok(_) -> {
      // Clean up and return true
      let _ =
        erlang_logger.uninstall_logger_handler_with_id("birch_platform_check")
      True
    }
    Error(_) -> False
  }
}

pub fn install_logger_handler_test() {
  // The install function should return Ok on Erlang, Error on JS
  let result = erlang_logger.install_logger_handler()

  case is_erlang_target() {
    True -> {
      // On Erlang, should succeed
      should.be_ok(result)
      // Clean up
      let _ = erlang_logger.uninstall_logger_handler()
      Nil
    }
    False -> {
      // On JavaScript, should return an error (feature not available)
      let _ = should.be_error(result)
      Nil
    }
  }
}

pub fn install_and_uninstall_logger_handler_test() {
  // Test install/uninstall cycle
  case is_erlang_target() {
    True -> {
      let install_result = erlang_logger.install_logger_handler()
      should.be_ok(install_result)

      let uninstall_result = erlang_logger.uninstall_logger_handler()
      should.be_ok(uninstall_result)
    }
    False -> {
      // On JavaScript, both should return errors
      let _ =
        erlang_logger.install_logger_handler()
        |> should.be_error

      let _ =
        erlang_logger.uninstall_logger_handler()
        |> should.be_error
      Nil
    }
  }
}

pub fn install_logger_handler_with_custom_id_test() {
  case is_erlang_target() {
    True -> {
      // On Erlang, should be able to install with custom ID
      let result =
        erlang_logger.install_logger_handler_with_id("birch_custom_test")
      should.be_ok(result)

      // Clean up
      let _ =
        erlang_logger.uninstall_logger_handler_with_id("birch_custom_test")
      Nil
    }
    False -> {
      // On JavaScript, should return error
      let _ =
        erlang_logger.install_logger_handler_with_id("birch_custom_test")
        |> should.be_error
      Nil
    }
  }
}

pub fn install_logger_handler_twice_fails_test() {
  case is_erlang_target() {
    True -> {
      // On Erlang, installing twice should fail the second time
      let result1 = erlang_logger.install_logger_handler()
      should.be_ok(result1)

      let result2 = erlang_logger.install_logger_handler()
      let _ = should.be_error(result2)

      // Clean up
      let _ = erlang_logger.uninstall_logger_handler()
      Nil
    }
    False -> {
      // On JavaScript, both installs fail with the same error
      let _ =
        erlang_logger.install_logger_handler()
        |> should.be_error

      let _ =
        erlang_logger.install_logger_handler()
        |> should.be_error
      Nil
    }
  }
}

// ============================================================================
// Level Conversion Tests
// (These are pure Gleam and work on both platforms)
// ============================================================================

pub fn gleam_level_to_erlang_level_test() {
  // Test that Gleam log levels convert correctly to Erlang :logger levels
  erlang_logger.gleam_level_to_erlang(level.Trace)
  |> should.equal(erlang_logger.ErlangDebug)

  erlang_logger.gleam_level_to_erlang(level.Debug)
  |> should.equal(erlang_logger.ErlangDebug)

  erlang_logger.gleam_level_to_erlang(level.Info)
  |> should.equal(erlang_logger.ErlangInfo)

  erlang_logger.gleam_level_to_erlang(level.Warn)
  |> should.equal(erlang_logger.ErlangWarning)

  erlang_logger.gleam_level_to_erlang(level.Err)
  |> should.equal(erlang_logger.ErlangError)

  erlang_logger.gleam_level_to_erlang(level.Fatal)
  |> should.equal(erlang_logger.ErlangEmergency)
}

pub fn erlang_level_to_gleam_level_test() {
  // Test that Erlang :logger levels convert correctly to Gleam levels
  erlang_logger.erlang_level_to_gleam(erlang_logger.ErlangDebug)
  |> should.equal(level.Debug)

  erlang_logger.erlang_level_to_gleam(erlang_logger.ErlangInfo)
  |> should.equal(level.Info)

  erlang_logger.erlang_level_to_gleam(erlang_logger.ErlangNotice)
  |> should.equal(level.Info)

  erlang_logger.erlang_level_to_gleam(erlang_logger.ErlangWarning)
  |> should.equal(level.Warn)

  erlang_logger.erlang_level_to_gleam(erlang_logger.ErlangError)
  |> should.equal(level.Err)

  erlang_logger.erlang_level_to_gleam(erlang_logger.ErlangCritical)
  |> should.equal(level.Fatal)

  erlang_logger.erlang_level_to_gleam(erlang_logger.ErlangAlert)
  |> should.equal(level.Fatal)

  erlang_logger.erlang_level_to_gleam(erlang_logger.ErlangEmergency)
  |> should.equal(level.Fatal)
}

// ============================================================================
// Integration with Logger Tests
// (These work on both platforms - use console on JavaScript)
// ============================================================================

pub fn logger_with_forward_handler_test() {
  // Create a logger that forwards to Erlang :logger
  let lgr =
    logger.new("test.erlang")
    |> logger.with_handler(erlang_logger.forward_to_logger())

  // Should be able to log without crashing
  logger.info(lgr, "Message forwarded to Erlang :logger", [
    #("source", "gleam_log"),
  ])
}

pub fn configure_with_forward_handler_test() {
  // Reset config first
  log.reset_config()

  // Configure gleam_log to forward to Erlang :logger
  log.configure([
    log.config_handlers([erlang_logger.forward_to_logger()]),
  ])

  // Verify the handler was added
  let config = log.get_config()
  config.handlers
  |> list.length
  |> should.equal(1)

  config.handlers
  |> list.first
  |> should.be_ok
  |> handler.name
  |> should.equal("erlang:logger")

  // Reset for other tests
  log.reset_config()
}
