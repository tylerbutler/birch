//// Tests for Erlang :logger integration.
////
//// These tests cover the Erlang :logger backend. Some tests are
//// platform-specific and will behave differently on Erlang vs JavaScript:
////
//// - **Level conversion tests**: Pure Gleam, work on both platforms
//// - **Forward handler tests**: Work on both (uses console.log on JS)
//// - **Formatter install/remove tests**: Only succeed on Erlang, expect errors on JS

import birch as log
import birch/erlang_logger
import birch/formatter
import birch/handler
import birch/handler/console
import birch/level
import birch/logger
import birch/meta
import birch/record
import gleam/list
import gleeunit/should

// ============================================================================
// Forward to :logger Handler Tests
// (These work on both Erlang and JavaScript targets)
// ============================================================================

pub fn forward_to_beam_creates_handler_test() {
  // Creating a handler that forwards to :logger should work
  let h = erlang_logger.forward_to_beam()

  // The handler should have a descriptive name
  handler.name(h)
  |> should.equal("erlang:logger")
}

pub fn forward_to_beam_handler_can_handle_records_test() {
  // The forward handler should not crash when handling records
  let h = erlang_logger.forward_to_beam()

  let r =
    record.new(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test.erlang_logger",
      message: "Test message for :logger",
      metadata: [meta.string("key", "value")],
    )

  // Should not crash
  handler.handle(h, r)
}

pub fn forward_to_beam_with_all_levels_test() {
  // Test that all log levels can be forwarded without error
  let h = erlang_logger.forward_to_beam()

  let levels = [
    level.Trace,
    level.Debug,
    level.Info,
    level.Notice,
    level.Warn,
    level.Err,
    level.Critical,
    level.Alert,
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
// Install Formatter Tests
// (These tests are platform-specific - succeed on Erlang, error on JavaScript)
// ============================================================================

/// Helper to check if we're running on Erlang.
/// Uses the install result as a platform indicator.
fn is_erlang_target() -> Bool {
  // Try to install formatter - if it succeeds, we're on Erlang
  let result = erlang_logger.install_formatter()
  case result {
    Ok(_) -> {
      // Clean up and return true
      let _ = erlang_logger.remove_formatter()
      True
    }
    Error(_) -> False
  }
}

pub fn install_formatter_test() {
  // The install function should return Ok on Erlang, Error on JS
  let result = erlang_logger.install_formatter()

  case is_erlang_target() {
    True -> {
      // On Erlang, should succeed
      should.be_ok(result)
      // Clean up
      let _ = erlang_logger.remove_formatter()
      Nil
    }
    False -> {
      // On JavaScript, should return an error (feature not available)
      let _ = should.be_error(result)
      Nil
    }
  }
}

pub fn install_and_remove_formatter_test() {
  // Test install/remove cycle
  case is_erlang_target() {
    True -> {
      let install_result = erlang_logger.install_formatter()
      should.be_ok(install_result)

      let remove_result = erlang_logger.remove_formatter()
      should.be_ok(remove_result)
    }
    False -> {
      // On JavaScript, both should return errors
      let _ =
        erlang_logger.install_formatter()
        |> should.be_error

      let _ =
        erlang_logger.remove_formatter()
        |> should.be_error
      Nil
    }
  }
}

pub fn install_formatter_with_custom_format_test() {
  case is_erlang_target() {
    True -> {
      // On Erlang, should be able to install with a custom formatter
      let result = erlang_logger.install_formatter_with(formatter.simple)
      should.be_ok(result)

      // Clean up
      let _ = erlang_logger.remove_formatter()
      Nil
    }
    False -> {
      // On JavaScript, should return error
      let _ =
        erlang_logger.install_formatter_with(formatter.simple)
        |> should.be_error
      Nil
    }
  }
}

pub fn install_formatter_on_nonexistent_handler_test() {
  case is_erlang_target() {
    True -> {
      // Installing on a non-existent handler should fail
      let result =
        erlang_logger.install_formatter_on(
          "nonexistent_handler",
          formatter.human_readable,
        )
      let _ = should.be_error(result)
      Nil
    }
    False -> {
      // On JavaScript, always errors
      let _ =
        erlang_logger.install_formatter_on(
          "nonexistent_handler",
          formatter.human_readable,
        )
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

  erlang_logger.gleam_level_to_erlang(level.Notice)
  |> should.equal(erlang_logger.ErlangNotice)

  erlang_logger.gleam_level_to_erlang(level.Warn)
  |> should.equal(erlang_logger.ErlangWarning)

  erlang_logger.gleam_level_to_erlang(level.Err)
  |> should.equal(erlang_logger.ErlangError)

  erlang_logger.gleam_level_to_erlang(level.Critical)
  |> should.equal(erlang_logger.ErlangCritical)

  erlang_logger.gleam_level_to_erlang(level.Alert)
  |> should.equal(erlang_logger.ErlangAlert)

  erlang_logger.gleam_level_to_erlang(level.Fatal)
  |> should.equal(erlang_logger.ErlangEmergency)
}

pub fn erlang_level_to_gleam_level_test() {
  // Test that Erlang :logger levels convert correctly to Gleam levels (1:1 mapping)
  erlang_logger.erlang_level_to_gleam(erlang_logger.ErlangDebug)
  |> should.equal(level.Debug)

  erlang_logger.erlang_level_to_gleam(erlang_logger.ErlangInfo)
  |> should.equal(level.Info)

  erlang_logger.erlang_level_to_gleam(erlang_logger.ErlangNotice)
  |> should.equal(level.Notice)

  erlang_logger.erlang_level_to_gleam(erlang_logger.ErlangWarning)
  |> should.equal(level.Warn)

  erlang_logger.erlang_level_to_gleam(erlang_logger.ErlangError)
  |> should.equal(level.Err)

  erlang_logger.erlang_level_to_gleam(erlang_logger.ErlangCritical)
  |> should.equal(level.Critical)

  erlang_logger.erlang_level_to_gleam(erlang_logger.ErlangAlert)
  |> should.equal(level.Alert)

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
    |> logger.with_handler(erlang_logger.forward_to_beam())

  // Should be able to log without crashing
  logger.info(lgr, "Message forwarded to Erlang :logger", [
    meta.string("source", "birch"),
  ])
}

pub fn configure_with_forward_handler_test() {
  // Reset config first
  log.reset_config()

  // Configure birch to forward to Erlang :logger
  log.configure([
    log.config_handlers([erlang_logger.forward_to_beam()]),
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

// ============================================================================
// Formatter Setup Tests
// (Platform-specific - succeed on Erlang, error/no-op on JavaScript)
// ============================================================================

pub fn setup_configures_default_handler_test() {
  // setup() should succeed on Erlang, error on JS
  let result = erlang_logger.setup()

  case is_erlang_target() {
    True -> {
      should.be_ok(result)
    }
    False -> {
      let _ = should.be_error(result)
      Nil
    }
  }
}

pub fn setup_with_fancy_config_test() {
  // setup_with_config with fancy style should succeed on Erlang
  let result = erlang_logger.setup_with_config(console.default_fancy_config())

  case is_erlang_target() {
    True -> {
      should.be_ok(result)
      // Restore simple style for subsequent tests
      let _ = erlang_logger.setup()
      Nil
    }
    False -> {
      let _ = should.be_error(result)
      Nil
    }
  }
}

pub fn setup_idempotent_test() {
  // Calling setup twice should not error
  case is_erlang_target() {
    True -> {
      let result1 = erlang_logger.setup()
      should.be_ok(result1)

      let result2 = erlang_logger.setup()
      should.be_ok(result2)
    }
    False -> Nil
  }
}

pub fn ensure_formatter_configured_does_not_crash_test() {
  // ensure_formatter_configured should be safe to call multiple times
  erlang_logger.ensure_formatter_configured()
  erlang_logger.ensure_formatter_configured()
}

pub fn forward_to_beam_sends_structured_data_test() {
  // The forward_to_beam handler should not crash when sending structured data
  let h = erlang_logger.forward_to_beam()

  let r =
    record.new(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test.structured",
      message: "Structured forward test",
      metadata: [meta.string("status", "200"), meta.string("method", "GET")],
    )

  // Should not crash
  handler.handle(h, r)
}

pub fn forward_to_beam_with_caller_id_test() {
  // Test that caller_id is passed through without crashing
  let h = erlang_logger.forward_to_beam()

  let r =
    record.new(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Debug,
      logger_name: "test.caller_id",
      message: "With caller ID",
      metadata: [],
    )
    |> record.with_caller_id("<0.123.0>")

  // Should not crash
  handler.handle(h, r)
}

pub fn forward_to_beam_level_roundtrip_test() {
  // Verify that birch levels survive the round-trip through Erlang level mapping
  // birch level → Erlang level → birch level should preserve the level
  // (except Trace → Debug, since both map to ErlangDebug)
  let levels_with_expected = [
    // Trace maps to ErlangDebug which maps back to Debug (lossy)
    #(level.Trace, level.Debug),
    #(level.Debug, level.Debug),
    #(level.Info, level.Info),
    #(level.Notice, level.Notice),
    #(level.Warn, level.Warn),
    #(level.Err, level.Err),
    #(level.Critical, level.Critical),
    #(level.Alert, level.Alert),
    #(level.Fatal, level.Fatal),
  ]

  list.each(levels_with_expected, fn(pair) {
    let #(input_level, expected_level) = pair
    input_level
    |> erlang_logger.gleam_level_to_erlang
    |> erlang_logger.erlang_level_to_gleam
    |> should.equal(expected_level)
  })
}
