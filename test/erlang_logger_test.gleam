//// Tests for Erlang :logger integration.
////
//// These tests cover the Erlang :logger backend. Some tests are
//// platform-specific and will behave differently on Erlang vs JavaScript:
////
//// - **Level conversion tests**: Pure Gleam, work on both platforms
//// - **Formatter install/remove tests**: Only succeed on Erlang, expect errors on JS
//// - **Round-trip tests**: Erlang-only, verify birch→:logger→birch formatting

import birch as log
import birch/erlang_logger
import birch/formatter
import birch/handler/console
import birch/level
import birch/logger
import birch/meta
import birch/record
import gleam/list
import gleam/string
import gleeunit/should

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
  let result = erlang_logger.install_formatter()

  case is_erlang_target() {
    True -> {
      should.be_ok(result)
      let _ = erlang_logger.remove_formatter()
      Nil
    }
    False -> {
      let _ = should.be_error(result)
      Nil
    }
  }
}

pub fn install_and_remove_formatter_test() {
  case is_erlang_target() {
    True -> {
      let install_result = erlang_logger.install_formatter()
      should.be_ok(install_result)

      let remove_result = erlang_logger.remove_formatter()
      should.be_ok(remove_result)
    }
    False -> {
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
      let result = erlang_logger.install_formatter_with(formatter.simple)
      should.be_ok(result)

      let _ = erlang_logger.remove_formatter()
      Nil
    }
    False -> {
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
      let result =
        erlang_logger.install_formatter_on(
          "nonexistent_handler",
          formatter.human_readable,
        )
      let _ = should.be_error(result)
      Nil
    }
    False -> {
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

pub fn forward_to_beam_level_roundtrip_test() {
  // Verify that birch levels survive the round-trip through Erlang level mapping
  // (except Trace → Debug, since both map to ErlangDebug)
  let levels_with_expected = [
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

// ============================================================================
// Integration Tests
// ============================================================================

pub fn logger_logs_without_crashing_test() {
  // Create a logger and log at various levels — should not crash
  let lgr = logger.new("test.erlang")

  logger.info(lgr, "Message through logger", [
    meta.string("source", "birch"),
  ])
}

pub fn configure_default_test() {
  // Reset config first
  log.reset_config()

  let config = log.get_config()

  case is_erlang_target() {
    True -> {
      // On BEAM: no birch handlers (birch sends to :logger directly)
      config.handlers
      |> list.length
      |> should.equal(0)
    }
    False -> {
      // On JS: one console handler
      config.handlers
      |> list.length
      |> should.equal(1)
    }
  }

  // Reset for other tests
  log.reset_config()
}

// ============================================================================
// Formatter Setup Tests
// ============================================================================

pub fn setup_configures_default_handler_test() {
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
  let result = erlang_logger.setup_with_config(console.default_fancy_config())

  case is_erlang_target() {
    True -> {
      should.be_ok(result)
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
  erlang_logger.ensure_formatter_configured()
  erlang_logger.ensure_formatter_configured()
}

pub fn emit_does_not_crash_test() {
  // Direct emit should work without crashing
  let r =
    record.new(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test.emit",
      message: "Direct emit test",
      metadata: [meta.string("key", "value")],
    )
  erlang_logger.emit(r)
}

pub fn emit_with_all_levels_test() {
  let levels = [
    level.Trace, level.Debug, level.Info, level.Notice, level.Warn, level.Err,
    level.Critical, level.Alert, level.Fatal,
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
    erlang_logger.emit(r)
  })
}

pub fn emit_with_caller_id_test() {
  let r =
    record.new(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Warn,
      logger_name: "test.caller_id",
      message: "With caller ID",
      metadata: [],
    )
    |> record.with_caller_id("<0.123.0>")

  erlang_logger.emit(r)
}

// ============================================================================
// Round-Trip Formatting Tests (Erlang-only)
//
// These tests install a capture formatter on the :logger default handler,
// log through birch, and verify the output. Combined into a single test
// to avoid race conditions (gleeunit runs tests concurrently, and multiple
// tests modifying the same :logger handler would interfere).
// ============================================================================

pub fn logger_round_trip_formatting_test() {
  case is_erlang_target() {
    True -> {
      // === Test 1: No double-formatting for birch logs ===
      let captured1 = new_capture_buffer()
      let capture_formatter1 = fn(r: record.LogRecord) -> String {
        let formatted = formatter.human_readable(r)
        append_to_buffer(captured1, formatted)
        formatted
      }

      let assert Ok(Nil) =
        erlang_logger.install_formatter_on("default", capture_formatter1)

      // Log through birch → :logger → birch formatter
      // Use Warn level (above :logger's default `notice` threshold)
      let lgr =
        logger.new("myapp.test")
        |> logger.with_level(level.Warn)
      logger.warn(lgr, "Hello", [meta.string("key", "val")])
      sleep(100)

      let output1 = get_buffer_contents(captured1)

      // Output should contain "Hello"
      output1 |> string.contains("Hello") |> should.be_true

      // Output should contain the birch logger name "myapp.test"
      output1 |> string.contains("myapp.test") |> should.be_true

      // Should NOT contain duplicate "WARN" (double-formatting artifact)
      let warn_count = count_occurrences(output1, "WARN")
      { warn_count == 1 } |> should.be_true

      // Output should contain metadata
      output1 |> string.contains("key=val") |> should.be_true

      // === Test 2: Birch metadata preserved ===
      let captured2 = new_capture_buffer()
      let capture_formatter2 = fn(r: record.LogRecord) -> String {
        let formatted = formatter.human_readable(r)
        append_to_buffer(captured2, formatted)
        formatted
      }

      let assert Ok(Nil) =
        erlang_logger.install_formatter_on("default", capture_formatter2)

      let lgr2 =
        logger.new("myapp.database")
        |> logger.with_level(level.Warn)
      logger.warn(lgr2, "Connection pool exhausted", [
        meta.string("pool", "primary"),
        meta.int("active", 50),
      ])
      sleep(100)

      let output2 = get_buffer_contents(captured2)

      // Should use birch logger name, NOT the Erlang module name
      output2 |> string.contains("myapp.database") |> should.be_true

      // Should contain the birch metadata
      output2 |> string.contains("pool=primary") |> should.be_true
      output2 |> string.contains("active=50") |> should.be_true

      // === Test 3: Level preserved through round-trip ===
      let captured3 = new_capture_buffer()
      let capture_formatter3 = fn(r: record.LogRecord) -> String {
        let formatted = formatter.human_readable(r)
        append_to_buffer(captured3, formatted)
        formatted
      }

      let assert Ok(Nil) =
        erlang_logger.install_formatter_on("default", capture_formatter3)

      let lgr3 =
        logger.new("test.level")
        |> logger.with_level(level.Err)
      logger.error(lgr3, "Error occurred", [])
      sleep(100)

      let output3 = get_buffer_contents(captured3)

      // Should show ERROR level (birch Err → Erlang error → birch Err)
      output3 |> string.contains("ERROR") |> should.be_true

      // === Test 4: OTP log events formatted correctly ===
      let captured4 = new_capture_buffer()
      let capture_formatter4 = fn(r: record.LogRecord) -> String {
        let formatted = formatter.human_readable(r)
        append_to_buffer(captured4, formatted)
        formatted
      }

      let assert Ok(Nil) =
        erlang_logger.install_formatter_on("default", capture_formatter4)

      // Send a plain OTP :logger warning (above default threshold)
      do_otp_logger_warning("OTP test message")
      sleep(100)

      let output4 = get_buffer_contents(captured4)

      // Should contain the message
      output4 |> string.contains("OTP test message") |> should.be_true

      // Should have exactly one WARN level
      let warn_count4 = count_occurrences(output4, "WARN")
      { warn_count4 == 1 } |> should.be_true

      // === Test 5: OTP report_cb formatting ===
      let captured5 = new_capture_buffer()
      let capture_formatter5 = fn(r: record.LogRecord) -> String {
        let formatted = formatter.human_readable(r)
        append_to_buffer(captured5, formatted)
        formatted
      }

      let assert Ok(Nil) =
        erlang_logger.install_formatter_on("default", capture_formatter5)

      // Send a structured report with report_cb
      do_otp_logger_report_with_cb()
      sleep(100)

      let output5 = get_buffer_contents(captured5)

      // The report_cb should format the report as "Test report: hello"
      output5 |> string.contains("Test report: hello") |> should.be_true

      // === Cleanup ===
      let _ = erlang_logger.remove_formatter()
      Nil
    }
    False -> Nil
  }
}

// ============================================================================
// Utility functions
// ============================================================================

/// Count occurrences of a substring in a string
fn count_occurrences(haystack: String, needle: String) -> Int {
  let parts = string.split(haystack, needle)
  list.length(parts) - 1
}

// ============================================================================
// Erlang-only FFI for test helpers
// ============================================================================

@external(erlang, "birch_logger_test_ffi", "new_capture_buffer")
@external(javascript, "./birch_logger_test_ffi.mjs", "new_capture_buffer")
fn new_capture_buffer() -> CaptureBuffer

@external(erlang, "birch_logger_test_ffi", "append_to_buffer")
@external(javascript, "./birch_logger_test_ffi.mjs", "append_to_buffer")
fn append_to_buffer(buffer: CaptureBuffer, value: String) -> Nil

@external(erlang, "birch_logger_test_ffi", "get_buffer_contents")
@external(javascript, "./birch_logger_test_ffi.mjs", "get_buffer_contents")
fn get_buffer_contents(buffer: CaptureBuffer) -> String

@external(erlang, "birch_logger_test_ffi", "sleep")
@external(javascript, "./birch_logger_test_ffi.mjs", "sleep")
fn sleep(ms: Int) -> Nil

/// Send a plain OTP logger:warning message (not from birch).
/// Uses warning level which is above the default :logger threshold (notice).
@external(erlang, "birch_logger_test_ffi", "otp_logger_warning")
@external(javascript, "./birch_logger_test_ffi.mjs", "otp_logger_warning")
fn do_otp_logger_warning(message: String) -> Nil

/// Send a structured OTP report with a report_cb callback
@external(erlang, "birch_logger_test_ffi", "otp_logger_report_with_cb")
@external(javascript, "./birch_logger_test_ffi.mjs", "otp_logger_report_with_cb")
fn do_otp_logger_report_with_cb() -> Nil

/// Opaque type for the capture buffer
type CaptureBuffer
