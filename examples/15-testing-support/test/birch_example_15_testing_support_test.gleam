import birch as log
import gleeunit
import gleeunit/should
import testing_support

pub fn main() {
  gleeunit.main()
}

pub fn test_logger_creates_logger_test() {
  let logger = testing_support.test_logger("my-test")

  // Should be able to log without error
  logger |> log.logger_info("Test message", [])
  |> should.equal(Nil)
}

pub fn debug_logger_creates_logger_test() {
  let logger = testing_support.debug_logger("my-debug")

  logger |> log.logger_info("Debug message", [])
  |> should.equal(Nil)
}

pub fn silence_logging_test() {
  // Silence logging
  testing_support.silence_logging()

  // Should not crash even when silenced
  log.info("Silenced message")
  log.error("Silenced error")

  // Restore
  log.reset_config()
}

pub fn fixed_timestamp_test() {
  let logger =
    log.new("test")
    |> log.with_time_provider(fn() { "FIXED" })

  // Just verify it doesn't crash
  logger |> log.logger_info("Message", [])
  |> should.equal(Nil)
}
