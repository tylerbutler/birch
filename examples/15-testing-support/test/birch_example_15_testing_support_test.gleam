import birch
import birch/log
import gleeunit
import gleeunit/should
import birch_example_15_testing_support as testing_support

pub fn main() {
  gleeunit.main()
}

pub fn test_logger_creates_logger_test() {
  let lgr = testing_support.test_logger("my-test")

  // Should be able to log without error
  lgr |> log.info("Test message", [])
  |> should.equal(Nil)
}

pub fn debug_logger_creates_logger_test() {
  let lgr = testing_support.debug_logger("my-debug")

  lgr |> log.info("Debug message", [])
  |> should.equal(Nil)
}

pub fn silence_logging_test() {
  // Silence logging
  testing_support.silence_logging()

  // Should not crash even when silenced
  birch.info("Silenced message")
  birch.error("Silenced error")

  // Restore
  birch.reset_config()
}

pub fn fixed_timestamp_test() {
  let lgr =
    birch.new("test")
    |> birch.with_time_provider(fn() { "FIXED" })

  // Just verify it doesn't crash
  lgr |> log.info("Message", [])
  |> should.equal(Nil)
}
