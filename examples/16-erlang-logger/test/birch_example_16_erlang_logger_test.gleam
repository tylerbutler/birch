import birch as log
import birch/erlang_logger
import birch_example_16_erlang_logger as example
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn direct_logging_does_not_crash_test() {
  // On BEAM, birch sends to :logger directly — just log and verify no crash
  log.info("Test message through :logger")
  log.warn("Warning through :logger")
  log.error("Error through :logger")
}

pub fn install_remove_formatter_test() {
  // Should not crash
  let _ = erlang_logger.install_formatter()
  let _ = erlang_logger.remove_formatter()

  should.be_true(True)
}

pub fn setup_teardown_test() {
  // Using example functions
  let _ = example.setup_erlang_integration()
  let _ = example.teardown_erlang_integration()

  should.be_true(True)
}
