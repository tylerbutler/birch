import birch as log
import birch/erlang_logger
import birch_example_16_erlang_logger as example
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn forward_to_logger_creates_handler_test() {
  let handler = erlang_logger.forward_to_logger()

  // Should be able to use it without error
  log.configure([log.config_handlers([handler])])

  log.info("Test message through Erlang logger")

  log.reset_config()
}

pub fn create_erlang_handler_test() {
  let handler = example.create_erlang_handler()

  // Should create a valid handler
  log.configure([log.config_handlers([handler])])
  log.info("Using example handler")
  log.reset_config()
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
