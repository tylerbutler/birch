import birch as log
import birch/erlang_logger
import erlang_logger as example
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

pub fn install_uninstall_handler_test() {
  // Should not crash
  erlang_logger.install_logger_handler()
  erlang_logger.uninstall_logger_handler()

  should.be_true(True)
}

pub fn setup_teardown_test() {
  // Using example functions
  example.setup_erlang_integration()
  example.teardown_erlang_integration()

  should.be_true(True)
}
