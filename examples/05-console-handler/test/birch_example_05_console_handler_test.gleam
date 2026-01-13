import birch as log
import birch/handler
import birch_example_05_console_handler as console_handler
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn create_custom_console_handler_stdout_test() {
  let h = console_handler.create_custom_console_handler(True, False)
  handler.name(h)
  |> should.equal("console")
}

pub fn create_custom_console_handler_stderr_test() {
  let h = console_handler.create_custom_console_handler(False, True)
  handler.name(h)
  |> should.equal("console")
}

pub fn logging_with_console_handler_test() {
  // Just verify logging doesn't crash
  log.reset_config()
  log.info("Test message")
  |> should.equal(Nil)
}
