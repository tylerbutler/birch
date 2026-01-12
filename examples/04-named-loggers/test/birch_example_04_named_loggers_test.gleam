import birch as log
import gleeunit
import gleeunit/should
import birch_example_04_named_loggers

pub fn main() {
  gleeunit.main()
}

pub fn create_database_logger_test() {
  let logger = birch_example_04_named_loggers.create_database_logger()
  // Verify we can log without error
  logger |> log.logger_info("Test message", [])
  |> should.equal(Nil)
}

pub fn create_http_logger_test() {
  let logger = birch_example_04_named_loggers.create_http_logger()
  logger |> log.logger_info("Test message", [])
  |> should.equal(Nil)
}

pub fn process_with_logger_test() {
  let logger = log.new("test")
  birch_example_04_named_loggers.process_with_logger(logger, "hello")
  |> should.equal("processed: hello")
}
