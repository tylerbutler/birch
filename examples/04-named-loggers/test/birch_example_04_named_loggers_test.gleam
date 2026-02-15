import birch as log
import birch/logger
import gleeunit
import gleeunit/should
import birch_example_04_named_loggers as named_loggers

pub fn main() {
  gleeunit.main()
}

pub fn create_database_logger_test() {
  let lgr = named_loggers.create_database_logger()
  // Verify we can log without error
  lgr |> logger.info("Test message", [])
  |> should.equal(Nil)
}

pub fn create_http_logger_test() {
  let lgr = named_loggers.create_http_logger()
  lgr |> logger.info("Test message", [])
  |> should.equal(Nil)
}

pub fn process_with_logger_test() {
  let lgr = log.new("test")
  named_loggers.process_with_logger(lgr, "hello")
  |> should.equal("processed: hello")
}
