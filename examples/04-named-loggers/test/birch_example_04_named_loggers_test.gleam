import birch
import birch/log
import gleeunit
import gleeunit/should
import birch_example_04_named_loggers as named_loggers

pub fn main() {
  gleeunit.main()
}

pub fn create_database_logger_test() {
  let lgr = named_loggers.create_database_logger()
  // Verify we can log without error
  lgr |> log.info("Test message", [])
  |> should.equal(Nil)
}

pub fn create_http_logger_test() {
  let lgr = named_loggers.create_http_logger()
  lgr |> log.info("Test message", [])
  |> should.equal(Nil)
}

pub fn process_with_logger_test() {
  let logger = birch.new("test")
  named_loggers.process_with_logger(logger, "hello")
  |> should.equal("processed: hello")
}
