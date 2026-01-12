import birch as log
import birch/handler
import gleeunit
import gleeunit/should
import birch_example_06_json_handler

pub fn main() {
  gleeunit.main()
}

pub fn create_service_json_handler_test() {
  let h = birch_example_06_json_handler.create_service_json_handler("my-api", "1.0.0", "prod")
  handler.name(h)
  |> should.equal("json")
}

pub fn logging_with_json_handler_test() {
  // Configure with JSON handler and log
  log.reset_config()
  log.configure([log.config_handlers([birch_example_06_json_handler.create_service_json_handler("test", "0.0.0", "test")])])
  log.info("Test message")
  |> should.equal(Nil)
}
