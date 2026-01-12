import birch as log
import birch_example_11_error_helpers
import gleeunit
import gleeunit/should
import simplifile

pub fn main() {
  gleeunit.main()
}

pub fn load_config_success_test() {
  // Write a test file
  let test_path = "/tmp/birch-test-config.txt"
  let _ = simplifile.write(test_path, "test content")

  birch_example_11_error_helpers.load_config(test_path)
  |> should.be_ok()
  |> should.equal("test content")

  // Clean up
  let _ = simplifile.delete(test_path)
  log.reset_config()
}

pub fn load_config_failure_test() {
  birch_example_11_error_helpers.load_config("/nonexistent/path/config.txt")
  |> should.be_error()

  log.reset_config()
}
