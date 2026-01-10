import birch as log
import birch/handler
import birch/handler/file
import gleeunit
import gleeunit/should
import simplifile

pub fn main() {
  gleeunit.main()
}

pub fn file_handler_creates_file_test() {
  let test_path = "/tmp/birch-test-file-handler.log"

  // Clean up any existing file
  let _ = simplifile.delete(test_path)

  // Create handler and log
  let h =
    file.handler(file.FileConfig(path: test_path, rotation: file.NoRotation))

  log.configure([log.config_handlers([h])])
  log.info("Test message")

  // Verify file was created
  simplifile.is_file(test_path)
  |> should.be_ok()
  |> should.be_true()

  // Clean up
  let _ = simplifile.delete(test_path)
  log.reset_config()
}

pub fn file_handler_name_includes_path_test() {
  let h =
    file.handler(file.FileConfig(
      path: "/var/log/app.log",
      rotation: file.NoRotation,
    ))

  handler.name(h)
  |> should.equal("file:/var/log/app.log")
}
