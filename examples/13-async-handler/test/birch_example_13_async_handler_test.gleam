import async_handler
import birch as log
import birch/handler
import birch/handler/async
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn create_async_file_handler_test() {
  let h = async_handler.create_async_file_handler()

  // Async handlers have "async:" prefix
  handler.name(h)
  |> should.equal("async:console")

  log.reset_config()
}

pub fn async_config_builder_test() {
  let config =
    async.config()
    |> async.with_queue_size(5000)
    |> async.with_flush_interval(200)
    |> async.with_overflow(async.Block)

  // Just verify we can build the config without error
  should.be_true(True)
}

pub fn flush_does_not_crash_test() {
  // Flush should work even with no async handlers
  async.flush()
  |> should.equal(Nil)
}
