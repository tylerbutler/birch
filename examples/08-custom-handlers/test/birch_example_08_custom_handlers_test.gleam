import birch/handler
import birch/level
import birch_example_08_custom_handlers as custom_handlers
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn create_filtered_handler_test() {
  let h = custom_handlers.create_filtered_handler("test", level.Warn)
  handler.name(h)
  |> should.equal("test")
}

pub fn create_prefix_handler_test() {
  let h = custom_handlers.create_prefix_handler("prefix-test", "[PREFIX] ")
  handler.name(h)
  |> should.equal("prefix-test")
}

pub fn filtered_handler_should_handle_test() {
  let h = custom_handlers.create_filtered_handler("test", level.Warn)

  // Should handle Warn and above
  handler.should_handle(h, level.Warn)
  |> should.be_true()

  handler.should_handle(h, level.Err)
  |> should.be_true()

  // Should not handle below Warn
  handler.should_handle(h, level.Info)
  |> should.be_false()

  handler.should_handle(h, level.Debug)
  |> should.be_false()
}
