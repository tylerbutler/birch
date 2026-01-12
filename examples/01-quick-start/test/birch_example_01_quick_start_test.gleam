import gleeunit
import gleeunit/should
import birch_example_01_quick_start as quick_start

pub fn main() {
  gleeunit.main()
}

pub fn process_item_success_test() {
  quick_start.process_item("test-item")
  |> should.be_ok()
  |> should.equal("Processed: test-item")
}

pub fn process_item_empty_test() {
  quick_start.process_item("")
  |> should.be_error()
  |> should.equal("Item cannot be empty")
}
