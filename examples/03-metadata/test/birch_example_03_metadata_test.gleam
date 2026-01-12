import gleeunit
import gleeunit/should
import birch_example_03_metadata as metadata

pub fn main() {
  gleeunit.main()
}

pub fn process_order_test() {
  // This test verifies the function runs without error
  // The logging output is not captured, but we verify no crash
  metadata.process_order("order_123", 5, 99.95)
  |> should.equal(Nil)
}
