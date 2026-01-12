import birch as log
import gleeunit
import gleeunit/should
import birch_example_10_lazy_evaluation

pub fn main() {
  gleeunit.main()
}

pub fn log_large_data_test() {
  log.reset_config()

  // Should not crash
  birch_example_10_lazy_evaluation.log_large_data(["a", "b", "c"])
  |> should.equal(Nil)
}

pub fn lazy_evaluation_at_filtered_level_test() {
  // At default Info level, debug_lazy should not evaluate
  log.reset_config()

  // If this crashes, something is wrong
  log.debug_lazy(fn() {
    // This should NOT be called
    "test"
  })
  |> should.equal(Nil)
}
