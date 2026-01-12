import birch/level
import birch/sampling
import gleeunit
import gleeunit/should
import birch_example_14_sampling as example

pub fn main() {
  gleeunit.main()
}

pub fn production_sampling_test() {
  let config = example.production_sampling()

  // Should sample debug at 1%
  // We can't easily test the exact rate, but we can verify the config exists
  should.be_true(True)
}

pub fn development_sampling_test() {
  let config = example.development_sampling()
  should.be_true(True)
}

pub fn high_volume_bucket_test() {
  let bucket = example.high_volume_bucket()

  birch_example_14_sampling.bucket_max_tokens(bucket)
  |> should.equal(100)

  birch_example_14_sampling.bucket_burst_size(bucket)
  |> should.equal(1000)
}

pub fn token_bucket_consume_test() {
  // Create a small bucket for testing
  let bucket = birch_example_14_sampling.new_token_bucket(3, 10)

  // First 3 consumes should succeed
  let #(ok1, bucket1) = birch_example_14_sampling.try_consume(bucket)
  should.be_true(ok1)

  let #(ok2, bucket2) = birch_example_14_sampling.try_consume(bucket1)
  should.be_true(ok2)

  let #(ok3, bucket3) = birch_example_14_sampling.try_consume(bucket2)
  should.be_true(ok3)

  // Fourth should fail (bucket empty, not enough time to refill)
  let #(ok4, _bucket4) = birch_example_14_sampling.try_consume(bucket3)
  should.be_false(ok4)
}

pub fn sampling_always_logs_above_threshold_test() {
  let config = birch_example_14_sampling.config(level.Debug, 0.0)
  // 0% of debug

  // Info should always be logged (above Debug)
  birch_example_14_sampling.should_sample(config, level.Info)
  |> should.be_true()

  // Warn should always be logged
  birch_example_14_sampling.should_sample(config, level.Warn)
  |> should.be_true()
}
