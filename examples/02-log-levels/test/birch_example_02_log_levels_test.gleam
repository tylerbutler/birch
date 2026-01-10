import birch as log
import birch/level
import gleeunit
import gleeunit/should
import log_levels

pub fn main() {
  gleeunit.main()
}

pub fn current_level_default_test() {
  // Reset to ensure clean state
  log.reset_config()
  log_levels.current_level()
  |> should.equal(level.Info)
}

pub fn would_log_at_info_test() {
  log.reset_config()

  // At Info level, Debug should not log
  log_levels.would_log(level.Debug)
  |> should.be_false()

  // But Info and above should
  log_levels.would_log(level.Info)
  |> should.be_true()

  log_levels.would_log(level.Err)
  |> should.be_true()
}

pub fn level_from_string_test() {
  level.from_string("debug")
  |> should.be_ok()
  |> should.equal(level.Debug)

  level.from_string("WARNING")
  |> should.be_ok()
  |> should.equal(level.Warn)

  level.from_string("invalid")
  |> should.be_error()
}

pub fn level_comparison_test() {
  level.gte(level.Warn, level.Debug)
  |> should.be_true()

  level.gte(level.Debug, level.Warn)
  |> should.be_false()

  level.gte(level.Info, level.Info)
  |> should.be_true()
}
