import birch as log
import birch/level
import birch_example_09_global_config as global_config
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn configure_production_test() {
  global_config.configure_production()

  log.get_level()
  |> should.equal(level.Info)

  log.reset_config()
}

pub fn configure_development_test() {
  global_config.configure_development()

  log.get_level()
  |> should.equal(level.Debug)

  log.reset_config()
}

pub fn set_level_changes_level_test() {
  log.reset_config()

  log.set_level(level.Warn)
  log.get_level()
  |> should.equal(level.Warn)

  log.set_level(level.Trace)
  log.get_level()
  |> should.equal(level.Trace)

  log.reset_config()
}

pub fn reset_config_restores_defaults_test() {
  log.set_level(level.Fatal)
  log.reset_config()

  log.get_level()
  |> should.equal(level.Info)
}
