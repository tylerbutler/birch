import birch as log
import birch/handler/console
import birch/level

pub fn main() {
  // DON'T configure birch — use default which installs birch formatter on :logger
  // Actually, let's skip formatter setup entirely by using explicit handlers
  log.configure([
    log.config_handlers([console.handler()]),
    log.config_level(level.Warn),
  ])
  
  log.warn("Test warning")
}
