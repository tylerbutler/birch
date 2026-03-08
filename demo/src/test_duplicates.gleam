import birch as log
import birch/handler/console
import birch/level

pub fn main() {
  log.configure([
    log.config_handlers([console.handler()]),
    log.config_level(level.Warn),
  ])
  
  // This should show duplicates
  log.warn("Test warning")
  log.error("Test error")
  log.fatal("Test fatal")
}
