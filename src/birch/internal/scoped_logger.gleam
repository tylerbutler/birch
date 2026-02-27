//// Internal module for scoped logger override storage.
////
//// This is separated from platform.gleam to avoid circular dependencies
//// with the logger module.

import birch/logger.{type Logger}

/// Get the current scoped logger override.
/// Returns Ok(Logger) if a scoped logger is set, Error(Nil) otherwise.
@external(erlang, "birch_ffi", "get_scoped_logger")
@external(javascript, "../../birch_ffi.mjs", "get_scoped_logger")
pub fn get_scoped_logger() -> Result(Logger, Nil)

/// Set the scoped logger override.
/// On Erlang: Uses the process dictionary.
/// On JavaScript: Used for fallback only (AsyncLocalStorage uses run_with_scoped_logger).
@external(erlang, "birch_ffi", "set_scoped_logger")
@external(javascript, "../../birch_ffi.mjs", "set_scoped_logger")
pub fn set_scoped_logger(lgr: Logger) -> Nil

/// Clear the scoped logger override.
@external(erlang, "birch_ffi", "clear_scoped_logger")
@external(javascript, "../../birch_ffi.mjs", "clear_scoped_logger")
pub fn clear_scoped_logger() -> Nil

/// Run a function with a scoped logger override.
/// On JavaScript, uses AsyncLocalStorage.run() for proper async propagation.
/// On Erlang, uses FFI helper with try/after for exception safety.
@external(javascript, "../../birch_ffi.mjs", "run_with_scoped_logger")
pub fn with_scoped_logger(lgr: Logger, work: fn() -> a) -> a {
  // Erlang implementation: use FFI helper with try/after for exception safety
  run_with_scoped_logger_cleanup(lgr, work)
}

/// Erlang FFI: run work function with scoped logger, restoring previous
/// logger state in an `after` block even if work() raises an exception.
@external(erlang, "birch_ffi", "run_with_scoped_logger_cleanup")
fn run_with_scoped_logger_cleanup(lgr: Logger, work: fn() -> a) -> a
