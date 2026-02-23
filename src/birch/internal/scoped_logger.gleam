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
/// On Erlang, uses process dictionary (set/clear pattern).
@external(javascript, "../../birch_ffi.mjs", "run_with_scoped_logger")
pub fn with_scoped_logger(lgr: Logger, work: fn() -> a) -> a {
  // Erlang implementation: use process dictionary
  let previous = get_scoped_logger()

  // Set the new scoped logger
  set_scoped_logger(lgr)

  // Execute the work function
  let result = work()

  // Restore the previous scoped logger (or clear it)
  case previous {
    Ok(prev) -> set_scoped_logger(prev)
    Error(Nil) -> clear_scoped_logger()
  }

  result
}
