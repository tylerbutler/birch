//// Scoped context for temporary, request-scoped metadata.
////
//// Scoped context allows you to attach metadata that automatically applies
//// to all logs within a scope, without needing to pass context explicitly.
////
//// ## Example
////
//// ```gleam
//// import birch/scope
////
//// pub fn handle_request(request_id: String) {
////   scope.with_scope([#("request_id", request_id)], fn() {
////     // All logs in this block include request_id
////     log.info("processing request")
////     do_work()
////     log.info("request complete")
////   })
//// }
//// ```
////
//// ## Platform Support
////
//// - **Erlang:** Uses the process dictionary for context storage.
////   Each process has its own scope context, naturally supporting concurrency.
////
//// - **JavaScript (Node.js):** Uses AsyncLocalStorage for context propagation.
////   This works across async operations within the same logical execution chain.
////
//// - **JavaScript (Other):** Falls back to stack-based context storage.
////   Use `is_available()` to check if async context propagation is supported.

import birch/internal/platform
import birch/record.{type Metadata}
import gleam/list

/// Execute a function with the given context applied.
///
/// All logs made within the scope (directly or through nested calls)
/// will include the scoped context metadata.
///
/// Scopes can be nested, with inner scopes adding to (and potentially
/// shadowing) the outer scope's context.
///
/// ## Example
///
/// ```gleam
/// with_scope([#("request_id", "123")], fn() {
///   log.info("processing")  // Includes request_id=123
///   with_scope([#("step", "validation")], fn() {
///     log.info("validating")  // Includes request_id=123 AND step=validation
///   })
///   log.info("done")  // Only request_id=123
/// })
/// ```
@external(javascript, "../birch_ffi.mjs", "run_with_scope")
pub fn with_scope(context: Metadata, work: fn() -> a) -> a {
  // Erlang implementation: use process dictionary with try/after cleanup
  // Get current scope context and depth (may be empty or from outer scope)
  let current_context = platform.get_scope_context()
  let current_depth = platform.get_scope_depth()

  // Merge new context with current (new values prepended for shadowing)
  let merged_context = list.append(context, current_context)
  let new_depth = current_depth + 1

  // Use FFI helper that wraps work() in try/after for exception safety
  run_with_scope_cleanup(
    merged_context,
    new_depth,
    current_context,
    current_depth,
    work,
  )
}

/// Erlang FFI: run work function with scope context, restoring previous
/// context/depth in an `after` block even if work() raises an exception.
@external(erlang, "birch_ffi", "run_with_scope_cleanup")
fn run_with_scope_cleanup(
  merged_context: Metadata,
  new_depth: Int,
  old_context: Metadata,
  old_depth: Int,
  work: fn() -> a,
) -> a

/// Get the current scope context.
///
/// Returns the metadata from all active scopes, with inner scope values
/// taking precedence (appearing first in the list).
///
/// Returns an empty list if called outside of any scope.
pub fn get_context() -> Metadata {
  platform.get_scope_context()
}

/// Get the current scope depth (nesting level).
///
/// Returns 0 if no scope is active, 1 for one level of nesting, etc.
///
/// This can be used to determine visual indentation level or to track
/// how deeply nested the current execution context is.
pub fn get_depth() -> Int {
  platform.get_scope_depth()
}

/// Check if scoped context is available on the current platform.
///
/// - On Erlang: Always returns `True` (uses process dictionary)
/// - On Node.js: Returns `True` (uses AsyncLocalStorage)
/// - On other JavaScript runtimes: Returns `False`
///
/// When not available, `with_scope` still works but context won't propagate
/// to nested async operations.
pub fn is_available() -> Bool {
  platform.is_scope_context_available()
}
