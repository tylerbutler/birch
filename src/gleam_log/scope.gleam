//// Scoped context for temporary, request-scoped metadata.
////
//// Scoped context allows you to attach metadata that automatically applies
//// to all logs within a scope, without needing to pass context explicitly.
////
//// ## Example
////
//// ```gleam
//// import gleam_log/scope
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
//// - **JavaScript (Other):** Falls back to returning empty context.
////   Use `is_available()` to check if scoped context is supported.

import gleam/list
import gleam_log/internal/platform
import gleam_log/record.{type Metadata}

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
pub fn with_scope(context: Metadata, work: fn() -> a) -> a {
  // Get current scope context (may be empty or from outer scope)
  let current_context = platform.get_scope_context()

  // Merge new context with current (new values prepended for shadowing)
  let merged_context = list.append(context, current_context)

  // Set the new scope context
  platform.set_scope_context(merged_context)

  // Execute the work function
  let result = work()

  // Restore the previous context
  platform.set_scope_context(current_context)

  result
}

/// Get the current scope context.
///
/// Returns the metadata from all active scopes, with inner scope values
/// taking precedence (appearing first in the list).
///
/// Returns an empty list if called outside of any scope.
pub fn get_context() -> Metadata {
  platform.get_scope_context()
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
