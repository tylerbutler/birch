//// Scoped Context Example
////
//// Demonstrates request-scoped metadata propagation.

import birch as log
import gleam/int

pub fn main() {
  log.info("=== Scoped Context Demo ===")

  // Check platform availability
  demo_availability_check()

  // Basic scoped context
  demo_basic_scope()

  // Nested scopes
  demo_nested_scopes()

  // Simulated request handling
  demo_request_handling()

  log.reset_config()
  log.info("Demo complete")
}

/// Check if scoped context is fully available.
fn demo_availability_check() {
  log.info("--- Platform Availability ---")

  case log.is_scoped_context_available() {
    True -> log.info("Full scoped context support on this platform")
    False ->
      log.warn(
        "Limited scoped context - may not propagate to async operations",
      )
  }
}

/// Demonstrate basic scoped context.
fn demo_basic_scope() {
  log.info("--- Basic Scope ---")

  log.info("Before scope - no context")

  log.with_scope([#("request_id", "req-123")], fn() {
    log.info("Inside scope - request_id is attached")
    log.info("All logs in this block have request_id")
  })

  log.info("After scope - context is gone")
}

/// Demonstrate nested scopes.
fn demo_nested_scopes() {
  log.info("--- Nested Scopes ---")

  log.with_scope([#("request_id", "req-456")], fn() {
    log.info("Outer scope: request_id only")

    log.with_scope([#("step", "validation")], fn() {
      log.info("Inner scope: request_id AND step")

      log.with_scope([#("field", "email")], fn() {
        log.info("Deepest scope: all three keys")
      })

      log.info("Back to: request_id AND step")
    })

    log.info("Back to: request_id only")
  })
}

/// Demonstrate simulated request handling.
fn demo_request_handling() {
  log.info("--- Request Handling Simulation ---")

  // Simulate handling two requests
  handle_request("req-001", "/api/users", "GET")
  handle_request("req-002", "/api/orders", "POST")
}

/// Simulate handling a web request with scoped context.
fn handle_request(request_id: String, path: String, method: String) {
  log.with_scope(
    [#("request_id", request_id), #("path", path), #("method", method)],
    fn() {
      log.info("Request received")

      // Call various functions - they all get the context
      validate_request()
      process_request()

      log.info("Request complete")
    },
  )
}

fn validate_request() {
  log.info("Validating request")
  // The request_id, path, and method are automatically included
}

fn process_request() {
  log.info("Processing request")
  // Same here - scoped context propagates through function calls
}

/// Example: Wrap a function with request context.
pub fn with_request_context(
  request_id: String,
  user_id: String,
  work: fn() -> a,
) -> a {
  log.with_scope([#("request_id", request_id), #("user_id", user_id)], work)
}

/// Example: Get current request ID from scope context.
pub fn get_current_request_id() -> Result(String, Nil) {
  let context = log.get_scope_context()
  find_key(context, "request_id")
}

fn find_key(
  context: List(#(String, String)),
  key: String,
) -> Result(String, Nil) {
  case context {
    [] -> Error(Nil)
    [#(k, v), ..rest] ->
      case k == key {
        True -> Ok(v)
        False -> find_key(rest, key)
      }
  }
}

/// Generate a simple request ID for examples.
pub fn generate_request_id() -> String {
  // In real code, use UUID or similar
  "req-" <> int.to_string(int.random(100_000))
}
