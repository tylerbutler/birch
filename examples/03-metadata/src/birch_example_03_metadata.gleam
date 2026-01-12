//// Metadata Example
////
//// Demonstrates structured logging with key-value metadata.

import birch as log
import gleam/float
import gleam/int

pub fn main() {
  log.info("=== Structured Logging with Metadata ===")

  // Basic metadata
  demo_basic_metadata()

  // Simulate a request with metadata
  demo_request_tracking()

  // Performance tracking
  demo_performance_metrics()

  // Error context
  demo_error_context()
}

/// Basic metadata examples.
fn demo_basic_metadata() {
  log.info("--- Basic Metadata ---")

  // Simple message (no metadata)
  log.info("Application started")

  // Message with metadata
  log.info_m("User logged in", [#("user_id", "user_123"), #("ip", "192.168.1.1")])

  // Debug with metadata (won't show at default level)
  log.debug_m("Session created", [
    #("session_id", "sess_abc"),
    #("expires_in", "3600"),
  ])

  // Warning with metadata
  log.warn_m("Rate limit approaching", [
    #("current", "95"),
    #("limit", "100"),
    #("window", "60s"),
  ])
}

/// Simulate request tracking with metadata.
fn demo_request_tracking() {
  log.info("--- Request Tracking ---")

  let request_id = "req_" <> int.to_string(12_345)

  log.info_m("Request received", [
    #("request_id", request_id),
    #("method", "POST"),
    #("path", "/api/users"),
  ])

  log.info_m("Request processed", [
    #("request_id", request_id),
    #("status", "200"),
    #("duration_ms", "42"),
  ])
}

/// Demonstrate performance metrics logging.
fn demo_performance_metrics() {
  log.info("--- Performance Metrics ---")

  log.info_m("Database query completed", [
    #("query_type", "SELECT"),
    #("table", "users"),
    #("duration_ms", "15"),
    #("rows_returned", "42"),
  ])

  log.info_m("Cache operation", [
    #("operation", "GET"),
    #("key", "user:123:profile"),
    #("hit", "true"),
  ])
}

/// Demonstrate error context with metadata.
fn demo_error_context() {
  log.info("--- Error Context ---")

  log.error_m("Failed to process payment", [
    #("order_id", "order_789"),
    #("amount", "99.99"),
    #("currency", "USD"),
    #("error_code", "CARD_DECLINED"),
  ])

  log.error_m("Connection failed", [
    #("host", "db.example.com"),
    #("port", "5432"),
    #("retry_count", "3"),
  ])
}

/// Example function that logs with metadata.
/// Shows how to build metadata dynamically.
pub fn process_order(order_id: String, items: Int, total: Float) -> Nil {
  log.info_m("Processing order", [
    #("order_id", order_id),
    #("item_count", int.to_string(items)),
    #("total", float.to_string(total)),
  ])
}
