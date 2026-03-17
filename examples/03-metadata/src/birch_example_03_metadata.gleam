//// Metadata Example
////
//// Demonstrates structured logging with key-value metadata.

import birch
import birch/log
import birch/meta

pub fn main() {
  birch.info("=== Structured Logging with Metadata ===")

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
  birch.info("--- Basic Metadata ---")

  let lgr = birch.new("app")

  // Simple message (no metadata)
  birch.info("Application started")

  // Message with metadata
  log.info(lgr, "User logged in", [meta.string("user_id", "user_123"), meta.string("ip", "192.168.1.1")])

  // Debug with metadata (won't show at default level)
  log.debug(lgr, "Session created", [
    meta.string("session_id", "sess_abc"),
    meta.int("expires_in", 3600),
  ])

  // Warning with metadata
  log.warn(lgr, "Rate limit approaching", [
    meta.int("current", 95),
    meta.int("limit", 100),
    meta.string("window", "60s"),
  ])
}

/// Simulate request tracking with metadata.
fn demo_request_tracking() {
  birch.info("--- Request Tracking ---")

  let lgr = birch.new("app")
  let request_id = "req_12345"

  log.info(lgr, "Request received", [
    meta.string("request_id", request_id),
    meta.string("method", "POST"),
    meta.string("path", "/api/users"),
  ])

  log.info(lgr, "Request processed", [
    meta.string("request_id", request_id),
    meta.int("status", 200),
    meta.int("duration_ms", 42),
  ])
}

/// Demonstrate performance metrics logging.
fn demo_performance_metrics() {
  birch.info("--- Performance Metrics ---")

  let lgr = birch.new("app")

  log.info(lgr, "Database query completed", [
    meta.string("query_type", "SELECT"),
    meta.string("table", "users"),
    meta.int("duration_ms", 15),
    meta.int("rows_returned", 42),
  ])

  log.info(lgr, "Cache operation", [
    meta.string("operation", "GET"),
    meta.string("key", "user:123:profile"),
    meta.bool("hit", True),
  ])
}

/// Demonstrate error context with metadata.
fn demo_error_context() {
  birch.info("--- Error Context ---")

  let lgr = birch.new("app")

  log.error(lgr, "Failed to process payment", [
    meta.string("order_id", "order_789"),
    meta.float("amount", 99.99),
    meta.string("currency", "USD"),
    meta.string("error_code", "CARD_DECLINED"),
  ])

  log.error(lgr, "Connection failed", [
    meta.string("host", "db.example.com"),
    meta.int("port", 5432),
    meta.int("retry_count", 3),
  ])
}

/// Example function that logs with metadata.
/// Shows how to build metadata dynamically.
pub fn process_order(order_id: String, items: Int, total: Float) -> Nil {
  let lgr = birch.new("app")
  log.info(lgr, "Processing order", [
    meta.string("order_id", order_id),
    meta.int("item_count", items),
    meta.float("total", total),
  ])
}
