//// Metadata Example
////
//// Demonstrates structured logging with key-value metadata.

import birch as log
import birch/logger
import birch/meta

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

  let lgr = log.new("app")

  // Simple message (no metadata)
  log.info("Application started")

  // Message with metadata
  logger.info(lgr, "User logged in", [meta.string("user_id", "user_123"), meta.string("ip", "192.168.1.1")])

  // Debug with metadata (won't show at default level)
  logger.debug(lgr, "Session created", [
    meta.string("session_id", "sess_abc"),
    meta.int("expires_in", 3600),
  ])

  // Warning with metadata
  logger.warn(lgr, "Rate limit approaching", [
    meta.int("current", 95),
    meta.int("limit", 100),
    meta.string("window", "60s"),
  ])
}

/// Simulate request tracking with metadata.
fn demo_request_tracking() {
  log.info("--- Request Tracking ---")

  let lgr = log.new("app")
  let request_id = "req_12345"

  logger.info(lgr, "Request received", [
    meta.string("request_id", request_id),
    meta.string("method", "POST"),
    meta.string("path", "/api/users"),
  ])

  logger.info(lgr, "Request processed", [
    meta.string("request_id", request_id),
    meta.int("status", 200),
    meta.int("duration_ms", 42),
  ])
}

/// Demonstrate performance metrics logging.
fn demo_performance_metrics() {
  log.info("--- Performance Metrics ---")

  let lgr = log.new("app")

  logger.info(lgr, "Database query completed", [
    meta.string("query_type", "SELECT"),
    meta.string("table", "users"),
    meta.int("duration_ms", 15),
    meta.int("rows_returned", 42),
  ])

  logger.info(lgr, "Cache operation", [
    meta.string("operation", "GET"),
    meta.string("key", "user:123:profile"),
    meta.bool("hit", True),
  ])
}

/// Demonstrate error context with metadata.
fn demo_error_context() {
  log.info("--- Error Context ---")

  let lgr = log.new("app")

  logger.error(lgr, "Failed to process payment", [
    meta.string("order_id", "order_789"),
    meta.float("amount", 99.99),
    meta.string("currency", "USD"),
    meta.string("error_code", "CARD_DECLINED"),
  ])

  logger.error(lgr, "Connection failed", [
    meta.string("host", "db.example.com"),
    meta.int("port", 5432),
    meta.int("retry_count", 3),
  ])
}

/// Example function that logs with metadata.
/// Shows how to build metadata dynamically.
pub fn process_order(order_id: String, items: Int, total: Float) -> Nil {
  let lgr = log.new("app")
  logger.info(lgr, "Processing order", [
    meta.string("order_id", order_id),
    meta.int("item_count", items),
    meta.float("total", total),
  ])
}
