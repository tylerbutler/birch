//// JSON Handler Example
////
//// Demonstrates JSON output for log aggregation systems.

import birch
import birch/handler.{type Handler}
import birch/handler/json
import birch/log
import birch/meta
import gleam/json as gjson

pub fn main() {
  birch.info("=== JSON Handler Demo ===")

  // Default JSON handler
  demo_default_json()

  // Custom JSON with builder
  demo_custom_json()

  // Minimal JSON
  demo_minimal_json()

  // Reset to defaults
  birch.reset_config()
  birch.info("Demo complete")
}

/// Demonstrate the default JSON handler.
fn demo_default_json() {
  birch.configure([birch.config_handlers([json.handler()])])

  let lgr = birch.new("app")
  birch.info("Default JSON format")
  log.info(lgr, "With metadata", [meta.string("user_id", "12345"), meta.string("action", "login")])
  birch.error("Error message")
}

/// Demonstrate custom JSON with builder pattern.
fn demo_custom_json() {
  // Build a custom JSON format with service info
  let custom_handler =
    json.standard_builder()
    |> json.add_custom(fn(_record) {
      [
        #("service", gjson.string("my-api")),
        #("version", gjson.string("1.0.0")),
        #("environment", gjson.string("production")),
      ]
    })
    |> json.build()
    |> json.handler_with_formatter()

  birch.configure([birch.config_handlers([custom_handler])])

  let lgr = birch.new("app")
  birch.info("Custom JSON with service info")
  log.info(lgr, "Request processed", [meta.string("status", "200"), meta.string("duration_ms", "42")])
}

/// Demonstrate minimal JSON format.
fn demo_minimal_json() {
  // Build minimal JSON with only essential fields
  let minimal_handler =
    json.builder()
    |> json.add_timestamp()
    |> json.add_level()
    |> json.add_message()
    |> json.build()
    |> json.handler_with_formatter()

  birch.configure([birch.config_handlers([minimal_handler])])

  let lgr = birch.new("app")
  birch.info("Minimal JSON (no logger name or metadata)")
  log.info(lgr, "Metadata is ignored", [meta.string("key", "value")])
}

/// Create a JSON handler for a microservice.
/// Includes service name, version, and environment.
pub fn create_service_json_handler(
  service: String,
  version: String,
  env: String,
) -> Handler {
  json.standard_builder()
  |> json.add_custom(fn(_record) {
    [
      #("service", gjson.string(service)),
      #("version", gjson.string(version)),
      #("env", gjson.string(env)),
    ]
  })
  |> json.build()
  |> json.handler_with_formatter()
}
