//// JSON handler for structured log output.
////
//// Outputs log records as JSON objects, suitable for log aggregation systems.
////
//// This module supports two usage patterns:
////
//// 1. Simple usage with default format:
////    ```gleam
////    let handler = json.handler()
////    ```
////
//// 2. Builder pattern for custom JSON format:
////    ```gleam
////    let custom_handler =
////      json.standard_builder()
////      |> json.add_custom(fn(_) { [#("service", json.string("my-app"))] })
////      |> json.build()
////      |> json.handler_with_formatter()
////    ```

import birch/formatter
import birch/handler.{type Handler}
import birch/internal/platform
import birch/level
import birch/record.{type LogRecord}
import gleam/json.{type Json}
import gleam/list

// ============================================================================
// Builder Types
// ============================================================================

/// A field extractor function that takes a LogRecord and returns JSON fields.
pub type FieldExtractor =
  fn(LogRecord) -> List(#(String, Json))

/// Builder for customizing JSON output format.
/// Accumulates field extractors that are applied when formatting a log record.
pub opaque type JsonBuilder {
  JsonBuilder(fields: List(FieldExtractor))
}

// ============================================================================
// Builder Functions
// ============================================================================

/// Create a new empty JSON builder.
/// Use this as a starting point to add custom fields.
///
/// Example:
/// ```gleam
/// let formatter =
///   json.builder()
///   |> json.add_timestamp()
///   |> json.add_level()
///   |> json.add_message()
///   |> json.build()
/// ```
pub fn builder() -> JsonBuilder {
  JsonBuilder(fields: [])
}

/// Add a field extractor to the builder.
/// This is a low-level function; prefer using specific add_* functions.
pub fn add_field(builder: JsonBuilder, extractor: FieldExtractor) -> JsonBuilder {
  JsonBuilder(fields: list.append(builder.fields, [extractor]))
}

/// Add the timestamp field to the JSON output.
/// Output: `"timestamp": "2024-12-26T10:30:45.123Z"`
pub fn add_timestamp(b: JsonBuilder) -> JsonBuilder {
  add_field(b, fn(r) { [#("timestamp", json.string(r.timestamp))] })
}

/// Add the log level field to the JSON output.
/// Output: `"level": "info"`
pub fn add_level(b: JsonBuilder) -> JsonBuilder {
  add_field(b, fn(r) {
    [#("level", json.string(level.to_string_lowercase(r.level)))]
  })
}

/// Add the logger name field to the JSON output.
/// Output: `"logger": "myapp.http"`
pub fn add_logger(b: JsonBuilder) -> JsonBuilder {
  add_field(b, fn(r) { [#("logger", json.string(r.logger_name))] })
}

/// Add the message field to the JSON output.
/// Output: `"message": "Request complete"`
pub fn add_message(b: JsonBuilder) -> JsonBuilder {
  add_field(b, fn(r) { [#("message", json.string(r.message))] })
}

/// Add all metadata fields to the JSON output.
/// Each metadata key-value pair becomes a JSON field.
/// Output: `"method": "POST", "path": "/api/users"`
pub fn add_metadata(b: JsonBuilder) -> JsonBuilder {
  add_field(b, fn(r) {
    list.map(r.metadata, fn(pair) { #(pair.0, json.string(pair.1)) })
  })
}

/// Add custom fields to the JSON output.
/// The extractor function receives the LogRecord and returns a list of JSON fields.
///
/// Example:
/// ```gleam
/// json.standard_builder()
/// |> json.add_custom(fn(_record) {
///   [
///     #("service", json.string("my-app")),
///     #("version", json.string("1.0.0")),
///   ]
/// })
/// ```
pub fn add_custom(b: JsonBuilder, extractor: FieldExtractor) -> JsonBuilder {
  add_field(b, extractor)
}

/// Build a formatter from the builder.
/// The formatter can be used with `handler_with_formatter` to create a handler.
pub fn build(b: JsonBuilder) -> formatter.Formatter {
  fn(record: LogRecord) -> String {
    b.fields
    |> list.flat_map(fn(f) { f(record) })
    |> json.object
    |> json.to_string
  }
}

/// Create a standard JSON builder with all common fields.
/// This includes: timestamp, level, logger, message, and metadata.
///
/// Use this as a starting point and add custom fields:
/// ```gleam
/// json.standard_builder()
/// |> json.add_custom(fn(_) { [#("env", json.string("production"))] })
/// |> json.build()
/// |> json.handler_with_formatter()
/// ```
pub fn standard_builder() -> JsonBuilder {
  builder()
  |> add_timestamp()
  |> add_level()
  |> add_logger()
  |> add_message()
  |> add_metadata()
}

// ============================================================================
// Handler Creation Functions
// ============================================================================

/// Create a JSON handler with a custom formatter.
/// Outputs to stdout.
///
/// Example:
/// ```gleam
/// let custom_handler =
///   json.standard_builder()
///   |> json.add_custom(fn(_) { [#("service", json.string("my-app"))] })
///   |> json.build()
///   |> json.handler_with_formatter()
/// ```
pub fn handler_with_formatter(format: formatter.Formatter) -> Handler {
  handler.new(name: "json", write: platform.write_stdout, format: format)
}

/// Create a JSON handler with a custom formatter that writes to stderr.
pub fn handler_stderr_with_formatter(format: formatter.Formatter) -> Handler {
  handler.new(name: "json_stderr", write: platform.write_stderr, format: format)
}

// ============================================================================
// Default Handler Functions
// ============================================================================

/// Create a JSON console handler with the default format.
/// Outputs JSON-formatted log records to stdout.
///
/// Default JSON format includes: timestamp, level, logger, message, and metadata.
pub fn handler() -> Handler {
  handler.new(name: "json", write: platform.write_stdout, format: format_json)
}

/// Create a JSON handler that writes to stderr with the default format.
pub fn handler_stderr() -> Handler {
  handler.new(
    name: "json_stderr",
    write: platform.write_stderr,
    format: format_json,
  )
}

// ============================================================================
// Default Formatter
// ============================================================================

/// Format a log record as a JSON string using the default format.
/// This is equivalent to using `standard_builder() |> build()`.
pub fn format_json(record: LogRecord) -> String {
  let base_fields = [
    #("timestamp", json.string(record.timestamp)),
    #("level", json.string(level.to_string_lowercase(record.level))),
    #("logger", json.string(record.logger_name)),
    #("message", json.string(record.message)),
  ]

  let metadata_fields =
    list.map(record.metadata, fn(pair) { #(pair.0, json.string(pair.1)) })

  list.append(base_fields, metadata_fields)
  |> json.object
  |> json.to_string
}
