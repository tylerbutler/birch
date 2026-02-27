//// Ergonomic metadata constructors.
////
//// Provides typed helper functions for building metadata entries,
//// following the same pattern as `gleam/json`.
////
//// ## Example
////
//// ```gleam
//// import birch/meta
//// import birch/logger
////
//// logger.info(lgr, "Request processed", [
////   meta.string("request_id", request_id),
////   meta.int("status", 200),
////   meta.float("duration_ms", 42.5),
////   meta.bool("cached", True),
//// ])
//// ```

import birch/record.{type MetadataValue, BoolVal, FloatVal, IntVal, StringVal}

/// Create a string metadata entry.
pub fn string(key: String, value: String) -> #(String, MetadataValue) {
  #(key, StringVal(value))
}

/// Create an integer metadata entry.
pub fn int(key: String, value: Int) -> #(String, MetadataValue) {
  #(key, IntVal(value))
}

/// Create a float metadata entry.
pub fn float(key: String, value: Float) -> #(String, MetadataValue) {
  #(key, FloatVal(value))
}

/// Create a boolean metadata entry.
pub fn bool(key: String, value: Bool) -> #(String, MetadataValue) {
  #(key, BoolVal(value))
}
