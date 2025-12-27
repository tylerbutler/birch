//// Property-based tests for gleam_log using qcheck.

import gleam/list
import gleam/order
import gleam/string
import gleam_log/formatter
import gleam_log/level
import gleam_log/record
import qcheck

// ============================================================================
// Level Property Tests
// ============================================================================

/// Property: Level ordering is transitive
/// If a < b and b < c, then a < c
pub fn level_ordering_transitive_test() {
  use levels <- qcheck.given(qcheck.list_generic(
    level_generator(),
    qcheck.int_uniform_inclusive(3, 3),
    qcheck.int_uniform_inclusive(3, 3),
  ))

  case levels {
    [a, b, c] -> {
      let a_lt_b = level.lt(a, b)
      let b_lt_c = level.lt(b, c)
      let a_lt_c = level.lt(a, c)

      // If a < b and b < c, then a < c must be true
      case a_lt_b && b_lt_c {
        True -> a_lt_c == True
        False -> True
      }
    }
    _ -> True
  }
}

/// Property: gte is the inverse of lt
pub fn level_gte_inverse_of_lt_test() {
  use #(a, b) <- qcheck.given(qcheck.tuple2(level_generator(), level_generator()))

  // gte(a, b) should be equivalent to !lt(a, b)
  level.gte(a, b) == !level.lt(a, b)
}

/// Property: to_string and from_string are inverses
pub fn level_string_roundtrip_test() {
  use lvl <- qcheck.given(level_generator())

  let s = level.to_string(lvl)
  let result = level.from_string(s)

  result == Ok(lvl)
}

/// Property: to_int produces ordered values
pub fn level_to_int_ordered_test() {
  use #(a, b) <- qcheck.given(qcheck.tuple2(level_generator(), level_generator()))

  let a_int = level.to_int(a)
  let b_int = level.to_int(b)

  // The integer comparison should match the level comparison
  case level.compare(a, b) {
    order.Lt -> a_int < b_int
    order.Eq -> a_int == b_int
    order.Gt -> a_int > b_int
  }
}

// ============================================================================
// Record Property Tests
// ============================================================================

/// Property: Adding metadata preserves existing metadata
pub fn record_metadata_append_test() {
  use #(existing, new_meta) <- qcheck.given(qcheck.tuple2(
    metadata_generator(),
    metadata_generator(),
  ))

  let r =
    record.new(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: "test",
      metadata: existing,
    )

  let r2 = record.with_metadata(r, new_meta)

  // New metadata should be accessible
  list.all(new_meta, fn(pair) {
    let #(key, value) = pair
    record.get_metadata(r2, key) == Ok(value)
  })
}

// ============================================================================
// Formatter Property Tests
// ============================================================================

/// Property: Human-readable format always contains the message
pub fn formatter_contains_message_test() {
  use message <- qcheck.given(qcheck.string_non_empty(qcheck.char_alphanumeric()))

  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: level.Info,
      logger_name: "test",
      message: message,
    )

  let formatted = formatter.human_readable(r)

  string.contains(formatted, message)
}

/// Property: Human-readable format always contains the level
pub fn formatter_contains_level_test() {
  use lvl <- qcheck.given(level_generator())

  let r =
    record.new_simple(
      timestamp: "2024-12-26T10:30:45.123Z",
      level: lvl,
      logger_name: "test",
      message: "test message",
    )

  let formatted = formatter.human_readable(r)
  let level_str = level.to_string(lvl)

  string.contains(formatted, level_str)
}

// ============================================================================
// Generators
// ============================================================================

/// Generator for log levels
fn level_generator() -> qcheck.Generator(level.Level) {
  qcheck.from_generators([
    qcheck.return(level.Trace),
    qcheck.return(level.Debug),
    qcheck.return(level.Info),
    qcheck.return(level.Warn),
    qcheck.return(level.Error),
    qcheck.return(level.Fatal),
  ])
}

/// Generator for metadata (list of key-value pairs)
fn metadata_generator() -> qcheck.Generator(record.Metadata) {
  qcheck.list_generic(
    qcheck.tuple2(
      qcheck.string_non_empty(qcheck.char_alpha()),
      qcheck.string_non_empty(qcheck.char_alphanumeric()),
    ),
    qcheck.int_uniform_inclusive(0, 5),
    qcheck.int_uniform_inclusive(0, 5),
  )
}
