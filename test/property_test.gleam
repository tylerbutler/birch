//// Property-based tests for gleam_log using qcheck.
////
//// These tests verify invariants that should hold for all inputs.

import gleam/list
import gleam/order
import gleam/string
import gleam_log/formatter
import gleam_log/level.{Debug, Err, Fatal, Info, Trace, Warn}
import gleam_log/record
import gleeunit/should
import qcheck

// ============================================================================
// Level Generators
// ============================================================================

/// Generator for all possible log levels
fn level_generator() -> qcheck.Generator(level.Level) {
  qcheck.from_generators([
    qcheck.return(Trace),
    qcheck.return(Debug),
    qcheck.return(Info),
    qcheck.return(Warn),
    qcheck.return(Err),
    qcheck.return(Fatal),
  ])
}

// ============================================================================
// Level Property Tests
// ============================================================================

/// Property: to_int produces unique values for each level
pub fn level_to_int_unique_test() {
  let all_levels = [Trace, Debug, Info, Warn, Err, Fatal]
  let ints = list.map(all_levels, level.to_int)
  let unique_ints = list.unique(ints)

  list.length(unique_ints)
  |> should.equal(list.length(all_levels))
}

/// Property: to_int values are strictly increasing with severity
pub fn level_to_int_ordering_test() {
  let all_levels = [Trace, Debug, Info, Warn, Err, Fatal]
  let ints = list.map(all_levels, level.to_int)

  // Check that each successive int is greater than the previous
  ints
  |> list.window_by_2
  |> list.all(fn(pair) { pair.0 < pair.1 })
  |> should.be_true
}

/// Property: from_string(to_string_lowercase(level)) == Ok(level) for all levels
pub fn level_roundtrip_lowercase_test() {
  use lvl <- qcheck.given(level_generator())

  lvl
  |> level.to_string_lowercase
  |> level.from_string
  |> should.equal(Ok(lvl))
}

/// Property: from_string is case insensitive
pub fn level_from_string_case_insensitive_test() {
  use lvl <- qcheck.given(level_generator())

  let lowercase_str = level.to_string_lowercase(lvl)
  let uppercase_str = level.to_string(lvl)

  level.from_string(lowercase_str)
  |> should.equal(level.from_string(uppercase_str))
}

/// Property: compare is reflexive (compare(x, x) == Eq)
pub fn level_compare_reflexive_test() {
  use lvl <- qcheck.given(level_generator())

  level.compare(lvl, lvl)
  |> should.equal(order.Eq)
}

/// Property: gte is reflexive (gte(x, x) == True)
pub fn level_gte_reflexive_test() {
  use lvl <- qcheck.given(level_generator())

  level.gte(lvl, lvl)
  |> should.be_true
}

/// Property: lte is reflexive (lte(x, x) == True)
pub fn level_lte_reflexive_test() {
  use lvl <- qcheck.given(level_generator())

  level.lte(lvl, lvl)
  |> should.be_true
}

/// Property: gt and lt are irreflexive
pub fn level_gt_lt_irreflexive_test() {
  use lvl <- qcheck.given(level_generator())

  level.gt(lvl, lvl)
  |> should.be_false

  level.lt(lvl, lvl)
  |> should.be_false
}

/// Property: gte(a, b) is consistent with compare(a, b)
pub fn level_gte_consistent_with_compare_test() {
  use #(a, b) <- qcheck.given(qcheck.tuple2(
    level_generator(),
    level_generator(),
  ))

  let cmp = level.compare(a, b)
  let gte_result = level.gte(a, b)

  case cmp {
    order.Gt | order.Eq -> gte_result |> should.be_true
    order.Lt -> gte_result |> should.be_false
  }
}

/// Property: gt(a, b) implies gte(a, b)
pub fn level_gt_implies_gte_test() {
  use #(a, b) <- qcheck.given(qcheck.tuple2(
    level_generator(),
    level_generator(),
  ))

  case level.gt(a, b) {
    True -> level.gte(a, b) |> should.be_true
    False -> Nil
  }
}

/// Property: compare is antisymmetric
pub fn level_compare_antisymmetric_test() {
  use #(a, b) <- qcheck.given(qcheck.tuple2(
    level_generator(),
    level_generator(),
  ))

  let ab = level.compare(a, b)
  let ba = level.compare(b, a)

  case ab {
    order.Lt -> ba |> should.equal(order.Gt)
    order.Eq -> ba |> should.equal(order.Eq)
    order.Gt -> ba |> should.equal(order.Lt)
  }
}

// ============================================================================
// Record Property Tests
// ============================================================================

/// Generator for simple metadata (non-empty key-value pairs)
fn metadata_generator() -> qcheck.Generator(record.Metadata) {
  let pair_gen =
    qcheck.tuple2(
      qcheck.string_non_empty_from(qcheck.codepoint_alphanumeric()),
      qcheck.string_from(qcheck.codepoint_printable_ascii()),
    )

  qcheck.list_generic(pair_gen, qcheck.int_uniform_inclusive(0, 5))
}

/// Generator for log records
fn log_record_generator() -> qcheck.Generator(record.LogRecord) {
  use timestamp <- qcheck.map(qcheck.string_non_empty())
  use lvl <- qcheck.bind(level_generator())
  use logger_name <- qcheck.bind(
    qcheck.string_non_empty_from(qcheck.codepoint_alphanumeric()),
  )
  use message <- qcheck.bind(qcheck.string())
  use metadata <- qcheck.bind(metadata_generator())

  record.new(
    timestamp: timestamp,
    level: lvl,
    logger_name: logger_name,
    message: message,
    metadata: metadata,
  )
}

/// Property: get_metadata returns first matching key
pub fn record_get_metadata_first_match_test() {
  use #(key, value1, value2) <- qcheck.given(qcheck.tuple3(
    qcheck.string_non_empty_from(qcheck.codepoint_alphanumeric()),
    qcheck.string_from(qcheck.codepoint_printable_ascii()),
    qcheck.string_from(qcheck.codepoint_printable_ascii()),
  ))

  let r =
    record.new(
      timestamp: "2024-01-01T00:00:00Z",
      level: Info,
      logger_name: "test",
      message: "test",
      metadata: [#(key, value1), #(key, value2)],
    )

  // Should return the first occurrence
  record.get_metadata(r, key)
  |> should.equal(Ok(value1))
}

/// Property: get_metadata returns Error for non-existent keys
pub fn record_get_metadata_missing_test() {
  use rec <- qcheck.given(log_record_generator())

  // Use a key that's very unlikely to be in the metadata
  record.get_metadata(rec, "___nonexistent_key_12345___")
  |> should.equal(Error(Nil))
}

/// Property: with_metadata prepends new metadata
pub fn record_with_metadata_prepends_test() {
  use #(key, value) <- qcheck.given(qcheck.tuple2(
    qcheck.string_non_empty_from(qcheck.codepoint_alphanumeric()),
    qcheck.string_from(qcheck.codepoint_printable_ascii()),
  ))

  let r =
    record.new_simple(
      timestamp: "2024-01-01T00:00:00Z",
      level: Info,
      logger_name: "test",
      message: "test",
    )
    |> record.with_metadata([#(key, value)])

  record.get_metadata(r, key)
  |> should.equal(Ok(value))
}

/// Property: new_simple creates record with empty metadata
pub fn record_new_simple_empty_metadata_test() {
  use #(timestamp, message) <- qcheck.given(qcheck.tuple2(
    qcheck.string_non_empty(),
    qcheck.string(),
  ))
  use lvl <- qcheck.bind(level_generator())

  let r =
    record.new_simple(
      timestamp: timestamp,
      level: lvl,
      logger_name: "test",
      message: message,
    )

  r.metadata
  |> should.equal([])
}

// ============================================================================
// Formatter Property Tests
// ============================================================================

/// Property: simple formatter includes level and message
pub fn formatter_simple_contains_level_and_message_test() {
  use rec <- qcheck.given(log_record_generator())

  let formatted = formatter.simple(rec)
  let level_str = level.to_string(rec.level)

  formatted
  |> string.contains(level_str)
  |> should.be_true

  formatted
  |> string.contains(rec.message)
  |> should.be_true
}

/// Property: human_readable formatter includes timestamp
pub fn formatter_human_readable_contains_timestamp_test() {
  use rec <- qcheck.given(log_record_generator())

  let formatted = formatter.human_readable(rec)

  formatted
  |> string.contains(rec.timestamp)
  |> should.be_true
}

/// Property: human_readable formatter includes logger name
pub fn formatter_human_readable_contains_logger_test() {
  use rec <- qcheck.given(log_record_generator())

  let formatted = formatter.human_readable(rec)

  formatted
  |> string.contains(rec.logger_name)
  |> should.be_true
}
