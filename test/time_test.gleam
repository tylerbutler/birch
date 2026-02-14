import birch/internal/time
import gleam/string
import gleam/time/timestamp
import gleeunit/should

// ============================================================================
// Test Fixtures
// ============================================================================

/// Known timestamp: 2024-12-26T10:30:45.123Z (Unix: 1735209045 seconds, 123000000 nanoseconds)
fn known_timestamp() {
  timestamp.from_unix_seconds_and_nanoseconds(1_735_209_045, 123_000_000)
}

/// Epoch timestamp: 1970-01-01T00:00:00.000Z
fn epoch_timestamp() {
  timestamp.from_unix_seconds(0)
}

// ============================================================================
// now() Tests
// ============================================================================

pub fn now_returns_timestamp_test() {
  let ts = time.now()
  // Just verify it returns something we can convert to unix
  let #(seconds, _nanoseconds) = timestamp.to_unix_seconds_and_nanoseconds(ts)
  // Should be after 2020 (unix timestamp > 1577836800)
  { seconds > 1_577_836_800 }
  |> should.be_true
}

// ============================================================================
// to_iso8601() Tests
// ============================================================================

pub fn to_iso8601_known_timestamp_test() {
  let ts = known_timestamp()
  let result = time.to_iso8601(ts)
  // Should be RFC 3339 format with Z suffix
  result
  |> string.starts_with("2024-12-26T10:30:45")
  |> should.be_true

  result
  |> string.ends_with("Z")
  |> should.be_true
}

pub fn to_iso8601_epoch_test() {
  let ts = epoch_timestamp()
  let result = time.to_iso8601(ts)
  result
  |> string.starts_with("1970-01-01T00:00:00")
  |> should.be_true
}
