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

/// January 1st timestamp for testing month edge case
/// 2024-01-15T08:00:00Z (Monday)
fn january_timestamp() {
  timestamp.from_unix_seconds(1_705_305_600)
}

/// February timestamp for testing month edge case
/// 2024-02-29T12:00:00Z (Thursday, leap year)
fn february_leap_timestamp() {
  timestamp.from_unix_seconds(1_709_208_000)
}

// ============================================================================
// now() Tests
// ============================================================================

pub fn now_returns_timestamp_test() {
  let ts = time.now()
  // Just verify it returns something we can convert to unix
  let unix = time.to_unix(ts)
  // Should be after 2020 (unix timestamp > 1577836800)
  { unix > 1_577_836_800 }
  |> should.be_true
}

// ============================================================================
// to_unix_milli() Tests
// ============================================================================

pub fn to_unix_milli_known_timestamp_test() {
  let ts = known_timestamp()
  time.to_unix_milli(ts)
  |> should.equal(1_735_209_045_123)
}

pub fn to_unix_milli_epoch_test() {
  let ts = epoch_timestamp()
  time.to_unix_milli(ts)
  |> should.equal(0)
}

pub fn to_unix_milli_with_nanoseconds_test() {
  // 1 second + 500 milliseconds = 1500 milliseconds
  let ts = timestamp.from_unix_seconds_and_nanoseconds(1, 500_000_000)
  time.to_unix_milli(ts)
  |> should.equal(1500)
}

// ============================================================================
// to_unix() Tests
// ============================================================================

pub fn to_unix_known_timestamp_test() {
  let ts = known_timestamp()
  time.to_unix(ts)
  |> should.equal(1_735_209_045)
}

pub fn to_unix_epoch_test() {
  let ts = epoch_timestamp()
  time.to_unix(ts)
  |> should.equal(0)
}

pub fn to_unix_ignores_nanoseconds_test() {
  let ts = timestamp.from_unix_seconds_and_nanoseconds(100, 999_999_999)
  time.to_unix(ts)
  |> should.equal(100)
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

// ============================================================================
// to_naive() Tests
// ============================================================================

pub fn to_naive_known_timestamp_test() {
  let ts = known_timestamp()
  time.to_naive(ts)
  |> should.equal("2024-12-26T10:30:45.123")
}

pub fn to_naive_epoch_test() {
  let ts = epoch_timestamp()
  time.to_naive(ts)
  |> should.equal("1970-01-01T00:00:00.000")
}

pub fn to_naive_no_timezone_suffix_test() {
  let ts = known_timestamp()
  let result = time.to_naive(ts)
  // Should NOT have Z or timezone offset
  result
  |> string.ends_with("Z")
  |> should.be_false

  result
  |> string.contains("+")
  |> should.be_false
}

pub fn to_naive_includes_milliseconds_test() {
  // 999 milliseconds
  let ts = timestamp.from_unix_seconds_and_nanoseconds(0, 999_000_000)
  let result = time.to_naive(ts)
  result
  |> string.ends_with(".999")
  |> should.be_true
}

pub fn to_naive_pads_single_digit_millis_test() {
  // 5 milliseconds = should be .005
  let ts = timestamp.from_unix_seconds_and_nanoseconds(0, 5_000_000)
  let result = time.to_naive(ts)
  result
  |> string.ends_with(".005")
  |> should.be_true
}

pub fn to_naive_pads_double_digit_millis_test() {
  // 50 milliseconds = should be .050
  let ts = timestamp.from_unix_seconds_and_nanoseconds(0, 50_000_000)
  let result = time.to_naive(ts)
  result
  |> string.ends_with(".050")
  |> should.be_true
}

// ============================================================================
// to_http() Tests
// ============================================================================

pub fn to_http_known_timestamp_test() {
  let ts = known_timestamp()
  time.to_http(ts)
  |> should.equal("Thu, 26 Dec 2024 10:30:45 GMT")
}

pub fn to_http_epoch_test() {
  let ts = epoch_timestamp()
  time.to_http(ts)
  |> should.equal("Thu, 01 Jan 1970 00:00:00 GMT")
}

pub fn to_http_january_test() {
  let ts = january_timestamp()
  let result = time.to_http(ts)
  // January 15, 2024 was a Monday
  result
  |> should.equal("Mon, 15 Jan 2024 08:00:00 GMT")
}

pub fn to_http_february_leap_year_test() {
  let ts = february_leap_timestamp()
  let result = time.to_http(ts)
  // February 29, 2024 was a Thursday (leap year)
  result
  |> should.equal("Thu, 29 Feb 2024 12:00:00 GMT")
}

pub fn to_http_all_weekdays_test() {
  // Test each day of a week to cover all weekday branches
  // Week of 2024-01-01 (Monday) through 2024-01-07 (Sunday)
  let monday = timestamp.from_unix_seconds(1_704_067_200)
  // 2024-01-01
  let tuesday = timestamp.from_unix_seconds(1_704_153_600)
  // 2024-01-02
  let wednesday = timestamp.from_unix_seconds(1_704_240_000)
  // 2024-01-03
  let thursday = timestamp.from_unix_seconds(1_704_326_400)
  // 2024-01-04
  let friday = timestamp.from_unix_seconds(1_704_412_800)
  // 2024-01-05
  let saturday = timestamp.from_unix_seconds(1_704_499_200)
  // 2024-01-06
  let sunday = timestamp.from_unix_seconds(1_704_585_600)
  // 2024-01-07

  time.to_http(monday)
  |> string.starts_with("Mon,")
  |> should.be_true
  time.to_http(tuesday)
  |> string.starts_with("Tue,")
  |> should.be_true
  time.to_http(wednesday)
  |> string.starts_with("Wed,")
  |> should.be_true
  time.to_http(thursday)
  |> string.starts_with("Thu,")
  |> should.be_true
  time.to_http(friday)
  |> string.starts_with("Fri,")
  |> should.be_true
  time.to_http(saturday)
  |> string.starts_with("Sat,")
  |> should.be_true
  time.to_http(sunday)
  |> string.starts_with("Sun,")
  |> should.be_true
}

pub fn to_http_all_months_test() {
  // Test one day from each month to cover all month abbreviations
  // Using 15th of each month in 2024
  let months = [
    #(1_705_305_600, "Jan"),
    // Jan 15
    #(1_707_984_000, "Feb"),
    // Feb 15
    #(1_710_489_600, "Mar"),
    // Mar 15
    #(1_713_168_000, "Apr"),
    // Apr 15
    #(1_715_760_000, "May"),
    // May 15
    #(1_718_438_400, "Jun"),
    // Jun 15
    #(1_721_030_400, "Jul"),
    // Jul 15
    #(1_723_708_800, "Aug"),
    // Aug 15
    #(1_726_387_200, "Sep"),
    // Sep 15
    #(1_728_979_200, "Oct"),
    // Oct 15
    #(1_731_657_600, "Nov"),
    // Nov 15
    #(1_734_249_600, "Dec"),
    // Dec 15
  ]

  months
  |> list.each(fn(pair) {
    let #(unix, month_abbrev) = pair
    let ts = timestamp.from_unix_seconds(unix)
    let result = time.to_http(ts)
    result
    |> string.contains(month_abbrev)
    |> should.be_true
  })
}

import gleam/list

// ============================================================================
// to_date_string() Tests
// ============================================================================

pub fn to_date_string_known_timestamp_test() {
  let ts = known_timestamp()
  time.to_date_string(ts)
  |> should.equal("2024-12-26")
}

pub fn to_date_string_epoch_test() {
  let ts = epoch_timestamp()
  time.to_date_string(ts)
  |> should.equal("1970-01-01")
}

pub fn to_date_string_pads_month_test() {
  // January should be 01
  let ts = january_timestamp()
  let result = time.to_date_string(ts)
  result
  |> string.contains("-01-")
  |> should.be_true
}

pub fn to_date_string_pads_day_test() {
  // Day 1 should be 01
  let ts = epoch_timestamp()
  let result = time.to_date_string(ts)
  result
  |> string.ends_with("-01")
  |> should.be_true
}

// ============================================================================
// to_time_string() Tests
// ============================================================================

pub fn to_time_string_known_timestamp_test() {
  let ts = known_timestamp()
  time.to_time_string(ts)
  |> should.equal("10:30:45")
}

pub fn to_time_string_epoch_test() {
  let ts = epoch_timestamp()
  time.to_time_string(ts)
  |> should.equal("00:00:00")
}

pub fn to_time_string_pads_hours_test() {
  // 5 AM should be 05
  let ts = timestamp.from_unix_seconds(5 * 3600)
  let result = time.to_time_string(ts)
  result
  |> string.starts_with("05:")
  |> should.be_true
}

pub fn to_time_string_pads_minutes_test() {
  // 5 minutes should be :05:
  let ts = timestamp.from_unix_seconds(5 * 60)
  let result = time.to_time_string(ts)
  result
  |> string.contains(":05:")
  |> should.be_true
}

pub fn to_time_string_pads_seconds_test() {
  // 5 seconds should be :05
  let ts = timestamp.from_unix_seconds(5)
  let result = time.to_time_string(ts)
  result
  |> string.ends_with(":05")
  |> should.be_true
}

pub fn to_time_string_no_milliseconds_test() {
  // Should NOT include milliseconds (unlike to_naive)
  let ts = timestamp.from_unix_seconds_and_nanoseconds(0, 123_000_000)
  let result = time.to_time_string(ts)
  result
  |> string.contains(".")
  |> should.be_false
}
