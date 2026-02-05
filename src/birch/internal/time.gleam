//// Internal time formatting utilities.
////
//// Provides formatting functions for timestamps using gleam_time,
//// including formats not directly supported by the library.

import gleam/int
import gleam/string
import gleam/time/calendar
import gleam/time/timestamp.{type Timestamp}

/// Get the current timestamp.
pub fn now() -> Timestamp {
  timestamp.system_time()
}

/// Convert a timestamp to Unix milliseconds.
pub fn to_unix_milli(ts: Timestamp) -> Int {
  let #(seconds, nanoseconds) = timestamp.to_unix_seconds_and_nanoseconds(ts)
  seconds * 1000 + nanoseconds / 1_000_000
}

/// Convert a timestamp to Unix seconds.
pub fn to_unix(ts: Timestamp) -> Int {
  let #(seconds, _nanoseconds) = timestamp.to_unix_seconds_and_nanoseconds(ts)
  seconds
}

/// Format timestamp as ISO 8601 with timezone (RFC 3339).
/// Example: "2024-12-26T10:30:45.123Z"
pub fn to_iso8601(ts: Timestamp) -> String {
  timestamp.to_rfc3339(ts, calendar.utc_offset)
}

/// Format timestamp as naive ISO 8601 (no timezone).
/// Example: "2024-12-26T10:30:45.123"
pub fn to_naive(ts: Timestamp) -> String {
  let #(date, time) = timestamp.to_calendar(ts, calendar.utc_offset)
  let date_str = format_date(date)
  let time_str = format_time_with_millis(time)
  date_str <> "T" <> time_str
}

/// Format timestamp as HTTP date (RFC 2616).
/// Example: "Thu, 26 Dec 2024 10:30:45 GMT"
pub fn to_http(ts: Timestamp) -> String {
  let #(date, time) = timestamp.to_calendar(ts, calendar.utc_offset)
  let weekday = weekday_abbrev(calculate_weekday(date))
  let day = pad2(date.day)
  let month = month_abbrev(date.month)
  let year = int.to_string(date.year)
  let time_str = format_time_hms(time)
  weekday
  <> ", "
  <> day
  <> " "
  <> month
  <> " "
  <> year
  <> " "
  <> time_str
  <> " GMT"
}

/// Format timestamp as date only.
/// Example: "2024-12-26"
pub fn to_date_string(ts: Timestamp) -> String {
  let #(date, _time) = timestamp.to_calendar(ts, calendar.utc_offset)
  format_date(date)
}

/// Format timestamp as time only.
/// Example: "10:30:45"
pub fn to_time_string(ts: Timestamp) -> String {
  let #(_date, time) = timestamp.to_calendar(ts, calendar.utc_offset)
  format_time_hms(time)
}

// Helper functions

fn format_date(date: calendar.Date) -> String {
  let year = int.to_string(date.year)
  let month = pad2(month_to_int(date.month))
  let day = pad2(date.day)
  year <> "-" <> month <> "-" <> day
}

fn format_time_with_millis(time: calendar.TimeOfDay) -> String {
  let hours = pad2(time.hours)
  let minutes = pad2(time.minutes)
  let seconds = pad2(time.seconds)
  let millis = pad3(time.nanoseconds / 1_000_000)
  hours <> ":" <> minutes <> ":" <> seconds <> "." <> millis
}

fn format_time_hms(time: calendar.TimeOfDay) -> String {
  let hours = pad2(time.hours)
  let minutes = pad2(time.minutes)
  let seconds = pad2(time.seconds)
  hours <> ":" <> minutes <> ":" <> seconds
}

fn pad2(n: Int) -> String {
  let s = int.to_string(n)
  case string.length(s) {
    1 -> "0" <> s
    _ -> s
  }
}

fn pad3(n: Int) -> String {
  let s = int.to_string(n)
  case string.length(s) {
    1 -> "00" <> s
    2 -> "0" <> s
    _ -> s
  }
}

fn month_to_int(month: calendar.Month) -> Int {
  case month {
    calendar.January -> 1
    calendar.February -> 2
    calendar.March -> 3
    calendar.April -> 4
    calendar.May -> 5
    calendar.June -> 6
    calendar.July -> 7
    calendar.August -> 8
    calendar.September -> 9
    calendar.October -> 10
    calendar.November -> 11
    calendar.December -> 12
  }
}

fn month_abbrev(month: calendar.Month) -> String {
  case month {
    calendar.January -> "Jan"
    calendar.February -> "Feb"
    calendar.March -> "Mar"
    calendar.April -> "Apr"
    calendar.May -> "May"
    calendar.June -> "Jun"
    calendar.July -> "Jul"
    calendar.August -> "Aug"
    calendar.September -> "Sep"
    calendar.October -> "Oct"
    calendar.November -> "Nov"
    calendar.December -> "Dec"
  }
}

/// Weekday type for internal use
type Weekday {
  Monday
  Tuesday
  Wednesday
  Thursday
  Friday
  Saturday
  Sunday
}

/// Calculate the day of the week using Zeller's congruence (Gregorian calendar).
/// Returns 0 for Saturday, 1 for Sunday, 2 for Monday, etc.
fn calculate_weekday(date: calendar.Date) -> Weekday {
  let month_num = month_to_int(date.month)
  // Adjust month and year for Zeller's formula (Jan and Feb are months 13 and 14 of prev year)
  let #(m, y) = case month_num {
    1 -> #(13, date.year - 1)
    2 -> #(14, date.year - 1)
    _ -> #(month_num, date.year)
  }
  let q = date.day
  let k = y % 100
  let j = y / 100

  // Zeller's formula: h = (q + floor(13(m+1)/5) + K + floor(K/4) + floor(J/4) - 2J) mod 7
  let h = { q + { 13 * { m + 1 } / 5 } + k + { k / 4 } + { j / 4 } - 2 * j } % 7

  // Convert to our Weekday type (Zeller: 0=Sat, 1=Sun, 2=Mon, ...)
  // Handle negative modulo result
  let h_normalized = case h < 0 {
    True -> h + 7
    False -> h
  }

  case h_normalized {
    0 -> Saturday
    1 -> Sunday
    2 -> Monday
    3 -> Tuesday
    4 -> Wednesday
    5 -> Thursday
    6 -> Friday
    _ -> Monday
  }
}

fn weekday_abbrev(weekday: Weekday) -> String {
  case weekday {
    Monday -> "Mon"
    Tuesday -> "Tue"
    Wednesday -> "Wed"
    Thursday -> "Thu"
    Friday -> "Fri"
    Saturday -> "Sat"
    Sunday -> "Sun"
  }
}
