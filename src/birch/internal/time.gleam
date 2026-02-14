//// Internal time formatting utilities.
////
//// Provides formatting functions for timestamps using gleam_time.

import gleam/time/calendar
import gleam/time/timestamp.{type Timestamp}

/// Get the current timestamp.
pub fn now() -> Timestamp {
  timestamp.system_time()
}

/// Format timestamp as ISO 8601 with timezone (RFC 3339).
/// Example: "2024-12-26T10:30:45.123Z"
pub fn to_iso8601(ts: Timestamp) -> String {
  timestamp.to_rfc3339(ts, calendar.utc_offset)
}
