// JavaScript test FFI stubs for birch :logger integration tests.
//
// These are no-ops since :logger is not available on JavaScript.
// The tests using these helpers are guarded by is_erlang_target() checks.

export function new_capture_buffer() {
  return { entries: [] };
}

export function append_to_buffer(buffer, value) {
  buffer.entries.push(value);
  return undefined;
}

export function get_buffer_contents(buffer) {
  return buffer.entries.join("\n");
}

export function sleep(_ms) {
  return undefined;
}

export function otp_logger_warning(_message) {
  return undefined;
}

export function otp_logger_report_with_cb() {
  return undefined;
}
