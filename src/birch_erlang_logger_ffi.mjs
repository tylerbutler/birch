// JavaScript FFI stubs for birch's Erlang :logger integration.
//
// Erlang's :logger is not available on the JavaScript target.
// These stubs provide graceful fallback behavior:
// - logger_log: Falls back to console output
// - install_formatter/remove_formatter: Return errors indicating unavailability

import { Ok, Error } from "./gleam.mjs";

/**
 * Log a message (fallback to console on JavaScript).
 *
 * Since Erlang's :logger is not available, this falls back to console output.
 * The level is used to determine which console method to use.
 *
 * @param {object} level - Gleam ErlangLevel type
 * @param {string} message - The formatted log message
 */
export function logger_log(level, message) {
  // Map Erlang level to console method
  const levelName = getLevelName(level);

  switch (levelName) {
    case "emergency":
    case "alert":
    case "critical":
    case "error":
      console.error(message);
      break;
    case "warning":
      console.warn(message);
      break;
    case "debug":
      console.debug(message);
      break;
    case "info":
    case "notice":
    default:
      console.log(message);
      break;
  }

  return undefined; // nil
}

/**
 * Extract level name from Gleam ErlangLevel type.
 * @param {object} level - Gleam ErlangLevel variant
 * @returns {string} Level name
 */
function getLevelName(level) {
  if (typeof level === "object" && level !== null) {
    if ("$" in level) {
      switch (level.$) {
        case "ErlangEmergency":
          return "emergency";
        case "ErlangAlert":
          return "alert";
        case "ErlangCritical":
          return "critical";
        case "ErlangError":
          return "error";
        case "ErlangWarning":
          return "warning";
        case "ErlangNotice":
          return "notice";
        case "ErlangInfo":
          return "info";
        case "ErlangDebug":
          return "debug";
      }
    }
  }
  return "info"; // Default
}

/**
 * Install birch as a :logger formatter.
 *
 * This is not available on JavaScript - always returns an error.
 *
 * @param {string} _handlerId - Handler ID (unused on JS)
 * @param {function} _formatFn - Format callback (unused on JS)
 * @returns {Error} Always returns an error
 */
export function install_formatter(_handlerId, _formatFn) {
  return new Error(
    "erlang:logger is not available on JavaScript target"
  );
}

/**
 * Remove birch formatter from a :logger handler.
 *
 * This is not available on JavaScript - always returns an error.
 *
 * @param {string} _handlerId - Handler ID (unused on JS)
 * @returns {Error} Always returns an error
 */
export function remove_formatter(_handlerId) {
  return new Error(
    "erlang:logger is not available on JavaScript target"
  );
}
