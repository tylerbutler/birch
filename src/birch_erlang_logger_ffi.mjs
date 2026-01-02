// JavaScript FFI stubs for gleam_log's Erlang :logger integration.
//
// Erlang's :logger is not available on the JavaScript target.
// These stubs provide graceful fallback behavior:
// - logger_log: Falls back to console output
// - install_handler/uninstall_handler: Return errors indicating unavailability

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
  // Level is a Gleam record like {erlang_info}
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
  // Gleam variants are objects with a constructor property
  // e.g., { constructor: "ErlangInfo" } or similar
  if (typeof level === "object" && level !== null) {
    // Check for the variant type
    if ("$" in level) {
      // New Gleam format uses $ for variant name
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
 * Install gleam_log as an Erlang :logger handler.
 *
 * This is not available on JavaScript - always returns an error.
 *
 * @param {string} handlerId - Handler ID (unused on JS)
 * @returns {Error} Always returns an error
 */
export function install_handler(_handlerId) {
  return new Error("erlang:logger is not available on JavaScript target");
}

/**
 * Uninstall an Erlang :logger handler.
 *
 * This is not available on JavaScript - always returns an error.
 *
 * @param {string} handlerId - Handler ID (unused on JS)
 * @returns {Error} Always returns an error
 */
export function uninstall_handler(_handlerId) {
  return new Error("erlang:logger is not available on JavaScript target");
}
