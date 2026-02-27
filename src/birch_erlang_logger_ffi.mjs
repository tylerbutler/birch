// JavaScript FFI stubs for birch's Erlang :logger integration.
//
// Erlang's :logger is not available on the JavaScript target.
// These stubs provide graceful fallback behavior:
// - logger_log: Falls back to console output
// - install_formatter/remove_formatter: Return errors indicating unavailability

import { Ok, Error as GleamError } from "./gleam.mjs";

const UNAVAILABLE_ERROR = new GleamError(
  "erlang:logger is not available on JavaScript target",
);

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

const LEVEL_NAMES = {
  erlang_emergency: "emergency",
  erlang_alert: "alert",
  erlang_critical: "critical",
  erlang_error: "error",
  erlang_warning: "warning",
  erlang_notice: "notice",
  erlang_info: "info",
  erlang_debug: "debug",
  // Also support PascalCase variant names (legacy codegen)
  ErlangEmergency: "emergency",
  ErlangAlert: "alert",
  ErlangCritical: "critical",
  ErlangError: "error",
  ErlangWarning: "warning",
  ErlangNotice: "notice",
  ErlangInfo: "info",
  ErlangDebug: "debug",
};

/**
 * Extract level name from Gleam ErlangLevel type.
 *
 * TODO: Revisit this and see if there is a better way to accomplish this.
 * Relies on Gleam's JS codegen representing custom type variants as objects
 * with a `$` property containing the variant name (e.g., "ErlangEmergency").
 * If this codegen convention changes, all levels will silently fall through
 * to the "info" default.
 *
 * @param {object} level - Gleam ErlangLevel variant
 * @returns {string} Level name
 */
function getLevelName(level) {
  if (typeof level === "object" && level !== null && "$" in level) {
    return LEVEL_NAMES[level.$] ?? "info";
  }
  return "info";
}

/**
 * Check if the birch formatter is configured on the default :logger handler.
 *
 * Always returns false on JavaScript since :logger is not available.
 */
export function is_formatter_configured() {
  return false;
}

/**
 * Install birch as a :logger formatter.
 * Not available on JavaScript - always returns an error.
 */
export function install_formatter(_handlerId, _formatFn) {
  return UNAVAILABLE_ERROR;
}

/**
 * Remove birch formatter from a :logger handler.
 * Not available on JavaScript - always returns an error.
 */
export function remove_formatter(_handlerId) {
  return UNAVAILABLE_ERROR;
}
