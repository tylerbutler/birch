// JavaScript FFI for gleam_log

/**
 * Get current timestamp in ISO 8601 format with milliseconds.
 * @returns {string} Timestamp like "2024-12-26T10:30:45.123Z"
 */
export function timestamp_iso8601() {
  return new Date().toISOString();
}

/**
 * Write a string to stdout with newline.
 * Works in Node.js, Deno, and Bun.
 * @param {string} message
 */
export function write_stdout(message) {
  // Node.js and Bun
  if (typeof process !== "undefined" && process.stdout) {
    process.stdout.write(message + "\n");
  }
  // Deno
  else if (typeof Deno !== "undefined") {
    Deno.stdout.writeSync(new TextEncoder().encode(message + "\n"));
  }
  // Browser fallback
  else {
    console.log(message);
  }
}

/**
 * Write a string to stderr with newline.
 * Works in Node.js, Deno, and Bun.
 * @param {string} message
 */
export function write_stderr(message) {
  // Node.js and Bun
  if (typeof process !== "undefined" && process.stderr) {
    process.stderr.write(message + "\n");
  }
  // Deno
  else if (typeof Deno !== "undefined") {
    Deno.stderr.writeSync(new TextEncoder().encode(message + "\n"));
  }
  // Browser fallback
  else {
    console.error(message);
  }
}

/**
 * Check if stdout is a TTY (for color support detection).
 * @returns {boolean}
 */
export function is_stdout_tty() {
  // Node.js and Bun
  if (typeof process !== "undefined" && process.stdout) {
    return process.stdout.isTTY === true;
  }
  // Deno
  if (typeof Deno !== "undefined" && Deno.isatty) {
    return Deno.isatty(Deno.stdout.rid);
  }
  // Browser - assume no TTY
  return false;
}

// ============================================================================
// Global Configuration Storage
// ============================================================================

// Module-level storage for global config
// JavaScript is single-threaded so no synchronization needed
let globalConfig = undefined;

/**
 * Get the global configuration.
 * @returns {{ type: "Ok", 0: any } | { type: "Error", 0: undefined }}
 */
export function get_global_config() {
  if (globalConfig !== undefined) {
    return { type: "Ok", 0: globalConfig };
  }
  return { type: "Error", 0: undefined };
}

/**
 * Set the global configuration.
 * @param {any} config - The configuration object to store
 */
export function set_global_config(config) {
  globalConfig = config;
  return undefined;
}

/**
 * Clear the global configuration (reset to unset state).
 */
export function clear_global_config() {
  globalConfig = undefined;
  return undefined;
}
