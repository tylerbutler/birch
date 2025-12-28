// JavaScript FFI for gleam_log

// Import Gleam's Result constructors and List helpers from the prelude
import { Ok, Error, toList } from "./gleam.mjs";

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
 * @returns {Ok | Error} Gleam Result type
 */
export function get_global_config() {
  if (globalConfig !== undefined) {
    return new Ok(globalConfig);
  }
  return new Error(undefined);
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

// ============================================================================
// Async Writer Implementation
// ============================================================================

// Registry for async writers
const asyncWriterRegistry = new Map();

// Counter for generating unique writer IDs
let writerIdCounter = 0;

/**
 * Async writer state class
 */
class AsyncWriter {
  constructor(name, callback, queueSize, flushIntervalMs, overflow) {
    this.id = ++writerIdCounter;
    this.name = name;
    this.callback = callback;
    this.queueSize = queueSize;
    this.flushIntervalMs = flushIntervalMs;
    this.overflow = overflow; // 0=DropOldest, 1=DropNewest, 2=Block
    this.queue = [];
    this.processing = false;
    this.flushResolvers = [];

    // Start periodic flush
    this.scheduleFlush();
  }

  /**
   * Schedule a flush after the configured interval
   */
  scheduleFlush() {
    if (this.flushTimer) {
      return;
    }
    this.flushTimer = setTimeout(() => {
      this.flushTimer = null;
      this.processQueue();
      if (this.queue.length > 0) {
        this.scheduleFlush();
      }
    }, this.flushIntervalMs);
  }

  /**
   * Enqueue a log record
   */
  enqueue(record) {
    if (this.queue.length >= this.queueSize) {
      switch (this.overflow) {
        case 0: // DropOldest
          this.queue.shift();
          break;
        case 1: // DropNewest
          return; // Don't add
        case 2: // Block - process immediately
          this.processQueueSync();
          break;
      }
    }
    this.queue.push(record);
    this.scheduleFlush();
  }

  /**
   * Process the queue asynchronously (one record per microtask)
   */
  processQueue() {
    if (this.processing || this.queue.length === 0) {
      return;
    }
    this.processing = true;

    // Use setImmediate if available (Node.js), otherwise setTimeout
    const scheduleNext =
      typeof setImmediate !== "undefined"
        ? setImmediate
        : (fn) => setTimeout(fn, 0);

    const processOne = () => {
      if (this.queue.length === 0) {
        this.processing = false;
        // Resolve any pending flush promises
        this.flushResolvers.forEach((resolve) => resolve());
        this.flushResolvers = [];
        return;
      }

      const record = this.queue.shift();
      try {
        this.callback(record);
      } catch (e) {
        // Log errors but don't crash
        console.error("Async log handler error:", e);
      }

      scheduleNext(processOne);
    };

    scheduleNext(processOne);
  }

  /**
   * Process the queue synchronously (for Block overflow or flush)
   */
  processQueueSync() {
    while (this.queue.length > 0) {
      const record = this.queue.shift();
      try {
        this.callback(record);
      } catch (e) {
        console.error("Async log handler error:", e);
      }
    }
    this.processing = false;
    // Resolve any pending flush promises
    this.flushResolvers.forEach((resolve) => resolve());
    this.flushResolvers = [];
  }

  /**
   * Flush all pending records
   * @returns {Promise<void>}
   */
  flush() {
    return new Promise((resolve) => {
      if (this.queue.length === 0 && !this.processing) {
        resolve();
        return;
      }
      this.flushResolvers.push(resolve);
      this.processQueueSync();
    });
  }

  /**
   * Stop the writer
   */
  stop() {
    if (this.flushTimer) {
      clearTimeout(this.flushTimer);
      this.flushTimer = null;
    }
    this.processQueueSync();
  }
}

/**
 * Start an async writer.
 * @param {string} name - Writer name for registry
 * @param {function} callback - Gleam callback function
 * @param {number} queueSize - Maximum queue size
 * @param {number} flushIntervalMs - Flush interval in milliseconds
 * @param {number} overflow - Overflow behavior (0=DropOldest, 1=DropNewest, 2=Block)
 * @returns {object} Writer ID (opaque object)
 */
export function start_async_writer(
  name,
  callback,
  queueSize,
  flushIntervalMs,
  overflow
) {
  const writer = new AsyncWriter(
    name,
    callback,
    queueSize,
    flushIntervalMs,
    overflow
  );
  asyncWriterRegistry.set(name, writer);
  asyncWriterRegistry.set(writer.id, writer);
  return { id: writer.id, name: name };
}

/**
 * Send a log record to an async writer.
 * @param {object} writerId - Writer ID from start_async_writer
 * @param {object} record - Log record
 */
export function async_send(writerId, record) {
  const writer = asyncWriterRegistry.get(writerId.id);
  if (writer) {
    writer.enqueue(record);
  }
}

/**
 * Flush all async writers.
 */
export function flush_async_writers() {
  const writers = Array.from(asyncWriterRegistry.values()).filter(
    (w) => w instanceof AsyncWriter
  );
  // Flush all writers synchronously
  writers.forEach((writer) => {
    writer.processQueueSync();
  });
}

/**
 * Flush a specific async writer by name.
 * @param {string} name - Writer name
 */
export function flush_async_writer(name) {
  const writer = asyncWriterRegistry.get(name);
  if (writer && writer instanceof AsyncWriter) {
    writer.processQueueSync();
  }
}

// ============================================================================
// Scoped Context Implementation
// ============================================================================

// Scope context state
let scopeContextState = {
  initialized: false,
  asyncLocalStorage: null,
  asyncLocalStorageAvailable: false,
  // Stack-based fallback context for non-Node.js environments
  fallbackContextStack: [],
};

/**
 * Initialize the scope context system (lazy initialization).
 * This is called on first use to set up AsyncLocalStorage if available.
 */
function initScopeContext() {
  if (scopeContextState.initialized) {
    return;
  }
  scopeContextState.initialized = true;

  // Try to use AsyncLocalStorage in Node.js
  try {
    if (
      typeof process !== "undefined" &&
      process.versions &&
      process.versions.node
    ) {
      // Node.js: use require for synchronous loading
      // eslint-disable-next-line no-undef
      const async_hooks = require("node:async_hooks");
      scopeContextState.asyncLocalStorage = new async_hooks.AsyncLocalStorage();
      scopeContextState.asyncLocalStorageAvailable = true;
    }
  } catch (e) {
    // AsyncLocalStorage not available
    scopeContextState.asyncLocalStorageAvailable = false;
  }
}

/**
 * Convert a Gleam list to a JavaScript array.
 * Gleam lists are linked lists with head/tail structure.
 * @param {*} gleamList - A Gleam list
 * @returns {Array} JavaScript array
 */
function gleamListToArray(gleamList) {
  const result = [];
  let current = gleamList;
  while (current && current.head !== undefined) {
    result.push(current.head);
    current = current.tail;
  }
  return result;
}

/**
 * Get the current scope context.
 * @returns {List} Gleam List of [key, value] tuples (Gleam Metadata format)
 */
export function get_scope_context() {
  initScopeContext();

  if (
    scopeContextState.asyncLocalStorageAvailable &&
    scopeContextState.asyncLocalStorage
  ) {
    const store = scopeContextState.asyncLocalStorage.getStore();
    return store !== undefined ? store : toList([]);
  }
  // Fallback: return top of stack or empty list
  const stack = scopeContextState.fallbackContextStack;
  return stack.length > 0 ? stack[stack.length - 1] : toList([]);
}

/**
 * Set the scope context (used internally for fallback only).
 * @param {List} context - Gleam List of [key, value] tuples
 */
export function set_scope_context(context) {
  initScopeContext();

  if (
    scopeContextState.asyncLocalStorageAvailable &&
    scopeContextState.asyncLocalStorage
  ) {
    // For AsyncLocalStorage, we need to use run() for proper scoping
    // This function is only used for fallback now
    // enterWith doesn't properly scope, so we avoid it
  }
  // Fallback: replace the entire stack with just this context
  // (This is used when restoring - we want to reset to previous state)
  // Check if context is empty by checking if it has a head
  const isEmpty = !context || context.head === undefined;
  scopeContextState.fallbackContextStack = isEmpty ? [] : [context];
  return undefined;
}

/**
 * Merge two Gleam lists by converting to JS arrays and back.
 * New context items are prepended (higher priority).
 * @param {List} newContext - New context to add (Gleam list)
 * @param {List} currentContext - Current context (Gleam list)
 * @returns {List} Merged Gleam list
 */
function mergeGleamLists(newContext, currentContext) {
  const newArray = gleamListToArray(newContext);
  const currentArray = gleamListToArray(currentContext);
  return toList([...newArray, ...currentArray]);
}

/**
 * Run a function with scoped context.
 * This properly handles AsyncLocalStorage.run() for Node.js.
 * @param {List} context - Gleam List of context to add to the scope
 * @param {function} callback - Function to run with the context
 * @returns {*} The result of the callback
 */
export function run_with_scope(context, callback) {
  initScopeContext();

  if (
    scopeContextState.asyncLocalStorageAvailable &&
    scopeContextState.asyncLocalStorage
  ) {
    // Get current context and merge
    const currentContext =
      scopeContextState.asyncLocalStorage.getStore() || toList([]);
    const mergedContext = mergeGleamLists(context, currentContext);

    // Use run() for proper scoping - context is automatically restored after
    return scopeContextState.asyncLocalStorage.run(mergedContext, callback);
  }

  // Fallback for non-Node.js environments: use stack-based approach
  const stack = scopeContextState.fallbackContextStack;
  const currentContext = stack.length > 0 ? stack[stack.length - 1] : toList([]);
  const mergedContext = mergeGleamLists(context, currentContext);

  // Push new context onto stack
  stack.push(mergedContext);
  try {
    return callback();
  } finally {
    // Pop context from stack
    stack.pop();
  }
}

/**
 * Check if scoped context is available.
 * @returns {boolean} True on Node.js (AsyncLocalStorage), False otherwise
 */
export function is_scope_context_available() {
  initScopeContext();
  return scopeContextState.asyncLocalStorageAvailable;
}

// ============================================================================
// Sampling FFI
// ============================================================================

/**
 * Generate a random float between 0.0 (inclusive) and 1.0 (exclusive).
 * @returns {number}
 */
export function random_float() {
  // TODO: use a better random
  return Math.random();
}

/**
 * Get the current time in milliseconds since epoch.
 * @returns {number}
 */
export function current_time_ms() {
  return Date.now();
}
