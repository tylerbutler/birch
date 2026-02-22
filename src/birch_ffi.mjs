// JavaScript FFI for birch
//
// Note: Time functions are now provided by gleam_time via birch/internal/time.
// Note: write_stdout, write_stderr, and random_float have been removed.
// Use gleam/io.println, io.println_error, and gleam/float.random() instead.

// Import Gleam's Result constructors and List helpers from the prelude
import { Ok, Error, toList } from "./gleam.mjs";
import { StringVal } from "./birch/record.mjs";

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

/**
 * Get terminal color depth (number of colors supported).
 * @returns {number} 16777216 for truecolor, 256 for 256-color, 16 for basic, 0 for none
 */
export function get_color_depth() {
  if (!is_stdout_tty()) {
    return 0;
  }

  // Node.js and Bun
  if (typeof process !== "undefined" && process.env) {
    const colorTerm = process.env.COLORTERM;
    const term = process.env.TERM || "";

    // Check for truecolor support
    if (colorTerm === "truecolor" || colorTerm === "24bit") {
      return 16777216;
    }

    // Check for 256-color support
    if (term.includes("256color") || term.includes("256")) {
      return 256;
    }

    // Basic 16 colors
    return 16;
  }

  // Deno
  if (typeof Deno !== "undefined" && Deno.env) {
    const colorTerm = Deno.env.get("COLORTERM");
    const term = Deno.env.get("TERM") || "";

    if (colorTerm === "truecolor" || colorTerm === "24bit") {
      return 16777216;
    }

    if (term.includes("256color") || term.includes("256")) {
      return 256;
    }

    return 16;
  }

  // Browser - no terminal colors
  return 0;
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
// File Compression
// ============================================================================

// Node.js fs/zlib modules for file compression
// These are loaded at module initialization time using top-level await.
// This is supported by Gleam's ESM output and modern Node.js/bundlers.
// The compression feature is optional - if modules fail to load, compress_file_gzip
// will fall back to Deno/Bun paths or return an error.
let _nodeFs = null;
let _nodeZlib = null;

if (typeof process !== "undefined" && process.versions?.node) {
  try {
    const module = await import("node:module");
    const require = module.createRequire(import.meta.url);
    _nodeFs = require("node:fs");
    _nodeZlib = require("node:zlib");
  } catch (e) {
    // Module loading failed - compress_file_gzip will return an error for Node.js
  }
}

/**
 * Compress a file using gzip.
 * Works in Node.js, Bun, and Deno.
 * @param {string} sourcePath - Path to the source file
 * @param {string} destPath - Path to write the compressed file
 * @returns {Ok | Error} Gleam Result type
 */
export function compress_file_gzip(sourcePath, destPath) {
  try {
    // Node.js (modules pre-loaded at top level)
    if (_nodeFs && _nodeZlib) {
      // Read source file
      const data = _nodeFs.readFileSync(sourcePath);

      // Compress with gzip (synchronous)
      const compressed = _nodeZlib.gzipSync(data);

      // Write compressed data
      _nodeFs.writeFileSync(destPath, compressed);

      return new Ok(undefined);
    }

    // Deno
    if (typeof Deno !== "undefined") {
      // Use gzip command for synchronous compression in Deno
      const command = new Deno.Command("gzip", {
        args: ["-c", sourcePath],
        stdout: "piped",
        stderr: "piped",
      });

      const { code, stdout, stderr } = command.outputSync();

      if (code !== 0) {
        const errorMsg = new TextDecoder().decode(stderr);
        return new Error(`gzip command failed: ${errorMsg}`);
      }

      Deno.writeFileSync(destPath, stdout);
      return new Ok(undefined);
    }

    // Bun - has native require() support
    if (typeof Bun !== "undefined") {
      try {
        const fs = require("fs");
        const zlib = require("zlib");

        // Read source file
        const data = fs.readFileSync(sourcePath);

        // Compress with gzip (synchronous)
        const compressed = zlib.gzipSync(data);

        // Write compressed data
        fs.writeFileSync(destPath, compressed);

        return new Ok(undefined);
      } catch (e) {
        return new Error(`Bun compression error: ${e.message || String(e)}`);
      }
    }

    // Browser or unsupported environment
    return new Error("File compression not supported in this environment");
  } catch (e) {
    return new Error(`compression error: ${e.message || String(e)}`);
  }
}

// ============================================================================
// Safe Call (Error Catching)
// ============================================================================

/**
 * Safely call a function, catching any errors/exceptions.
 * @param {function} fn - Function to call
 * @returns {Ok | Error} Gleam Result type
 */
export function safe_call(fn) {
  try {
    fn();
    return new Ok(undefined);
  } catch (e) {
    const errorMsg = e instanceof Error ? e.message : String(e);
    return new Error(errorMsg);
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
    return store !== undefined ? store.context : toList([]);
  }
  // Fallback: return top of stack or empty list
  const stack = scopeContextState.fallbackContextStack;
  return stack.length > 0 ? stack[stack.length - 1].context : toList([]);
}

/**
 * Get the current scope depth (nesting level).
 * @returns {number} Current depth (0 if no scope active)
 */
export function get_scope_depth() {
  initScopeContext();

  if (
    scopeContextState.asyncLocalStorageAvailable &&
    scopeContextState.asyncLocalStorage
  ) {
    const store = scopeContextState.asyncLocalStorage.getStore();
    return store !== undefined ? store.depth : 0;
  }
  // Fallback: return top of stack depth or 0
  const stack = scopeContextState.fallbackContextStack;
  return stack.length > 0 ? stack[stack.length - 1].depth : 0;
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
  scopeContextState.fallbackContextStack = isEmpty ? [] : [{ context, depth: 0 }];
  return undefined;
}

/**
 * Set the scope depth (used internally for fallback only).
 * @param {number} depth - Depth to set
 */
export function set_scope_depth(depth) {
  initScopeContext();

  if (
    scopeContextState.asyncLocalStorageAvailable &&
    scopeContextState.asyncLocalStorage
  ) {
    // For AsyncLocalStorage, depth is managed in run_with_scope
    // This is for fallback only
  }
  // Fallback: update depth in top of stack
  const stack = scopeContextState.fallbackContextStack;
  if (stack.length > 0) {
    stack[stack.length - 1].depth = depth;
  }
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

  // Extract keys from the new context being added
  const newKeys = gleamListToArray(context).map(pair => {
    // Each pair is a tuple with [0] = key, [1] = value
    return pair[0];
  });

  // Create _scope_highlight_keys metadata entry
  const highlightKeysValue = newKeys.join(",");
  const highlightKeysPair = ["_scope_highlight_keys", new StringVal(highlightKeysValue)];

  // Add _scope_highlight_keys to the context
  const contextWithHighlight = toList([...gleamListToArray(context), highlightKeysPair]);

  if (
    scopeContextState.asyncLocalStorageAvailable &&
    scopeContextState.asyncLocalStorage
  ) {
    // Get current store (contains context and depth)
    const currentStore = scopeContextState.asyncLocalStorage.getStore() || {
      context: toList([]),
      depth: 0
    };
    const mergedContext = mergeGleamLists(contextWithHighlight, currentStore.context);
    const newStore = {
      context: mergedContext,
      depth: currentStore.depth + 1
    };

    // Use run() for proper scoping - store is automatically restored after
    return scopeContextState.asyncLocalStorage.run(newStore, callback);
  }

  // Fallback for non-Node.js environments: use stack-based approach
  const stack = scopeContextState.fallbackContextStack;
  const currentStore = stack.length > 0 ? stack[stack.length - 1] : {
    context: toList([]),
    depth: 0
  };
  const mergedContext = mergeGleamLists(contextWithHighlight, currentStore.context);
  const newStore = {
    context: mergedContext,
    depth: currentStore.depth + 1
  };

  // Push new store onto stack
  stack.push(newStore);
  try {
    return callback();
  } finally {
    // Pop store from stack
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
// Process/Thread ID
// ============================================================================

/**
 * Get the current process or thread identifier as a string.
 * On JavaScript: Returns "main" for the main thread, or worker ID if in a worker.
 * @returns {string}
 */
export function get_caller_id() {
  // Web Workers have a name property
  if (typeof self !== "undefined" && self.name) {
    return self.name;
  }
  // Node.js worker threads
  if (typeof process !== "undefined" && process.versions?.node) {
    try {
      // Try to get worker thread ID if in a worker
      const worker_threads = require("worker_threads");
      if (!worker_threads.isMainThread) {
        return `worker-${worker_threads.threadId}`;
      }
    } catch (e) {
      // Not in a worker thread or module not available
    }
    // Return process ID for main thread
    return `pid-${process.pid}`;
  }
  // Deno - check if in a worker
  if (typeof Deno !== "undefined") {
    // Deno workers don't have a direct way to get ID, use "worker" if not main
    if (typeof WorkerGlobalScope !== "undefined") {
      return "worker";
    }
    return `pid-${Deno.pid}`;
  }
  // Browser main thread or fallback
  return "main";
}
