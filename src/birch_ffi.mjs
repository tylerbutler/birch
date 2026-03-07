// JavaScript FFI for birch
//
// Note: Time functions are now provided by gleam_time via birch/internal/time.
// Note: write_stdout, write_stderr, and random_float have been removed.
// Use gleam/io.println, io.println_error, and gleam/float.random() instead.

// Import Gleam's Result constructors and List helpers from the prelude
import { Ok, Error, toList } from "./gleam.mjs";

// ============================================================================
// Standardized module loading — single _require via createRequire.
// ESM (.mjs) doesn't have require() by default. We create one at module
// load time for Node.js and Bun, then reuse it everywhere.
// ============================================================================

let _require = null;
if (typeof process !== "undefined" && process.versions?.node) {
  try {
    const mod = await import("node:module");
    _require = mod.createRequire(import.meta.url);
  } catch (_e) {
    // Module loading failed — _require stays null
  }
}

// Pre-load fs/zlib for file compression (optional — used by compress_file_gzip)
let _nodeFs = null;
let _nodeZlib = null;
if (_require) {
  try {
    _nodeFs = _require("node:fs");
    _nodeZlib = _require("node:zlib");
  } catch (_e) {
    // Not available — compress_file_gzip will fall back to Deno path or error
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
  if (typeof Deno !== "undefined") {
    // Deno 2.0+: stdout.isTerminal()
    if (typeof Deno.stdout?.isTerminal === "function") {
      return Deno.stdout.isTerminal();
    }
    // Deno 1.x fallback: isatty(rid)
    if (Deno.isatty && Deno.stdout?.rid !== undefined) {
      return Deno.isatty(Deno.stdout.rid);
    }
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
// Cached Default Logger
// ============================================================================

let cachedDefaultLogger = undefined;

/**
 * Get the cached default logger.
 * @returns {Ok | Error} Gleam Result type
 */
export function get_cached_default_logger() {
  if (cachedDefaultLogger !== undefined) {
    return new Ok(cachedDefaultLogger);
  }
  return new Error(undefined);
}

/**
 * Set the cached default logger.
 * @param {any} logger - The Logger object to cache
 */
export function set_cached_default_logger(logger) {
  cachedDefaultLogger = logger;
  return undefined;
}

/**
 * Clear the cached default logger (invalidate on config change).
 */
export function clear_cached_default_logger() {
  cachedDefaultLogger = undefined;
  return undefined;
}

// ============================================================================
// Async Writer Implementation
// ============================================================================

// Registry for async writers (keyed by name only)
const asyncWriterRegistry = new Map();

/**
 * Async writer state class
 */
class AsyncWriter {
  constructor(name, callback, queueSize, flushIntervalMs, overflow) {
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
 * The writer object itself is returned as the opaque AsyncWriterId.
 * @param {string} name - Writer name for registry
 * @param {function} callback - Gleam callback function
 * @param {number} queueSize - Maximum queue size
 * @param {number} flushIntervalMs - Flush interval in milliseconds
 * @param {number} overflow - Overflow behavior (0=DropOldest, 1=DropNewest, 2=Block)
 * @returns {AsyncWriter} Writer object (opaque to Gleam)
 */
export function start_async_writer(
  name,
  callback,
  queueSize,
  flushIntervalMs,
  overflow,
) {
  const writer = new AsyncWriter(
    name,
    callback,
    queueSize,
    flushIntervalMs,
    overflow,
  );
  asyncWriterRegistry.set(name, writer);
  return writer;
}

/**
 * Send a log record to an async writer.
 * writerId is the AsyncWriter object returned by start_async_writer.
 * @param {AsyncWriter} writerId - Writer object from start_async_writer
 * @param {object} record - Log record
 */
export function async_send(writerId, record) {
  writerId.enqueue(record);
}

/**
 * Flush all async writers.
 */
export function flush_async_writers() {
  for (const writer of asyncWriterRegistry.values()) {
    writer.processQueueSync();
  }
}

/**
 * Flush a specific async writer by name.
 * @param {string} name - Writer name
 */
export function flush_async_writer(name) {
  const writer = asyncWriterRegistry.get(name);
  if (writer) {
    writer.processQueueSync();
  }
}

// ============================================================================
// File Compression
// ============================================================================

/**
 * Compress a file using gzip.
 * Works in Node.js, Bun, and Deno.
 * @param {string} sourcePath - Path to the source file
 * @param {string} destPath - Path to write the compressed file
 * @returns {Ok | Error} Gleam Result type
 */
export function compress_file_gzip(sourcePath, destPath) {
  try {
    // Node.js and Bun (modules pre-loaded at top level via _require)
    if (_nodeFs && _nodeZlib) {
      const data = _nodeFs.readFileSync(sourcePath);
      const compressed = _nodeZlib.gzipSync(data);
      _nodeFs.writeFileSync(destPath, compressed);
      return new Ok(undefined);
    }

    // Deno
    if (typeof Deno !== "undefined") {
      const command = new Deno.Command("gzip", {
        args: ["-c", "--", sourcePath],
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

    // Bun fallback — in practice unreachable since Bun sets
    // process.versions.node, so _nodeFs/_nodeZlib are loaded at top level.
    if (typeof Bun !== "undefined") {
      try {
        const fs = require("node:fs");
        const zlib = require("node:zlib");

        const data = fs.readFileSync(sourcePath);
        const compressed = zlib.gzipSync(data);
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
// AsyncLocalStorage — Shared Instance
// Consolidated two separate AsyncLocalStorage instances (scope context and
// scoped logger) into one shared instance. Store shape:
//   { context: GleamList, depth: number, logger: any | undefined }
// This reduces overhead and ensures run_with_scope and run_with_scoped_logger
// properly preserve each other's state when nested.
// ============================================================================

let _als = null;
let _alsAvailable = false;
let _alsInitialized = false;

// Fallback stacks for non-ALS environments (Deno, Bun, browser)
const _scopeContextFallbackStack = [];
const _scopedLoggerFallbackStack = [];

/**
 * Initialize the shared AsyncLocalStorage instance (lazy, once).
 * Uses the module-level _require created at top level.
 */
function initALS() {
  if (_alsInitialized) {
    return;
  }
  _alsInitialized = true;

  if (_require) {
    try {
      const async_hooks = _require("node:async_hooks");
      _als = new async_hooks.AsyncLocalStorage();
      _alsAvailable = true;
    } catch (_e) {
      _alsAvailable = false;
    }
  }
}

/** Default store shape when no ALS context is active. */
function defaultStore() {
  return { context: toList([]), depth: 0, logger: undefined };
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
 * Merge two Gleam lists by converting to JS arrays and back.
 * New context items are prepended (higher priority).
 *
 * This round-trip through JS arrays is necessary because the JS @external
 * replaces the entire Gleam function body of with_scope(). On Erlang, the
 * Gleam body uses list.append directly. On JS, we must merge here since the
 * Gleam body is skipped.
 *
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
 * Get the current scope context.
 * @returns {List} Gleam List of [key, value] tuples (Gleam Metadata format)
 */
export function get_scope_context() {
  initALS();

  if (_alsAvailable && _als) {
    const store = _als.getStore();
    return store !== undefined ? store.context : toList([]);
  }
  // Fallback: return top of stack or empty list
  const stack = _scopeContextFallbackStack;
  return stack.length > 0 ? stack[stack.length - 1].context : toList([]);
}

/**
 * Get the current scope depth (nesting level).
 * @returns {number} Current depth (0 if no scope active)
 */
export function get_scope_depth() {
  initALS();

  if (_alsAvailable && _als) {
    const store = _als.getStore();
    return store !== undefined ? store.depth : 0;
  }
  // Fallback: return top of stack depth or 0
  const stack = _scopeContextFallbackStack;
  return stack.length > 0 ? stack[stack.length - 1].depth : 0;
}

/**
 * Set the scope context.
 * Uses enterWith() to properly update the ALS store when available.
 * @param {List} context - Gleam List of [key, value] tuples
 */
export function set_scope_context(context) {
  initALS();

  if (_alsAvailable && _als) {
    const currentStore = _als.getStore() || defaultStore();
    _als.enterWith({ ...currentStore, context });
    return undefined;
  }
  // Fallback: replace the entire stack with just this context
  const isEmpty = !context || context.head === undefined;
  _scopeContextFallbackStack.length = 0;
  if (!isEmpty) {
    _scopeContextFallbackStack.push({ context, depth: 0 });
  }
  return undefined;
}

/**
 * Set the scope depth.
 * Uses enterWith() to properly update the ALS store when available.
 * @param {number} depth - Depth to set
 */
export function set_scope_depth(depth) {
  initALS();

  if (_alsAvailable && _als) {
    const currentStore = _als.getStore() || defaultStore();
    _als.enterWith({ ...currentStore, depth });
    return undefined;
  }
  // Fallback: update depth in top of stack
  const stack = _scopeContextFallbackStack;
  if (stack.length > 0) {
    stack[stack.length - 1].depth = depth;
  }
  return undefined;
}

/**
 * Run a function with scoped context.
 * On Node.js, uses AsyncLocalStorage.run() for proper async propagation.
 * Preserves any existing scoped logger in the shared ALS store.
 * @param {List} context - Gleam List of context to add to the scope
 * @param {function} callback - Function to run with the context
 * @returns {*} The result of the callback
 */
export function run_with_scope(context, callback) {
  initALS();

  if (_alsAvailable && _als) {
    const currentStore = _als.getStore() || defaultStore();
    const mergedContext = mergeGleamLists(context, currentStore.context);
    const newStore = {
      ...currentStore, // Preserves logger from shared store
      context: mergedContext,
      depth: currentStore.depth + 1,
    };
    return _als.run(newStore, callback);
  }

  // Fallback for non-Node.js environments: use stack-based approach
  const stack = _scopeContextFallbackStack;
  const currentStore =
    stack.length > 0
      ? stack[stack.length - 1]
      : { context: toList([]), depth: 0 };
  const mergedContext = mergeGleamLists(context, currentStore.context);
  const newStore = {
    context: mergedContext,
    depth: currentStore.depth + 1,
  };

  stack.push(newStore);
  try {
    return callback();
  } finally {
    stack.pop();
  }
}

/**
 * Check if scoped context is available.
 * @returns {boolean} True on Node.js (AsyncLocalStorage), False otherwise
 */
export function is_scope_context_available() {
  initALS();
  return _alsAvailable;
}

// ============================================================================
// Scoped Logger Override
// Uses the same shared AsyncLocalStorage as scope context.
// The `logger` field in the ALS store holds the scoped logger override.
// ============================================================================

/**
 * Get the current scoped logger override.
 * @returns {Result} Ok(Logger) if set, Error(nil) otherwise
 */
export function get_scoped_logger() {
  initALS();

  if (_alsAvailable && _als) {
    const store = _als.getStore();
    if (store && store.logger !== undefined) {
      return new Ok(store.logger);
    }
    return new Error(undefined);
  }

  // Fallback
  const stack = _scopedLoggerFallbackStack;
  if (stack.length > 0) {
    return new Ok(stack[stack.length - 1]);
  }
  return new Error(undefined);
}

/**
 * Set the scoped logger override.
 * Uses enterWith() to properly update the ALS store when available.
 */
export function set_scoped_logger(logger) {
  initALS();

  if (_alsAvailable && _als) {
    const currentStore = _als.getStore() || defaultStore();
    _als.enterWith({ ...currentStore, logger });
    return undefined;
  }

  // Fallback
  _scopedLoggerFallbackStack.push(logger);
  return undefined;
}

/**
 * Clear the scoped logger override.
 * Uses enterWith() to properly update the ALS store when available.
 */
export function clear_scoped_logger() {
  initALS();

  if (_alsAvailable && _als) {
    const currentStore = _als.getStore() || defaultStore();
    _als.enterWith({ ...currentStore, logger: undefined });
    return undefined;
  }

  // Fallback
  _scopedLoggerFallbackStack.pop();
  return undefined;
}

/**
 * Run a function with a scoped logger override.
 * On Node.js, uses AsyncLocalStorage.run() for proper async propagation.
 * Preserves any existing scope context in the shared ALS store.
 * @param {*} logger - The Logger to use as override
 * @param {function} callback - Function to run with the scoped logger
 * @returns {*} The result of the callback
 */
export function run_with_scoped_logger(logger, callback) {
  initALS();

  if (_alsAvailable && _als) {
    const currentStore = _als.getStore() || defaultStore();
    const newStore = { ...currentStore, logger };
    return _als.run(newStore, callback);
  }

  // Fallback for non-Node.js environments: use stack-based approach
  _scopedLoggerFallbackStack.push(logger);
  try {
    return callback();
  } finally {
    _scopedLoggerFallbackStack.pop();
  }
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
      const worker_threads = _require("node:worker_threads");
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

// ============================================================================
// File Size Cache (module-level Map)
// ============================================================================

// Module-level Map to store file sizes per path
const fileSizeCache = new Map();

/**
 * Get the cached file size for a path.
 * Returns Ok(size) if cached, Error(undefined) if not cached.
 * @param {string} path - The file path
 * @returns {Ok | Error} Gleam Result type
 */
export function get_file_size_cache(path) {
  if (fileSizeCache.has(path)) {
    return new Ok(fileSizeCache.get(path));
  }
  return new Error(undefined);
}

/**
 * Set the cached file size for a path.
 * @param {string} path - The file path
 * @param {number} size - The file size in bytes
 */
export function set_file_size_cache(path, size) {
  fileSizeCache.set(path, size);
  return undefined;
}

/**
 * Reset (delete) the cached file size for a path.
 * Called after file rotation to start fresh.
 * @param {string} path - The file path
 */
export function reset_file_size_cache(path) {
  fileSizeCache.delete(path);
  return undefined;
}
