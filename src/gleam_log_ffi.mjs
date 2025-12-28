// JavaScript FFI for gleam_log

// Import Gleam's Result constructors from the prelude
import { Ok, Error } from "./gleam.mjs";

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
// File Compression
// ============================================================================

// Pre-load Node.js modules if available (for synchronous access)
let _nodeFs = null;
let _nodeZlib = null;

// Initialize Node.js modules synchronously
if (typeof process !== "undefined" && process.versions?.node) {
  try {
    // In Node.js ESM, we can use createRequire to load CommonJS modules synchronously
    // This works because Gleam generates ESM output
    const module = await import("node:module");
    const require = module.createRequire(import.meta.url);
    _nodeFs = require("node:fs");
    _nodeZlib = require("node:zlib");
  } catch (e) {
    // Module loading failed - will fall back to error in compress_file_gzip
    console.error("gleam_log: Failed to load Node.js compression modules:", e);
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

    // Bun
    if (typeof Bun !== "undefined") {
      const fs = require("fs");
      const zlib = require("zlib");

      // Read source file
      const data = fs.readFileSync(sourcePath);

      // Compress with gzip (synchronous)
      const compressed = zlib.gzipSync(data);

      // Write compressed data
      fs.writeFileSync(destPath, compressed);

      return new Ok(undefined);
    }

    // Browser or unsupported environment
    return new Error("File compression not supported in this environment");
  } catch (e) {
    return new Error(`compression error: ${e.message || String(e)}`);
  }
}
