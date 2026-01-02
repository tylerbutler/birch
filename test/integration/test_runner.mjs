/**
 * Integration test runner for birch JavaScript target.
 *
 * This test harness:
 * 1. Runs compiled Gleam fixtures as separate processes
 * 2. Captures stdout/stderr output
 * 3. Verifies output content, format, and structure
 *
 * Supports Node.js, Deno, and Bun runtimes.
 */

import { spawn } from "node:child_process";
import { test, describe, before } from "node:test";
import assert from "node:assert";
import { fileURLToPath } from "node:url";
import { dirname, join, resolve } from "node:path";
import { existsSync } from "node:fs";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const projectRoot = resolve(__dirname, "../..");

// Path to compiled JavaScript modules
const buildDir = join(projectRoot, "build/dev/javascript/birch");

/**
 * Run a compiled Gleam fixture and capture output.
 * @param {string} fixtureName - Name of the fixture (without extension)
 * @param {string} runtime - Runtime to use: 'node', 'deno', or 'bun'
 * @returns {Promise<{stdout: string, stderr: string, exitCode: number}>}
 */
async function runFixture(fixtureName, runtime = "node") {
  // Gleam compiles test/integration/fixtures/*.gleam to integration/fixtures/*.mjs
  // (the test/ prefix is stripped by the compiler)
  const fixturePath = join(
    buildDir,
    "integration/fixtures",
    `${fixtureName}.mjs`
  );

  // Verify fixture exists
  if (!existsSync(fixturePath)) {
    throw new Error(
      `Fixture not found: ${fixturePath}. Run 'gleam build --target javascript' first.`
    );
  }

  // Create a wrapper script that imports and calls main()
  // Gleam exports main() but doesn't auto-call it
  const wrapperScript = `
    import { main } from '${fixturePath}';
    main();
  `;

  return new Promise((resolve, reject) => {
    let command;
    let args;

    switch (runtime) {
      case "deno":
        command = "deno";
        // Deno eval has implicit access to all permissions
        args = ["eval", wrapperScript];
        break;
      case "bun":
        command = "bun";
        args = ["--eval", wrapperScript];
        break;
      case "node":
      default:
        command = "node";
        args = ["--eval", wrapperScript];
        break;
    }

    const child = spawn(command, args, {
      cwd: projectRoot,
      env: {
        ...process.env,
        // Force non-TTY mode to get consistent output without colors
        TERM: "dumb",
        NO_COLOR: "1",
      },
    });

    let stdout = "";
    let stderr = "";

    child.stdout.on("data", (data) => {
      stdout += data.toString();
    });

    child.stderr.on("data", (data) => {
      stderr += data.toString();
    });

    child.on("error", (err) => {
      reject(new Error(`Failed to spawn ${command}: ${err.message}`));
    });

    child.on("close", (exitCode) => {
      resolve({ stdout, stderr, exitCode });
    });
  });
}

/**
 * Run a fixture with colors enabled (TTY mode).
 * @param {string} fixtureName - Name of the fixture
 * @param {string} runtime - Runtime to use
 * @returns {Promise<{stdout: string, stderr: string, exitCode: number}>}
 */
async function runFixtureWithColors(fixtureName, runtime = "node") {
  // Gleam compiles test/integration/fixtures/*.gleam to integration/fixtures/*.mjs
  const fixturePath = join(
    buildDir,
    "integration/fixtures",
    `${fixtureName}.mjs`
  );

  if (!existsSync(fixturePath)) {
    throw new Error(
      `Fixture not found: ${fixturePath}. Run 'gleam build --target javascript' first.`
    );
  }

  // Create a wrapper script that imports and calls main()
  const wrapperScript = `
    import { main } from '${fixturePath}';
    main();
  `;

  return new Promise((resolve, reject) => {
    let command;
    let args;

    switch (runtime) {
      case "deno":
        command = "deno";
        // Deno eval has implicit access to all permissions
        args = ["eval", wrapperScript];
        break;
      case "bun":
        command = "bun";
        args = ["--eval", wrapperScript];
        break;
      case "node":
      default:
        command = "node";
        args = ["--eval", wrapperScript];
        break;
    }

    const child = spawn(command, args, {
      cwd: projectRoot,
      env: {
        ...process.env,
        // Remove NO_COLOR to allow colors
        FORCE_COLOR: "1",
      },
    });

    let stdout = "";
    let stderr = "";

    child.stdout.on("data", (data) => {
      stdout += data.toString();
    });

    child.stderr.on("data", (data) => {
      stderr += data.toString();
    });

    child.on("error", (err) => {
      reject(new Error(`Failed to spawn ${command}: ${err.message}`));
    });

    child.on("close", (exitCode) => {
      resolve({ stdout, stderr, exitCode });
    });
  });
}

/**
 * Parse JSON lines from output.
 * @param {string} output - Output containing JSON lines
 * @returns {object[]} Array of parsed JSON objects
 */
function parseJsonLines(output) {
  return output
    .trim()
    .split("\n")
    .filter((line) => line.trim())
    .map((line) => {
      try {
        return JSON.parse(line);
      } catch (e) {
        throw new Error(`Failed to parse JSON line: ${line}`);
      }
    });
}

/**
 * Check if a string contains ANSI escape codes.
 * @param {string} str - String to check
 * @returns {boolean} True if ANSI codes are present
 */
function hasAnsiCodes(str) {
  // ANSI escape sequence pattern
  return /\u001b\[\d+m/.test(str);
}

// Detect current runtime or use override from environment
// When running from Node.js test runner but targeting a different runtime,
// set INTEGRATION_TEST_RUNTIME=deno or INTEGRATION_TEST_RUNTIME=bun
const currentRuntime = (() => {
  // Check for environment variable override first
  if (process.env.INTEGRATION_TEST_RUNTIME) {
    return process.env.INTEGRATION_TEST_RUNTIME;
  }
  // Otherwise detect the actual runtime
  if (typeof Deno !== "undefined") return "deno";
  if (typeof Bun !== "undefined") return "bun";
  return "node";
})();

// ============================================================================
// Test Suites
// ============================================================================

describe("Console Handler Integration Tests", () => {
  test("outputs log messages at all levels", async () => {
    const { stdout, exitCode } = await runFixture("console_fixture");

    assert.strictEqual(exitCode, 0, "Fixture should exit successfully");

    // Check for expected log messages
    assert.match(stdout, /Debug message for testing/);
    assert.match(stdout, /Info message for testing/);
    assert.match(stdout, /Warn message for testing/);
    assert.match(stdout, /Error message for testing/);
  });

  test("includes metadata in output", async () => {
    const { stdout, exitCode } = await runFixture("console_fixture");

    assert.strictEqual(exitCode, 0);
    assert.match(stdout, /Message with metadata/);
    assert.match(stdout, /request_id/);
    assert.match(stdout, /test-123/);
  });

  test("includes log level in output", async () => {
    const { stdout, exitCode } = await runFixture("console_fixture");

    assert.strictEqual(exitCode, 0);
    // Log levels should appear in the output
    assert.match(stdout, /DEBUG|debug/i);
    assert.match(stdout, /INFO|info/i);
    assert.match(stdout, /WARN|warn/i);
    assert.match(stdout, /ERROR|error/i);
  });

  test("includes timestamps in output", async () => {
    const { stdout, exitCode } = await runFixture("console_fixture");

    assert.strictEqual(exitCode, 0);
    // ISO 8601 timestamp pattern
    assert.match(stdout, /\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}/);
  });
});

describe("JSON Handler Integration Tests", () => {
  test("outputs valid JSON for each log message", async () => {
    const { stdout, exitCode } = await runFixture("json_fixture");

    assert.strictEqual(exitCode, 0, "Fixture should exit successfully");

    const jsonLines = parseJsonLines(stdout);
    assert.ok(jsonLines.length >= 5, "Should have at least 5 log messages");
  });

  test("includes required fields in JSON output", async () => {
    const { stdout, exitCode } = await runFixture("json_fixture");

    assert.strictEqual(exitCode, 0);

    const jsonLines = parseJsonLines(stdout);

    for (const line of jsonLines) {
      assert.ok(line.timestamp, "Should have timestamp field");
      assert.ok(line.level, "Should have level field");
      assert.ok(line.logger, "Should have logger field");
      assert.ok(line.message, "Should have message field");
    }
  });

  test("includes metadata as JSON fields", async () => {
    const { stdout, exitCode } = await runFixture("json_fixture");

    assert.strictEqual(exitCode, 0);

    const jsonLines = parseJsonLines(stdout);

    // Find the line with metadata
    const withMetadata = jsonLines.find((line) => line.transaction_id);
    assert.ok(withMetadata, "Should have a log with metadata");
    assert.strictEqual(withMetadata.transaction_id, "txn-456");
    assert.strictEqual(withMetadata.amount, "100.50");
  });

  test("uses lowercase level names", async () => {
    const { stdout, exitCode } = await runFixture("json_fixture");

    assert.strictEqual(exitCode, 0);

    const jsonLines = parseJsonLines(stdout);

    const levels = jsonLines.map((line) => line.level);
    assert.ok(levels.includes("debug"));
    assert.ok(levels.includes("info"));
    assert.ok(levels.includes("warn"));
    assert.ok(levels.includes("error"));
  });
});

describe("Metadata and Context Integration Tests", () => {
  test("includes global context in all logs", async () => {
    const { stdout, exitCode } = await runFixture("metadata_fixture");

    assert.strictEqual(exitCode, 0, "Fixture should exit successfully");

    const jsonLines = parseJsonLines(stdout);

    // All logs should have global context
    for (const line of jsonLines) {
      assert.strictEqual(
        line.service,
        "test-service",
        "Should have service field"
      );
      assert.strictEqual(line.env, "test", "Should have env field");
    }
  });

  test("merges additional metadata with global context", async () => {
    const { stdout, exitCode } = await runFixture("metadata_fixture");

    assert.strictEqual(exitCode, 0);

    const jsonLines = parseJsonLines(stdout);

    // Find the line with extra metadata
    const withExtra = jsonLines.find((line) => line.request_id === "req-789");
    assert.ok(withExtra, "Should have log with request_id");
    // Should still have global context
    assert.strictEqual(withExtra.service, "test-service");
    assert.strictEqual(withExtra.env, "test");
  });

  test("applies scoped context correctly", async () => {
    const { stdout, exitCode } = await runFixture("metadata_fixture");

    assert.strictEqual(exitCode, 0);

    const jsonLines = parseJsonLines(stdout);

    // Find logs inside scope
    const insideScope = jsonLines.find(
      (line) => line.message === "Inside scoped context"
    );
    assert.ok(insideScope, "Should have log from inside scope");
    assert.strictEqual(insideScope.scope_id, "scope-abc");

    // Find logs in nested scope
    const nestedScope = jsonLines.find(
      (line) => line.message === "Inside nested scope"
    );
    assert.ok(nestedScope, "Should have log from nested scope");
    assert.strictEqual(nestedScope.scope_id, "scope-abc");
    assert.strictEqual(nestedScope.nested, "true");
  });

  test("scope context does not leak outside scope", async () => {
    const { stdout, exitCode } = await runFixture("metadata_fixture");

    assert.strictEqual(exitCode, 0);

    const jsonLines = parseJsonLines(stdout);

    // Find log after scope
    const afterScope = jsonLines.find(
      (line) => line.message === "After scoped context"
    );
    assert.ok(afterScope, "Should have log from after scope");
    assert.strictEqual(
      afterScope.scope_id,
      undefined,
      "Should not have scope_id after scope"
    );
    assert.strictEqual(
      afterScope.nested,
      undefined,
      "Should not have nested after scope"
    );
  });

  test("named logger includes its context", async () => {
    const { stdout, exitCode } = await runFixture("metadata_fixture");

    assert.strictEqual(exitCode, 0);

    const jsonLines = parseJsonLines(stdout);

    // Find the database log
    const dbLog = jsonLines.find((line) => line.logger === "myapp.database");
    assert.ok(dbLog, "Should have database log");
    assert.strictEqual(dbLog.component, "database");
    assert.strictEqual(dbLog.query, "SELECT *");
  });
});

describe(`Runtime Compatibility (${currentRuntime})`, () => {
  test("fixtures run without errors", async () => {
    const fixtures = ["console_fixture", "json_fixture", "metadata_fixture"];

    for (const fixture of fixtures) {
      const { exitCode } = await runFixture(fixture, currentRuntime);
      assert.strictEqual(
        exitCode,
        0,
        `${fixture} should exit successfully on ${currentRuntime}`
      );
    }
  });

  test("JSON output is valid across runtimes", async () => {
    const { stdout, exitCode } = await runFixture(
      "json_fixture",
      currentRuntime
    );

    assert.strictEqual(exitCode, 0);

    // Should be able to parse all JSON lines
    const jsonLines = parseJsonLines(stdout);
    assert.ok(
      jsonLines.length >= 5,
      `Should have at least 5 JSON lines on ${currentRuntime}`
    );
  });
});
