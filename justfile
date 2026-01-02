# justfile for gleam_log
# https://just.systems/

# Default recipe: show available commands
default:
    @just --list

# Aliases
alias b := build
alias t := test
alias f := format
alias c := check
alias d := docs

# Build the project (Erlang target)
build:
    gleam build

# Build for JavaScript target
build-js:
    gleam build --target javascript

# Build for all targets
build-all: build build-js

# Run tests on Erlang target
test-erlang:
    gleam test

# Run tests on JavaScript target
test-js:
    gleam test --target javascript

# Run tests on both targets
test: test-erlang test-js

# Format source code
format:
    gleam format src test

# Check formatting
format-check:
    gleam format --check src test

# Generate documentation
docs:
    gleam docs build

# Download dependencies
deps:
    gleam deps download

# Clean build artifacts
clean:
    rm -rf build

# Run all checks (format + tests)
check: format-check test

# Run quick checks (format + erlang tests only)
check-quick: format-check test-erlang

# Watch and rebuild on changes (requires watchexec)
watch:
    watchexec -e gleam -r -- gleam build

# Watch and run tests on changes (requires watchexec)
watch-test:
    watchexec -e gleam -r -- gleam test

# Run JavaScript tests with code coverage (requires npm install)
coverage:
    npm run test:coverage

# Generate coverage report (after running coverage)
coverage-report:
    npm run coverage:report

# Generate LCOV coverage report for CI integration
coverage-lcov:
    npm run coverage:lcov

# ============================================================================
# Integration Tests (JavaScript Target)
# ============================================================================

# Run integration tests on Node.js
test-integration-node: build-js
    node --test test/integration/test_runner.mjs

# Run integration tests on Deno
test-integration-deno: build-js
    deno test --allow-read --allow-env --allow-run --allow-write test/integration/test_runner.mjs

# Run integration tests targeting Bun runtime
# Note: Uses Node.js test runner but spawns Bun subprocesses for the actual test execution
test-integration-bun: build-js
    INTEGRATION_TEST_RUNTIME=bun node --test test/integration/test_runner.mjs

# Run integration tests on all available runtimes
test-integration: test-integration-node

# Run full integration test suite (all runtimes, requires deno and bun installed)
test-integration-all: test-integration-node test-integration-deno test-integration-bun
