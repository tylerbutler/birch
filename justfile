# justfile for birch
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

# Build with warnings as errors (Erlang target)
build-strict:
    gleam build --warnings-as-errors

# Build with warnings as errors (JavaScript target)
build-strict-js:
    gleam build --target javascript --warnings-as-errors

# Build with warnings as errors (all targets)
build-strict-all: build-strict build-strict-js

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

# Full local validation (no act/Docker required)
# Covers: format, strict build, tests, examples (Erlang), integration (Node.js)
check-full: format-check build-strict-all test test-examples test-integration-node

# Watch and rebuild on changes (requires watchexec)
watch:
    watchexec -e gleam -r -- gleam build

# Watch and run tests on changes (requires watchexec)
watch-test:
    watchexec -e gleam -r -- gleam test

# Run JavaScript tests with code coverage (requires npm install)
coverage:
    npm run test:coverage

# Run all tests (gleeunit + integration) with combined coverage
coverage-all:
    npm run test:all:coverage

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

# ============================================================================
# Demo
# ============================================================================

# Run the console handler demo showcasing all presentation options
demo:
    cd demo && gleam run

# Run the console handler demo on JavaScript target
demo-js:
    cd demo && gleam run --target javascript

# ============================================================================
# Examples
# ============================================================================

# Test all examples on Erlang target
test-examples:
    #!/usr/bin/env bash
    set -e
    for dir in examples/*/; do
        echo "Testing $dir (Erlang)..."
        (cd "$dir" && gleam deps download && gleam test)
    done

# Quick smoke test of representative examples (Erlang)
test-examples-smoke:
    #!/usr/bin/env bash
    set -e
    for dir in examples/01-quick-start examples/06-json-handler examples/07-file-handler; do
        echo "Testing $dir (Erlang)..."
        (cd "$dir" && gleam deps download && gleam test)
    done

# Test all examples on Node.js (excluding BEAM-only examples)
test-examples-node:
    #!/usr/bin/env bash
    set -e
    for dir in examples/*/; do
        if [[ "$dir" != *"16-erlang-logger"* ]]; then
            echo "Testing $dir (Node.js)..."
            (cd "$dir" && gleam deps download && gleam test --target javascript)
        fi
    done

# Test all examples on Deno (excluding BEAM-only examples)
test-examples-deno:
    #!/usr/bin/env bash
    set -e
    for dir in examples/*/; do
        if [[ "$dir" != *"16-erlang-logger"* ]]; then
            name=$(basename "$dir")
            pkg_name="birch_example_${name//-/_}"
            echo "Testing $dir (Deno)..."
            # gleam run generates gleam.main.mjs pointing to the app's main()
            (cd "$dir" && gleam deps download && gleam run --target javascript > /dev/null && \
                deno run --no-check --allow-read --allow-env --allow-write "build/dev/javascript/${pkg_name}/gleam.main.mjs")
        fi
    done

# Test all examples on Bun (excluding BEAM-only examples)
test-examples-bun:
    #!/usr/bin/env bash
    set -e
    for dir in examples/*/; do
        if [[ "$dir" != *"16-erlang-logger"* ]]; then
            name=$(basename "$dir")
            pkg_name="birch_example_${name//-/_}"
            echo "Testing $dir (Bun)..."
            # gleam run generates gleam.main.mjs pointing to the app's main()
            (cd "$dir" && gleam deps download && gleam run --target javascript > /dev/null && \
                bun "build/dev/javascript/${pkg_name}/gleam.main.mjs")
        fi
    done

# Alias for Node.js (default JavaScript runtime)
test-examples-js: test-examples-node

# Test all examples on Erlang and Node.js
test-examples-all: test-examples test-examples-node

# Test all examples on all runtimes (requires deno and bun installed)
test-examples-all-runtimes: test-examples test-examples-node test-examples-deno test-examples-bun

# Test a specific example on Erlang target
test-example-erlang example:
    #!/usr/bin/env bash
    cd "examples/{{example}}"
    gleam deps download
    gleam test

# Test a specific example on Node.js (via gleam test)
test-example-node example:
    #!/usr/bin/env bash
    cd "examples/{{example}}"
    gleam deps download
    gleam test --target javascript

# Test a specific example on Deno (runs main function via gleam.main.mjs)
test-example-deno example:
    #!/usr/bin/env bash
    cd "examples/{{example}}"
    gleam deps download
    # gleam run generates gleam.main.mjs pointing to the app's main()
    gleam run --target javascript > /dev/null
    pkg_name="birch_example_$(echo '{{example}}' | tr '-' '_')"
    deno run --no-check --allow-read --allow-env --allow-write "build/dev/javascript/${pkg_name}/gleam.main.mjs"

# Test a specific example on Bun (runs main function via gleam.main.mjs)
test-example-bun example:
    #!/usr/bin/env bash
    cd "examples/{{example}}"
    gleam deps download
    # gleam run generates gleam.main.mjs pointing to the app's main()
    gleam run --target javascript > /dev/null
    pkg_name="birch_example_$(echo '{{example}}' | tr '-' '_')"
    bun "build/dev/javascript/${pkg_name}/gleam.main.mjs"

# ============================================================================
# Act-based CI Testing (https://github.com/nektos/act)
# Requires: Docker running, act installed (https://nektosact.com/installation/)
# ============================================================================

# List all act CI jobs available
ci-list:
    act -l

# Run full local CI validation (no Docker required)
ci: check-full

# Run a specific CI job locally
ci-job job:
    act -j {{job}}

# Run CI test job for Erlang target
ci-test-erlang:
    act -j test --matrix target:erlang

# Run CI test job for JavaScript target
ci-test-js:
    act -j test --matrix target:javascript

# Run CI coverage job
ci-coverage:
    act -j coverage

# Run CI integration tests (all runtimes)
ci-integration:
    act -j integration-test

# Run CI integration test for a specific runtime (node, deno, bun)
ci-integration-runtime runtime:
    act -j integration-test --matrix runtime:{{runtime}}

# Run CI docs job
ci-docs:
    act -j docs

# Run CI examples job for a specific example and target
ci-example example target="erlang":
    act -j examples --matrix example:{{example}} --matrix target:{{target}}

# Run all CI examples for Erlang target only (faster validation)
ci-examples-erlang:
    act -j examples --matrix target:erlang

# Dry-run CI (show what would execute without running)
ci-dry:
    act push --dryrun

# Run CI with verbose output for debugging
ci-verbose:
    act push --verbose --concurrent-jobs 1

# Run CI on host system without Docker (uses locally installed tools)
ci-host:
    act push -P ubuntu-latest=-self-hosted --concurrent-jobs 1

# Run specific CI job on host system without Docker
ci-host-job job:
    act -j {{job}} -P ubuntu-latest=-self-hosted
