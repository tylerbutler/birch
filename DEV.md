# Development Guide

This document covers development setup, testing, and guidelines for contributors.

## Prerequisites

Ensure you have the correct tool versions installed (see `.tool-versions`):
- Erlang: 27.2.1
- Gleam: 1.14.0
- just: 1.38.0

## Commands

This project uses [just](https://just.systems/) as a task runner. Run `just` to see all available commands.

```bash
just deps         # Download dependencies
just build        # Build for Erlang target (alias: b)
just build-js     # Build for JavaScript target
just build-all    # Build for both targets
just test         # Run tests on both targets (alias: t)
just test-erlang  # Run tests on Erlang only
just test-js      # Run tests on JavaScript only
just format       # Format source code (alias: f)
just format-check # Check formatting without modifying
just check        # Run format-check + tests on both targets (alias: c)
just check-quick  # Run format-check + Erlang tests only
just docs         # Generate documentation (alias: d)
just watch        # Watch and rebuild on changes (requires watchexec)
just watch-test   # Watch and run tests on changes
```

> [!NOTE]
> You can also use `gleam` commands directly (e.g., `gleam build`, `gleam test --target javascript`).

## Code Coverage

This project has code coverage for both Erlang and JavaScript targets.

### Local Coverage

```bash
just coverage              # Run coverage on both Erlang and JavaScript
just coverage-js           # JavaScript only (uses c8/V8 native coverage)
just coverage-erlang       # Erlang only (uses Erlang's cover tool)
just coverage-js-report    # Generate HTML report (after coverage-js)
```

### How It Works

**JavaScript (c8)**: Uses V8's native code coverage via [c8](https://github.com/bcoe/c8). Runs gleeunit tests and integration tests, outputs to `coverage/lcov.info`.

**Erlang (cover)**: Uses a custom escript (`scripts/gleam_cover.escript`) that:
1. Compiles BEAM files with Erlang's [cover](https://www.erlang.org/doc/apps/tools/cover.html) tool
2. Runs tests via EUnit (not gleeunit's `main()` which calls `halt()`)
3. Collects per-module line coverage
4. Optionally exports to LCOV format (`--lcov` flag)

> [!NOTE]
> Erlang coverage line numbers refer to the **generated Erlang code** in `build/dev/erlang/birch/_gleam_artefacts/*.erl`, not the original Gleam source files.

### Why No Hosted Coverage (Codecov/Coveralls)?

Hosted coverage services like Codecov and Coveralls don't work well with Gleam because:

1. **Path mismatch**: Coverage reports contain paths to compiled artifacts (`build/dev/javascript/*.mjs`, `build/dev/erlang/*.erl`), not Gleam source files
2. **Files not in repo**: The compiled files don't exist in the repository
3. **Line number mismatch**: Line numbers in compiled Erlang/JS code don't correspond to Gleam source lines

This is a fundamental limitation of transpiled languages. Elixir works better with these tools because it has tighter BEAM runtime integration, while Gleam compiles via intermediate Erlang source.

**Use local coverage instead** - `just coverage` provides accurate module-level percentages.

### Reusing for Other Gleam Projects

The `scripts/gleam_cover.escript` is generic and can be copied to any Gleam project:

1. Copy `scripts/gleam_cover.escript` to your project
2. Add to your justfile:
   ```just
   coverage-erlang: build
       escript scripts/gleam_cover.escript

   coverage-erlang-lcov: build
       escript scripts/gleam_cover.escript --lcov
   ```
3. The script auto-detects the project name from `gleam.toml` and finds all `*_test` modules

## Project Structure

```
src/
├── birch.gleam                  # Main public API
├── birch/
│   ├── level.gleam              # LogLevel type
│   ├── record.gleam             # LogRecord type and Metadata
│   ├── logger.gleam             # Logger type with handlers and context
│   ├── handler.gleam            # Handler interface
│   ├── formatter.gleam          # Format functions
│   ├── config.gleam             # Global configuration
│   ├── sampling.gleam           # Sampling and rate limiting
│   ├── scope.gleam              # Scoped context
│   ├── handler/
│   │   ├── console.gleam        # Console output with colors
│   │   ├── json.gleam           # JSON-formatted output
│   │   ├── file.gleam           # File output with rotation
│   │   └── async.gleam          # Async (non-blocking) handler
│   └── internal/                # Internal modules (not public API)
├── birch_ffi.erl                # Erlang FFI implementation
└── birch_ffi.mjs                # JavaScript FFI implementation

test/
├── birch_test.gleam             # Unit tests
└── property_test.gleam          # Property-based tests (qcheck)
```

## Cross-Platform FFI

When modifying platform-specific code:

1. Update both `birch_ffi.erl` AND `birch_ffi.mjs`
2. Ensure behavior is consistent across platforms
3. Test on both Erlang and JavaScript targets

## Adding a New Handler

1. Create a new file in `src/birch/handler/`
2. Implement formatting using `formatter.Formatter` type
3. Use `handler.new()` to create the handler
4. Add tests in `test/birch_test.gleam`
5. Document with module-level and function doc comments

## Code Style

- Follow Gleam's built-in formatter (`just format`)
- Use doc comments (`///`) for all public functions and types
- Use `Err` instead of `Error` for the error log level (avoids Result conflict)

## Commit Conventions

This project uses [conventional commits](https://www.conventionalcommits.org/). Commit types and rules are defined in `commit-types.json` (single source of truth).

| Type | Description | In Changelog? |
|------|-------------|---------------|
| `feat` | A new feature | Yes (Features) |
| `fix` | A bug fix | Yes (Bug Fixes) |
| `perf` | Performance improvement | Yes (Performance) |
| `refactor` | Code change (not fix or feature) | Yes (Code Refactoring) |
| `docs` | Documentation only | No |
| `style` | Code style (no logic change) | No |
| `test` | Adding/correcting tests | No |
| `build` | Build system or dependencies | No |
| `ci` | CI configuration | No |
| `chore` | Other changes | No |
| `revert` | Reverts a previous commit | No |

Commits with scopes `ci` or `deps` are excluded from the changelog regardless of type.

If you edit `commit-types.json`, regenerate derived config files:

```bash
just generate-configs    # Regenerate commitlint and git-cliff configs
just check-configs-sync  # Verify configs are in sync
```

This requires the `commit-config-gen` tool (`go install github.com/tylerbutler/commit-config-gen@latest`).

## Release Process

birch uses [changie](https://changie.dev/) for changelog management and a fully automated GitHub Actions pipeline for releases and publishing.

### Overview

```
PR with change fragments → merge to main → release workflow creates release PR
→ merge release PR → auto-tag → publish to Hex.pm
```

### 1. Adding Changelog Entries

Every PR that includes user-facing changes should include a changie fragment. The PR validation workflow checks for this and comments on the PR if an entry is missing.

```bash
just change              # Create a new changelog entry (interactive)
just changelog-preview   # Preview what the next version will look like
```

Changie will prompt you to select a **kind** which determines the version bump:

| Kind | Version Bump |
|------|-------------|
| Added | minor |
| Changed, Deprecated, Fixed, Performance, Removed, Reverted, Dependencies, Security | patch |

Fragments are stored in `.changes/unreleased/` and committed with your PR.

### 2. PR Validation

The PR validation workflow (`.github/workflows/pr.yml`) runs on every PR and:

- **Validates the PR title** against conventional commit format using commitlint
- **Checks for changelog entries** and posts a preview comment if fragments are found, or a reminder if they're missing

### 3. Release PR Creation

The release workflow (`.github/workflows/release.yml`) runs on every push to `main` and can also be triggered manually. It:

1. Checks for unreleased changie fragments in `.changes/unreleased/`
2. If fragments exist, batches them into a versioned changelog entry (version auto-determined from changie kinds)
3. Updates the version in `gleam.toml`
4. Creates or updates a release PR with the changelog changes

If no unreleased fragments are found, the workflow skips silently.

### 4. Auto-tagging

When the release PR is merged to `main`, the auto-tag workflow (`.github/workflows/auto-tag.yml`):

1. Detects the new version from the merged PR
2. Creates a Git tag (`v{version}`)
3. Creates a GitHub Release from the tag

### 5. Publishing to Hex.pm

When a `v*` tag is pushed, the publish workflow (`.github/workflows/publish.yml`):

1. Runs the full CI test suite (both Erlang and JavaScript targets)
2. If tests pass, publishes the package to [Hex.pm](https://hex.pm/packages/birch)

### Workflow Summary

| Workflow | Trigger | Purpose |
|----------|---------|---------|
| `ci.yml` | Push/PR to main | Tests, formatting, docs build |
| `pr.yml` | PR opened/updated | Title validation, changelog check |
| `release.yml` | Push to main | Creates release PR from changie fragments |
| `auto-tag.yml` | Release PR merged | Creates Git tag and GitHub Release |
| `publish.yml` | `v*` tag pushed | Publishes to Hex.pm |

### Required Secrets

| Secret | Purpose |
|--------|---------|
| `RELEASE_PAT` | GitHub PAT with `contents:write` and `pull-requests:write` (needed so release PRs trigger other workflows) |
| `HEXPM_API_KEY` | API key for publishing to Hex.pm |
