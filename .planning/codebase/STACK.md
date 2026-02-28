# Technology Stack

**Analysis Date:** 2025-02-27

## Languages

**Primary:**
- Gleam 1.14.0 - Core library implementation (cross-platform)
- Erlang 27.2.1 - Runtime for Erlang target (BEAM VM)
- JavaScript (ES modules) - Runtime for JavaScript target (Node.js, Deno, Bun)

**Secondary:**
- Erlang FFI (birch_ffi.erl) - Platform-specific operations on Erlang target
- JavaScript FFI (birch_ffi.mjs) - Platform-specific operations on JavaScript target
- HTML/CSS - Documentation site (Astro/Starlight)

## Runtime

**Primary Environments:**
- Erlang VM (BEAM) - Production target via OTP/gleam_otp
- Node.js 22 - JavaScript testing and runtime
- Deno - Alternative JavaScript runtime (via gleam build --target javascript)
- Bun - Alternative JavaScript runtime (via gleam build --target javascript)
- Browser - Theoretical support (limited features)

**Package Manager:**
- pnpm (monorepo + Node.js dependencies)
- Gleam package manager (via gleam deps download)

## Frameworks & Build Tools

**Core:**
- Gleam 1.14.0 - Language/compiler
- just 1.38.0 - Task runner (justfile configuration)
- OTP (Erlang Open Telecom Platform) - Distributed systems via gleam_otp

**Testing:**
- gleeunit 1.9.0 - Test runner for Gleam
- qcheck 1.0.4 - Property-based testing
- vitest - (referenced in npm scripts for integration tests, Node.js only)
- c8 10.1.3 - Code coverage for JavaScript target

**Documentation:**
- Astro 5.17.1 - Static site generator (docs-site)
- Starlight 0.37.0 - Documentation theme
- Gleam docs - Built-in documentation generator (gleam docs build)

**CI/CD:**
- act 1.x - Local GitHub Actions testing (via mise)
- GitHub Actions - Cloud CI pipeline (.github/workflows/ci.yml)

**Development Tools:**
- mise - Multi-language tool manager
- watchexec - File watching for development
- Go 1.23 - Required for commit-config-gen tool
- commit-config-gen - Configuration generation from commit-types.json
- changie - Changelog generation

## Key Dependencies

**Runtime (Gleam Packages):**
- gleam_stdlib 0.68.1 - Gleam standard library
- gleam_json 3.1.0 - JSON encoding/decoding
- gleam_otp 1.2.0 - OTP integration for async handlers
- gleam_erlang 1.3.0 - Erlang FFI bindings
- gleam_time 1.7.0 - Time operations
- simplifile 2.3.2 - File I/O operations (with filepath 1.1.2)

**Development (Gleam Packages):**
- gleeunit 1.9.0 - Unit testing framework
- qcheck 1.0.4 - Property testing (depends on: exception, gleam_regexp, gleam_stdlib, gleam_yielder, prng)

**Dev Dependencies (npm):**
- c8 10.1.3 - Code coverage reporting

**Documentation (npm):**
- @astrojs/starlight 0.37.0 - Documentation framework
- @astrojs/check 0.9.6 - Type checking
- @astrojs/netlify 6.6.3 - Netlify adapter
- starlight-links-validator 0.19.1 - Link validation
- sharp 0.34.5 - Image optimization
- typescript 5.9.3 - Type checking (docs-site)
- @fontsource/open-sans 5.2.7 - Font
- @fontsource-variable/fira-code 5.2.7 - Monospace font

## Configuration

**Environment:**
- Tool versions specified in `.tool-versions` (Erlang, Gleam, just)
- Additional dev tools configured in `.mise.toml` (act, bun, deno, go, node)
- Locale hardcoded: `LC_ALL = "en_US.UTF-8"` (mise.toml)

**Build Configuration:**
- `gleam.toml` - Gleam package manifest
  - Internal modules marked: birch/internal, birch/internal/*, birch/sampling, birch/handler/async
  - Version: 0.3.0
  - MIT license
  - Repository: github.com/tylerbutler/birch

**Testing Configuration:**
- `package.json` - Node.js test scripts (JavaScript target)
  - c8 coverage config: includes build/dev/javascript/birch/**/*.mjs, excludes *_test.mjs
  - Coverage reports: text, html, lcov formats
- No explicit test configuration file for gleam (tests discovered by naming: *_test.gleam)

**Documentation Site:**
- `docs-site/astro.config.mjs` - Astro configuration
  - Output: static site
  - Netlify adapter with imageCDN disabled
  - Site URL: https://birch.tylerbutler.com
  - Starlight integration with custom CSS and Gleam syntax highlighting
- `docs-site/netlify.toml` - Netlify deployment config
  - Build command: `pnpm build`
  - Publish directory: dist
  - Node.js 20

**Commit Configuration:**
- `commit-types.json` - Single source of truth for commit types and changelog
- `.commitlintrc.json` - commitlint configuration (generated from commit-types.json)
- `.changie.yaml` - Changelog generation config

## Platform Requirements

**Development:**
- Erlang 27.2.1 (required by CI and .tool-versions)
- Gleam 1.14.0 (required)
- just 1.38.0 (required)
- pnpm (required for Node.js dependencies)
- Node.js 22 (for JavaScript target, integration tests, and docs-site)
- Deno, Bun (optional, for alternative JavaScript runtime testing)
- Docker + act (optional, for local CI validation)
- watchexec (optional, for watch mode development)

**Production (Erlang Target):**
- Erlang VM 27.2.1 (or compatible version)
- OTP libraries (included with Erlang)

**Production (JavaScript Target):**
- Node.js 22+ (primary)
- Deno 1.x+ (alternative)
- Bun 1.x+ (alternative)

## Build Outputs

**Erlang Target:**
- `build/dev/erlang/` - Development artifacts
- `build/prod/` - Production builds (when relevant)
- Compiled .beam files for BEAM VM

**JavaScript Target:**
- `build/dev/javascript/birch/` - ES modules (.mjs files)
- Named after package: birch_*.mjs modules
- Supports tree-shaking and dynamic imports

**Documentation:**
- `docs-site/dist/` - Built static site (Astro output)
- Deployed to https://birch.tylerbutler.com via Netlify

## Special Configurations

**Cross-Platform FFI:**
- FFI layer in `src/birch_ffi.erl` (Erlang) and `src/birch_ffi.mjs` (JavaScript)
- Handles platform-specific:
  - TTY detection
  - Color depth detection
  - File operations (gzip compression via zlib on Node.js, via gzip command on Deno)
  - Global configuration storage (persistent_term on Erlang, module-level vars on JS)
  - Async writer management

**Module Privacy:**
- Internal modules marked in gleam.toml prevent public export
- Pattern: `birch/internal/*` modules are implementation details

**Version Management:**
- Single version source: gleam.toml (version = "0.3.0")
- CHANGELOG.md auto-generated from commit history
- version.txt file tracks current version

---

*Stack analysis: 2025-02-27*
