# External Integrations

**Analysis Date:** 2025-02-27

## APIs & External Services

**OTP Logger Integration (Erlang-only):**
- Service: Erlang OTP Logger (:logger module)
- What it's used for: Integration with Erlang's built-in logging system
- Implementation: `src/birch/erlang_logger.gleam`
- Use case: Route birch logs through OTP :logger for production Erlang deployments

## Data Storage

**File I/O:**
- Client: simplifile 2.3.2 (Gleam file library)
- Purpose: File handler writes log output to files
- Handler: `src/birch/handler/file.gleam`
- Features:
  - Size-based file rotation
  - Gzip compression of rotated files (via native zlib)

**No Database:**
- Birch is a library, not an application
- No direct database integrations
- Logs are streamed to handlers (console, file, JSON)

## File Storage

**Local Filesystem Only:**
- All file operations are local
- File handler writes to specified paths
- Compressed archived logs also written locally
- No cloud storage integrations

## Authentication & Identity

**Not Applicable:**
- Birch is a logging library with no authentication requirements
- No identity management or API authentication
- No protected resources

## Monitoring & Observability

**Self-Contained:**
- Birch IS the observability tool (logging library)
- No external error tracking dependencies
- Handlers are the integration points:
  - Console output for development
  - File output for persistent logs
  - JSON output for log aggregation

**Handler Interface:**
- Custom handlers can integrate with:
  - Datadog via JSON handler output
  - Honeycomb via JSON handler output
  - CloudWatch via file handler output
  - Any aggregation service that accepts JSON or plaintext logs

## CI/CD & Deployment

**Hosting:**
- Documentation: Netlify (https://birch.tylerbutler.com)
- Repository: GitHub (github.com/tylerbutler/birch)
- Package distribution: Hex (Gleam package manager)

**CI Pipeline:**
- Service: GitHub Actions
- Config: `.github/workflows/ci.yml`
- Local testing: act (can run workflows locally)
- Triggers: push to main, pull requests

**Code Coverage Integration:**
- Services: (Optional) Codecov, Coveralls
- Note: Gleam projects have known coverage line mapping issues
  - Coverage reports show compiled code line numbers
  - Useful for local verification, less suitable for SaaS platforms
- Tools:
  - Erlang target: cover tool (escript scripts/gleam_cover.escript)
  - JavaScript target: c8 with LCOV export

**Documentation Deployment:**
- Platform: Netlify
- Deployment config: `docs-site/netlify.toml`
- Build: `pnpm build` (Astro)
- Publish directory: dist
- Build environment: Node.js 20
- Site URL: https://birch.tylerbutler.com

## Version Management & Release

**Changelog Generation:**
- Tool: changie (via just changelog)
- Source: Commit history with structured types
- Config: `commit-types.json` (single source of truth)
- Generated files: CHANGELOG.md, `.commitlintrc.json` (via commit-config-gen)

**Package Publishing:**
- Destination: Hex (Gleam package registry)
- Package: birch (version 0.3.0)
- Repository: github.com/tylerbutler/birch
- License: MIT
- Dependencies managed via manifest.toml

## Cross-Platform Runtime Support

**JavaScript Runtimes (all supported):**
- Node.js 22+ (primary)
  - AsyncLocalStorage for scoped context (via node:async_hooks)
  - Worker threads (via node:worker_threads)
  - File compression (via node:fs and node:zlib)
- Deno 1.x+
  - Deno.stdout.isTerminal() for TTY detection
  - Deno.Command for gzip compression
  - Deno.env for environment variables
  - No AsyncLocalStorage (stack-based fallback)
- Bun 1.x+
  - require() for native modules (fs, zlib)
  - process object compatibility
  - File compression via native zlib
- Browser (limited)
  - Console output only
  - No file operations
  - No TTY detection

**Erlang Target (BEAM):**
- Standard Erlang 27.2.1 modules:
  - `:os` for environment variables (COLORTERM, TERM, TERM env detection)
  - `:io` for TTY detection and output
  - `:file` for file operations (read, write)
  - `:zlib` for gzip compression
  - `:persistent_term` for global configuration storage
  - `:erlang` for process dictionary (scoped context)
  - `:queue` for async writer queue management

## Scoped Context & Async Operations

**Node.js (Full Support):**
- Uses AsyncLocalStorage from node:async_hooks
- Proper async context propagation
- Process dictionary equivalent via AsyncLocalStorage

**Deno (Limited Support):**
- No AsyncLocalStorage equivalent
- Uses stack-based fallback
- Context is per-scope, not preserved across async boundaries

**Bun (Fallback):**
- Limited AsyncLocalStorage support
- Uses stack-based approach
- Synchronous in most cases

**Erlang (Full Support):**
- Uses Erlang process dictionary (:erlang.get/put)
- Process-local storage (each Erlang process has its own dict)
- Full async support via OTP

## No External Service Dependencies

**What is NOT integrated:**
- No cloud platforms (AWS, GCP, Azure)
- No message queues (RabbitMQ, Kafka)
- No APM tools (New Relic, DataDog agents)
- No secrets management (Vault, AWS Secrets Manager)
- No webhooks (incoming or outgoing)
- No HTTP APIs to external services

Birch is a self-contained logging library designed for flexibility. External integrations happen at the application layer through custom handlers or by consuming JSON/plaintext output.

---

*Integration audit: 2025-02-27*
