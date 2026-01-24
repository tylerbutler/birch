---
name: dual-target-check
description: Run format check and tests on both Erlang and JavaScript targets
disable-model-invocation: true
---

# Dual Target Check

Run the full cross-platform validation suite for birch.

## What This Does

1. **Format check**: Verifies all `.gleam` files are properly formatted
2. **Erlang tests**: Runs `gleam test` on the Erlang/BEAM target
3. **JavaScript tests**: Runs `gleam test --target javascript`

## Command

```bash
just check
```

This is equivalent to running:
```bash
gleam format --check src test && gleam test && gleam test --target javascript
```

## When to Run

- Before committing changes
- After modifying any `.gleam` source file
- After modifying FFI files (`birch_ffi.erl` or `birch_ffi.mjs`)
- Before creating a pull request

## Quick Check Option

For faster iteration during development, use the quick check (Erlang only):

```bash
just check-quick
```

This skips JavaScript tests but still validates formatting and Erlang tests.

## Troubleshooting

If tests fail on one target but pass on another:
1. Check for platform-specific code in `src/birch/internal/platform.gleam`
2. Verify FFI implementations match in both `birch_ffi.erl` and `birch_ffi.mjs`
3. Look for Erlang-specific or JavaScript-specific behavior differences
