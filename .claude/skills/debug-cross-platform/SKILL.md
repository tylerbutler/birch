---
name: debug-cross-platform
description: Debug issues that only occur on one target (Erlang or JavaScript). Use when tests pass on one target but fail on the other.
disable-model-invocation: true
---

# Cross-Platform Debugging

Systematic approach to debugging issues that only occur on Erlang OR JavaScript.

## Quick Diagnosis

```bash
# Run on both targets to identify which fails
just test-erlang
just test-js
```

## Common Causes

### 1. FFI Implementation Mismatch

**Symptom**: Different behavior between targets for the same function.

**Check**:
```bash
# Compare FFI implementations
diff <(grep -A 20 "function_name" src/birch_ffi.erl) \
     <(grep -A 20 "function_name" src/birch_ffi.mjs)
```

**Common issues**:
- Return type mismatch (tuples vs arrays)
- Error handling differences
- String encoding (UTF-8 handling)

### 2. Platform-Specific APIs

**Symptom**: Function works on one platform, undefined/error on other.

**JavaScript-only APIs**:
- `process.stdout`, `process.stderr` (Node.js)
- `Deno.isatty()` (Deno)
- `console.log` behavior varies by runtime

**Erlang-only APIs**:
- Process dictionary (`erlang:put/get`)
- OTP behaviors (`gen_server`, etc.)
- `:logger` integration

### 3. Async Behavior

**Symptom**: Race conditions or timing issues on JavaScript.

**Check**:
- Erlang is synchronous by default
- JavaScript may need `await` or callbacks
- Look for Promise handling in FFI

### 4. Type Coercion

**Symptom**: Unexpected values or type errors.

| Gleam Type | Erlang | JavaScript |
|------------|--------|------------|
| `Int` | Integer | Number |
| `Float` | Float | Number |
| `String` | Binary | String |
| `List` | List | Array |
| `Tuple` | Tuple | Array |
| `Nil` | `nil` atom | `undefined` |

### 5. TTY/Terminal Detection

**Symptom**: Color output works on one platform.

**Check**:
```gleam
// In platform.gleam
is_stdout_tty()  // Different implementation per platform
```

## Debugging Steps

### Step 1: Isolate the Failure

```bash
# Run specific test
gleam test --target erlang -- --filter "test_name"
gleam test --target javascript -- --filter "test_name"
```

### Step 2: Add Debug Output

```gleam
// Temporary debug logging
import gleam/io
io.debug(value)  // Works on both targets
```

### Step 3: Check FFI

1. Read `src/birch/internal/platform.gleam` for declarations
2. Compare `src/birch_ffi.erl` and `src/birch_ffi.mjs`
3. Look for behavioral differences

### Step 4: Test JavaScript Runtimes

```bash
# Test on all JS runtimes
just test-integration-node
just test-integration-deno
just test-integration-bun
```

### Step 5: Check Runtime Detection

In `birch_ffi.mjs`, verify runtime detection:
```javascript
// Node.js
typeof process !== 'undefined' && process.versions?.node

// Deno
typeof Deno !== 'undefined'

// Bun
typeof Bun !== 'undefined'

// Browser
typeof window !== 'undefined'
```

## FFI Debugging Checklist

- [ ] Both implementations exist for the function
- [ ] Return types match Gleam declaration
- [ ] Error cases handled identically
- [ ] Edge cases (empty string, null, special chars) tested
- [ ] All JS runtimes have fallbacks (Node, Deno, Bun, browser)

## Quick Fixes

| Issue | Erlang Fix | JavaScript Fix |
|-------|------------|----------------|
| Undefined function | Check module exports | Check `export` statement |
| Wrong return type | Match Gleam type exactly | Return correct JS type |
| Encoding issues | Use `unicode:characters_to_binary` | Use `TextEncoder` |
| TTY detection | Check `os:type()` | Check `process.stdout.isTTY` |
