# Typed Metadata Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace stringly-typed `Metadata` with a `MetadataValue` sum type and add a `birch/meta` helper module.

**Architecture:** Change `Metadata` from `List(#(String, String))` to `List(#(String, MetadataValue))` where `MetadataValue` is a four-variant sum type. Add `birch/meta` module with ergonomic constructors. Update all formatters, handlers, FFI, tests, and examples.

**Tech Stack:** Gleam, Erlang FFI, JavaScript FFI

**Design doc:** `docs/plans/2026-02-21-typed-metadata-design.md`

---

### Task 1: Create MetadataValue type and meta module

This is a type-propagation refactoring — changing the core type breaks everything until all consumers are updated. This task and the next several tasks form one atomic unit. **The project will not compile until Task 8 is complete.**

**Files:**
- Modify: `src/birch/record.gleam`
- Create: `src/birch/meta.gleam`

**Step 1: Add MetadataValue type to record.gleam**

In `src/birch/record.gleam`, replace the Metadata type alias (lines 10-13):

```gleam
/// A typed value in a metadata entry.
/// Supports the four common structured logging value types.
pub type MetadataValue {
  StringVal(String)
  IntVal(Int)
  FloatVal(Float)
  BoolVal(Bool)
}

/// Metadata is a list of key-value pairs attached to a log record.
/// Keys are strings; values are typed via MetadataValue.
pub type Metadata =
  List(#(String, MetadataValue))
```

**Step 2: Add metadata_value_to_string to record.gleam**

Add after the `MetadataValue` type definition:

```gleam
/// Convert a MetadataValue to its string representation.
pub fn metadata_value_to_string(value: MetadataValue) -> String {
  case value {
    StringVal(s) -> s
    IntVal(i) -> int.to_string(i)
    FloatVal(f) -> float.to_string(f)
    BoolVal(b) -> case b {
      True -> "true"
      False -> "false"
    }
  }
}
```

Add imports for `gleam/int` and `gleam/float` at the top of record.gleam.

**Step 3: Update get_metadata return type**

In `src/birch/record.gleam`, update `get_metadata` (line 61):

```gleam
/// Get a metadata value by key.
pub fn get_metadata(record: LogRecord, key: String) -> Result(MetadataValue, Nil) {
  list.find_map(record.metadata, fn(pair) {
    case pair {
      #(k, v) if k == key -> Ok(v)
      _ -> Error(Nil)
    }
  })
}
```

**Step 4: Create birch/meta.gleam**

Create `src/birch/meta.gleam`:

```gleam
//// Ergonomic metadata constructors.
////
//// Provides typed helper functions for building metadata entries,
//// following the same pattern as `gleam/json`.
////
//// ## Example
////
//// ```gleam
//// import birch/meta
//// import birch/logger
////
//// logger.info(lgr, "Request processed", [
////   meta.string("request_id", request_id),
////   meta.int("status", 200),
////   meta.float("duration_ms", 42.5),
////   meta.bool("cached", True),
//// ])
//// ```

import birch/record.{type MetadataValue, BoolVal, FloatVal, IntVal, StringVal}

/// Create a string metadata entry.
pub fn string(key: String, value: String) -> #(String, MetadataValue) {
  #(key, StringVal(value))
}

/// Create an integer metadata entry.
pub fn int(key: String, value: Int) -> #(String, MetadataValue) {
  #(key, IntVal(value))
}

/// Create a float metadata entry.
pub fn float(key: String, value: Float) -> #(String, MetadataValue) {
  #(key, FloatVal(value))
}

/// Create a boolean metadata entry.
pub fn bool(key: String, value: Bool) -> #(String, MetadataValue) {
  #(key, BoolVal(value))
}
```

**Step 5: Commit**

```bash
git add src/birch/record.gleam src/birch/meta.gleam
git commit -m "feat: add MetadataValue sum type and birch/meta module

Part of #58. Changes Metadata from List(#(String, String)) to
List(#(String, MetadataValue)) with StringVal, IntVal, FloatVal,
BoolVal variants. Adds birch/meta helper module."
```

---

### Task 2: Update formatter.gleam

**Files:**
- Modify: `src/birch/formatter.gleam`

**Step 1: Update format_metadata_colored**

In `src/birch/formatter.gleam`, update the `format_metadata_colored` function (lines 69-86) to use `metadata_value_to_string`:

```gleam
pub fn format_metadata_colored(
  metadata: record.Metadata,
  use_color: Bool,
) -> String {
  metadata
  |> list.map(fn(pair) {
    let #(key, value) = pair
    let value_str = record.metadata_value_to_string(value)
    let formatted_kv = key <> "=" <> escape_value(value_str)
    case use_color {
      True -> {
        let color = hash_color(key)
        color <> formatted_kv <> ansi.reset
      }
      False -> formatted_kv
    }
  })
  |> string.join(" ")
}
```

**Step 2: Commit**

```bash
git add src/birch/formatter.gleam
git commit -m "refactor: update formatter to use MetadataValue

Use record.metadata_value_to_string for metadata formatting."
```

---

### Task 3: Update JSON handler for typed output

**Files:**
- Modify: `src/birch/handler/json.gleam`

**Step 1: Add metadata_value_to_json helper**

Add a helper function in `src/birch/handler/json.gleam`:

```gleam
import birch/record.{type LogRecord, type MetadataValue, StringVal, IntVal, FloatVal, BoolVal}
```

Add the helper function before the builder functions:

```gleam
/// Convert a MetadataValue to a typed JSON value.
fn metadata_value_to_json(value: MetadataValue) -> Json {
  case value {
    StringVal(s) -> json.string(s)
    IntVal(i) -> json.int(i)
    FloatVal(f) -> json.float(f)
    BoolVal(b) -> json.bool(b)
  }
}
```

**Step 2: Update add_metadata builder**

Update the `add_metadata` function (lines 98-102):

```gleam
pub fn add_metadata(b: JsonBuilder) -> JsonBuilder {
  add_field(b, fn(r) {
    list.map(r.metadata, fn(pair) { #(pair.0, metadata_value_to_json(pair.1)) })
  })
}
```

**Step 3: Update format_json default formatter**

Update `format_json` (lines 198-212):

```gleam
pub fn format_json(record: LogRecord) -> String {
  let base_fields = [
    #("timestamp", json.string(record.timestamp)),
    #("level", json.string(level.to_string_lowercase(record.level))),
    #("logger", json.string(record.logger_name)),
    #("message", json.string(record.message)),
  ]

  let metadata_fields =
    list.map(record.metadata, fn(pair) { #(pair.0, metadata_value_to_json(pair.1)) })

  list.append(base_fields, metadata_fields)
  |> json.object
  |> json.to_string
}
```

**Step 4: Commit**

```bash
git add src/birch/handler/json.gleam
git commit -m "feat: JSON handler outputs typed metadata values

Integer, float, and boolean metadata values are now emitted as
proper JSON types instead of strings. Closes #58."
```

---

### Task 4: Update console handler

**Files:**
- Modify: `src/birch/handler/console.gleam`

**Step 1: Update format_metadata_visible**

The function at line 268 filters metadata then delegates to `formatter.format_metadata_colored`. The filtering uses `pair.0` (the key), which is still a `String` — no change needed in the filter logic. The function should compile as-is once formatter.gleam is updated.

Verify no other metadata construction happens in console.gleam. The semantic log functions (`success`, `start`, `ready`, `fail`) accept `Metadata` parameters and pass them through — their signatures are correct via the type alias.

**Step 2: Commit (if changes needed)**

```bash
git add src/birch/handler/console.gleam
git commit -m "refactor: update console handler for MetadataValue type"
```

---

### Task 5: Update logger.gleam

**Files:**
- Modify: `src/birch/logger.gleam`

**Step 1: Update extract_error_metadata**

In `src/birch/logger.gleam`, update `extract_error_metadata` (lines 482-487):

```gleam
fn extract_error_metadata(result: Result(a, e)) -> Metadata {
  case result {
    Ok(_) -> []
    Error(e) -> [#("error", record.StringVal(string.inspect(e)))]
  }
}
```

Add `import birch/record` to imports if not already importing the constructors. The import at line 12 already imports `{type Metadata}` — update to:

```gleam
import birch/record.{type Metadata, StringVal}
```

**Step 2: Verify all other functions**

All other functions in logger.gleam (`log`, `log_lazy`, `trace`, `debug`, `info`, `warn`, `error`, `fatal`, their `_lazy` variants, `error_result`, `fatal_result`, `merge_metadata`, `emit_record`, `with_context`, `get_context`) accept `Metadata` as a parameter type. Since `Metadata` is a type alias, these signatures don't need textual changes — they automatically pick up the new definition.

The `merge_metadata` function at line 274 uses `list.append` which works on any list type. No change needed.

The `with_context` function at line 127 uses `list.append`. No change needed.

**Step 3: Commit**

```bash
git add src/birch/logger.gleam
git commit -m "refactor: update logger extract_error_metadata for MetadataValue"
```

---

### Task 6: Update config.gleam, scope.gleam, birch.gleam

**Files:**
- Modify: `src/birch/config.gleam`
- Modify: `src/birch/scope.gleam`
- Modify: `src/birch.gleam`

**Step 1: config.gleam**

No code changes needed. All functions use the `Metadata` type alias, which resolves to the new definition. The `context: []` default at line 103 is still valid (empty list).

**Step 2: scope.gleam**

No code changes needed. Uses `list.append` for merging, which works on any list. The `Metadata` type alias propagates.

**Step 3: birch.gleam**

The only code change needed is in the docstring examples that show `#("key", "value")` syntax. These are in comments/docs, not executable code. The actual function signatures all use the `Metadata` type alias.

Check if any inline metadata construction exists in birch.gleam:

The doc examples in `with_scope` (line 620) show `[#("request_id", request_id)]` — these are in doc comments, not code. No runtime code changes needed.

**Step 4: Commit**

```bash
git add src/birch/config.gleam src/birch/scope.gleam src/birch.gleam
git commit -m "refactor: update module doc examples for MetadataValue type"
```

---

### Task 7: Update erlang_logger.gleam and Erlang FFI

**Files:**
- Modify: `src/birch/erlang_logger.gleam`
- Modify: `src/birch_logger_formatter.erl`

**Step 1: erlang_logger.gleam**

No code changes needed. The `FormatCallback` type at line 258-259 uses `record.Metadata` which is a type alias. It propagates automatically.

**Step 2: Update birch_logger_formatter.erl format_metadata**

The `format_metadata` function (lines 112-137) currently converts all OTP metadata values to binary strings and returns `{KeyBin, ValueBin}` tuples. Update it to wrap values in `MetadataValue` constructors.

Gleam compiles custom type constructors to Erlang atoms/tuples:
- `StringVal("hello")` → `{string_val, <<"hello">>}`
- `IntVal(42)` → `{int_val, 42}`
- `FloatVal(3.14)` → `{float_val, 3.14}`
- `BoolVal(True)` → `{bool_val, true}`
- `BoolVal(False)` → `{bool_val, false}`

Replace lines 112-137:

```erlang
%% Format metadata to Gleam format.
%% Returns a list of {Key, MetadataValue} tuples.
%% MetadataValue is a Gleam sum type: StringVal | IntVal | FloatVal | BoolVal.
%% Filters out internal :logger metadata and birch-specific keys.
format_metadata(Meta) ->
    InternalKeys = [time, mfa, file, line, gl, pid, domain, report_cb,
                    birch_logger_name, birch_metadata, birch_caller_id],
    lists:filtermap(fun({Key, Value}) ->
        case lists:member(Key, InternalKeys) of
            true -> false;
            false ->
                KeyBin = case Key of
                    K when is_atom(K) -> atom_to_binary(K, utf8);
                    K when is_binary(K) -> K;
                    K -> list_to_binary(io_lib:format("~p", [K]))
                end,
                MetadataValue = case Value of
                    V when is_boolean(V) -> {bool_val, V};
                    V when is_integer(V) -> {int_val, V};
                    V when is_float(V) -> {float_val, V};
                    V when is_binary(V) -> {string_val, V};
                    V when is_list(V) ->
                        Bin = try unicode:characters_to_binary(V)
                              catch _:_ -> list_to_binary(io_lib:format("~p", [V]))
                              end,
                        {string_val, Bin};
                    V when is_atom(V) -> {string_val, atom_to_binary(V, utf8)};
                    V -> {string_val, list_to_binary(io_lib:format("~p", [V]))}
                end,
                {true, {KeyBin, MetadataValue}}
        end
    end, maps:to_list(Meta)).
```

**Important:** `is_boolean` must come before `is_atom` because Erlang booleans are atoms. Also, `is_boolean` must come before `is_integer` because in some contexts boolean checks can interact with integer guards.

**Step 3: Verify Gleam constructor compilation**

Run `gleam build` (it won't succeed yet, but check the generated Erlang in `build/dev/erlang/birch/_gleam_artefacts/birch@record.erl` to confirm constructor names match `{string_val, ...}`, `{int_val, ...}`, etc.).

**Step 4: Commit**

```bash
git add src/birch/erlang_logger.gleam src/birch_logger_formatter.erl
git commit -m "refactor: update Erlang FFI to produce typed MetadataValue tuples

OTP logger metadata values are now wrapped in MetadataValue
constructors: integers -> IntVal, floats -> FloatVal,
booleans -> BoolVal, everything else -> StringVal."
```

---

### Task 8: Update JavaScript FFI

**Files:**
- Modify: `src/birch_ffi.mjs`

**Step 1: Determine JS representation of MetadataValue**

Build the project first to check how Gleam compiles `MetadataValue` to JavaScript. Look in `build/dev/javascript/birch/birch/record.mjs` for the constructor classes. They will be something like:

```javascript
export class StringVal extends CustomType { constructor(x0) { super(); this[0] = x0; } }
export class IntVal extends CustomType { constructor(x0) { super(); this[0] = x0; } }
export class FloatVal extends CustomType { constructor(x0) { super(); this[0] = x0; } }
export class BoolVal extends CustomType { constructor(x0) { super(); this[0] = x0; } }
```

**Step 2: Update run_with_scope in birch_ffi.mjs**

At line 608, the `_scope_highlight_keys` metadata pair is constructed as a raw JS array (Gleam tuple):

```javascript
const highlightKeysPair = ["_scope_highlight_keys", highlightKeysValue];
```

This needs to wrap the value in `StringVal`. Add an import at the top of `birch_ffi.mjs`:

```javascript
import { StringVal } from "./birch/record.mjs";
```

Then update line 608:

```javascript
const highlightKeysPair = ["_scope_highlight_keys", new StringVal(highlightKeysValue)];
```

**Step 3: Commit**

```bash
git add src/birch_ffi.mjs
git commit -m "refactor: update JS FFI to use StringVal for scope metadata"
```

---

### Task 9: Update tests

**Files:**
- Modify: `test/birch_test.gleam` (~61 occurrences of `#("key", "value")`)
- Modify: `test/erlang_logger_test.gleam` (~3 occurrences)
- Modify: `test/integration/fixtures/metadata_fixture.gleam` (~6 occurrences)
- Modify: `test/integration/fixtures/json_fixture.gleam` (~2 occurrences)
- Modify: `test/integration/fixtures/console_fixture.gleam` (~2 occurrences)

**Step 1: Update test imports**

Add to test file imports:

```gleam
import birch/meta
import birch/record.{StringVal, IntVal, FloatVal, BoolVal}
```

**Step 2: Replace all metadata tuple construction**

Replace all `#("key", "value")` patterns with `meta.string("key", "value")`. Use find-and-replace across test files.

Examples of transformations in `test/birch_test.gleam`:

- `[#("key", "value")]` → `[meta.string("key", "value")]`
- `[#("key1", "value1"), #("key2", "value2")]` → `[meta.string("key1", "value1"), meta.string("key2", "value2")]`
- `[#("method", "POST"), #("path", "/api/users")]` → `[meta.string("method", "POST"), meta.string("path", "/api/users")]`

**Step 3: Update get_metadata assertions**

Tests that assert on `get_metadata` results need updating. For example:

```gleam
// Before:
|> should.equal(Ok("value"))

// After:
|> should.equal(Ok(StringVal("value")))
```

**Step 4: Add new tests for typed metadata**

Add tests that verify typed values work correctly:

```gleam
pub fn meta_string_test() {
  meta.string("key", "value")
  |> should.equal(#("key", StringVal("value")))
}

pub fn meta_int_test() {
  meta.int("count", 42)
  |> should.equal(#("count", IntVal(42)))
}

pub fn meta_float_test() {
  meta.float("rate", 3.14)
  |> should.equal(#("rate", FloatVal(3.14)))
}

pub fn meta_bool_test() {
  meta.bool("active", True)
  |> should.equal(#("active", BoolVal(True)))
}
```

Add a test for typed JSON output:

```gleam
pub fn json_typed_metadata_test() {
  let record = record.new(
    timestamp: "2024-01-01T00:00:00.000Z",
    level: level.Info,
    logger_name: "test",
    message: "typed",
    metadata: [
      meta.int("count", 42),
      meta.float("rate", 3.14),
      meta.bool("active", True),
      meta.string("name", "test"),
    ],
  )
  let output = json.format_json(record)
  // Verify integers are not quoted
  output |> should.be_true(string.contains(output, "\"count\":42") || string.contains(output, "\"count\": 42"))
}
```

**Step 5: Update metadata_value_to_string test**

```gleam
pub fn metadata_value_to_string_test() {
  record.metadata_value_to_string(StringVal("hello"))
  |> should.equal("hello")

  record.metadata_value_to_string(IntVal(42))
  |> should.equal("42")

  record.metadata_value_to_string(FloatVal(3.14))
  |> should.equal("3.14")

  record.metadata_value_to_string(BoolVal(True))
  |> should.equal("true")

  record.metadata_value_to_string(BoolVal(False))
  |> should.equal("false")
}
```

**Step 6: Run tests on both targets**

Run: `just test`
Expected: All tests pass on both Erlang and JavaScript targets.

**Step 7: Commit**

```bash
git add test/
git commit -m "test: update all tests for MetadataValue type

Replace #(String, String) metadata construction with meta.*
helpers. Add tests for MetadataValue, meta module, and typed
JSON output."
```

---

### Task 10: Update examples

**Files:**
- Modify: 8 example files that construct metadata (see list below)

Affected examples (54 total occurrences):
- `examples/03-metadata/src/birch_example_03_metadata.gleam` (24 occurrences)
- `examples/04-named-loggers/src/birch_example_04_named_loggers.gleam` (11)
- `examples/06-json-handler/src/birch_example_06_json_handler.gleam` (3)
- `examples/07-file-handler/src/birch_example_07_file_handler.gleam` (1)
- `examples/09-global-config/src/birch_example_09_global_config.gleam` (5)
- `examples/11-error-helpers/src/birch_example_11_error_helpers.gleam` (4)
- `examples/12-scoped-context/src/birch_example_12_scoped_context.gleam` (4)
- `examples/15-testing-support/src/birch_example_15_testing_support.gleam` (2)

**Step 1: Add import to each affected example**

Add `import birch/meta` to each file's imports.

**Step 2: Replace metadata construction**

Same transformation as tests: `#("key", "value")` → `meta.string("key", "value")`.

For the metadata example (03), take the opportunity to showcase typed values:
- `#("current", "95")` → `meta.int("current", 95)`
- `#("limit", "100")` → `meta.int("limit", 100)`
- `#("hit", "true")` → `meta.bool("hit", True)`
- `#("amount", "99.99")` → `meta.float("amount", 99.99)`
- `#("port", "5432")` → `meta.int("port", 5432)`
- `#("retry_count", "3")` → `meta.int("retry_count", 3)`

For the `process_order` function in example 03, the `int.to_string` and `float.to_string` calls can be removed:

```gleam
pub fn process_order(order_id: String, items: Int, total: Float) -> Nil {
  let lgr = log.new("app")
  logger.info(lgr, "Processing order", [
    meta.string("order_id", order_id),
    meta.int("item_count", items),
    meta.float("total", total),
  ])
}
```

**Step 3: Add birch/meta as dependency to each example's gleam.toml**

Each example has its own `gleam.toml` with birch as a path dependency. The `meta` module is part of birch, so no additional dependency is needed — it's already available.

**Step 4: Test examples**

Run: `just test-examples`
Expected: All examples pass.

Run: `just test-examples-node`
Expected: All examples pass on Node.js.

**Step 5: Commit**

```bash
git add examples/
git commit -m "refactor: update all examples for MetadataValue type

Replace string metadata with typed meta.* constructors.
Example 03 now showcases typed values (int, float, bool)."
```

---

### Task 11: Update property tests

**Files:**
- Modify: `test/property_test.gleam`

**Step 1: Check if property tests construct metadata**

Check if `test/property_test.gleam` has metadata construction that needs updating. Property tests for records and formatters may generate metadata.

**Step 2: Update generators**

If property tests generate metadata, update generators to produce `#(String, MetadataValue)` tuples instead of `#(String, String)`.

**Step 3: Commit (if changes needed)**

```bash
git add test/property_test.gleam
git commit -m "test: update property test generators for MetadataValue"
```

---

### Task 12: Full verification

**Step 1: Build both targets**

Run: `just build-all`
Expected: Clean build on both Erlang and JavaScript.

**Step 2: Run full test suite**

Run: `just check`
Expected: Format check passes, tests pass on both targets.

**Step 3: Run integration tests**

Run: `just test-integration`
Expected: All integration tests pass.

**Step 4: Run examples on both targets**

Run: `just test-examples-all`
Expected: All 17 examples pass on both Erlang and Node.js.

**Step 5: Generate docs**

Run: `just docs`
Expected: Docs build successfully, `birch/meta` module appears in generated docs.

---

### Task 13: Changelog entry

**Step 1: Add changie entry**

Run: `changie new --kind feat` (or create manually in `.changes/unreleased/`)

Entry content:
```
Added MetadataValue sum type (StringVal, IntVal, FloatVal, BoolVal) replacing stringly-typed metadata. New birch/meta module provides ergonomic constructors. JSON handler now outputs typed values.
```

**Step 2: Commit**

```bash
git add .changes/
git commit -m "docs: add changelog entry for typed metadata (#58)"
```
