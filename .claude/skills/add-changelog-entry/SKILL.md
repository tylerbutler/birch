---
name: add-changelog-entry
description: Create a changie changelog fragment for a code change. Use when asked to add a changelog entry, create a changelog fragment, or document a change for the changelog.
---

<required>
*CRITICAL* Add the following steps to your Todo list using TodoWrite:

1. Determine the change kind and draft body text
2. Confirm kind and body with the user
3. Create the fragment YAML file in `.changes/unreleased/`
4. Verify the fragment is valid
</required>

# Add Changelog Entry

This project uses [changie](https://changie.dev/) for changelog management. Each user-facing change needs a YAML fragment file in `.changes/unreleased/`.

## Step 1: Determine Kind and Body

Review the recent code changes (staged files, current branch diff, or conversation context) to determine:

- **Kind**: One of the 9 allowed kinds (see below)
- **Body**: A short title on the first line, optionally followed by a longer description

### Allowed Kinds

| Kind           | When to use                                      | SemVer impact |
|----------------|--------------------------------------------------|---------------|
| `Added`        | New features or capabilities                     | minor         |
| `Changed`      | Changes to existing functionality                | patch         |
| `Deprecated`   | Features marked for future removal               | patch         |
| `Fixed`        | Bug fixes                                        | patch         |
| `Performance`  | Performance improvements                         | patch         |
| `Removed`      | Removed features or capabilities                 | patch         |
| `Reverted`     | Reverted previous changes                        | patch         |
| `Dependencies` | Dependency updates                               | patch         |
| `Security`     | Security-related fixes                           | patch         |

### Body Guidelines

- **First line**: Short summary title (this becomes an `#####` heading in the changelog)
- **Subsequent lines** (optional): Longer description with details, migration notes, or examples
- Reference GitHub issues/PRs with `(#123)` when applicable
- Use imperative mood for the title (e.g., "Add X" not "Added X")

## Step 2: Confirm with User

Use the `ask_user` tool to present the proposed kind and body text. Let the user confirm or edit before creating the file.

Example confirmation prompt:
```
I'd like to create this changelog entry:

Kind: Fixed
Body:
  Fix `unregister_writer` memory leak
  The `unregister_writer` function was using the wrong ETS key...

Does this look right, or would you like to change anything?
```

## Step 3: Create the Fragment File

### File Naming

Fragment files follow this naming pattern:
```
{Kind}-{YYYYMMDD}-{HHMMSS}.yaml
```

Generate the timestamp from the current time in the **local timezone** (Pacific Time / America/Los_Angeles). Use 24-hour format.

Example: `Fixed-20260307-220100.yaml`

If a file with that exact name already exists, increment the seconds component until a unique name is found.

### File Format

```yaml
kind: {Kind}
body: |-
    {First line: short title}
    {Optional subsequent lines with details}
time: {ISO 8601 timestamp with timezone offset}
```

The `time` field uses full ISO 8601 with nanoseconds and timezone offset. Generate it to match this format:
```
2026-03-07T22:01:00.000000000-08:00
```

### File Location

Write the file to: `.changes/unreleased/{Kind}-{YYYYMMDD}-{HHMMSS}.yaml`

## Step 4: Verify

After creating the file, read it back and verify:

1. The YAML is valid (kind, body, time fields all present)
2. The `kind` matches one of the 9 allowed kinds exactly (case-sensitive)
3. The `body` uses the `|-` block scalar indicator
4. The file is in `.changes/unreleased/`

## Reference: Example Fragments

**Simple (title only):**
```yaml
kind: Performance
body: |-
    Lazy evaluation for expensive log formatting
time: 2026-02-22T12:52:21.000000000-08:00
```

**Detailed (title + description):**
```yaml
kind: Added
body: |-
    Typed metadata values (#58)
    Replace stringly-typed `Metadata` with a `MetadataValue` sum type (`StringVal`, `IntVal`, `FloatVal`, `BoolVal`). New `birch/meta` module provides ergonomic constructors (`meta.string`, `meta.int`, `meta.float`, `meta.bool`). JSON handler now outputs typed values instead of strings.
time: 2026-02-21T22:07:16.915874519-08:00
```
