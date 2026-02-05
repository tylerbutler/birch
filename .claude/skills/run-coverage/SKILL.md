---
name: run-coverage
description: Run code coverage and identify modules needing more tests. Use when checking test coverage or deciding what to test next.
disable-model-invocation: true
---

# Coverage Analysis Skill

Run code coverage and analyze the results to identify testing gaps.

## Quick Commands

```bash
just coverage-erlang    # Fast: Erlang only with module percentages
just coverage           # Full: Both Erlang and JavaScript targets
just coverage-js        # JavaScript only with c8
```

## Steps

1. **Run coverage** (prefer Erlang for speed):
   ```bash
   just coverage-erlang
   ```

2. **Analyze the output** - Look for:
   - Modules below 70% coverage (priority targets)
   - Modules below 50% coverage (critical gaps)
   - 100% modules (well-tested, skip these)

3. **For low-coverage modules**, identify what's missing:
   - Read the module source to find untested functions
   - Check if functions are public API vs internal helpers
   - Prioritize public API coverage

## Coverage Interpretation

| Coverage | Status | Action |
|----------|--------|--------|
| 90%+ | Excellent | Maintain |
| 70-89% | Good | Improve opportunistically |
| 50-69% | Needs work | Add tests for main paths |
| <50% | Critical | Prioritize testing |

## Important Limitations

- **Line numbers don't match**: Coverage reports show compiled Erlang/JS code, not Gleam source
- **Module percentages are accurate**: Focus on module-level metrics, not line-level
- **No hosted coverage**: Codecov/Coveralls don't work with Gleam (see DEV.md)

## Stale Build Artifacts

If coverage shows unexpected modules (0% for non-existent files):

```bash
rm -rf build/dev && just build
just coverage-erlang
```

## Example Output Interpretation

```
TOTAL: 59.7% (746/1250 lines)

  birch@internal@time                             98.0%  # Good
  birch@formatter                                 50.9%  # Needs tests
  birch@handler@file                              49.1%  # Critical
```

Focus testing efforts on `formatter` and `handler/file` modules.
