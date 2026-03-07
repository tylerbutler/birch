---
name: release-checklist
description: Pre-release verification checklist before merging release-please PR
disable-model-invocation: true
---

# Release Checklist

Verify everything is ready before merging a release-please PR.

## Automated Checks

These run in CI but verify locally for confidence:

```bash
# 1. Tests pass on both targets
just test

# 2. Format is correct
just format-check

# 3. Build succeeds with warnings as errors
just build-strict-all

# 4. Examples run successfully
just test-examples-all

# 5. Docs build
just docs
```

## Manual Verification

### Version Bump
- [ ] Version in `gleam.toml` matches the release PR title
- [ ] CHANGELOG.md has entries for all notable changes
- [ ] Breaking changes are clearly documented (if any)

### Documentation
- [ ] README.md examples work with new version
- [ ] Any new public API is documented
- [ ] CLAUDE.md is up to date

### Cross-Platform
- [ ] Tested on Erlang target: `just test-erlang`
- [ ] Tested on JavaScript target: `just test-js`
- [ ] Integration tests pass: `just test-integration-all`

### Dependencies
- [ ] No unnecessary dependency version bumps
- [ ] All deps are published to Hex.pm

## Release Process

1. **Merge the release-please PR** - This creates a GitHub release
2. **Publish workflow triggers automatically** - Publishes to Hex.pm
3. **Verify on Hex.pm** - Check https://hex.pm/packages/birch

## Rollback

If something goes wrong after publish:

1. Hex.pm packages can be retired (not deleted)
2. Create a patch release with fix
3. Document the issue in CHANGELOG.md

## Post-Release

- [ ] Verify package on hex.pm
- [ ] Test installation: `gleam add birch@{version}`
- [ ] Update any dependent projects
