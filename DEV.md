# Development Guide

## Releasing

This project uses [Release-Please](https://github.com/googleapis/release-please) for automated releases. The release workflow is PR-based and follows conventional commits.

### How Releases Work

1. **Conventional Commits**: Use conventional commit messages in your PRs:
   - `feat: add new feature` → Minor version bump (0.x.0)
   - `fix: resolve bug` → Patch version bump (0.0.x)
   - `feat!: breaking change` → Major version bump (x.0.0)
   - `chore:`, `docs:`, `test:`, `ci:` → No version bump

2. **Release PR**: After merging PRs to `main`, Release-Please automatically creates/updates a "Release PR" that:
   - Bumps the version in `gleam.toml` and `version.txt`
   - Updates `CHANGELOG.md` with changes since the last release
   - Groups changes by type (Features, Bug Fixes, etc.)

3. **Publishing**: When you merge the Release PR:
   - Release-Please creates a git tag and GitHub Release
   - The publish workflow runs tests on both Erlang and JavaScript targets
   - If tests pass, the package is published to Hex.pm

### Required GitHub Secrets

Before the first release, configure these secrets in the repository settings:

| Secret | Description | How to Get |
|--------|-------------|------------|
| `RELEASE_TOKEN` | GitHub PAT with `contents:write` and `pull-requests:write` | [Create a PAT](https://github.com/settings/tokens) |
| `HEXPM_USER` | Your Hex.pm username | Your Hex.pm account |
| `HEXPM_PASS` | Hex.pm API key | [Hex.pm Dashboard → Keys](https://hex.pm/dashboard/keys) |

**Note**: `RELEASE_TOKEN` is required (instead of `GITHUB_TOKEN`) because GitHub Actions doesn't trigger workflows on PRs created by actions using the default token.

### Manual Release (Emergency)

If you need to release manually:

```bash
# Update version in gleam.toml and version.txt
# Update CHANGELOG.md manually

# Create and push a tag
git tag v0.2.0
git push origin v0.2.0

# Create a GitHub Release from the tag
# This will trigger the publish workflow
```

### Configuration Files

- `release-please-config.json` - Release-Please configuration
- `.release-please-manifest.json` - Tracks current version
- `version.txt` - Version source of truth for Release-Please
- `CHANGELOG.md` - Auto-generated changelog
