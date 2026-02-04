#!/usr/bin/env bash
# Update vendored birl from upstream
#
# This script fetches the latest birl changes and merges them into birch
# using josh-filter to preserve git history.
#
# Prerequisites:
#   - josh-filter installed (cargo install --git https://github.com/josh-project/josh.git josh-cli)
#   - Clean working directory in birch repo
#
# Usage:
#   ./scripts/update-vendored-birl.sh [birl-ref]
#
# Arguments:
#   birl-ref: Git ref to update to (default: main). Can be a branch, tag, or commit.

set -euo pipefail

BIRL_REF="${1:-main}"
BIRL_REPO="https://github.com/massivefermion/birl.git"
TEMP_DIR=$(mktemp -d)

echo "==> Updating vendored birl to ref: $BIRL_REF"

# Check for clean working directory
if ! git diff --quiet || ! git diff --cached --quiet; then
    echo "Error: Working directory is not clean. Please commit or stash changes first."
    exit 1
fi

# Check josh-filter is installed
if ! command -v josh-filter &> /dev/null; then
    echo "Error: josh-filter not found. Install with:"
    echo "  cargo install --git https://github.com/josh-project/josh.git josh-cli"
    exit 1
fi

echo "==> Cloning birl to $TEMP_DIR"
git clone --quiet "$BIRL_REPO" "$TEMP_DIR"
cd "$TEMP_DIR"
git checkout --quiet "$BIRL_REF"

echo "==> Running josh-filter"
FILTERED_SHA=$(josh-filter ':prefix=src/birch/internal/vendored/birl')
echo "    Filtered SHA: $FILTERED_SHA"

echo "==> Fetching filtered history into birch"
cd - > /dev/null
git fetch "$TEMP_DIR" "$FILTERED_SHA"

echo "==> Merging upstream changes"
# Use --no-edit to auto-generate merge message, or remove for interactive
git merge FETCH_HEAD -m "chore: update vendored birl to $BIRL_REF

Updated using josh-filter to preserve upstream git history."

echo "==> Cleaning up unnecessary files"
rm -rf src/birch/internal/vendored/birl/.github \
       src/birch/internal/vendored/birl/test \
       src/birch/internal/vendored/birl/zones-provider \
       src/birch/internal/vendored/birl/*.png \
       src/birch/internal/vendored/birl/update-zones.sh \
       src/birch/internal/vendored/birl/manifest.toml \
       src/birch/internal/vendored/birl/.gitignore \
       src/birch/internal/vendored/birl/README.md 2>/dev/null || true

if ! git diff --quiet; then
    git add -A
    git commit -m "chore: remove unnecessary files from vendored birl"
fi

echo "==> Cleaning up temp directory"
rm -rf "$TEMP_DIR"

echo "==> Done! Vendored birl updated to $BIRL_REF"
echo "    Run 'just check' to verify the update."
