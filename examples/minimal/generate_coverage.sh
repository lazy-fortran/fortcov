#!/usr/bin/env bash
set -euo pipefail

# Build and run tests with coverage flags
fpm test --flag "-fprofile-arcs -ftest-coverage"

# Generate .gcov files next to object dirs
find build -name "*.gcda" | xargs -r dirname | sort -u | while read -r d; do
  gcov --object-directory="$d" "$d"/*.gcno 2>/dev/null || true
done

# Resolve fortcov binary: prefer $FORTCOV_BIN, then PATH, then repo-local build
FORTCOV_BIN="${FORTCOV_BIN:-fortcov}"
if ! command -v "$FORTCOV_BIN" >/dev/null 2>&1; then
  REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || echo "")"
  if [ -n "$REPO_ROOT" ] && [ -d "$REPO_ROOT/build" ]; then
    CANDIDATE=$(find "$REPO_ROOT/build" -type f -path '*/app/fortcov' | head -n1 || true)
    if [ -n "${CANDIDATE:-}" ] && [ -x "$CANDIDATE" ]; then
      FORTCOV_BIN="$CANDIDATE"
    fi
  fi
fi

# Produce a Markdown report using fortcov
"$FORTCOV_BIN" --source=src *.gcov --output=coverage.md
echo "Coverage report: coverage.md"
