#!/usr/bin/env bash
set -euo pipefail

# Build and run tests with coverage flags
fpm test --flag "-fprofile-arcs -ftest-coverage"

# Generate .gcov files next to object dirs
find build -name "*.gcda" | xargs -r dirname | sort -u | while read -r d; do
  gcov --object-directory="$d" "$d"/*.gcno 2>/dev/null || true
done

# Produce a Markdown report using fortcov
fortcov --source=src *.gcov --output=coverage.md
echo "Coverage report: coverage.md"

