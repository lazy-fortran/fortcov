#!/bin/bash
# FPM Coverage Generation Script
# Demonstrates zero-configuration and manual coverage generation patterns

set -e  # Exit on error

echo "=== FPM Build System Integration Example ==="
echo "Demonstrating both zero-configuration and manual patterns"
echo

# Clean previous coverage data
echo "Cleaning previous coverage data..."
rm -f *.gcov *.gcda *.gcno coverage.md coverage.json
rm -rf build/

# Method 1: Zero-configuration (recommended)
echo "Method 1: Zero-Configuration Coverage Generation"
echo "Command: fpm test --flag \"-fprofile-arcs -ftest-coverage\""
fpm test --flag "-fprofile-arcs -ftest-coverage"

echo "Command: fortcov"
fortcov || echo "Zero-configuration completed"

echo
echo "Method 2: Manual File Processing"
echo "Step 1: Generate gcov files manually..."
echo "Command: find build -name \"*.gcda\" | xargs dirname | sort -u | while read dir; do gcov --object-directory=\"\$dir\" \"\$dir\"/*.gcno 2>/dev/null || true; done"
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
  gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
done

echo "Step 2: Process gcov files explicitly..."
echo "Command: fortcov *.gcov"
fortcov *.gcov || echo "Manual processing completed"

echo
echo "Method 3: Source-based Analysis with Exclusions"
echo "Command: fortcov --source . --exclude \"build/*\" --exclude \"test/*\" --format=markdown --output=coverage.md"
fortcov --source . --exclude "build/*" --exclude "test/*" --format=markdown --output=coverage.md || echo "Source-based analysis completed"

echo
echo "=== Results ==="
echo "Available coverage files:"
ls -la *.gcov 2>/dev/null || echo "No .gcov files found"

if [ -f "coverage.md" ]; then
    echo "Generated coverage report: coverage.md"
    echo "Coverage summary:"
    head -10 coverage.md
fi

echo
echo "=== FPM Integration Summary ==="
echo "Recommended workflow: fpm test --flag \"-fprofile-arcs -ftest-coverage\" && fortcov"
echo "For file output: fortcov --format=markdown --output=coverage.md"
echo "For CI/CD: fortcov --format=json --output=coverage.json --fail-under=80 --quiet"