#!/bin/bash
# FPM Coverage Generation Script
# Demonstrates manual coverage generation with FPM

set -e  # Exit on error

echo "=== FPM Build System Integration Example ==="
echo "Demonstrating manual coverage generation"
echo

# Clean previous coverage data (write outputs to isolated directory)
echo "Cleaning previous coverage data..."
rm -f coverage_out/*.gcov coverage.md coverage.json 2>/dev/null || true
rm -rf build/
mkdir -p coverage_out

# Method 1: Manual coverage generation (current working approach)
echo "Method 1: Manual Coverage Generation"
echo "Command: fpm test --flag \"-fprofile-arcs -ftest-coverage\""
fpm test --flag "-fprofile-arcs -ftest-coverage"

echo "Generating .gcov files (run gcov from within build dirs)..."
project_root="$(pwd)"
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
  (
    cd "$dir" && gcov *.gcno >/dev/null 2>&1 || true
    mv ./*.gcov "$project_root"/coverage_out 2>/dev/null || true
  )
done

echo "Command: gcov src/*.f90"
gcov src/*.f90 >/dev/null 2>&1 && echo "Source gcov generation completed"

echo "Command: fortcov --source . --exclude build/* --exclude test/* coverage_out/*.gcov"
fortcov --source . --exclude build/* --exclude test/* coverage_out/*.gcov && echo "Coverage analysis completed"

echo
echo "Method 2: Alternative approach with different files"
echo "Command: fortcov --source=src demo_calculator.f90.gcov"
fortcov --source=src demo_calculator.f90.gcov 2>/dev/null && echo "Single file analysis completed"

echo
echo "=== Results ==="
echo "Available coverage files (coverage_out/):"
ls -la coverage_out/*.gcov 2>/dev/null || echo "No .gcov files found"

echo
echo "=== FPM Integration Summary ==="
echo "Current workflow:"
echo "1. fpm test --flag \"-fprofile-arcs -ftest-coverage\""
echo "2. find build -name \"*.gcda\" | xargs dirname | sort -u | while read dir; do (cd \"\$dir\" && gcov *.gcno && mv ./*.gcov \"$PWD/coverage_out\" ); done"
echo "3. fortcov --source=src coverage_out/*.gcov"
echo ""
echo "Generating report files (markdown and JSON)"
fortcov --source=. --exclude build/* --exclude test/* coverage_out/*.gcov \
  --format=markdown --output=coverage.md && echo "Markdown report generated"
fortcov --source=. --exclude build/* --exclude test/* coverage_out/*.gcov \
  --format=json --output=coverage.json && echo "JSON report generated"
test -f coverage.md && echo "✓ coverage.md created"
test -f coverage.json && echo "✓ coverage.json created"
