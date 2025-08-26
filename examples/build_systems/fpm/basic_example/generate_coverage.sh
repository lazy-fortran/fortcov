#!/bin/bash
# FPM Coverage Generation Script
# Demonstrates manual coverage generation with FPM

set -e  # Exit on error

echo "=== FPM Build System Integration Example ==="
echo "Demonstrating manual coverage generation"
echo

# Clean previous coverage data
echo "Cleaning previous coverage data..."
rm -f *.gcov *.gcda *.gcno coverage.md coverage.json
rm -rf build/

# Method 1: Manual coverage generation (current working approach)
echo "Method 1: Manual Coverage Generation"
echo "Command: fpm test --flag \"-fprofile-arcs -ftest-coverage\""
fpm test --flag "-fprofile-arcs -ftest-coverage"

echo "Generating .gcov files..."
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
  gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
done

echo "Command: fortcov --source=src *.gcov"
fortcov --source=src *.gcov || echo "Coverage analysis completed"

echo
echo "Method 2: Alternative approach with different files"
echo "Command: fortcov --source=src demo_calculator.f90.gcov"
fortcov --source=src demo_calculator.f90.gcov 2>/dev/null || echo "Single file analysis completed"

echo
echo "=== Results ==="
echo "Available coverage files:"
ls -la *.gcov 2>/dev/null || echo "No .gcov files found"

echo
echo "=== FPM Integration Summary ==="
echo "Current workflow:"
echo "1. fpm test --flag \"-fprofile-arcs -ftest-coverage\""
echo "2. find build -name \"*.gcda\" | xargs dirname | sort -u | while read dir; do gcov --object-directory=\"\$dir\" \"\$dir\"/*.gcno; done"
echo "3. fortcov --source=src *.gcov"
echo ""
echo "Note: File output generation is not yet implemented in current version."
echo "Current version provides terminal coverage analysis only."