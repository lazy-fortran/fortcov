#!/bin/bash
# FPM Coverage Generation Script
# Demonstrates Pattern 2: Build-Integrated Coverage Discovery from DESIGN.md

set -e  # Exit on error

echo "=== FPM Build System Integration Example ==="
echo "Pattern 2: Build-Integrated Coverage Discovery"
echo

# Clean previous coverage data
echo "Cleaning previous coverage data..."
rm -f *.gcov *.gcda *.gcno
rm -rf build/

# Step 1: Generate coverage instrumentation
echo "Step 1: Building with coverage instrumentation..."
echo "Command: fpm test --flag \"-fprofile-arcs -ftest-coverage\""
fpm test --flag "-fprofile-arcs -ftest-coverage"

echo
echo "Step 2: Using the FPM Coverage Bridge Script..."
echo "This demonstrates how to use the bridge script for REAL coverage analysis"

# Use the bridge script for actual coverage generation (not mock data)
echo "Command: ../../../../scripts/fpm_coverage_bridge.sh root coverage.md"
../../../../scripts/fpm_coverage_bridge.sh root coverage.md

echo "✓ Real coverage report generated using bridge script: coverage.md"
echo
echo "Available coverage files:"
ls -la *.gcov 2>/dev/null || echo "No .gcov files found - check compilation flags"

echo
echo "=== FPM Integration Complete ==="
echo "This example demonstrates DESIGN.md Pattern 2 for FPM integration."
echo "The workflow: fpm test (with coverage flags) → gcov → fortcov"
echo
echo "For simpler usage, use the bridge script:"
echo "../../../../scripts/fpm_coverage_bridge.sh root coverage.md"