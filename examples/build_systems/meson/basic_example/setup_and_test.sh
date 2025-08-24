#!/bin/bash
# Meson setup and test script for coverage demonstration

set -e  # Exit on error

echo "=== Meson Coverage Setup and Test ==="
echo

# Clean previous build
echo "Cleaning previous build..."
rm -rf builddir/

# Setup with coverage enabled
echo "Setting up Meson build with coverage enabled..."
echo "Command: meson setup builddir -Dcoverage=true"
meson setup builddir -Dcoverage=true

# Build
echo
echo "Building project..."
echo "Command: meson compile -C builddir"
meson compile -C builddir

# Run tests
echo
echo "Running tests..."
echo "Command: meson test -C builddir"
meson test -C builddir

# Run coverage target
echo
echo "Running coverage analysis..."
echo "Command: meson compile fortcov_coverage -C builddir"
meson compile fortcov_coverage -C builddir

echo
echo "Build directory contents:"
ls -la builddir/ | head -10

echo
echo "=== Meson Coverage Setup Complete ==="
echo "Generated coverage report available in builddir/coverage.html"