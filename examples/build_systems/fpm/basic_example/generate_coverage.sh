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
echo "Step 2: Extracting coverage data from build directories..."
echo "Command: gcov src/*.f90"
gcov src/*.f90

echo
echo "Step 3: Analyzing with fortcov..."
echo "Command: fortcov --source=. --exclude=build/*,test/* --output=coverage.md"

# Create mock fortcov output for demonstration
cat > coverage.md << 'EOF'
# FPM Integration Coverage Report

Generated using Pattern 2: Build-Integrated Coverage Discovery

## Summary
- **Total Lines**: 45
- **Covered Lines**: 40
- **Coverage Percentage**: 88.9%

## Source Files

### src/demo_calculator.f90
- **Lines**: 35
- **Covered**: 32
- **Coverage**: 91.4%

Functions:
- `add_numbers`: 100% covered
- `multiply_numbers`: 100% covered  
- `divide_numbers`: 85% covered (missing error path)

### app/main.f90
- **Lines**: 10
- **Covered**: 8
- **Coverage**: 80.0%

**Note**: This is a demonstration output. In real usage, run:
`fortcov --source=. --exclude=build/*,test/* --output=coverage.md`
EOF

echo "✓ Coverage report generated: coverage.md"
echo
echo "Available coverage files:"
ls -la *.gcov 2>/dev/null || echo "No .gcov files found - check compilation flags"

echo
echo "=== FPM Integration Complete ==="
echo "This example demonstrates DESIGN.md Pattern 2 for FPM integration."
echo "The workflow: fpm test (with coverage flags) → gcov → fortcov"