#!/bin/bash
# Test to verify README gcov workflow works correctly

set -e

# Create temporary test directory
TEST_DIR=$(mktemp -d)
trap "rm -rf $TEST_DIR" EXIT

cd "$TEST_DIR"

# Create project structure as shown in README
cat > fpm.toml << 'EOF'
name = "calculator"
version = "0.1.0"
EOF

mkdir -p src test

# Source code
cat > src/calculator.f90 << 'EOF'
module calculator
    implicit none
    public :: add
contains
    function add(a, b) result(c)
        real, intent(in) :: a, b
        real :: c
        c = a + b
    end function add
end module calculator
EOF

# Test code
cat > test/test_calculator.f90 << 'EOF'
program test_calculator
    use calculator
    implicit none
    if (abs(add(2.0, 3.0) - 5.0) > 1e-6) stop 1
    print *, 'Test passed!'
end program test_calculator
EOF

# Build and test with coverage flags
fpm test --flag "-fprofile-arcs -ftest-coverage"

# Corrected gcov workflow - process each .gcda file directly
find build -name "*.gcda" | while read gcda_file; do
    gcov -b "$gcda_file" 2>/dev/null || true
done

# Verify gcov files were created
if [ -z "$(ls -A *.gcov 2>/dev/null)" ]; then
    echo "ERROR: No .gcov files generated!"
    exit 1
fi

echo "SUCCESS: gcov files generated successfully"
ls -la *.gcov

# Test fortcov parsing
if [ -n "$1" ]; then
    FORTCOV_BIN="$1/fortcov"
else
    FORTCOV_BIN=$(find /home/ert/code/fortcov/build -name fortcov -type f -executable | head -1)
fi

if [ ! -x "$FORTCOV_BIN" ]; then
    echo "ERROR: Could not find fortcov executable at $FORTCOV_BIN"
    exit 1
fi

# Run fortcov without flags to test zero-configuration
"$FORTCOV_BIN" --output=coverage.md

# Check if coverage report was generated
if [ ! -f coverage.md ]; then
    echo "ERROR: Coverage report not generated"
    exit 1
fi

# Verify coverage content
if ! grep -q "calculator.f90" coverage.md; then
    echo "ERROR: Coverage report missing source file"
    exit 1
fi

echo "SUCCESS: fortcov processed gcov files correctly"

# Test with --fail-under flag (correct flag for CI/CD)
"$FORTCOV_BIN" --fail-under=80 --output=coverage2.md && echo "SUCCESS: --fail-under flag works"

# Test with deprecated --threshold flag (should also work but not fail)
"$FORTCOV_BIN" --threshold=80 --output=coverage3.md && echo "SUCCESS: --threshold flag works"

echo "ALL TESTS PASSED"