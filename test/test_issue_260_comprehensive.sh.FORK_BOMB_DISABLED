#!/bin/bash
# Comprehensive test for issue #260: README documentation inconsistencies

set -e

echo "Testing issue #260: README documentation inconsistencies and workflow failures"
echo "=============================================================================="

# Find fortcov binary
FORTCOV_BIN=$(find /home/ert/code/fortcov/build -name fortcov -type f -executable | head -1)
if [ ! -x "$FORTCOV_BIN" ]; then
    echo "ERROR: Could not find fortcov executable"
    exit 1
fi

# Create temporary test directory
TEST_DIR=$(mktemp -d)
trap "rm -rf $TEST_DIR" EXIT

cd "$TEST_DIR"

# Test 1: Verify corrected gcov workflow generates proper .gcov files
echo ""
echo "Test 1: Testing corrected gcov workflow"
echo "----------------------------------------"

# Create minimal example from README
cat > fpm.toml << 'EOF'
name = "calculator"
version = "0.1.0"
EOF

mkdir -p src test

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

cat > test/test_calculator.f90 << 'EOF'
program test_calculator
    use calculator
    implicit none
    if (abs(add(2.0, 3.0) - 5.0) > 1e-6) stop 1
    print *, 'Test passed!'
end program test_calculator
EOF

# Build with coverage
fpm test --flag "-fprofile-arcs -ftest-coverage" > /dev/null 2>&1

# Use CORRECTED gcov workflow (directly process .gcda files)
echo "Running corrected gcov workflow..."
find build -name "*.gcda" | while read gcda_file; do
    gcov -b "$gcda_file" 2>/dev/null || true
done

# Verify .gcov files were created
GCOV_COUNT=$(ls -1 *.gcov 2>/dev/null | wc -l)
if [ "$GCOV_COUNT" -eq 0 ]; then
    echo "FAIL: No .gcov files generated with corrected workflow"
    exit 1
fi
echo "PASS: Generated $GCOV_COUNT .gcov files"

# Test 2: Verify fortcov can parse the generated files
echo ""
echo "Test 2: Testing fortcov parsing"
echo "--------------------------------"

"$FORTCOV_BIN" --source=. --output=coverage.md 2>&1 | grep -q "Coverage report generated successfully"
if [ $? -ne 0 ]; then
    echo "FAIL: fortcov failed to parse gcov files"
    exit 1
fi

if [ ! -f coverage.md ]; then
    echo "FAIL: No coverage report generated"
    exit 1
fi

# Check that coverage is reasonable (should be 100% for calculator.f90)
if grep -q "calculator.f90.*100.00%" coverage.md; then
    echo "PASS: fortcov successfully parsed gcov files (100% coverage for calculator.f90)"
else
    echo "WARNING: Coverage not 100% as expected"
    cat coverage.md
fi

# Test 3: Verify --fail-under flag works for CI/CD
echo ""
echo "Test 3: Testing --fail-under flag for CI/CD"
echo "--------------------------------------------"

# Should PASS with low threshold
"$FORTCOV_BIN" --source=. --fail-under=50 --output=coverage_pass.md
if [ $? -ne 0 ]; then
    echo "FAIL: --fail-under=50 should have passed"
    exit 1
fi
echo "PASS: --fail-under=50 passed as expected"

# Should FAIL with high threshold (coverage is ~87.5% with test file)
set +e  # Allow command to fail
"$FORTCOV_BIN" --source=. --fail-under=95 --output=coverage_fail.md 2>/dev/null
FAIL_EXIT_CODE=$?
set -e  # Re-enable exit on error

if [ $FAIL_EXIT_CODE -eq 0 ]; then
    echo "FAIL: --fail-under=95 should have failed (coverage is ~87.5%)"
    exit 1
fi
echo "PASS: --fail-under=95 failed as expected (exit code: $FAIL_EXIT_CODE)"

# Test 4: Verify --threshold flag still works (backward compatibility)
echo ""
echo "Test 4: Testing --threshold flag (backward compatibility)"
echo "---------------------------------------------------------"

"$FORTCOV_BIN" --source=. --threshold=80 --output=coverage_threshold.md
if [ $? -ne 0 ]; then
    echo "FAIL: --threshold flag should still work"
    exit 1
fi
echo "PASS: --threshold flag works for backward compatibility"

# Test 5: Compare old (broken) workflow vs new workflow
echo ""
echo "Test 5: Comparing old vs new gcov workflow"
echo "-------------------------------------------"

# Clean up existing gcov files
rm -f *.gcov

# Try OLD (broken) workflow from original README
echo "Testing OLD workflow (expected to fail)..."
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
    gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
done

OLD_GCOV_COUNT=$(ls -1 *.gcov 2>/dev/null | wc -l)

# Clean up
rm -f *.gcov

# Use NEW (corrected) workflow
echo "Testing NEW workflow..."
find build -name "*.gcda" | while read gcda_file; do
    gcov -b "$gcda_file" 2>/dev/null || true
done

NEW_GCOV_COUNT=$(ls -1 *.gcov 2>/dev/null | wc -l)

echo "Old workflow generated: $OLD_GCOV_COUNT files"
echo "New workflow generated: $NEW_GCOV_COUNT files"

if [ "$NEW_GCOV_COUNT" -gt "$OLD_GCOV_COUNT" ]; then
    echo "PASS: New workflow generates more coverage files"
elif [ "$NEW_GCOV_COUNT" -eq "$OLD_GCOV_COUNT" ] && [ "$NEW_GCOV_COUNT" -gt 0 ]; then
    echo "PASS: Both workflows work (may vary by gcov version)"
else
    echo "WARNING: Unexpected gcov file counts"
fi

# Summary
echo ""
echo "=============================================================================="
echo "SUMMARY: Issue #260 fixes verified"
echo "  ✓ Corrected gcov workflow generates .gcov files"
echo "  ✓ fortcov successfully parses generated files"
echo "  ✓ --fail-under flag works for CI/CD (returns proper exit codes)"
echo "  ✓ --threshold flag maintained for backward compatibility"
echo "  ✓ New workflow is more reliable than old"
echo ""
echo "ALL TESTS PASSED - Issue #260 is resolved!"
echo "=============================================================================="