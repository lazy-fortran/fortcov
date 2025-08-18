#!/bin/bash

# Documentation Validation Test Script for Issue #162
# Tests the key source path patterns documented in README

FORTCOV_EXEC="build/gfortran_2E1845944E00BB22/app/fortcov"
PASSED=0
FAILED=0

echo "=== Source Path Documentation Validation ==="
echo

# Helper function to run test
run_test() {
    local test_name="$1"
    local command="$2"
    local expect_success="$3"
    
    echo "Testing: $test_name"
    echo "Command: $command"
    
    if eval "$command" >/dev/null 2>&1; then
        exit_code=0
    else
        exit_code=$?
    fi
    
    if [[ "$expect_success" == "true" && $exit_code -eq 0 ]] || [[ "$expect_success" == "false" && $exit_code -ne 0 ]]; then
        echo "✓ PASSED"
        ((PASSED++))
    else
        echo "✗ FAILED (exit code: $exit_code)"
        ((FAILED++))
    fi
    echo
}

# Test 1: Missing source requirement should fail
run_test "Missing --source requirement" \
    "$FORTCOV_EXEC --output=test.md" \
    "false"

# Test 2: Basic source pattern should work (though no coverage files)
run_test "Basic source pattern" \
    "$FORTCOV_EXEC --source=src --output=test.md" \
    "true"

# Test 3: Quick start pattern should work
run_test "Quick start pattern" \
    "$FORTCOV_EXEC --source=. --exclude='build/*,test/*' --output=test.md" \
    "true"

# Test 4: Multiple excludes pattern should work
run_test "Multiple excludes pattern" \
    "$FORTCOV_EXEC --source=. --exclude='build/*' --exclude='test/*' --output=test.md" \
    "true"

# Test 5: Multiple source paths should work
run_test "Multiple source paths" \
    "$FORTCOV_EXEC --source=src --source=lib --output=test.md" \
    "true"

# Test 6: CI/CD pattern should work
run_test "CI/CD pattern with threshold" \
    "$FORTCOV_EXEC --source=src --fail-under=80 --quiet --output=test.md" \
    "true"

# Test 7: TUI mode should work
run_test "TUI mode pattern" \
    "timeout 2s $FORTCOV_EXEC --source=src --tui || [[ \$? -eq 124 ]]" \
    "true"

# Test 8: JSON output format should work
run_test "JSON output format" \
    "$FORTCOV_EXEC --source=src --output-format=json --output=test.json" \
    "true"

# Test 9: HTML output format should work
run_test "HTML output format" \
    "$FORTCOV_EXEC --source=src --output-format=html --output=test.html" \
    "true"

# Test 10: Help should show correct patterns
run_test "Help text contains source patterns" \
    "$FORTCOV_EXEC --help | grep -q 'fortcov --source='" \
    "true"

# Clean up test files
rm -f test.md test.json test.html

# Summary
echo "=== Test Results ==="
echo "Passed: $PASSED"
echo "Failed: $FAILED"

if [[ $FAILED -eq 0 ]]; then
    echo "✓ All documentation validation tests PASSED"
    exit 0
else
    echo "✗ Some tests FAILED - documentation-implementation gaps remain"
    exit 1
fi