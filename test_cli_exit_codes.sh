#!/bin/bash

# CLI Exit Code Comprehensive Test Script
# Validates all critical exit code scenarios for CI/CD integration
# Tests Issues #1051, #1053 - CLI exit code systematic failures

set -euo pipefail

FORTCOV_BIN="./build/gfortran_*/app/fortcov"
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

echo "=========================================="
echo "CLI EXIT CODE COMPREHENSIVE TEST SUITE"
echo "Testing Issues #1051, #1053 fixes"
echo "=========================================="
echo

test_exit_code() {
    local description="$1"
    local expected_code="$2"
    local command="$3"
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    echo "Test $TOTAL_TESTS: $description"
    echo "Command: $command"
    echo "Expected exit code: $expected_code"
    
    # Run command and capture exit code, suppress output
    set +e
    actual_code=0
    eval "$command" >/dev/null 2>&1 || actual_code=$?
    set -e
    
    echo "Actual exit code: $actual_code"
    
    if [ "$actual_code" -eq "$expected_code" ]; then
        echo "✅ PASSED"
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        echo "❌ FAILED - Expected $expected_code, got $actual_code"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
    echo
}

echo "Building FortCov..."
fpm build >/dev/null 2>&1

# Test 1: Invalid flag should return exit code 4
test_exit_code "Invalid flag" 4 "$FORTCOV_BIN --invalid-flag"

# Test 2: Invalid threshold value should return exit code 4  
test_exit_code "Invalid threshold value" 4 "$FORTCOV_BIN --source=src --fail-under=150"

# Test 3: Nonexistent source path should return exit code 3
test_exit_code "Nonexistent source path" 3 "$FORTCOV_BIN --source=/nonexistent/path"

# Test 4: Invalid output path should return exit code 4
test_exit_code "Invalid output path" 4 "$FORTCOV_BIN --source=src --output=/dev/null/invalid"

# Test 5: Zero-configuration mode with no coverage should return exit code 3  
test_exit_code "Zero-config mode no coverage data" 3 "$FORTCOV_BIN"

# Test 6: Help flag should return exit code 0
test_exit_code "Help flag" 0 "$FORTCOV_BIN --help"

# Test 7: Version flag should return exit code 0
test_exit_code "Version flag" 0 "$FORTCOV_BIN --version"

# Test 8: Configuration validation should return exit code 0
test_exit_code "Configuration validation" 0 "$FORTCOV_BIN --validate --source=src"

# Test 9: Invalid format should return exit code 4
test_exit_code "Invalid format" 4 "$FORTCOV_BIN --source=src --format=invalid-format"

# Test 10: Missing source with explicit coverage files should return exit code 3
test_exit_code "Missing source with coverage files" 3 "$FORTCOV_BIN nonexistent.gcov"

echo "=========================================="
echo "TEST SUMMARY"
echo "=========================================="
echo "Total tests: $TOTAL_TESTS"
echo "Passed: $PASSED_TESTS" 
echo "Failed: $FAILED_TESTS"

if [ "$FAILED_TESTS" -eq 0 ]; then
    echo "🎉 ALL TESTS PASSED - CLI exit codes are working correctly!"
    exit 0
else
    echo "⚠️  SOME TESTS FAILED - CLI exit code issues detected!"
    exit 1
fi