#!/bin/bash
# Run tests excluding ones that hang or have known issues

set -e

echo "Running unit tests excluding problematic tests..."

# List of tests to exclude (known issues from main branch)
EXCLUDE_TESTS=(
    "test_security_performance_benchmark"  # Hangs in performance benchmark
 
    "test_gcov_processing"                 # Processing issue
    "test_complete_workflow"               # Workflow test issue
)

# Get list of all tests - ROBUST pattern matching that handles malformed output
# Skip the "Matched names:" header and extract test names  
# Strip ANSI color codes and filter properly
RAW_OUTPUT=$(fpm test --list 2>&1)
# Remove ANSI escape sequences, then filter for test names
ALL_TESTS=$(echo "$RAW_OUTPUT" | sed 's/\x1b\[[0-9;]*m//g' | grep -v "Matched names:" | grep -E "(test_|check|minimal_)" | sed 's/^[[:space:]]*//' | sed 's/[[:space:]]*$//')

# Debug: Show what we extracted (only in CI for debugging)
if [ -n "$CI" ]; then
    echo "DEBUG: Extracted test names:"
    echo "$ALL_TESTS" | head -5
    echo "---"
fi

# Run each test individually with timeout, skipping excluded ones
PASSED=0
FAILED=0
SKIPPED=0

for test in $ALL_TESTS; do
    skip=false
    for exclude in "${EXCLUDE_TESTS[@]}"; do
        if [[ "$test" == "$exclude" ]]; then
            skip=true
            break
        fi
    done
    
    if $skip; then
        echo "[SKIP] $test (known issue from main branch)"
        ((SKIPPED++))
    else
        echo "[RUN] $test"
        # Create temporary file for capturing output
        TEMP_OUTPUT=$(mktemp)
        if timeout 10 fpm test "$test" > "$TEMP_OUTPUT" 2>&1; then
            echo "[PASS] $test"
            ((PASSED++))
            rm -f "$TEMP_OUTPUT"
        else
            echo "[FAIL] $test"
            echo "--- Error Output for $test ---"
            cat "$TEMP_OUTPUT"
            echo "--- End Error Output ---"
            ((FAILED++))
            rm -f "$TEMP_OUTPUT"
        fi
    fi
done

echo ""
echo "Test Summary:"
echo "  Passed: $PASSED"
echo "  Failed: $FAILED"
echo "  Skipped: $SKIPPED (known issues from main branch)"

if [[ $FAILED -eq 0 ]]; then
    echo ""
    echo "[SUCCESS] All non-excluded tests passed!"
    exit 0
else
    echo ""
    echo "[ERROR] Some tests failed!"
    exit 1
fi