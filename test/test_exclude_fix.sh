#!/bin/bash

# Test script to verify Issue #131 fix
set -e

echo "Testing exclude pattern fix for Issue #131..."

# Test 1: Ensure test* excludes test files
echo "Test 1: Pattern 'test*' should exclude test files"
output1=$(fpm run fortcov -- --source=. --exclude='test*' --quiet 2>/dev/null | grep -c "test_wd.f90" || true)
if [ "$output1" -eq 0 ]; then
    echo "  ✓ PASS: test* pattern correctly excludes test files"
else
    echo "  ✗ FAIL: test* pattern did not exclude test files"
    exit 1
fi

# Test 2: Ensure *.xyz excludes files with that extension  
echo "Test 2: Pattern '*.xyz' should exclude .xyz files"
output2=$(fpm run fortcov -- --source=. --exclude='*.xyz' --quiet 2>/dev/null | grep -c "\.gcov" || true)
if [ "$output2" -gt 0 ]; then
    echo "  ✓ PASS: *.xyz pattern does not exclude .gcov files"
else
    echo "  ✗ FAIL: *.xyz pattern excluded everything incorrectly"
    exit 1
fi

# Test 3: Verify normal behavior without exclude
echo "Test 3: No exclude pattern should process all files"
output3=$(fpm run fortcov -- --source=. --quiet 2>/dev/null | grep -c "test_wd.f90" || true)
if [ "$output3" -gt 0 ]; then
    echo "  ✓ PASS: Without exclude, test files are included"
else
    echo "  ✗ FAIL: test files missing even without exclude"
    exit 1
fi

echo "All tests passed! Issue #131 is fixed."