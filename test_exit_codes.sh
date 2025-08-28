#!/bin/bash

echo "=== EXIT CODE VERIFICATION TEST ==="
echo "Testing critical exit code scenarios..."

# Help command should return 0
echo "1. Testing help command (should return 0):"
fpm run -- --help > /dev/null 2>&1
help_exit=$?
echo "   Exit code: $help_exit"

# Invalid flag should return 1  
echo "2. Testing invalid flag (should return 1):"
fpm run -- --invalid-flag > /dev/null 2>&1
invalid_exit=$?
echo "   Exit code: $invalid_exit"

# Missing files should return 1
echo "3. Testing missing files (should return 1):"
fpm run -- non_existent.gcov > /dev/null 2>&1
missing_exit=$?
echo "   Exit code: $missing_exit"

echo "=== RESULTS ==="
echo "Help: $help_exit (expected: 0)"
echo "Invalid flag: $invalid_exit (expected: 1)" 
echo "Missing file: $missing_exit (expected: 1)"

if [ $help_exit -eq 0 ] && [ $invalid_exit -eq 1 ] && [ $missing_exit -eq 1 ]; then
    echo "✅ EXIT CODE VERIFICATION: PASSED"
    exit 0
else
    echo "❌ EXIT CODE VERIFICATION: FAILED"
    exit 1
fi