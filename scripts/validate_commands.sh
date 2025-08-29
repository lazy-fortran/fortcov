#!/bin/bash
# Command Validation Script
# Prevents documentation fraud by validating command existence

echo "=== Command Validation for Documentation ==="

# Test commands that should exist
REQUIRED_COMMANDS=("fpm" "gfortran" "git")
INVALID_COMMANDS=("fmp")  # Known invalid variants

echo "Checking required commands..."
for cmd in "${REQUIRED_COMMANDS[@]}"; do
    if command -v "$cmd" >/dev/null 2>&1; then
        echo "✅ $cmd - Available"
    else
        echo "❌ $cmd - NOT FOUND"
    fi
done

echo "Checking for invalid command variants..."
for cmd in "${INVALID_COMMANDS[@]}"; do
    if command -v "$cmd" >/dev/null 2>&1; then
        echo "⚠️  $cmd - Unexpectedly available"
    else
        echo "✅ $cmd - Correctly unavailable (prevents documentation fraud)"
    fi
done

echo "=== Validation Complete ==="
echo "Use only validated commands in PR documentation"