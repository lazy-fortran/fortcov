#!/bin/bash
# CI Hygiene Cleanup Script - Comprehensive test artifact removal
# 
# This script ensures CI environments maintain clean project root by
# removing all possible test artifacts that could contaminate the workspace.

set -e

echo "CI Hygiene Cleanup: Comprehensive test artifact removal"

# Count artifacts before cleanup
BEFORE_COUNT=$(find . -maxdepth 1 \( -name "test_infra_*.txt" -o -name "test_infra_*.tmp" -o -name "*.gcov" -o -name "*.log" -o -name "*workspace*" -o -name "test_*_workspace*" -o -name "auto_discovery_*" -o -name "coverage_test_workspace*" -o -name "gcov_test_workspace*" \) 2>/dev/null | wc -l)

echo "Pre-cleanup artifact count: $BEFORE_COUNT"

# Comprehensive artifact removal patterns
echo "Removing test infrastructure artifacts..."
rm -f test_infra_handle_*.txt 2>/dev/null || true
rm -f test_infra_cleanup_*.txt 2>/dev/null || true  
rm -f test_infra_rapid_*.txt 2>/dev/null || true
rm -f test_infra_cmd_test.txt 2>/dev/null || true
rm -f test_infra_io_test.txt 2>/dev/null || true
rm -f test_infra_isolation_test.txt 2>/dev/null || true
rm -f test_infra_temp_mgmt_test.tmp 2>/dev/null || true
rm -f test_infra_concurrent_*.txt 2>/dev/null || true

echo "Removing workspace directories..."
rm -rf *workspace* 2>/dev/null || true
rm -rf test_*_workspace* 2>/dev/null || true
rm -rf auto_discovery_* 2>/dev/null || true
rm -rf coverage_test_workspace* 2>/dev/null || true
rm -rf gcov_test_workspace* 2>/dev/null || true

echo "Removing coverage artifacts..."
rm -f *.gcov 2>/dev/null || true

echo "Removing test execution logs..."  
rm -f test_*.log 2>/dev/null || true
rm -f *_test_*.log 2>/dev/null || true

# Count artifacts after cleanup
AFTER_COUNT=$(find . -maxdepth 1 \( -name "test_infra_*.txt" -o -name "test_infra_*.tmp" -o -name "*.gcov" -o -name "*.log" -o -name "*workspace*" -o -name "test_*_workspace*" -o -name "auto_discovery_*" -o -name "coverage_test_workspace*" -o -name "gcov_test_workspace*" \) 2>/dev/null | wc -l)

echo "Post-cleanup artifact count: $AFTER_COUNT"
echo "Artifacts removed: $((BEFORE_COUNT - AFTER_COUNT))"

# Verification: List any remaining artifacts
REMAINING_ARTIFACTS=$(find . -maxdepth 1 \( -name "test_infra_*" -o -name "*_test_*.log" -o -name "*workspace*" -o -name "auto_discovery_*" \) 2>/dev/null)

if [ -n "$REMAINING_ARTIFACTS" ]; then
    echo "WARNING: Some artifacts remain:"
    echo "$REMAINING_ARTIFACTS"
    exit 1
else
    echo "CI Hygiene Success: Project root clean"
    exit 0
fi
