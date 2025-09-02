#!/bin/bash
# CI Hygiene Cleanup Script - Comprehensive test artifact removal
#
# This script ensures CI environments maintain clean project root by
# removing all possible test artifacts that could contaminate the workspace.

# Harden shell behavior: abort on any error, unset var, or pipeline failure
set -Eeuo pipefail

echo "CI Hygiene Cleanup: Comprehensive test artifact removal"

# Count artifacts before cleanup
BEFORE_COUNT=$(find . -maxdepth 1 \( -name "test_infra_*.txt" -o -name "test_infra_*.tmp" -o -name "*.gcov" -o -name "*.log" -o -name "*workspace*" -o -name "test_*_workspace*" -o -name "auto_discovery_*" -o -name "coverage_test_workspace*" -o -name "gcov_test_workspace*" -o -name "gcov_executor_test*" -o -name "test_*" \) 2>/dev/null | wc -l)

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
rm -rf gcov_executor_test* 2>/dev/null || true
rm -rf test_* 2>/dev/null || true

echo "Removing coverage artifacts..."
rm -f *.gcov 2>/dev/null || true

echo "Removing test execution logs..."
rm -f test_*.log 2>/dev/null || true
rm -f *_test_*.log 2>/dev/null || true

# Remove audit/debug logs that are not tracked by git (keep any committed logs)
echo "Removing untracked audit/debug logs..."
if command -v git >/dev/null 2>&1; then
    # Gather tracked .log files at repo root
    mapfile -t TRACKED_LOGS < <(git ls-files '*.log' 2>/dev/null | sed 's#^#./#')
    # Build an associative array for quick lookup (bash >=4)
    declare -A TRACKED_MAP
    for f in "${TRACKED_LOGS[@]}"; do TRACKED_MAP["$f"]=1; done
    # Find any .log files at project root and remove if untracked
    while IFS= read -r logfile; do
        # With 'set -u', direct expansion of an unset assoc key errors.
        # Use the '+x' parameter expansion to test key existence safely.
        if [[ -z "${TRACKED_MAP["$logfile"]+x}" ]]; then
            rm -f "$logfile" 2>/dev/null || true
        fi
    done < <(find . -maxdepth 1 -type f -name "*.log" 2>/dev/null)
else
    # Conservative fallback: remove common audit/debug pattern logs
    rm -f *_audit.log 2>/dev/null || true
    rm -f *audit*.log 2>/dev/null || true
    rm -f *debug*.log 2>/dev/null || true
    rm -f *validation*.log 2>/dev/null || true
fi

# Count artifacts after cleanup
AFTER_COUNT=$(find . -maxdepth 1 \( -name "test_infra_*.txt" -o -name "test_infra_*.tmp" -o -name "*.gcov" -o -name "*.log" -o -name "*workspace*" -o -name "test_*_workspace*" -o -name "auto_discovery_*" -o -name "coverage_test_workspace*" -o -name "gcov_test_workspace*" -o -name "gcov_executor_test*" -o -name "test_*" \) 2>/dev/null | wc -l)

echo "Post-cleanup artifact count: $AFTER_COUNT"
echo "Artifacts removed: $((BEFORE_COUNT - AFTER_COUNT))"

# Verification: List any remaining artifacts
REMAINING_ARTIFACTS=$(find . -maxdepth 1 \( -name "test_infra_*" -o -name "*.log" -o -name "*workspace*" -o -name "auto_discovery_*" -o -name "gcov_executor_test*" -o -name "test_*" \) 2>/dev/null)

# Filter out any tracked .log files from the remaining list
if command -v git >/dev/null 2>&1 && [ -n "$REMAINING_ARTIFACTS" ]; then
    TMP_FILTERED=$(mktemp)
    echo "$REMAINING_ARTIFACTS" > "$TMP_FILTERED"
    # Remove lines that are tracked .log files
    if git ls-files '*.log' >/dev/null 2>&1; then
        for tracked in $(git ls-files '*.log'); do
            sed -i "\#^\./$tracked$#d" "$TMP_FILTERED"
        done
    fi
    REMAINING_ARTIFACTS=$(cat "$TMP_FILTERED")
    rm -f "$TMP_FILTERED"
fi

if [ -n "$REMAINING_ARTIFACTS" ]; then
    echo "WARNING: Some artifacts remain:"
    echo "$REMAINING_ARTIFACTS"
    exit 1
else
    echo "CI Hygiene Success: Project root clean"
    exit 0
fi
