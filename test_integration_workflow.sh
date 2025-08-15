#!/bin/bash
# Given: CI Workflow test validating Max's fixes for Issue #45
# When: Running complete validation workflow with mock data  
# Then: All components work together without false failures

set -euo pipefail

echo "🔍 Testing CI Workflow Integration (Issue #45 Validation)"

WORK_DIR="$(mktemp -d)"
TEST_OUTPUT_DIR="test_outputs/system_test"

echo "📁 Test directory: $WORK_DIR"

# Function to cleanup on exit
cleanup() {
    echo "🧹 Cleaning up temporary files..."
    rm -rf "$WORK_DIR"
}
trap cleanup EXIT

# Test 1: Diff Comparator with Realistic Tolerances
echo ""
echo "🧪 Test 1: Diff Comparator with Realistic Tolerances"
echo "Given: Aligned mock data files"
echo "When: Running diff comparator with 5% tolerance"
echo "Then: Should pass with realistic mock data"

python3 test_integration/diff_comparator.py \
    "$TEST_OUTPUT_DIR/fortcov_diff_mock.json" \
    "$TEST_OUTPUT_DIR/pycobertura_diff.json" \
    --tolerance 0.05 \
    --report "$WORK_DIR/diff_comparison.txt"

echo "✅ Test 1 PASSED: Diff comparator works with realistic tolerances"

# Test 2: Performance Benchmark with Division by Zero Protection
echo ""
echo "🧪 Test 2: Performance Benchmark with Division by Zero Protection"
echo "Given: Mock XML and JSON files that may cause zero-time scenarios"
echo "When: Running performance benchmark"
echo "Then: Should handle zero-time gracefully without crashing"

python3 test_integration/performance_benchmark.py \
    --baseline-xml "$TEST_OUTPUT_DIR/baseline.xml" \
    --current-xml "$TEST_OUTPUT_DIR/current.xml" \
    --baseline-json "$TEST_OUTPUT_DIR/baseline.json" \
    --current-json "$TEST_OUTPUT_DIR/current.json" \
    --runs 1 \
    --report "$WORK_DIR/performance_report.txt"

echo "✅ Test 2 PASSED: Performance benchmark handles division by zero"

# Test 3: End-to-End Validation with Both Tools
echo ""
echo "🧪 Test 3: End-to-End Validation with Both Tools"
echo "Given: Complete CI workflow setup"
echo "When: Running both diff comparator and performance benchmark"
echo "Then: Both should pass without false failures"

# Run diff comparison
DIFF_EXIT_CODE=0
python3 test_integration/diff_comparator.py \
    "$TEST_OUTPUT_DIR/fortcov_diff_mock.json" \
    "$TEST_OUTPUT_DIR/pycobertura_diff.json" \
    --tolerance 0.05 || DIFF_EXIT_CODE=$?

# Run performance benchmark
PERF_EXIT_CODE=0
python3 test_integration/performance_benchmark.py \
    --baseline-xml "$TEST_OUTPUT_DIR/baseline.xml" \
    --current-xml "$TEST_OUTPUT_DIR/current.xml" \
    --baseline-json "$TEST_OUTPUT_DIR/baseline.json" \
    --current-json "$TEST_OUTPUT_DIR/current.json" \
    --runs 1 \
    --max-time-ratio 3.0 \
    --max-memory-ratio 3.0 || PERF_EXIT_CODE=$?

if [ $DIFF_EXIT_CODE -eq 0 ] && [ $PERF_EXIT_CODE -eq 0 ]; then
    echo "✅ Test 3 PASSED: End-to-end workflow successful"
else
    echo "❌ Test 3 FAILED: Exit codes - Diff: $DIFF_EXIT_CODE, Performance: $PERF_EXIT_CODE"
    exit 1
fi

# Test 4: Failure Detection with Realistic Thresholds
echo ""
echo "🧪 Test 4: Failure Detection with Realistic Thresholds"
echo "Given: Misaligned data that should legitimately fail"
echo "When: Running with strict thresholds"
echo "Then: Should detect real issues while avoiding false failures"

# Test with misaligned files (should fail)
EXPECTED_FAIL_EXIT_CODE=0
python3 test_integration/diff_comparator.py \
    "$TEST_OUTPUT_DIR/misaligned_fortcov.json" \
    "$TEST_OUTPUT_DIR/pycobertura_diff.json" \
    --tolerance 0.05 || EXPECTED_FAIL_EXIT_CODE=$?

if [ $EXPECTED_FAIL_EXIT_CODE -ne 0 ]; then
    echo "✅ Test 4 PASSED: Correctly detected misaligned data"
else
    echo "❌ Test 4 FAILED: Should have detected misaligned data"
    exit 1
fi

# Test 5: Mock Data Consistency 
echo ""
echo "🧪 Test 5: Mock Data Consistency"
echo "Given: Mock data files with proper structure"
echo "When: Examining data alignment"
echo "Then: Mock data should be properly aligned for CI testing"

# Check that mock data files exist and are non-empty
for file in "$TEST_OUTPUT_DIR/fortcov_diff_mock.json" "$TEST_OUTPUT_DIR/pycobertura_diff.json" \
           "$TEST_OUTPUT_DIR/baseline.json" "$TEST_OUTPUT_DIR/current.json" \
           "$TEST_OUTPUT_DIR/baseline.xml" "$TEST_OUTPUT_DIR/current.xml"; do
    if [ ! -s "$file" ]; then
        echo "❌ Test 5 FAILED: Mock data file missing or empty: $file"
        exit 1
    fi
done

echo "✅ Test 5 PASSED: All mock data files are present and non-empty"

echo ""
echo "🎯 CI Workflow Validation Summary:"
echo "✅ Diff Comparator: Realistic 5% tolerance instead of 50%"
echo "✅ Performance Benchmark: Division by zero protection working"  
echo "✅ Mock Data: Properly aligned between fortcov and pycobertura"
echo "✅ Error Handling: Detects real issues without false failures"
echo "✅ End-to-End: Complete workflow functions correctly"

echo ""
echo "🏁 SUCCESS: All Max's CI workflow fixes are working correctly!"
echo "   The CI infrastructure is ready for production use."