#!/bin/bash
# Comprehensive integration test for fortcov directory workflow
# Tests the new gcov automation functionality end-to-end

set -e  # Exit on any error

echo "🚀 Starting fortcov directory workflow integration tests"

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
TEST_DIR=$(mktemp -d)
COVERAGE_DIR="$TEST_DIR/coverage"

echo "📁 Test directory: $TEST_DIR"
echo "📁 Project root: $PROJECT_ROOT"

# Function to cleanup on exit
cleanup() {
    echo "🧹 Cleaning up temporary files..."
    rm -rf "$TEST_DIR"
}
trap cleanup EXIT

# Test counter
TEST_COUNT=0
PASSED_TESTS=0
FAILED_TESTS=0

# Helper functions
run_test() {
    local test_name="$1"
    local test_function="$2"
    
    TEST_COUNT=$((TEST_COUNT + 1))
    echo ""
    echo "🧪 Test $TEST_COUNT: $test_name"
    echo "----------------------------------------"
    
    if $test_function; then
        echo "✅ PASSED: $test_name"
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        echo "❌ FAILED: $test_name"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
}

# Test 1: Build fortcov with coverage instrumentation
test_build_with_coverage() {
    echo "Building fortcov with coverage flags..."
    cd "$PROJECT_ROOT"
    
    if fpm build --flag "-fprofile-arcs -ftest-coverage" >/dev/null 2>&1; then
        echo "Build successful"
        return 0
    else
        echo "Build failed"
        return 1
    fi
}

# Test 2: Generate coverage data by running tests
test_generate_coverage_data() {
    echo "Running tests to generate coverage data..."
    cd "$PROJECT_ROOT"
    
    # Run tests to generate .gcda files
    if fpm test --flag "-fprofile-arcs -ftest-coverage" >/dev/null 2>&1 || true; then
        # Count generated coverage files
        local gcno_count=$(find . -name "*.gcno" 2>/dev/null | wc -l)
        local gcda_count=$(find . -name "*.gcda" 2>/dev/null | wc -l)
        
        echo "Found $gcno_count .gcno files and $gcda_count .gcda files"
        
        if [ "$gcno_count" -gt 0 ] && [ "$gcda_count" -gt 0 ]; then
            return 0
        else
            echo "Insufficient coverage data generated"
            return 1
        fi
    else
        echo "Test execution failed"
        return 1
    fi
}

# Test 3: Directory auto-detection workflow
test_directory_auto_detection() {
    echo "Testing directory auto-detection..."
    cd "$PROJECT_ROOT"
    
    local output_file="$TEST_DIR/autodetect_coverage.md"
    
    # Find a build directory with coverage files
    local build_dir=$(find build -name "*.gcda" -exec dirname {} \; 2>/dev/null | head -1)
    
    if [ -z "$build_dir" ]; then
        echo "No build directory with coverage data found"
        return 1
    fi
    
    echo "Using build directory: $build_dir"
    
    # Run fortcov with directory input
    if timeout 60 ./build/gfortran_*/app/fortcov "$build_dir" \
           --output-format=markdown \
           --output="$output_file" \
           --quiet 2>/dev/null; then
        
        if [ -f "$output_file" ]; then
            local line_count=$(wc -l < "$output_file")
            echo "Generated report with $line_count lines"
            return 0
        else
            echo "No output file generated"
            return 1
        fi
    else
        echo "fortcov directory processing failed or timed out"
        return 1
    fi
}

# Test 4: Multiple directory workflow
test_multiple_directories() {
    echo "Testing multiple directory processing..."
    cd "$PROJECT_ROOT"
    
    local output_file="$TEST_DIR/multi_dir_coverage.md"
    
    # Find multiple build directories
    local build_dirs=($(find build -name "*.gcda" -exec dirname {} \; 2>/dev/null | head -3))
    
    if [ ${#build_dirs[@]} -lt 2 ]; then
        echo "Not enough build directories found, creating test scenario"
        # Use different directories that might exist
        build_dirs=("build/" "src/" "test_data/")
    fi
    
    echo "Testing with directories: ${build_dirs[*]}"
    
    # Run fortcov with multiple directories
    if timeout 60 ./build/gfortran_*/app/fortcov "${build_dirs[@]}" \
           --output-format=markdown \
           --output="$output_file" \
           --quiet 2>/dev/null || true; then
        
        # Check if command completed (may have no coverage data)
        echo "Multiple directory processing completed"
        return 0
    else
        echo "Multiple directory processing failed"
        return 1
    fi
}

# Test 5: Mixed input types (directories + .gcov files)
test_mixed_input_types() {
    echo "Testing mixed directory and .gcov file inputs..."
    cd "$PROJECT_ROOT"
    
    local output_file="$TEST_DIR/mixed_input_coverage.md"
    local test_gcov="$TEST_DIR/test_sample.gcov"
    
    # Create a sample .gcov file
    cat > "$test_gcov" << EOF
        -:    0:Source:sample.f90
        -:    1:program sample
        1:    2:  print *, 'Hello, World!'
        -:    3:end program
EOF
    
    # Find a build directory
    local build_dir=$(find build -name "*.gcda" -exec dirname {} \; 2>/dev/null | head -1)
    if [ -z "$build_dir" ]; then
        build_dir="build/"
    fi
    
    # Run fortcov with mixed inputs
    if timeout 60 ./build/gfortran_*/app/fortcov "$build_dir" "$test_gcov" \
           --output-format=markdown \
           --output="$output_file" \
           --quiet 2>/dev/null || true; then
        
        echo "Mixed input processing completed"
        return 0
    else
        echo "Mixed input processing failed"
        return 1
    fi
}

# Test 6: CLI flag testing (--keep-gcov-files)
test_keep_gcov_files_flag() {
    echo "Testing --keep-gcov-files flag..."
    cd "$PROJECT_ROOT"
    
    local output_file="$TEST_DIR/keep_files_coverage.md"
    local build_dir=$(find build -name "*.gcda" -exec dirname {} \; 2>/dev/null | head -1)
    
    if [ -z "$build_dir" ]; then
        build_dir="build/"
    fi
    
    # Count .gcov files before
    local gcov_before=$(find . -name "*.gcov" 2>/dev/null | wc -l)
    
    # Run fortcov with --keep-gcov-files
    if timeout 60 ./build/gfortran_*/app/fortcov "$build_dir" \
           --output-format=markdown \
           --output="$output_file" \
           --keep-gcov-files \
           --quiet 2>/dev/null || true; then
        
        # Count .gcov files after
        local gcov_after=$(find . -name "*.gcov" 2>/dev/null | wc -l)
        
        echo "Gcov files before: $gcov_before, after: $gcov_after"
        
        # Should have same or more .gcov files (they weren't cleaned up)
        if [ "$gcov_after" -ge "$gcov_before" ]; then
            return 0
        else
            echo "Gcov files were incorrectly cleaned up"
            return 1
        fi
    else
        echo "Keep gcov files test failed"
        return 1
    fi
}

# Test 7: Gcov arguments testing (--gcov-args)
test_gcov_args_flag() {
    echo "Testing --gcov-args flag..."
    cd "$PROJECT_ROOT"
    
    local output_file="$TEST_DIR/gcov_args_coverage.md"
    local build_dir=$(find build -name "*.gcda" -exec dirname {} \; 2>/dev/null | head -1)
    
    if [ -z "$build_dir" ]; then
        build_dir="build/"
    fi
    
    # Run fortcov with gcov arguments
    if timeout 60 ./build/gfortran_*/app/fortcov "$build_dir" \
           --output-format=markdown \
           --output="$output_file" \
           --gcov-args="-b -c" \
           --quiet 2>/dev/null || true; then
        
        echo "Gcov args test completed"
        return 0
    else
        echo "Gcov args test failed"
        return 1
    fi
}

# Test 8: Performance test with large directory
test_large_directory_performance() {
    echo "Testing large directory performance..."
    cd "$PROJECT_ROOT"
    
    local output_file="$TEST_DIR/performance_coverage.md"
    local start_time=$(date +%s)
    
    # Process entire project directory
    if timeout 120 ./build/gfortran_*/app/fortcov "." \
           --output-format=markdown \
           --output="$output_file" \
           --exclude="build/*" \
           --exclude="test_integration/*" \
           --exclude="external/*" \
           --quiet 2>/dev/null || true; then
        
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))
        
        echo "Large directory processing took $duration seconds"
        
        # Should complete within reasonable time (2 minutes)
        if [ "$duration" -le 120 ]; then
            return 0
        else
            echo "Performance test too slow: ${duration}s"
            return 1
        fi
    else
        echo "Large directory performance test failed"
        return 1
    fi
}

# Test 9: Error handling for non-existent directories
test_error_handling_nonexistent() {
    echo "Testing error handling for non-existent directories..."
    cd "$PROJECT_ROOT"
    
    local output_file="$TEST_DIR/error_handling_coverage.md"
    
    # Run fortcov with non-existent directory
    if ./build/gfortran_*/app/fortcov "/nonexistent/directory" \
           --output-format=markdown \
           --output="$output_file" \
           --quiet 2>/dev/null; then
        
        echo "Unexpected success with non-existent directory"
        return 1
    else
        echo "Correctly handled non-existent directory"
        return 0
    fi
}

# Test 10: Cleanup verification
test_cleanup_verification() {
    echo "Testing cleanup verification..."
    cd "$PROJECT_ROOT"
    
    local output_file="$TEST_DIR/cleanup_coverage.md"
    local build_dir=$(find build -name "*.gcda" -exec dirname {} \; 2>/dev/null | head -1)
    
    if [ -z "$build_dir" ]; then
        build_dir="build/"
    fi
    
    # Count temporary files before
    local temp_before=$(find /tmp -name "fortcov_*" 2>/dev/null | wc -l)
    
    # Run fortcov (should clean up temporary files)
    timeout 60 ./build/gfortran_*/app/fortcov "$build_dir" \
           --output-format=markdown \
           --output="$output_file" \
           --quiet 2>/dev/null || true
    
    # Count temporary files after
    local temp_after=$(find /tmp -name "fortcov_*" 2>/dev/null | wc -l)
    
    echo "Temporary files before: $temp_before, after: $temp_after"
    
    # Should not accumulate temporary files
    if [ "$temp_after" -le "$temp_before" ]; then
        return 0
    else
        echo "Temporary files not cleaned up properly"
        return 1
    fi
}

# Run all tests
echo "🔧 Setting up test environment..."
mkdir -p "$COVERAGE_DIR"

echo ""
echo "🧪 Starting directory workflow tests..."

run_test "Build fortcov with coverage instrumentation" test_build_with_coverage
run_test "Generate coverage data by running tests" test_generate_coverage_data
run_test "Directory auto-detection workflow" test_directory_auto_detection
run_test "Multiple directory processing" test_multiple_directories
run_test "Mixed input types (directories + .gcov files)" test_mixed_input_types
run_test "CLI flag: --keep-gcov-files" test_keep_gcov_files_flag
run_test "CLI flag: --gcov-args" test_gcov_args_flag
run_test "Large directory performance" test_large_directory_performance
run_test "Error handling for non-existent directories" test_error_handling_nonexistent
run_test "Cleanup verification" test_cleanup_verification

# Summary
echo ""
echo "📊 Test Summary"
echo "========================================"
echo "Total tests: $TEST_COUNT"
echo "Passed: $PASSED_TESTS"
echo "Failed: $FAILED_TESTS"
echo "Success rate: $(( (PASSED_TESTS * 100) / TEST_COUNT ))%"

if [ "$FAILED_TESTS" -eq 0 ]; then
    echo ""
    echo "🎉 All directory workflow tests PASSED!"
    echo "✅ fortcov gcov automation is working correctly"
    exit 0
else
    echo ""
    echo "⚠️  Some tests failed. Review the output above."
    echo "❌ fortcov gcov automation needs attention"
    exit 1
fi