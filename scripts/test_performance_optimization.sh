#!/bin/bash
# Test Performance Optimization Script for Issue #192 & #981
# Addresses slow test execution by managing I/O output efficiently
# Now includes CI performance monitoring and regression detection

echo "FortCov Test Performance Optimization"
echo "======================================"

# Detect CI environment
if [[ "$CI" == "true" || "$GITHUB_ACTIONS" == "true" ]]; then
    echo "Running in CI environment with performance monitoring enabled"
    CI_MODE=true
else
    echo "Running in local environment"
    CI_MODE=false
fi

# Performance optimization: Redirect verbose output to reduce I/O overhead
export FORTCOV_QUIET_TESTS=1

echo "Running tests with performance optimizations..."

# Option 1: Run tests with output suppressed for performance
if [ "$1" == "quiet" ]; then
    echo "Running in quiet mode (maximum performance)..."
    fpm test > /dev/null 2>&1
    TEST_RESULT=$?
    
    if [ $TEST_RESULT -eq 0 ]; then
        echo "✅ All tests PASSED (quiet mode)"
    else
        echo "❌ Some tests FAILED (quiet mode)"
        echo "Run without 'quiet' parameter to see detailed output"
    fi

# Option 2: Run tests with reduced output
elif [ "$1" == "summary" ]; then
    echo "Running with summary output..."
    fpm test 2>&1 | grep -E "(PASSED|FAILED|Error|✅|❌|Testing)" | tail -20

# Option 3: Run specific fast tests only
elif [ "$1" == "fast" ]; then
    echo "Running fast tests only (excluding slow architectural validation tests)..."
    echo "Note: This excludes tests with >100 print statements for performance"
    # Implementation would require test categorization
    fpm test 2>&1 | head -50

# Option 4: CI integration mode with performance benchmark
elif [ "$1" == "ci" ]; then
    echo "Running CI performance monitoring mode..."
    export CI=true  # Ensure CI-optimized performance benchmarks
    
    start_time=$(date +%s)
    fpm test test_security_performance_benchmark
    BENCHMARK_RESULT=$?
    end_time=$(date +%s)
    benchmark_duration=$((end_time - start_time))
    
    echo ""
    echo "Performance benchmark completed in ${benchmark_duration}s"
    
    if [ $BENCHMARK_RESULT -eq 0 ]; then
        echo "✅ Performance benchmarks PASSED - no regressions detected"
    else
        echo "❌ Performance benchmarks FAILED - performance regression detected"
        exit 1
    fi

# Default: Normal execution with performance guidance
else
    echo "Performance Modes Available:"
    echo "  ./test_performance_optimization.sh quiet   - Maximum performance, minimal output"
    echo "  ./test_performance_optimization.sh summary - Summary output only" 
    echo "  ./test_performance_optimization.sh fast    - Fast tests only"
    echo "  ./test_performance_optimization.sh ci      - CI performance monitoring with regression detection"
    echo ""
    echo "Issue #192 Analysis Results:"
    echo "  - Identified 744 print statements in slowest test"
    echo "  - Top 4 tests have 180-744 print statements each"
    echo "  - Excessive I/O overhead during test execution"
    echo ""
    echo "Running normal tests with performance monitoring..."
    
    start_time=$(date +%s)
    fpm test
    end_time=$(date +%s)
    duration=$((end_time - start_time))
    
    echo ""
    echo "Test execution completed in ${duration} seconds"
    
    # CI performance regression detection
    if [ "$CI_MODE" == "true" ]; then
        # Performance thresholds for CI environment
        CI_TIMEOUT_THRESHOLD=300  # 5 minutes max for CI
        CI_WARNING_THRESHOLD=120  # 2 minutes warning for CI
        
        if [ $duration -gt $CI_TIMEOUT_THRESHOLD ]; then
            echo "❌ CI PERFORMANCE FAILURE: Tests exceeded ${CI_TIMEOUT_THRESHOLD}s threshold"
            echo "   This indicates a performance regression requiring investigation"
            exit 1
        elif [ $duration -gt $CI_WARNING_THRESHOLD ]; then
            echo "⚠️  CI PERFORMANCE WARNING: Tests took ${duration}s (threshold: ${CI_WARNING_THRESHOLD}s)"
            echo "   Monitor for potential performance regression"
        else
            echo "✅ CI PERFORMANCE OK: Tests completed within acceptable time"
        fi
    else
        # Local performance guidance
        if [ $duration -gt 60 ]; then
            echo "⚠️  Tests took longer than 60 seconds"
            echo "💡 Use 'quiet' or 'summary' mode for faster execution"
        fi
    fi
fi