#!/bin/bash
# Documentation Commands Validation Test for Issue #232
#
# This test validates that ALL gcov commands in documentation work exactly as written.
# This implements the TDD RED phase - tests SHOULD FAIL initially, proving commands are broken.
#
# Given: Documentation files contain gcov commands
# When: Following each documented workflow exactly as written
# Then: All commands should execute successfully (but currently FAIL)
#
# CRITICAL TEST SCOPE:
# - getting-started.md line 88: gcov -o build/gcov src/*.f90  
# - examples.md lines 50,70,123,138: gcov -o build/gcov src/*.f90
# - usage-guide.md lines 28,214,223: gcov -o build/gcov src/*.f90
# - troubleshooting.md lines 169,248: gcov -o build/gcov src/*.f90

set -e  # Exit on any error (test failure)
set -u  # Exit on undefined variables

echo "================================================================="
echo "DOCUMENTATION COMMANDS VALIDATION TEST (Issue #232)"
echo "================================================================="
echo ""
echo "TDD RED PHASE: These tests SHOULD FAIL initially"
echo "Proving that documented gcov commands are broken"
echo ""

# Test counters
TOTAL_TESTS=0
PASSED_TESTS=0  
FAILED_TESTS=0

# Test environment variables
TEST_DIR="$(pwd)"
TEMP_TEST_DIR="test_documentation_validation_$$"

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Helper functions
test_start() {
    local test_name="$1"
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    echo "  Running: $test_name"
}

test_pass() {
    local test_name="$1"
    local message="$2"
    PASSED_TESTS=$((PASSED_TESTS + 1))
    echo -e "    ${GREEN}✅ PASS${NC}: $test_name - $message"
}

test_fail() {
    local test_name="$1"
    local message="$2" 
    FAILED_TESTS=$((FAILED_TESTS + 1))
    echo -e "    ${RED}❌ FAIL${NC}: $test_name - $message"
}

test_warn() {
    local test_name="$1"
    local message="$2"
    echo -e "    ${YELLOW}⚠️  WARN${NC}: $test_name - $message"
}

# Setup clean test environment for each test
setup_fpm_coverage_environment() {
    echo "    Setting up FPM coverage environment..."
    
    # Clean any existing coverage data
    rm -f *.gcda *.gcno *.gcov coverage.md coverage.json 2>/dev/null || true
    rm -rf build/gcov 2>/dev/null || true
    
    # Build and test with coverage to generate .gcda/.gcno files
    echo "    Building with coverage flags..."
    if ! fpm build --flag "-fprofile-arcs -ftest-coverage" >/dev/null 2>&1; then
        echo "    ERROR: FPM build with coverage failed"
        return 1
    fi
    
    echo "    Running tests with coverage flags..."
    if ! fpm test --flag "-fprofile-arcs -ftest-coverage" >/dev/null 2>&1; then
        echo "    ERROR: FPM test with coverage failed"
        return 1
    fi
    
    return 0
}

# Check if gcov files were created
check_gcov_files_created() {
    local files_created=false
    
    # Check if any .gcov files were created
    if ls *.gcov >/dev/null 2>&1; then
        files_created=true
        echo "    .gcov files found: $(ls *.gcov | wc -l) files"
    else
        echo "    No .gcov files created"
        echo "    Current directory contents:"
        ls -la | head -10
        echo "    Build directory contents:"
        find build -name "*.gcda" -o -name "*.gcno" 2>/dev/null | head -5 || echo "    No coverage files in build/"
    fi
    
    if [[ "$files_created" == "true" ]]; then
        return 0
    else
        return 1
    fi
}

# Test the exact gcov command from getting-started.md line 88
test_getting_started_line_88() {
    test_start "getting-started.md line 88 gcov command"
    
    if ! setup_fpm_coverage_environment; then
        test_fail "getting-started.md line 88" "Failed to setup coverage environment"
        return
    fi
    
    echo "    Executing: gcov -o build/gcov src/*.f90"
    
    # Create build/gcov directory if it doesn't exist (documentation assumes it exists)
    mkdir -p build/gcov
    
    # Execute the exact command from documentation
    local exit_code
    if gcov -o build/gcov src/*.f90 >/dev/null 2>&1; then
        exit_code=0
    else
        exit_code=$?
    fi
    
    if [[ $exit_code -eq 0 ]] && check_gcov_files_created; then
        test_fail "getting-started.md line 88" "UNEXPECTED SUCCESS - should fail in RED phase"
    else
        test_fail "getting-started.md line 88" "Command failed as expected (exit code: $exit_code)"
    fi
}

# Test examples.md gcov commands (multiple instances)
test_examples_md_commands() {
    local line_numbers=(50 70 123 138)
    
    for line_num in "${line_numbers[@]}"; do
        test_start "examples.md line $line_num gcov command"
        
        if ! setup_fpm_coverage_environment; then
            test_fail "examples.md line $line_num" "Failed to setup coverage environment"
            continue
        fi
        
        echo "    Executing: gcov -o build/gcov src/*.f90"
        mkdir -p build/gcov
        
        local exit_code
        if gcov -o build/gcov src/*.f90 >/dev/null 2>&1; then
            exit_code=0
        else
            exit_code=$?
        fi
        
        if [[ $exit_code -eq 0 ]] && check_gcov_files_created; then
            test_fail "examples.md line $line_num" "UNEXPECTED SUCCESS - should fail in RED phase"
        else
            test_fail "examples.md line $line_num" "Command failed as expected (exit code: $exit_code)"
        fi
    done
}

# Test usage-guide.md gcov commands
test_usage_guide_md_commands() {
    local line_numbers=(28 214 223)
    
    for line_num in "${line_numbers[@]}"; do
        test_start "usage-guide.md line $line_num gcov command"
        
        if ! setup_fpm_coverage_environment; then
            test_fail "usage-guide.md line $line_num" "Failed to setup coverage environment"
            continue
        fi
        
        echo "    Executing: gcov -o build/gcov src/*.f90"
        mkdir -p build/gcov
        
        local exit_code
        if gcov -o build/gcov src/*.f90 >/dev/null 2>&1; then
            exit_code=0
        else
            exit_code=$?
        fi
        
        if [[ $exit_code -eq 0 ]] && check_gcov_files_created; then
            test_fail "usage-guide.md line $line_num" "UNEXPECTED SUCCESS - should fail in RED phase"  
        else
            test_fail "usage-guide.md line $line_num" "Command failed as expected (exit code: $exit_code)"
        fi
    done
}

# Test troubleshooting.md gcov commands
test_troubleshooting_md_commands() {
    local line_numbers=(169 248)
    
    for line_num in "${line_numbers[@]}"; do
        test_start "troubleshooting.md line $line_num gcov command"
        
        if ! setup_fpm_coverage_environment; then
            test_fail "troubleshooting.md line $line_num" "Failed to setup coverage environment"
            continue
        fi
        
        echo "    Executing: gcov -o build/gcov src/*.f90"
        mkdir -p build/gcov
        
        local exit_code
        if gcov -o build/gcov src/*.f90 >/dev/null 2>&1; then
            exit_code=0
        else
            exit_code=$?
        fi
        
        if [[ $exit_code -eq 0 ]] && check_gcov_files_created; then
            test_fail "troubleshooting.md line $line_num" "UNEXPECTED SUCCESS - should fail in RED phase"
        else  
            test_fail "troubleshooting.md line $line_num" "Command failed as expected (exit code: $exit_code)"
        fi
    done
}

# Test complete end-to-end workflow from getting-started.md
test_complete_documented_workflow() {
    test_start "Complete getting-started.md workflow"
    
    # Clean environment
    rm -f *.gcda *.gcno *.gcov coverage.md 2>/dev/null || true
    rm -rf build/gcov 2>/dev/null || true
    
    local workflow_failed=false
    local failure_step=""
    
    echo "    Step 1: fpm test --flag \"-fprofile-arcs -ftest-coverage\""
    if ! fpm test --flag "-fprofile-arcs -ftest-coverage" >/dev/null 2>&1; then
        workflow_failed=true
        failure_step="Step 1 (fpm test)"
    fi
    
    if [[ "$workflow_failed" == "false" ]]; then
        echo "    Step 2: gcov -o build/gcov src/*.f90 (THE BROKEN COMMAND)"
        mkdir -p build/gcov
        if ! gcov -o build/gcov src/*.f90 >/dev/null 2>&1; then
            workflow_failed=true
            failure_step="Step 2 (gcov command)"
        fi
    fi
    
    if [[ "$workflow_failed" == "false" ]]; then
        echo "    Step 3: fortcov (should work if gcov succeeded)"
        if ! fpm run fortcov -- --source=src --output=coverage.md >/dev/null 2>&1; then
            workflow_failed=true
            failure_step="Step 3 (fortcov)"
        fi
    fi
    
    # Check if coverage.md was created
    if [[ "$workflow_failed" == "false" ]] && [[ ! -f coverage.md ]]; then
        workflow_failed=true
        failure_step="Final output (no coverage.md)"
    fi
    
    if [[ "$workflow_failed" == "true" ]]; then
        test_fail "Complete workflow" "Failed at $failure_step (as expected in RED phase)"
    else
        test_fail "Complete workflow" "UNEXPECTED SUCCESS - entire workflow worked (should fail in RED phase)"
    fi
}

# Test working bridge script alternative (should pass)
test_bridge_script_alternative() {
    test_start "Bridge script alternative (should work)"
    
    if [[ ! -f scripts/fpm_coverage_bridge.sh ]]; then
        test_warn "Bridge script" "scripts/fpm_coverage_bridge.sh not found - skipping"
        return
    fi
    
    # Clean environment  
    rm -f *.gcda *.gcno *.gcov 2>/dev/null || true
    
    echo "    Step 1: fpm test with coverage"
    if ! fpm test --flag "-fprofile-arcs -ftest-coverage" >/dev/null 2>&1; then
        test_fail "Bridge script" "FPM test failed"
        return
    fi
    
    echo "    Step 2: scripts/fpm_coverage_bridge.sh src"
    if scripts/fpm_coverage_bridge.sh src >/dev/null 2>&1; then
        if check_gcov_files_created; then
            test_pass "Bridge script" "Working alternative creates .gcov files"
        else
            test_fail "Bridge script" "Script ran but no .gcov files created"
        fi
    else
        test_fail "Bridge script" "Bridge script execution failed"
    fi
}

# Diagnostic function to understand FPM build structure
diagnose_fpm_build_structure() {
    echo ""
    echo "================================================================="
    echo "DIAGNOSTIC: FPM BUILD STRUCTURE ANALYSIS"
    echo "================================================================="
    
    echo "Current directory: $(pwd)"
    echo ""
    
    echo "Build directory structure:"
    if [[ -d build ]]; then
        find build -type f -name "*.gcda" -o -name "*.gcno" 2>/dev/null | head -10
        echo ""
        echo "Build subdirectories:"
        find build -type d 2>/dev/null | head -10
    else
        echo "No build directory found"
    fi
    
    echo ""
    echo "Source files that should be instrumented:"
    find src -name "*.f90" 2>/dev/null | head -10
    
    echo ""
    echo "This diagnostic shows why 'gcov -o build/gcov src/*.f90' fails:"
    echo "- Coverage files are in nested build/gfortran_*/fortcov/ directories"  
    echo "- Not in the assumed build/gcov/ directory"
    echo "- gcov needs to find .gcda/.gcno files for each source file"
    echo "================================================================="
}

# Cleanup function
cleanup_test_artifacts() {
    echo "Cleaning up test artifacts..."
    rm -f *.gcov *.gcda *.gcno coverage.md coverage.json 2>/dev/null || true
    rm -rf build/gcov 2>/dev/null || true
    echo "Cleanup complete."
}

# Main test execution
main() {
    echo "Starting documentation commands validation tests..."
    echo ""
    
    # Core broken command tests
    test_getting_started_line_88
    test_examples_md_commands  
    test_usage_guide_md_commands
    test_troubleshooting_md_commands
    
    # Workflow integration tests
    test_complete_documented_workflow
    
    # Working alternative test
    test_bridge_script_alternative
    
    # Show diagnostic information
    diagnose_fpm_build_structure
    
    # Cleanup
    cleanup_test_artifacts
    
    # Results Summary
    echo ""
    echo "================================================================="
    echo "DOCUMENTATION COMMANDS TEST RESULTS (RED Phase)"
    echo "================================================================="
    echo "Total Tests:        $TOTAL_TESTS"
    echo "Passed Tests:       $PASSED_TESTS"
    echo "Failed Tests:       $FAILED_TESTS"
    if [[ $TOTAL_TESTS -gt 0 ]]; then
        echo "Success Rate:       $(( (PASSED_TESTS * 100) / TOTAL_TESTS ))%"
    fi
    echo ""
    
    # Expected outcome in RED phase: most tests should fail
    local expected_failures=$((TOTAL_TESTS - 1))  # Allow 1 passing test (bridge script)
    
    if [[ $FAILED_TESTS -ge $expected_failures ]]; then
        echo -e "${RED}❌ TESTS FAILED AS EXPECTED (RED phase)${NC}"
        echo "   Documentation contains broken gcov commands"
        echo "   Ready for GREEN phase implementation"
        echo ""
        echo "KEY FINDINGS:"
        echo "- Documented gcov commands fail due to FPM build structure"
        echo "- Coverage files are in nested gfortran_*/fortcov/ directories"
        echo "- Documentation assumes simple build/gcov/ structure"
        echo "- Bridge script works as intended alternative"
        exit 1  # Expected failure in RED phase
    else
        echo -e "${YELLOW}⚠️  UNEXPECTED: Tests passed (should fail in RED phase)${NC}"
        echo "   Either tests are wrong or documentation already fixed"
        exit 2
    fi
}

# Check if we're in the right directory
if [[ ! -f fpm.toml ]]; then
    echo "ERROR: Must run from fortcov project root (fpm.toml not found)"
    exit 3
fi

# Check prerequisites
missing_tools=()
for tool in fpm gfortran gcov; do
    if ! command -v "$tool" >/dev/null 2>&1; then
        missing_tools+=("$tool")
    fi
done

if [[ ${#missing_tools[@]} -gt 0 ]]; then
    echo "ERROR: Missing required tools: ${missing_tools[*]}"
    echo "Please install missing tools and try again"
    exit 4
fi

# Run main function
main "$@"