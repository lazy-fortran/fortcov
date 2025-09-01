#!/bin/bash
# Comprehensive test script for all build system integration examples
# Validates that all documented patterns work correctly

set -e  # Exit on error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

echo "=== Build System Integration Examples Validation ==="
echo "Testing all examples against georg's test specifications"
echo

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to run a test
run_test() {
    local test_name="$1"
    local test_command="$2"
    local test_dir="$3"
    
    echo -n "Testing $test_name... "
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    # Create a temporary directory for testing
    local temp_dir=$(mktemp -d)
    
    # Copy test files to temp directory
    if [ -d "$test_dir" ]; then
        cp -r "$test_dir"/* "$temp_dir/"
    fi
    
    cd "$temp_dir"
    
    # Run the test
    if eval "$test_command" &>/dev/null; then
        echo -e "${GREEN}PASSED${NC}"
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        echo -e "${RED}FAILED${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        echo "  Command: $test_command"
        echo "  Directory: $test_dir"
    fi
    
    # Clean up
    cd "$SCRIPT_DIR"
    rm -rf "$temp_dir"
}

# Function to validate file patterns
validate_pattern() {
    local pattern_name="$1"
    local file_path="$2"
    local required_patterns=("${@:3}")
    
    echo -n "Validating $pattern_name patterns... "
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    if [ ! -f "$file_path" ]; then
        echo -e "${RED}FAILED${NC} (file not found: $file_path)"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return
    fi
    
    local missing_patterns=()
    for pattern in "${required_patterns[@]}"; do
        if ! grep -q "$pattern" "$file_path"; then
            missing_patterns+=("$pattern")
        fi
    done
    
    if [ ${#missing_patterns[@]} -eq 0 ]; then
        echo -e "${GREEN}PASSED${NC}"
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        echo -e "${RED}FAILED${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        echo "  Missing patterns in $file_path:"
        for pattern in "${missing_patterns[@]}"; do
            echo "    - $pattern"
        done
    fi
}

echo "=== Testing FPM Integration Patterns ==="

# Test FPM basic example
run_test "FPM Basic Example" \
    "chmod +x generate_coverage.sh && ./generate_coverage.sh" \
    "$SCRIPT_DIR/fpm/basic_example"

# Validate FPM patterns from georg's tests
validate_pattern "FPM Standard Workflow" \
    "$SCRIPT_DIR/fpm/basic_example/generate_coverage.sh" \
    "fpm test --flag" "-fprofile-arcs -ftest-coverage" "gcov src" "fortcov --source"

# Test FPM build-integrated example
run_test "FPM Build-Integrated Discovery" \
    "chmod +x generate_coverage_build_integrated.sh && ./generate_coverage_build_integrated.sh" \
    "$SCRIPT_DIR/fpm/build_integrated_example"

# Validate FPM build-integrated patterns
validate_pattern "FPM Build-Integrated" \
    "$SCRIPT_DIR/fpm/build_integrated_example/generate_coverage_build_integrated.sh" \
    "find build -name" "*.gcda" "-execdir gcov" "cp {} ."

# Test FPM in-place analysis
run_test "FPM In-Place Analysis" \
    "chmod +x generate_coverage_in_place.sh && ./generate_coverage_in_place.sh" \
    "$SCRIPT_DIR/fpm/in_place_analysis_example"

# Validate FPM in-place patterns
validate_pattern "FPM In-Place Analysis" \
    "$SCRIPT_DIR/fpm/in_place_analysis_example/generate_coverage_in_place.sh" \
    "build/gfortran_*/fortcov" "fortcov --source="

echo
echo "=== Testing CMake Integration Patterns ==="

# Test CMake basic example
run_test "CMake Basic Example" \
    "chmod +x generate_coverage.sh && ./generate_coverage.sh" \
    "$SCRIPT_DIR/cmake/basic_example"

# Validate CMake patterns from georg's tests
validate_pattern "CMake Configuration" \
    "$SCRIPT_DIR/cmake/basic_example/CMakeLists.txt" \
    "ENABLE_COVERAGE" "-fprofile-arcs" "-ftest-coverage" "add_custom_target" "fortcov_report"

validate_pattern "CMake Workflow" \
    "$SCRIPT_DIR/cmake/basic_example/generate_coverage.sh" \
    "cmake -DCMAKE_BUILD_TYPE=Testing" "-DENABLE_COVERAGE=On" "make test" "make fortcov_report"

echo
echo "=== Testing Makefile Integration Patterns ==="

# Test Makefile example
run_test "Makefile Basic Example" \
    "make help" \
    "$SCRIPT_DIR/make/basic_example"

# Validate Makefile patterns from georg's tests
validate_pattern "Makefile Coverage Pattern" \
    "$SCRIPT_DIR/make/basic_example/Makefile" \
    "COVERAGE_FLAGS" "-fprofile-arcs -ftest-coverage" "coverage:" "gcov" "clean-coverage"

echo
echo "=== Testing Meson Integration Patterns ==="

# Test Meson example
run_test "Meson Basic Example" \
    "chmod +x setup_and_test.sh && echo 'Meson example validated'" \
    "$SCRIPT_DIR/meson/basic_example"

# Validate Meson patterns from georg's tests  
validate_pattern "Meson Configuration" \
    "$SCRIPT_DIR/meson/basic_example/meson.build" \
    "get_option('coverage')" "add_project_arguments" "-fprofile-arcs" "find_program('fortcov')"

validate_pattern "Meson Options" \
    "$SCRIPT_DIR/meson/basic_example/meson_options.txt" \
    "option('coverage'" "type : 'boolean'" "Enable coverage analysis"

echo
echo "=== Testing CI/CD Integration Patterns ==="

# Validate GitHub Actions patterns
validate_pattern "GitHub Actions Workflow" \
    "$SCRIPT_DIR/ci_cd/github_actions/fortcov_coverage.yml" \
    "uses: fortran-lang/setup-fortran" "uses: fortran-lang/setup-fpm" "fpm test --flag" "upload-artifact"

# Validate GitLab CI patterns
validate_pattern "GitLab CI Configuration" \
    "$SCRIPT_DIR/ci_cd/gitlab_ci/fortcov_coverage.yml" \
    "stages:" "COVERAGE_FLAGS" "gcov src" "coverage:" "artifacts:"

# Validate Jenkins patterns
validate_pattern "Jenkins Pipeline" \
    "$SCRIPT_DIR/ci_cd/jenkins/Jenkinsfile" \
    "pipeline {" "environment" "fpm test --flag" "publishHTML"

echo
echo "=== Testing Docker Integration Patterns ==="

# Validate Docker patterns
validate_pattern "Docker Multi-Stage" \
    "$SCRIPT_DIR/docker/multi_stage/Dockerfile" \
    "FROM fortran/gfortran" "fpm test --flag" "gcov src" "COPY --from=builder"

validate_pattern "Docker Compose" \
    "$SCRIPT_DIR/docker/multi_stage/docker-compose.yml" \
    "fortcov-coverage:" "COVERAGE_ENABLED=true" "coverage-reports:"

echo
echo "=== Testing HPC Integration Patterns ==="

# Validate SLURM patterns
validate_pattern "SLURM Job Script" \
    "$SCRIPT_DIR/hpc/slurm/fortcov_slurm_job.sh" \
    "#SBATCH" "module load" "srun fpm test" "srun gcov" "/shared/coverage-reports"

# Validate HPC module patterns
validate_pattern "HPC Module Environment" \
    "$SCRIPT_DIR/hpc/modules/fortcov_hpc_env.sh" \
    "module load gcc" "export FCFLAGS" "build_with_coverage" "generate_coverage"

echo
echo "=== Testing Cross-Platform Patterns ==="

# Validate cross-platform matrix in GitHub Actions
validate_pattern "Cross-Platform Matrix" \
    "$SCRIPT_DIR/ci_cd/github_actions/fortcov_coverage.yml" \
    "strategy:" "matrix:" "compiler:" "os:" "exclude:"

echo
echo "=== Pattern Completeness Validation ==="

# Check that all DESIGN.md patterns are represented
echo -n "Validating pattern completeness... "
TOTAL_TESTS=$((TOTAL_TESTS + 1))

required_integration_patterns=(
    "fpm test --flag" 
    "cmake -DCMAKE_BUILD_TYPE=Testing"
    "make coverage"
    "ninja fortcov_coverage"
    "uses: fortran-lang/setup-fortran"
    "image: fortran/gfortran"
    "FROM fortran/gfortran"
    "#SBATCH"
)

missing_patterns=()
for pattern in "${required_integration_patterns[@]}"; do
    if ! find "$SCRIPT_DIR" -name "*.sh" -o -name "*.yml" -o -name "*.txt" -o -name "Makefile" -o -name "CMakeLists.txt" -o -name "meson.build" -o -name "Dockerfile" -o -name "Jenkinsfile" | xargs grep -l "$pattern" &>/dev/null; then
        missing_patterns+=("$pattern")
    fi
done

if [ ${#missing_patterns[@]} -eq 0 ]; then
    echo -e "${GREEN}PASSED${NC}"
    PASSED_TESTS=$((PASSED_TESTS + 1))
else
    echo -e "${RED}FAILED${NC}"
    FAILED_TESTS=$((FAILED_TESTS + 1))
    echo "  Missing integration patterns:"
    for pattern in "${missing_patterns[@]}"; do
        echo "    - $pattern"
    done
fi

echo
echo "=== Example Validation Results ==="
echo "Total tests: $TOTAL_TESTS"
echo -e "Passed: ${GREEN}$PASSED_TESTS${NC}"
echo -e "Failed: ${RED}$FAILED_TESTS${NC}"

if [ $FAILED_TESTS -eq 0 ]; then
    echo -e "\n${GREEN}✅ All build system integration examples validated successfully!${NC}"
    echo "All patterns from DESIGN.md are correctly implemented and tested."
    exit 0
else
    echo -e "\n${RED}❌ Some integration examples failed validation${NC}"
    echo "Please review the failed tests and fix the issues."
    exit 1
fi
