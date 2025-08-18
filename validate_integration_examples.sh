#!/bin/bash
# Simple validation script to check integration examples against georg's test patterns

set -e

echo "=== Validating Build System Integration Examples ==="
echo "Checking examples against georg's test specifications"
echo

EXAMPLES_DIR="examples/build_systems"
TESTS_PASSED=0
TESTS_FAILED=0

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

# Function to check pattern
check_pattern() {
    local test_name="$1"
    local file_path="$2"
    local pattern="$3"
    
    echo -n "Checking $test_name... "
    
    if [ -f "$file_path" ] && grep -F "$pattern" "$file_path" >/dev/null 2>&1; then
        echo -e "${GREEN}PASSED${NC}"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        echo -e "${RED}FAILED${NC}"
        echo "  File: $file_path"
        echo "  Pattern: $pattern"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi
}

# Georg's Test 1: FPM Standard Workflow Pattern
echo "=== FPM Integration Patterns ==="
check_pattern "FPM standard workflow command" \
    "$EXAMPLES_DIR/fpm/basic_example/generate_coverage.sh" \
    'fpm test --flag "-fprofile-arcs -ftest-coverage"'

check_pattern "FPM gcov command" \
    "$EXAMPLES_DIR/fpm/basic_example/generate_coverage.sh" \
    "gcov src/\*.f90"

check_pattern "FPM fortcov command" \
    "$EXAMPLES_DIR/fpm/basic_example/generate_coverage.sh" \
    "fortcov --source=. --exclude=build/\*,test/\*"

# Georg's Test 2: FPM Build-Integrated Discovery Pattern  
check_pattern "FPM build-integrated find command" \
    "$EXAMPLES_DIR/fpm/build_integrated_example/generate_coverage_build_integrated.sh" \
    'find build -name "\*.gcda" -path "\*/fortcov/\*"'

check_pattern "FPM build-integrated copy command" \
    "$EXAMPLES_DIR/fpm/build_integrated_example/generate_coverage_build_integrated.sh" \
    'find build -name "*.gcov" -exec cp'

# Georg's Test 3: FPM In-Place Analysis Pattern
check_pattern "FPM in-place analysis command" \
    "$EXAMPLES_DIR/fpm/in_place_analysis_example/generate_coverage_in_place.sh" \
    'fortcov --source="build/gfortran_\*/fortcov"'

# Georg's Test 4: CMake Integration Patterns
echo
echo "=== CMake Integration Patterns ==="
check_pattern "CMake coverage flags configuration" \
    "$EXAMPLES_DIR/cmake/basic_example/CMakeLists.txt" \
    "fprofile-arcs"

check_pattern "CMake custom target" \
    "$EXAMPLES_DIR/cmake/basic_example/CMakeLists.txt" \
    "add_custom_target(fortcov_report"

check_pattern "CMake workflow command" \
    "$EXAMPLES_DIR/cmake/basic_example/generate_coverage.sh" \
    "cmake -DCMAKE_BUILD_TYPE=Testing -DENABLE_COVERAGE=On"

# Georg's Test 5: Makefile Integration Patterns  
echo
echo "=== Makefile Integration Patterns ==="
check_pattern "Makefile coverage flags" \
    "$EXAMPLES_DIR/make/basic_example/Makefile" \
    "COVERAGE_FLAGS = -fprofile-arcs -ftest-coverage"

check_pattern "Makefile coverage target" \
    "$EXAMPLES_DIR/make/basic_example/Makefile" \
    "coverage:"

check_pattern "Makefile gcov command" \
    "$EXAMPLES_DIR/make/basic_example/Makefile" \
    "gcov $(SOURCES)"

# Georg's Test 6: Meson Integration Patterns
echo
echo "=== Meson Integration Patterns ==="
check_pattern "Meson coverage option" \
    "$EXAMPLES_DIR/meson/basic_example/meson.build" \
    "get_option('coverage')"

check_pattern "Meson coverage flags" \
    "$EXAMPLES_DIR/meson/basic_example/meson.build" \
    "add_project_arguments"

check_pattern "Meson fortcov integration" \
    "$EXAMPLES_DIR/meson/basic_example/meson.build" \
    "find_program('fortcov'"

# Georg's Test 7: CI/CD Integration Patterns
echo
echo "=== CI/CD Integration Patterns ==="
check_pattern "GitHub Actions Fortran setup" \
    "$EXAMPLES_DIR/ci_cd/github_actions/fortcov_coverage.yml" \
    "uses: fortran-lang/setup-fortran"

check_pattern "GitHub Actions FPM setup" \
    "$EXAMPLES_DIR/ci_cd/github_actions/fortcov_coverage.yml" \
    "uses: fortran-lang/setup-fpm"

check_pattern "GitLab CI coverage flags" \
    "$EXAMPLES_DIR/ci_cd/gitlab_ci/fortcov_coverage.yml" \
    "COVERAGE_FLAGS:"

check_pattern "Jenkins pipeline coverage" \
    "$EXAMPLES_DIR/ci_cd/jenkins/Jenkinsfile" \
    "Build with Coverage"

# Georg's Test 8: Docker Integration Patterns
echo
echo "=== Docker Integration Patterns ==="
check_pattern "Docker multi-stage build" \
    "$EXAMPLES_DIR/docker/multi_stage/Dockerfile" \
    "FROM fortran/gfortran:latest as builder"

check_pattern "Docker FPM installation" \
    "$EXAMPLES_DIR/docker/multi_stage/Dockerfile" \
    "curl -fsSL.*fpm-linux-x86_64"

# Georg's Test 9: HPC Integration Patterns
echo
echo "=== HPC Integration Patterns ==="
check_pattern "SLURM job directives" \
    "$EXAMPLES_DIR/hpc/slurm/fortcov_slurm_job.sh" \
    "#SBATCH --job-name=fortcov-coverage"

check_pattern "HPC module loading" \
    "$EXAMPLES_DIR/hpc/modules/fortcov_hpc_env.sh" \
    "module load gcc"

# Georg's Test 10: Cross-Platform Patterns
echo
echo "=== Cross-Platform Integration Patterns ==="
check_pattern "Multi-compiler matrix" \
    "$EXAMPLES_DIR/ci_cd/github_actions/fortcov_coverage.yml" \
    "compiler: \[gfortran"

check_pattern "Multi-OS matrix" \
    "$EXAMPLES_DIR/ci_cd/github_actions/fortcov_coverage.yml" \
    "os: \[ubuntu-latest, macos-latest\]"

# Summary
echo
echo "=== Validation Results ==="
TOTAL_TESTS=$((TESTS_PASSED + TESTS_FAILED))
echo "Total patterns tested: $TOTAL_TESTS"
echo -e "Passed: ${GREEN}$TESTS_PASSED${NC}"
echo -e "Failed: ${RED}$TESTS_FAILED${NC}"

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "\n${GREEN}✅ All integration examples pass georg's test patterns!${NC}"
    echo "Issue #164 build system integration examples are complete and validated."
    exit 0
else
    echo -e "\n${RED}❌ Some integration patterns are missing or incorrect${NC}"
    echo "Please review and fix the failed patterns."
    exit 1
fi