#!/bin/bash
# Fixed FPM Coverage Workflow Script
# Addresses Issue #1052 - FPM coverage workflow generates test coverage instead of application coverage

set -e

# Configuration
PROJECT_ROOT="$(pwd)"
COVERAGE_OUTPUT="${1:-coverage.md}"
SOURCE_DIR="${2:-src}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

print_info() {
    echo -e "${BLUE}INFO:${NC} $1"
}

print_success() {
    echo -e "${GREEN}SUCCESS:${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}WARNING:${NC} $1"
}

print_error() {
    echo -e "${RED}ERROR:${NC} $1"
}

# Main workflow function
generate_fpm_application_coverage() {
    print_info "FPM Coverage Workflow Fix - Issue #1052"
    print_info "Generating application source coverage (not test coverage)"
    
    # Step 1: Clean only project root coverage files (preserve build directory)
    print_info "Cleaning previous .gcov files from project root..."
    rm -f *.gcov
    
    # Step 2: Build application with coverage instrumentation
    print_info "Building application with coverage instrumentation..."
    fpm build --flag "-fprofile-arcs -ftest-coverage"
    
    # Step 3: Find the instrumented executable
    local fortcov_exe
    fortcov_exe=$(find build -name fortcov -type f -executable | head -1)
    
    if [ -z "$fortcov_exe" ]; then
        print_error "FortCov executable not found after build"
        print_error "Run 'fpm build --flag \"-fprofile-arcs -ftest-coverage\"' first"
        return 1
    fi
    
    print_success "Found instrumented executable: $fortcov_exe"
    
    # Step 4: Execute the instrumented application to generate coverage
    print_info "Running instrumented application to generate coverage data..."
    print_info "Command: $fortcov_exe --help"
    
    # Run the application help command to exercise code paths
    "$fortcov_exe" --help > /dev/null 2>&1 || true
    
    # Run application with version flag to exercise more code paths
    "$fortcov_exe" --version > /dev/null 2>&1 || true
    
    # Run application with configuration validation to exercise config modules
    "$fortcov_exe" --validate > /dev/null 2>&1 || true
    
    # Run zero-config discovery to exercise auto-discovery modules
    echo "module dummy_module" > /tmp/test_fortran.f90
    "$fortcov_exe" --source=/tmp --quiet --output=/tmp/dummy_coverage.md /tmp/test_fortran.f90 > /dev/null 2>&1 || true
    rm -f /tmp/test_fortran.f90 /tmp/dummy_coverage.md 2>/dev/null || true
    
    print_success "Application execution completed - coverage data generated"
    
    # Step 5: Verify coverage data generation
    local gcda_count gcno_count
    gcda_count=$(find build -name "*.gcda" 2>/dev/null | wc -l)
    gcno_count=$(find build -name "*.gcno" 2>/dev/null | wc -l)
    
    print_info "Coverage data files:"
    print_info "  .gcda files (runtime data): $gcda_count"
    print_info "  .gcno files (instrumentation): $gcno_count"
    
    if [ "$gcda_count" -eq 0 ]; then
        print_error "No .gcda files generated - application execution may have failed"
        return 1
    fi
    
    # Step 6: Generate .gcno files by rebuilding (they're consumed during runtime)
    print_info "Regenerating .gcno files for gcov processing..."
    fpm build --flag "-fprofile-arcs -ftest-coverage" > /dev/null
    
    gcno_count=$(find build -name "*.gcno" 2>/dev/null | wc -l)
    print_info "Regenerated .gcno files: $gcno_count"
    
    # Step 7: Process coverage data with gcov
    print_info "Processing coverage data with gcov..."
    local build_dirs
    build_dirs=$(find build -name "*.gcda" | xargs dirname | sort -u 2>/dev/null || true)
    
    if [ -z "$build_dirs" ]; then
        print_error "No build directories with coverage data found"
        return 1
    fi
    
    print_info "Processing build directories:"
    local total_gcov_files=0
    echo "$build_dirs" | while IFS= read -r build_dir; do
        if [ -n "$build_dir" ] && [ -d "$build_dir" ]; then
            print_info "  Processing: $build_dir"
            
            # Change to build directory and run gcov
            (
                cd "$build_dir" || exit 1
                if ls *.gcno >/dev/null 2>&1; then
                    gcov *.gcno 2>/dev/null || true
                    local gcov_count
                    gcov_count=$(ls *.gcov 2>/dev/null | wc -l || echo "0")
                    print_info "    Generated $gcov_count .gcov files"
                    
                    # Move meaningful .gcov files to project root
                    if [ "$gcov_count" -gt 0 ]; then
                        # Filter meaningful files (exclude empty ones)
                        for gcov_file in *.gcov; do
                            if [ -f "$gcov_file" ] && [ "$(wc -l < "$gcov_file")" -gt 1 ]; then
                                cp "$gcov_file" "$PROJECT_ROOT/" 2>/dev/null || true
                                total_gcov_files=$((total_gcov_files + 1))
                            fi
                        done
                    fi
                fi
            )
        fi
    done
    
    # Step 8: Verify .gcov files available for analysis
    local gcov_files_root
    gcov_files_root=$(ls *.gcov 2>/dev/null | wc -l || echo "0")
    
    print_info "Total meaningful .gcov files in project root: $gcov_files_root"
    
    if [ "$gcov_files_root" -eq 0 ]; then
        print_warning "No .gcov files found in project root"
        print_warning "This may indicate:"
        print_warning "  - Application execution didn't cover many source modules"
        print_warning "  - Most coverage is in dependencies/infrastructure code"
        print_warning "  - Need to run more comprehensive application scenarios"
        
        # Try to find any .gcov files that might have source coverage
        print_info "Searching for any .gcov files with source coverage..."
        find build -name "*.gcov" -exec grep -l "src/" {} \; 2>/dev/null | while IFS= read -r src_gcov; do
            print_info "Found source coverage file: $src_gcov"
            cp "$src_gcov" "$PROJECT_ROOT/" 2>/dev/null || true
        done
        
        gcov_files_root=$(ls *.gcov 2>/dev/null | wc -l || echo "0")
        print_info "Source .gcov files copied: $gcov_files_root"
    fi
    
    if [ "$gcov_files_root" -eq 0 ]; then
        print_error "No coverage data available for analysis"
        print_error "The application may need more comprehensive test scenarios"
        return 1
    fi
    
    # Step 9: Generate coverage report using FortCov
    print_info "Generating coverage report..."
    local clean_fortcov_exe
    clean_fortcov_exe=$(find build -name fortcov -type f -executable | head -1)
    
    if [ -n "$clean_fortcov_exe" ]; then
        local gcov_files_list
        gcov_files_list=$(ls *.gcov 2>/dev/null | tr '\n' ' ' || echo "")
        
        if [ -n "$gcov_files_list" ]; then
            print_info "Running: $clean_fortcov_exe --source $SOURCE_DIR --output $COVERAGE_OUTPUT $gcov_files_list"
            # shellcheck disable=SC2086
            "$clean_fortcov_exe" --source "$SOURCE_DIR" --output "$COVERAGE_OUTPUT" $gcov_files_list || {
                print_warning "Coverage analysis completed with warnings"
            }
        fi
    fi
    
    print_success "FPM application coverage workflow completed!"
    
    # Step 10: Report summary
    if [ -f "$COVERAGE_OUTPUT" ]; then
        print_success "Coverage report generated: $COVERAGE_OUTPUT"
        local lines
        lines=$(wc -l < "$COVERAGE_OUTPUT" 2>/dev/null || echo "0")
        print_info "Report size: $lines lines"
    else
        print_warning "Coverage report not generated, but .gcov files available"
        print_info "Manual analysis: ls *.gcov | head -5"
    fi
    
    print_info "Cleanup: .gcov files available for manual analysis"
    print_info "To clean: rm -f *.gcov"
    
    return 0
}

# Usage information
usage() {
    echo "FPM Coverage Workflow Fix Script"
    echo "Generates application coverage (not test coverage)"
    echo ""
    echo "Usage: $0 [OUTPUT_FILE] [SOURCE_DIR]"
    echo ""
    echo "Arguments:"
    echo "  OUTPUT_FILE    Coverage report output file (default: coverage.md)"
    echo "  SOURCE_DIR     Source directory to analyze (default: src)"
    echo ""
    echo "Examples:"
    echo "  $0                              # Generate coverage.md for src/"
    echo "  $0 my_coverage.html src         # Generate HTML report for src/"
    echo "  $0 coverage.json app            # Generate JSON report for app/"
    echo ""
    echo "This script fixes Issue #1052 by:"
    echo "1. Building instrumented application (not just tests)"
    echo "2. Running application to exercise source code"
    echo "3. Processing runtime coverage data with gcov"
    echo "4. Generating meaningful coverage reports"
}

# Main script execution
case "${1:-generate}" in
    "help"|"-h"|"--help")
        usage
        ;;
    *)
        generate_fpm_application_coverage
        ;;
esac