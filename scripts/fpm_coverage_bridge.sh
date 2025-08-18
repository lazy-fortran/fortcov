#!/bin/bash
# FPM Coverage Bridge Script
# Bridges the gap between simple README patterns and complex FPM build directories
# Implements the documented simple patterns by handling FPM complexity internally

set -e

# Configuration
PROJECT_ROOT="$(pwd)"
FORTCOV_EXCLUDE_DEFAULT="build/*,test/*"

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

# Function to find fortcov executable
find_fortcov() {
    local fortcov_exe
    
    # Try to find in build directory first
    fortcov_exe=$(find build -name "fortcov" -type f -executable 2>/dev/null | head -1)
    
    if [ -n "$fortcov_exe" ]; then
        echo "$fortcov_exe"
        return 0
    fi
    
    # Try system PATH
    if command -v fortcov >/dev/null 2>&1; then
        echo "fortcov"
        return 0
    fi
    
    print_error "fortcov executable not found. Run 'fpm build' first."
    return 1
}

# Function to generate coverage with simple pattern emulation
generate_simple_coverage() {
    local source_pattern="$1"
    local output_file="${2:-coverage.md}"
    local exclude_pattern="${3:-$FORTCOV_EXCLUDE_DEFAULT}"
    
    print_info "Starting FPM coverage generation using simple pattern emulation..."
    
    # Step 1: Clean any existing coverage data
    print_info "Cleaning previous coverage data..."
    rm -f *.gcov *.gcda *.gcno
    find . -name "*.gcda" -delete 2>/dev/null || true
    find . -name "*.gcno" -delete 2>/dev/null || true
    find . -name "*.gcov" -delete 2>/dev/null || true
    
    # Step 2: Build with coverage flags
    print_info "Building with coverage instrumentation..."
    fpm test --flag "-fprofile-arcs -ftest-coverage"
    
    # Step 3: Handle the complex FPM build directory structure
    print_info "Extracting coverage data from FPM build directories..."
    
    # Find build directories with coverage data
    local build_dirs
    build_dirs=$(find build -name "*.gcda" | xargs dirname | sort -u 2>/dev/null || true)
    
    if [ -z "$build_dirs" ]; then
        print_error "No coverage data (.gcda files) found in build directories"
        print_error "Make sure FPM compiled with coverage flags: --flag \"-fprofile-arcs -ftest-coverage\""
        return 1
    fi
    
    print_info "Found coverage data in build directories:"
    echo "$build_dirs" | while read -r dir; do
        echo "  - $dir"
    done
    
    # Step 4: Generate .gcov files in each build directory
    print_info "Generating .gcov files in build directories..."
    echo "$build_dirs" | while read -r build_dir; do
        if [ -n "$build_dir" ] && [ -d "$build_dir" ]; then
            print_info "Processing: $build_dir"
            (
                cd "$build_dir"
                gcov *.gcno 2>/dev/null || gcov *.gcda 2>/dev/null || true
            )
        fi
    done
    
    # Step 5: Implement the simple pattern by copying .gcov files appropriately
    case "$source_pattern" in
        "src")
            print_info "Implementing simple pattern: gcov files in src/ directory"
            mkdir -p src
            find build -name "*.gcov" -exec cp {} src/ \; 2>/dev/null || true
            ;;
        ".")
            print_info "Implementing simple pattern: gcov files in project root"
            find build -name "*.gcov" -exec cp {} . \; 2>/dev/null || true
            ;;
        *)
            print_info "Implementing custom pattern: gcov files in $source_pattern"
            mkdir -p "$source_pattern"
            find build -name "*.gcov" -exec cp {} "$source_pattern"/ \; 2>/dev/null || true
            ;;
    esac
    
    # Step 6: Verify .gcov files are available for analysis
    local gcov_files
    gcov_files=$(find "$source_pattern" -name "*.gcov" 2>/dev/null | wc -l)
    
    if [ "$gcov_files" -eq 0 ]; then
        print_error "No .gcov files found in $source_pattern after processing"
        print_error "This indicates a problem with the coverage generation process"
        return 1
    fi
    
    print_success "Found $gcov_files .gcov files in $source_pattern"
    
    # Step 7: Run fortcov analysis
    print_info "Running fortcov analysis..."
    local fortcov_exe
    fortcov_exe=$(find_fortcov) || return 1
    
    print_info "Command: $fortcov_exe --source=$source_pattern --exclude='$exclude_pattern' --output=$output_file"
    "$fortcov_exe" --source="$source_pattern" --exclude="$exclude_pattern" --output="$output_file"
    
    print_success "Coverage report generated: $output_file"
    
    # Step 8: Show summary
    print_info "Coverage analysis complete!"
    echo
    echo "Files generated:"
    echo "  - Coverage report: $output_file"
    echo "  - .gcov files in: $source_pattern"
    echo
    echo "Clean up with: rm -f $source_pattern/*.gcov"
}

# Main script logic
main() {
    case "${1:-help}" in
        "src")
            # Emulate: cd src && gcov *.f90 && cd .. && fortcov --source=src
            generate_simple_coverage "src" "${2:-coverage.md}" "${3:-$FORTCOV_EXCLUDE_DEFAULT}"
            ;;
        "root"|".")
            # Emulate: gcov src/*.f90 && fortcov --source=. --exclude='build/*,test/*'
            generate_simple_coverage "." "${2:-coverage.md}" "${3:-$FORTCOV_EXCLUDE_DEFAULT}"
            ;;
        "custom")
            # Custom source directory
            if [ -z "$2" ]; then
                print_error "Custom mode requires source directory: $0 custom <source_dir> [output_file] [exclude_pattern]"
                exit 1
            fi
            generate_simple_coverage "$2" "${3:-coverage.md}" "${4:-$FORTCOV_EXCLUDE_DEFAULT}"
            ;;
        "help"|*)
            echo "FPM Coverage Bridge Script"
            echo "Bridges simple README patterns with complex FPM build directory handling"
            echo
            echo "Usage:"
            echo "  $0 src [output_file] [exclude_pattern]"
            echo "    Emulates: cd src && gcov *.f90 && fortcov --source=src"
            echo
            echo "  $0 root [output_file] [exclude_pattern]"
            echo "    Emulates: gcov src/*.f90 && fortcov --source=. --exclude='build/*,test/*'"
            echo
            echo "  $0 custom <source_dir> [output_file] [exclude_pattern]"
            echo "    Custom source directory pattern"
            echo
            echo "Examples:"
            echo "  $0 src                     # Simple src/ pattern -> coverage.md"
            echo "  $0 root coverage.html      # Project root pattern -> coverage.html"
            echo "  $0 custom analysis         # Custom directory pattern"
            echo
            echo "Default exclude pattern: $FORTCOV_EXCLUDE_DEFAULT"
            ;;
    esac
}

# Run main function with all arguments
main "$@"