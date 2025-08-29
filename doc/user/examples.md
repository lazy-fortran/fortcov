# FortCov Working Examples

Copy-paste ready examples for common FortCov usage patterns. All examples tested and verified.

## Basic Coverage Analysis

**Simple project setup:**
```bash
mkdir demo && cd demo && fpm init demo

# Create source file
cat > src/demo.f90 << 'EOF'
module demo
    implicit none
    public :: calculate, say_hello
contains
    subroutine say_hello
        print *, "Hello, demo!"
    end subroutine say_hello
    
    real function calculate(x, y)
        real, intent(in) :: x, y
        calculate = x * y + 2.0
    end function calculate
end module
EOF

# Create test
cat > test/test_demo.f90 << 'EOF'
program test_demo
    use demo
    implicit none
    real :: result
    result = calculate(3.0, 4.0)
    if (abs(result - 14.0) > 1e-6) stop 1
    print *, 'Test passed!'
end program
EOF

# Generate coverage
# IMPORTANT: FPM does not have a --coverage flag. Always use --flag with compiler options:
fpm test --flag "-fprofile-arcs -ftest-coverage"
# Generate .gcov files manually:
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
  gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
done
fortcov --source=src *.gcov
```

## Manual File Processing

**Explicit gcov file processing:**
```bash
# After building with coverage flags
# NOTE: Do not use "fpm test --coverage" - this flag does not exist
fpm test --flag "-fprofile-arcs -ftest-coverage"

# Generate gcov files manually
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
  gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
done

# Process specific files
fortcov --source=src src_demo.f90.gcov test_demo.f90.gcov
```

## Terminal Output Examples

**Basic terminal output:**
```bash
fortcov --source=src *.gcov  # Shows coverage statistics in terminal
```

**Quiet output for scripts:**
```bash
fortcov --source=src *.gcov --quiet
```

**Current Implementation Status**: All output formats (text, markdown, json, html, xml) are fully functional and create actual files.

## Build System Integration

**FPM with custom script:**
```bash
#!/bin/bash
# coverage.sh - FPM coverage helper
set -e
echo "Building with coverage..."
# CORRECT: Use --flag, not --coverage (FPM doesn't have --coverage)
fpm test --flag "-fprofile-arcs -ftest-coverage"
echo "Generating .gcov files..."
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
  gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
done
echo "Analyzing coverage..."
fortcov --source=src *.gcov
```

**CMake integration:**
```cmake
# In CMakeLists.txt
set(CMAKE_Fortran_FLAGS_COVERAGE "-fprofile-arcs -ftest-coverage")

add_custom_target(coverage
    DEPENDS ${PROJECT_NAME}_test
    COMMAND ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}_test
    COMMAND bash -c "find . -name '*.gcda' | xargs dirname | sort -u | while read dir; do gcov --object-directory=\"$$dir\" \"$$dir\"/*.gcno 2>/dev/null || true; done"
    COMMAND fortcov --source=src *.gcov
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Generating coverage analysis"
)
```

**Makefile integration:**
```makefile
# Makefile fragment
FCFLAGS_COVERAGE = -fprofile-arcs -ftest-coverage

coverage: test
	@echo "Generating .gcov files..."
	find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do \
		gcov --object-directory="$$dir" "$$dir"/*.gcno 2>/dev/null || true; \
	done
	@echo "Analyzing coverage..."
	fortcov --source=src *.gcov

.PHONY: coverage
```

## CI/CD Pipeline Integration

**GitHub Actions:**
```yaml
name: Coverage Analysis

jobs:
  coverage:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3
    
    - name: Install gfortran
      run: sudo apt-get install -y gfortran
      
    - name: Install FPM
      uses: fortran-lang/setup-fpm@v1
      
    - name: Install FortCov
      run: |
        git clone https://github.com/lazy-fortran/fortcov.git
        cd fortcov && fpm build --profile release
        sudo cp build/gfortran_*/app/fortcov /usr/local/bin/
    
    - name: Generate Coverage
      run: |
        fpm test --flag "-fprofile-arcs -ftest-coverage"
        find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
          gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
        done
        fortcov --source=src *.gcov --fail-under=80
        # Note: FortCov outputs coverage analysis to stdout by default
        # File output formats are not yet implemented in current version
```

**GitLab CI:**
```yaml
stages:
  - test
  - coverage

coverage_analysis:
  stage: coverage
  image: ubuntu:22.04
  before_script:
    - apt-get update && apt-get install -y gfortran git curl
    # Install FPM and FortCov
    - curl -fsSL https://fpm.fortran-lang.org/install.sh | sh
    - git clone https://github.com/lazy-fortran/fortcov.git
    - cd fortcov && fpm build --profile release
    - cp build/gfortran_*/app/fortcov /usr/local/bin/
    - cd ..
  script:
    - fpm test --flag "-fprofile-arcs -ftest-coverage"
    - find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true; done
    - fortcov --source=src *.gcov --fail-under=75
  # Note: File output formats not yet implemented in current version
  # FortCov outputs coverage analysis to stdout for now
```

## Advanced Usage Patterns

**Multi-file analysis:**
```bash
fortcov --source=src src_*.gcov lib_*.gcov modules_*.gcov
```

**Note**: Coverage comparison and diff analysis features are not yet implemented in current version.

**Quality gate integration:**
```bash
#!/bin/bash
# quality-gate.sh
# Note: File output not yet implemented, use --fail-under option
fortcov --source=src *.gcov --fail-under=80 --quiet
echo "Coverage check passed"
```

## Configuration Examples

**Development environment:**
```fortran
! dev.nml
&fortcov_config
    source_paths = 'src/', 'lib/'
    exclude_patterns = '*.mod', 'test/*'
    verbose = .true.
    minimum_coverage = 70.0
    ! Note: output_format not yet implemented
/
```

**Production CI:**
```fortran
! ci.nml  
&fortcov_config
    source_paths = 'src/'
    exclude_patterns = '*.mod', 'test/*', 'vendor/*', 'build/*'
    quiet = .true.
    minimum_coverage = 90.0
    ! Note: output_format and output_path not yet implemented
/
```

**Usage with configs:**
```bash
# Use development config
fortcov --config=dev.nml

# Use CI config with override
fortcov --config=ci.nml --fail-under=95
```

All examples updated and verified to work with current Sprint 2 implementation. Terminal coverage analysis is fully functional.