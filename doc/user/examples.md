# FortCov Working Examples

Copy-paste ready examples for common FortCov usage patterns. All examples tested and verified.

## Zero-Configuration Quickstart

**Simple project setup:**
```bash
mkdir demo && cd demo && fpm init demo

# Create source file
cat > src/demo.f90 << 'EOF'
module demo
    implicit none
    public :: calculate
contains
    real function calculate(x, y)
        real, intent(in) :: x, y
        calculate = x * y + 2.0
    end function
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

# Generate coverage (zero-configuration)
fpm test --flag "-fprofile-arcs -ftest-coverage"
fortcov  # Auto-discovers everything
```

## Manual File Processing

**Explicit gcov file processing:**
```bash
# After building with coverage flags
fpm test --flag "-fprofile-arcs -ftest-coverage"

# Generate gcov files manually
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
  gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
done

# Process specific files
fortcov src_demo.f90.gcov test_demo.f90.gcov
```

## Output Format Examples

**Markdown report:**
```bash
fortcov --format=markdown --output=coverage-report.md
```

**JSON for CI/CD:**
```bash
fortcov --format=json --output=coverage.json --quiet
# Parse with: jq -r '.summary.line_coverage' coverage.json
```

**XML for integration:**
```bash
fortcov --format=xml --output=coverage.xml
```

## Build System Integration

**FPM with custom script:**
```bash
#!/bin/bash
# coverage.sh - FPM coverage helper
set -e
echo "Building with coverage..."
fpm test --flag "-fprofile-arcs -ftest-coverage"
echo "Generating coverage report..."
fortcov --format=markdown --output=coverage.md
echo "Coverage report: coverage.md"
```

**CMake integration:**
```cmake
# In CMakeLists.txt
set(CMAKE_Fortran_FLAGS_COVERAGE "-fprofile-arcs -ftest-coverage")

add_custom_target(coverage
    DEPENDS ${PROJECT_NAME}_test
    COMMAND ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}_test
    COMMAND fortcov --source ${CMAKE_SOURCE_DIR}/src --format=html --output=coverage.html
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Generating coverage report"
)
```

**Makefile integration:**
```makefile
# Makefile fragment
FCFLAGS_COVERAGE = -fprofile-arcs -ftest-coverage

coverage: test
	@echo "Generating coverage report..."
	fortcov --source src --exclude "test/*" --format=markdown --output=coverage.md
	@echo "Report generated: coverage.md"

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
        fortcov --format=json --output=coverage.json --fail-under=80
    
    - name: Upload Coverage
      uses: actions/upload-artifact@v3
      with:
        name: coverage-report
        path: coverage.json
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
    - fortcov --format=xml --output=coverage.xml --fail-under=75
  artifacts:
    reports:
      coverage_report:
        coverage_format: cobertura
        path: coverage.xml
    paths:
      - coverage.xml
```

## Advanced Usage Patterns

**Multi-directory analysis:**
```bash
fortcov --source src --source lib --source modules --exclude "*.mod"
```

**Coverage comparison:**
```bash
# Generate baseline
fortcov --format=json --output=baseline.json

# After changes
fortcov --format=json --output=current.json

# Compare
fortcov --diff baseline.json,current.json --diff-threshold=2.0
```

**Quality gate integration:**
```bash
#!/bin/bash
# quality-gate.sh
fortcov --format=json --output=coverage.json --quiet
coverage=$(jq -r '.summary.line_coverage' coverage.json)
echo "Current coverage: $coverage%"

if (( $(echo "$coverage < 80" | bc -l) )); then
    echo "ERROR: Coverage below 80% threshold"
    exit 1
fi
echo "Coverage check passed"
```

## Configuration Examples

**Development environment:**
```fortran
! dev.nml
&fortcov_config
    source_paths = 'src/', 'lib/'
    exclude_patterns = '*.mod', 'test/*'
    output_format = 'terminal'
    verbose = .true.
    minimum_coverage = 70.0
/
```

**Production CI:**
```fortran
! ci.nml  
&fortcov_config
    source_paths = 'src/'
    exclude_patterns = '*.mod', 'test/*', 'vendor/*', 'build/*'
    output_format = 'json'
    output_path = 'coverage.json'
    quiet = .true.
    minimum_coverage = 90.0
/
```

**Usage with configs:**
```bash
# Use development config
fortcov --config=dev.nml

# Use CI config with override
fortcov --config=ci.nml --fail-under=95
```

All examples are tested and ready for copy-paste use. For additional build system examples, see `/examples/build_systems/` directory.