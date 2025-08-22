# FortCov Examples and Tutorials

Practical examples showing how to use FortCov in real-world scenarios.

## Basic Tutorial

### Your First Coverage Report

```bash
# Create simple project
mkdir my-calculator && cd my-calculator

# Initialize FPM project
cat > fpm.toml << 'EOF'
name = "calculator"
version = "0.1.0"
EOF

mkdir -p src test
```

**Create source code** (`src/calculator.f90`):

```fortran
module calculator
    implicit none
    public :: add, subtract
contains
    function add(a, b) result(c)
        real, intent(in) :: a, b
        real :: c
        c = a + b
    end function add
    
    function subtract(a, b) result(c)
        real, intent(in) :: a, b
        real :: c
        c = a - b
    end function subtract
end module calculator
```

**Generate coverage:**

```bash
# Build and test with coverage
fpm test --flag "-fprofile-arcs -ftest-coverage"

# Zero-configuration mode (recommended)
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
  gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
done
fortcov  # That's it! Report in build/coverage/coverage.md

# Traditional mode (for custom configurations)
gcov src/*.f90
fortcov --source=src --output=coverage.md

# Enhanced with quality gate
fortcov --threshold=75 --quiet
```

## Build System Integration

### FPM Integration

**Zero-Configuration Workflow (Recommended)**

```bash
# Simplest possible FPM integration
fpm test --flag "-fprofile-arcs -ftest-coverage"
find build -name "*.gcda" -exec dirname {} \; | sort -u | while read dir; do gcov --object-directory="$dir" "$dir"/*.gcno; done  # FPM-aware coverage
fortcov                       # Auto-discovers everything!
```

**Traditional FPM Workflow**

```bash
# For custom configurations
fpm test --flag "-fprofile-arcs -ftest-coverage"
gcov src/*.f90
fortcov --source=. --exclude=build/*,test/* --output=coverage.md
```

### CMake Integration

```cmake
# CMakeLists.txt
set(CMAKE_Fortran_FLAGS_TESTING "-g -O0 -fprofile-arcs -ftest-coverage")

add_custom_target(coverage
    COMMAND gcov ${CMAKE_BINARY_DIR}/*.gcno
    COMMAND fortcov --source=${CMAKE_SOURCE_DIR} --output=coverage.html
)
```

### Makefile Integration

```makefile
COVERAGE_FLAGS = -fprofile-arcs -ftest-coverage

coverage: test
	gcov $(SOURCES)
	fortcov --source=. --output=coverage.html

.PHONY: coverage
```

## CI/CD Examples

### GitHub Actions

```yaml
name: Coverage
on: [push, pull_request]

jobs:
  coverage:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v4
    - name: Generate Coverage
      run: |
        fpm test --flag "-fprofile-arcs -ftest-coverage"
        find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
          gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
        done
        fortcov --threshold=80 --quiet
    - name: Upload Coverage Report
      uses: actions/upload-artifact@v4
      with:
        name: coverage-report
        path: build/coverage/coverage.md
```

### GitLab CI

```yaml
coverage:
  script:
    - fpm test --flag "-fprofile-arcs -ftest-coverage"
    - find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do gcov --object-directory="$dir" "$dir"/*.gcno; done
    - fortcov --format=xml --output=coverage.xml --threshold=80
  coverage: '/Total coverage: (\d+\.\d+)%/'
  artifacts:
    reports:
      coverage_report:
        coverage_format: cobertura
        path: coverage.xml
```

For comprehensive build system integration examples, see the examples/build_systems/ directory.