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

**Generate coverage with Issue #227 enhancements:**

```bash
# Build and test with coverage
fpm test --flag "-fprofile-arcs -ftest-coverage"

# Enhanced zero-configuration mode (recommended - Issue #227 fixed)
fortcov  # That's it! Auto-discovery + auto-generation now works seamlessly

# The enhanced zero-config mode now:
# 1. Discovers existing .gcov files in priority locations
# 2. Auto-generates .gcov files from .gcda/.gcno when needed  
# 3. Handles FPM, CMake, and generic build structures
# 4. Creates build/coverage/coverage.md automatically
# 5. Intelligent filtering prevents executable paths being treated as coverage files

# Traditional mode (for custom configurations)
gcov src/*.f90
fortcov --source=src --output=coverage.md

# Enhanced with quality gate and auto-generation
fortcov --threshold=75  # --quiet flag implementation pending
```

## Build System Integration

### FPM Integration (Enhanced - Issue #227)

**Enhanced Zero-Configuration Workflow (Recommended)**

```bash
# Ultra-simple FPM integration with Issue #227 fixes
fpm test --flag "-fprofile-arcs -ftest-coverage"
fortcov  # Automatically handles FPM build structure discovery and gcov generation!

# Behind the scenes, enhanced zero-config now:
# - Discovers .gcda files in build/gfortran_*/app/ and build/gfortran_*/test/
# - Automatically generates .gcov files using secure gcov execution
# - Handles multiple FPM compiler targets (gfortran_*, ifort_*, etc.)
# - Creates output in build/coverage/coverage.md
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

### GitHub Actions (Enhanced with Issue #227)

```yaml
name: Coverage
on: [push, pull_request]

jobs:
  coverage:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v4
    - name: Generate Coverage with Enhanced Zero-Config
      run: |
        fpm test --flag "-fprofile-arcs -ftest-coverage"
        fortcov --threshold=80  # Enhanced zero-config handles all discovery and generation
    - name: Upload Coverage Report
      uses: actions/upload-artifact@v4
      with:
        name: coverage-report
        path: build/coverage/coverage.md
```

### GitLab CI (Enhanced with Issue #227)

```yaml
coverage:
  script:
    - fpm test --flag "-fprofile-arcs -ftest-coverage"
    - fortcov --format=xml --output=coverage.xml --threshold=80  # Enhanced zero-config simplifies workflow
  coverage: '/Total coverage: (\d+\.\d+)%/'
  artifacts:
    reports:
      coverage_report:
        coverage_format: cobertura
        path: coverage.xml
```

For comprehensive build system integration examples, see the examples/build_systems/ directory.