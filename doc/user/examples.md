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
gcov src/*.f90
fortcov --source=src --output=coverage.md
```

## Build System Integration

### FPM Integration

```bash
# Standard workflow
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
        gcov src/*.f90
        fortcov --source=src --fail-under=80 --output=coverage.md
```

### GitLab CI

```yaml
coverage:
  script:
    - fpm test --flag "-fprofile-arcs -ftest-coverage"
    - gcov src/*.f90
    - fortcov --source=src --output-format=html --output=coverage.html
  coverage: '/Total coverage: (\d+\.\d+)%/'
```

For comprehensive build system integration examples, see the examples/build_systems/ directory.