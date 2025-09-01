# Build System Integration Guide

Comprehensive guide for integrating FortCov with different build systems.

## FPM (Fortran Package Manager)

### Standard Integration

```bash
# Generate coverage instrumentation
fpm test --flag "-fprofile-arcs -ftest-coverage"

# Extract coverage data
gcov src/*.f90

# Analyze with fortcov
fortcov --source=. --exclude=build/*,test/* --output=coverage.md
```

### Build-Integrated Discovery

```bash
# Alternative workflow for nested build structures
fpm test --flag "-fprofile-arcs -ftest-coverage"
find build -name "*.gcda" -execdir gcov {} \;
find build -name "*.gcov" -exec cp {} . \;
fortcov --source=. --exclude=build/*,test/* --output=coverage.md
```

### FPM Configuration Integration

```toml
# fpm.toml
[build]
auto-executables = true
auto-tests = true

[profiles.coverage]
flags = ["-fprofile-arcs", "-ftest-coverage"]
```

Usage:
```bash
fpm test --profile coverage
gcov src/*.f90
fortcov --source=src --output=coverage.md
```

## CMake Integration

### CMakeLists.txt Configuration

```cmake
# Enable coverage support
set(CMAKE_Fortran_FLAGS_TESTING "-g -O0 -fprofile-arcs -ftest-coverage")
set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")

# Find codecov support
find_package(codecov)

# Mark targets for coverage
add_coverage(fortran_target)

# Integration with fortcov
add_custom_target(fortcov_report
    COMMAND gcov ${CMAKE_BINARY_DIR}/CMakeFiles/fortran_target.dir/*.gcno
    COMMAND fortcov --source=${CMAKE_SOURCE_DIR} --output=coverage.html
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    DEPENDS fortran_target
)

# Add coverage to default build
add_custom_target(coverage
    DEPENDS fortcov_report
    COMMENT "Generating coverage report with FortCov"
)
```

### CMake Workflow

```bash
# Configure with coverage
cmake -DCMAKE_BUILD_TYPE=Testing -DENABLE_COVERAGE=On ..

# Build and test
make && make test

# Generate coverage report
make coverage
```

### Advanced CMake Integration

```cmake
# Function to add coverage to specific targets
function(add_fortcov_target TARGET_NAME SOURCE_DIR)
    add_custom_target(${TARGET_NAME}_coverage
        COMMAND gcov ${CMAKE_BINARY_DIR}/CMakeFiles/${TARGET_NAME}.dir/*.gcno
        COMMAND fortcov --source=${SOURCE_DIR} --output=${TARGET_NAME}_coverage.html
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
        DEPENDS ${TARGET_NAME}
        COMMENT "Generating coverage for ${TARGET_NAME}"
    )
endfunction()

# Usage
add_fortcov_target(my_library src/lib)
add_fortcov_target(my_executable src/app)
```

## Traditional Makefile

### Basic Makefile Integration

```makefile
# Coverage compilation flags
COVERAGE_FLAGS = -fprofile-arcs -ftest-coverage
FORTRAN_FLAGS = $(COVERAGE_FLAGS) -g -O0
LDFLAGS += -lgcov

# Source and object files
SOURCES = $(wildcard src/*.f90)
OBJECTS = $(SOURCES:.f90=.o)

# Default target
all: fortcov_example

# Build target
fortcov_example: $(OBJECTS)
	$(FC) $(LDFLAGS) -o $@ $^

# Pattern rule for compilation
%.o: %.f90
	$(FC) $(FORTRAN_FLAGS) -c $< -o $@

# Coverage target
coverage: test
	gcov $(SOURCES)
	fortcov --source=src --output=coverage.html

# Test target
test: fortcov_example
	./fortcov_example

# Clean targets
clean:
	rm -f $(OBJECTS) fortcov_example

clean-coverage:
	rm -f *.gcov *.gcda *.gcno coverage.html

.PHONY: all test coverage clean clean-coverage
```

### Advanced Makefile Integration

```makefile
# Multi-directory support
SRC_DIRS = src lib app
TEST_DIRS = test

# Find all sources
SOURCES = $(foreach dir,$(SRC_DIRS),$(wildcard $(dir)/*.f90))
TEST_SOURCES = $(foreach dir,$(TEST_DIRS),$(wildcard $(dir)/*.f90))

# Coverage configuration
COVERAGE_THRESHOLD = 80
COVERAGE_OUTPUT = coverage-report.html

# Coverage with threshold checking
coverage: test
	gcov $(SOURCES)
	fortcov --source=$(SRC_DIRS) --fail-under=$(COVERAGE_THRESHOLD) --output=$(COVERAGE_OUTPUT)
	@echo "Coverage report generated: $(COVERAGE_OUTPUT)"

# Multi-format reporting
coverage-all: test
	gcov $(SOURCES)
	fortcov --source=$(SRC_DIRS) --output=coverage.md
	fortcov --source=$(SRC_DIRS) --format=html --output=coverage.html
	fortcov --source=$(SRC_DIRS) --format=json --output=coverage.json
```

## Meson Integration

Note: FortCov’s primary build system is FPM. Meson support is provided via example projects under `examples/build_systems/meson/`. The repository no longer includes a top‑level `meson.build`/`meson_options.txt`; use the example as a template for your Meson projects.

### meson.build Configuration

```meson
project('fortran_project', 'fortran')

# Coverage configuration
if get_option('coverage')
    add_project_arguments('-fprofile-arcs', '-ftest-coverage', language: 'fortran')
    add_project_link_arguments('-lgcov', language: 'fortran')
endif

# Define sources
fortran_sources = files(
    'src/module1.f90',
    'src/module2.f90'
)

# Executable
executable('fortcov_demo', fortran_sources)

# Find fortcov
fortcov = find_program('fortcov', required: false)

if fortcov.found()
    # Custom target for coverage analysis
    run_target('fortcov_coverage',
        command: [
            find_program('bash'), '-c',
            'gcov @0@/*.f90 && fortcov --source=@0@ --output=coverage.html'.format(meson.source_root())
        ],
        depends: executable
    )
endif
```

### meson_options.txt

```meson
option('coverage', type: 'boolean', value: false, description: 'Enable coverage analysis')
```

### Meson Workflow

```bash
# Configure with coverage
meson setup builddir -Dcoverage=true

# Build and test
cd builddir
meson compile
meson test

# Generate coverage
meson compile fortcov_coverage
```

## Autotools Integration

### configure.ac

```autoconf
AC_ARG_ENABLE([coverage],
    AS_HELP_STRING([--enable-coverage], [Enable coverage analysis]),
    [coverage=$enableval], [coverage=no])

AS_IF([test "x$coverage" != "xno"], [
    FCFLAGS="$FCFLAGS -fprofile-arcs -ftest-coverage"
    LDFLAGS="$LDFLAGS -lgcov"
])

# Check for fortcov
AC_CHECK_PROG([FORTCOV], [fortcov], [fortcov])
AM_CONDITIONAL([HAVE_FORTCOV], [test -n "$FORTCOV"])
```

### Makefile.am

```makefile
if HAVE_FORTCOV
coverage:
	$(MAKE) check
	gcov $(srcdir)/*.f90
	$(FORTCOV) --source=$(srcdir) --output=coverage.html

clean-local:
	-rm -f *.gcov *.gcda *.gcno coverage.html

.PHONY: coverage
endif
```

## Integration Best Practices

### Directory Organization

```
project/
├── src/           # Source code
├── test/          # Tests
├── build/         # Build artifacts (excluded from coverage)
├── coverage/      # Coverage reports
└── scripts/       # CI helpers (no coverage orchestration needed)
```

### Coverage Workflow Script

```bash
#!/bin/bash
# coverage-workflow.sh - Universal coverage workflow

set -e

PROJECT_ROOT=$(cd "$(dirname "$0")" && pwd)
BUILD_SYSTEM=""

# Detect build system
if [ -f "fpm.toml" ]; then
    BUILD_SYSTEM="fpm"
elif [ -f "CMakeLists.txt" ]; then
    BUILD_SYSTEM="cmake"
elif [ -f "meson.build" ]; then
    BUILD_SYSTEM="meson"
elif [ -f "Makefile" ]; then
    BUILD_SYSTEM="make"
else
    echo "No supported build system found"
    exit 1
fi

echo "Detected build system: $BUILD_SYSTEM"

# Clean previous coverage
rm -f *.gcov *.gcda *.gcno

# Build and test with coverage based on build system
case $BUILD_SYSTEM in
    "fpm")
        fpm test --flag "-fprofile-arcs -ftest-coverage"
        ;;
    "cmake")
        cmake -DCMAKE_BUILD_TYPE=Debug ..
        make && make test
        ;;
    "meson")
        meson setup builddir -Dcoverage=true
        cd builddir && meson compile && meson test
        ;;
    "make")
        make coverage
        exit 0  # Make handles everything
        ;;
esac

# Generate coverage data
gcov src/*.f90 2>/dev/null || true

# Generate reports
fortcov --source=src --output=coverage.md
fortcov --source=src --format=html --output=coverage.html

echo "Coverage reports generated:"
echo "  - coverage.md (Markdown)"
echo "  - coverage.html (HTML)"
```

### CI/CD Integration Templates

These integration patterns work across different build systems:

- **GitHub Actions**: See [examples/ci_cd/github_actions/](../../../examples/build_systems/ci_cd/github_actions/)
- **GitLab CI**: See [examples/ci_cd/gitlab_ci/](../../../examples/build_systems/ci_cd/gitlab_ci/)  
- **Jenkins**: See [examples/ci_cd/jenkins/](../../../examples/build_systems/ci_cd/jenkins/)

### Performance Considerations

- Parallel processing is not implemented; FortCov runs single-threaded
- Exclude build artifacts and vendor code with patterns
- Use appropriate output formats for different environments:
  - JSON for automated processing
  - HTML for interactive review
  - Markdown for documentation embedding

For complete build system integration examples with working code, see the examples/build_systems/ directory.
