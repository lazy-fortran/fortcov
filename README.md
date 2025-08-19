# FortCov

A modern coverage analysis tool specifically designed for Fortran projects. FortCov processes coverage data from gfortran to generate comprehensive, actionable reports that help you understand and improve your code coverage.

## Why FortCov?

- **🎯 Fortran-First Design**: Built specifically for Fortran with deep understanding of modules, interfaces, and modern Fortran constructs
- **⚡ Fast & Reliable**: Processes large codebases efficiently with comprehensive error handling
- **🔒 Secure by Default**: Built-in protections against common security vulnerabilities and malformed input
- **📊 Multiple Output Formats**: Generate Markdown, JSON, HTML reports that integrate seamlessly with your workflow
- **🛠️ Developer Friendly**: Clear error messages, progress indicators, and extensive configuration options

## Quick Start

Get up and running with FortCov in under 2 minutes:

```bash
# 1. Build and test with coverage instrumentation
fpm test --flag "-fprofile-arcs -ftest-coverage"

# 2. Extract coverage data from FPM build directories
BUILD_DIR=$(find build -name "*.gcda" | head -1 | xargs dirname)
gcov --object-directory="$BUILD_DIR" "$BUILD_DIR"/*.gcno

# 3. Create coverage report
fortcov --source=. --exclude='build/*,test/*' --output=coverage.md
```

**Alternative approach** (using provided helper script):
```bash
# Use the FPM coverage bridge script for simplified workflow
./scripts/fpm_coverage_bridge.sh root coverage.md
# OR for src directory pattern:
./scripts/fpm_coverage_bridge.sh src coverage.md
```

The bridge script handles all FPM build directory complexity automatically while maintaining the simple README patterns.

That's it! Open `coverage.md` to see your coverage report.

## Installation

### From Source (Recommended)

```bash
git clone https://github.com/lazy-fortran/fortcov.git
cd fortcov
fpm build --profile release

# Optional: Add to PATH
sudo ln -s $(pwd)/build/gfortran_*/app/fortcov /usr/local/bin/fortcov
```

### Prerequisites

- **Fortran compiler**: gfortran 9.0+ recommended
- **Fortran Package Manager**: [Install fpm](https://github.com/fortran-lang/fpm)
- **gcov**: Usually included with gcc/gfortran

## Usage

### Basic Usage

The most common workflow:

```bash
# Generate coverage report for your src/ directory
# (Assumes .gcov files are in src/ directory)
fortcov --source=src --output=coverage.md
```

**Important**: The `--source` path must contain `.gcov` files. FortCov searches for `*.gcov` files in the specified directory but does not search subdirectories recursively.

### Understanding Source Paths

**How FortCov finds coverage files**:
- Searches for `*.gcov` files in the exact directory specified by `--source`
- Does NOT search recursively in subdirectories 
- Multiple `--source` paths can be specified to search multiple directories

**Common source path patterns**:

```bash
# When .gcov files are generated in your source directory
gcov src/*.f90  # Creates .gcov files in src/
fortcov --source=src --output=coverage.md

# When .gcov files are generated in project root  
gcov src/*.f90  # Creates .gcov files in current directory
fortcov --source=. --exclude='build/*,test/*' --output=coverage.md

# Multiple source directories
fortcov --source=src/core --source=src/utils --output=coverage.md

# Finding where your .gcov files are located
find . -name "*.gcov" -type f
```

**Source path troubleshooting**:
```bash
# Step 1: Locate your .gcov files
find . -name "*.gcov" -type f

# Step 2: Set --source to directory containing .gcov files (not source code)
# If files found in src/:
fortcov --source=src --output=coverage.md
# If files found in project root:
fortcov --source=. --exclude='build/*,test/*' --output=coverage.md

# Step 3: If no .gcov files exist, generate them first
fpm build --flag "-fprofile-arcs -ftest-coverage"
fpm test --flag "-fprofile-arcs -ftest-coverage"
gcov src/*.f90  # Creates .gcov files in current directory
# OR:
cd src && gcov *.f90 && cd ..  # Creates .gcov files in src/

# Step 4: Use absolute paths if relative paths cause issues
fortcov --source="$(pwd)/src" --output=coverage.md
```

### Common Use Cases

#### For CI/CD Pipelines

```bash
# Set coverage threshold and use quiet mode
fortcov --source=src --fail-under=80 --quiet --output=coverage.md

# Exit codes: 0=success, 1=error, 2=coverage below threshold
echo "Coverage check result: $?"
```

#### For Large Projects

```bash
# Exclude build artifacts and test files
fortcov --source=src --exclude='build/*' --exclude='test/*' --output=coverage.md

# Process specific directories
fortcov --source=src/core --source=src/utils --output=coverage.md
```

#### For Interactive Analysis

```bash
# Launch terminal user interface for browsing coverage
fortcov --source=src --tui

# Generate multiple output formats
fortcov --source=src --output-format=json --output=coverage.json
fortcov --source=src --output-format=html --output=coverage.html
```

### Configuration File

For complex projects, use a configuration file:

```bash
# Generate a sample configuration
cp fortcov.nml.example fortcov.nml

# Edit fortcov.nml to match your project
# Then run:
fortcov --config=fortcov.nml
```

### Import/Export Workflow

```bash
# Export coverage data to JSON for processing
fortcov --source=src --output-format=json --output=baseline.json

# Compare coverage between releases  
fortcov --diff=baseline.json,current.json --threshold=5.0 --output=diff.md
```

### Example Output

```markdown
| Filename                          |   Stmts |   Miss | Cover   | Missing            |
|-----------------------------------|---------|--------|---------|--------------------|
| src/module_a.f90                  |     100 |     10 | 90.00%  | 45-48, 72-77       |
| src/module_b.f90                  |      50 |      0 | 100.00% |                    |
| TOTAL                             |     150 |     10 | 93.33%  |                    |
```

## Command Reference

### Essential Options

| Option | Short | Description | Example |
|--------|-------|-------------|---------|
| `--source=PATH` | `-s` | Source directory to search for .gcov files (required) | `--source=src` |
| `--output=FILE` | `-o` | Output file | `--output=coverage.md` |
| `--fail-under=N` | `-t` | Coverage threshold | `--fail-under=80` |
| `--quiet` | `-q` | Suppress output | `--quiet` |
| `--verbose` | `-v` | Detailed output | `--verbose` |
| `--help` | `-h` | Show help | `--help` |

### All Options

```bash
fortcov --help  # See complete list of options
```

## Troubleshooting

### Quick Problem Resolution

| Error Message | Jump to Solution |
|---------------|------------------|
| "No coverage files found" | [→ Missing Coverage Files](#-no-coverage-files-found) |
| "Command not found: fortcov" | [→ Command Not Found](#-command-not-found-fortcov) |
| "Permission denied" | [→ Permission Issues](#-permission-denied) |
| "File too large" / "Memory exhaustion" | [→ Large Files](#-file-too-large-or-memory-exhaustion) |
| CI/CD pipeline failures | [→ CI/CD Issues](#cicd-troubleshooting) |

### Common Issues

#### ❌ "No coverage files found"

**Cause**: Missing `.gcov` files or incomplete coverage workflow

**Complete Resolution Workflow**:
```bash
# STEP 1: Clean build with coverage instrumentation
fpm clean
fpm build --flag "-fprofile-arcs -ftest-coverage"

# STEP 2: Run tests to generate .gcda execution data
fpm test --flag "-fprofile-arcs -ftest-coverage"

# STEP 3: Generate .gcov files using gcov
# Choose ONE approach:

# Approach A: Extract from FPM build directories
BUILD_DIR=$(find build -name "*.gcda" | head -1 | xargs dirname)
cd "$BUILD_DIR" && gcov *.gcno && cd -
find build -name "*.gcov" -exec cp {} . \;
fortcov --source=. --exclude='build/*,test/*' --output=coverage.md

# Approach B: Use helper script (simpler)
./scripts/fpm_coverage_bridge.sh root coverage.md

# STEP 4: Verify .gcov files exist
find . -name "*.gcov" -type f
# If no files found, repeat steps 1-3
```

**If still no .gcov files**:
```bash
# Check for .gcda files (test execution data)
find . -name "*.gcda" -type f
# If missing, tests may not be running - check test execution

# Check for .gcno files (instrumentation data)
find . -name "*.gcno" -type f  
# If missing, build may lack coverage flags

# Verify coverage flags are active
fpm build --flag "-fprofile-arcs -ftest-coverage" --verbose
```

#### ❌ "Command not found: fortcov"

**Cause**: FortCov not built or not in PATH

**Solution**:
```bash
# Option 1: Build FortCov first (if not built)
fpm build --profile release

# Option 2: Use fpm run (recommended for development)
fpm run fortcov -- --source=src --output=coverage.md

# Option 3: Run directly using full path
# Find the executable first:
find build -name "fortcov" -type f
# Then run it:
./build/gfortran_*/app/fortcov --source=src --output=coverage.md

# Option 4: Add to PATH permanently
# First expand the glob pattern:
FORTCOV_PATH=$(find "$(pwd)/build" -name "fortcov" -type f -executable | head -1 | xargs dirname)
if [ -n "$FORTCOV_PATH" ]; then
    export PATH="$PATH:$FORTCOV_PATH"
    echo "FortCov added to PATH: $FORTCOV_PATH"
else
    echo "FortCov executable not found - run 'fpm build' first"
fi

# Option 5: Create symbolic link (requires sudo)
sudo ln -sf $(find "$(pwd)/build" -name "fortcov" -type f -executable | head -1) /usr/local/bin/fortcov
```

#### ❌ "Permission denied"

**Cause**: Insufficient file permissions for reading source files or writing output

**Complete Permission Diagnosis**:
```bash
# Step 1: Identify the specific permission issue
# Test source directory read access
ls -la src/ || echo "Cannot read source directory"

# Test output directory write access
touch coverage.md && rm coverage.md || echo "Cannot write to output directory"

# Test .gcov file access
find . -name "*.gcov" -exec ls -la {} \; | head -5
```

**Solution by Permission Type**:
```bash
# For source directory read issues:
chmod -R +r src/
chmod +rx src/  # Ensure directory is executable

# For output directory write issues:
# Check directory exists and is writable
OUTPUT_DIR=$(dirname "coverage.md")
if [ ! -d "$OUTPUT_DIR" ]; then
    mkdir -p "$OUTPUT_DIR"
fi
chmod +w "$OUTPUT_DIR"

# For .gcov file access issues:
find . -name "*.gcov" -exec chmod +r {} \;

# For executable permission issues:
find build -name "fortcov" -type f -exec chmod +x {} \; 2>/dev/null || echo "Executable not found"
```

**CI/CD Permission Issues**:
```bash
# In CI environments, use relative paths and check permissions
pwd && ls -la
fpm build --profile release
find build -name "fortcov" -exec chmod +x {} \;
EXECUTABLE=$(find build -name "fortcov" -type f -executable | head -1)
if [ -n "$EXECUTABLE" ]; then
    "$EXECUTABLE" --source=. --output=coverage.md
else
    fpm run fortcov -- --source=. --output=coverage.md
fi
```

#### ❌ "File too large" or "Memory exhaustion"

**Cause**: Very large `.gcov` files consuming excessive memory

**Immediate Diagnosis**:
```bash
# Identify largest .gcov files
find . -name "*.gcov" -exec ls -lh {} \; | sort -k5 -hr | head -10

# Check total .gcov file size
du -sh $(find . -name "*.gcov")

# Check available system memory
free -h
```

**Solution Strategy**:
```bash
# Strategy 1: Process in smaller batches by directory
fortcov --source=src/core --output=core-coverage.md
fortcov --source=src/utils --output=utils-coverage.md
fortcov --source=src/tests --output=tests-coverage.md

# Strategy 2: Remove largest files and regenerate cleaner coverage
# Remove files larger than 10MB
find . -name "*.gcov" -size +10M -ls
find . -name "*.gcov" -size +10M -delete

# Clean rebuild for smaller coverage files
fpm clean
fpm build --flag "-fprofile-arcs -ftest-coverage"
# Use timeout to prevent hanging tests
timeout 300 fpm test --flag "-fprofile-arcs -ftest-coverage" || echo "Tests timed out"
gcov src/*.f90

# Strategy 3: Use exclude patterns to skip problematic files
fortcov --source=. --exclude='**/large_module.f90.gcov,build/*,test/*' --output=coverage.md

# Strategy 4: For CI/CD - set memory limits and timeouts
timeout 300 fortcov --source=src --output=coverage.md || echo "Coverage generation timed out"
```

### CI/CD Troubleshooting

#### GitHub Actions Issues

**Problem**: Coverage generation fails in CI environment

**Common CI-specific solutions**:
```yaml
# Fix 1: Ensure proper permissions and paths
- name: Generate coverage with error handling
  run: |
    set -e
    pwd && ls -la
    fpm build --profile release
    fpm test --flag "-fprofile-arcs -ftest-coverage"
    gcov src/*.f90 || echo "gcov failed, trying alternative"
    find . -name "*.gcov" -type f | head -5
    
    # Use fpm run if executable not found
    if [ -x "./build/gfortran_*/app/fortcov" ]; then
        ./build/gfortran_*/app/fortcov --source=. --exclude='build/*,test/*' --output=coverage.md
    else
        fpm run fortcov -- --source=. --exclude='build/*,test/*' --output=coverage.md
    fi

# Fix 2: Handle missing gcov files gracefully
- name: Generate coverage with fallback
  run: |
    fpm build --flag "-fprofile-arcs -ftest-coverage"
    fpm test --flag "-fprofile-arcs -ftest-coverage"
    
    # Extract coverage from FPM build directories
    BUILD_DIR=$(find build -name "*.gcda" | head -1 | xargs dirname)
    if [ -n "$BUILD_DIR" ]; then
        echo "Found coverage data in: $BUILD_DIR"
        cd "$BUILD_DIR" && gcov *.gcno && cd -
        find build -name "*.gcov" -exec cp {} . \;
        SOURCE_PATH="."
    else
        echo "No FPM coverage data found, trying fallback"
        if gcov src/*.f90 2>/dev/null; then
            SOURCE_PATH="."
        else
            echo "Coverage generation failed"
            find . -name "*.gcda" -type f
            exit 1
        fi
    fi
    
    fpm run fortcov -- --source="$SOURCE_PATH" --exclude='build/*,test/*' --output=coverage.md
```

#### GitLab CI Issues

**Problem**: Inconsistent coverage generation across runners

**Solution**:
```yaml
coverage:
  before_script:
    - pwd && ls -la
    - which gfortran gcov || (apt-get update && apt-get install -y gcc gfortran)
  script:
    - fpm clean  # Ensure clean state
    - fpm build --flag "-fprofile-arcs -ftest-coverage" --verbose
    - fpm test --flag "-fprofile-arcs -ftest-coverage" --verbose
    - |
      # Generate .gcov files with error handling
      if ! gcov src/*.f90; then
          echo "Direct gcov failed, trying from src directory"
          cd src && gcov *.f90 && cd ..
      fi
    - find . -name "*.gcov" -type f | wc -l  # Verify files exist
    - fpm run fortcov -- --source=. --exclude='build/*,test/*' --output=coverage.md --verbose
  artifacts:
    paths:
      - coverage.md
    reports:
      coverage_report:
        coverage_format: generic
        path: coverage.md
  coverage: '/TOTAL.*?(\d+\.\d+)%/'
```

### Advanced Troubleshooting

#### Workflow Conflicts

**Problem**: Confusion between manual gcov and source discovery approaches

**Clear Workflow Selection**:
```bash
# WORKFLOW A: FPM build directory extraction (recommended for FPM)
# Extract coverage from complex FPM build directories
BUILD_DIR=$(find build -name "*.gcda" | head -1 | xargs dirname)
cd "$BUILD_DIR" && gcov *.gcno && cd -
find build -name "*.gcov" -exec cp {} . \;
fortcov --source=. --exclude='build/*,test/*' --output=coverage.md

# WORKFLOW B: Helper script (simplest for FPM users)
# Use provided bridge script to handle FPM complexity
./scripts/fpm_coverage_bridge.sh root coverage.md

# WORKFLOW C: Direct build analysis (fastest for CI/CD)
# Analyze coverage directly in build directories
BUILD_DIR=$(find build -name "*.gcda" | head -1 | xargs dirname)
cd "$BUILD_DIR" && gcov *.gcno
fortcov --source="$BUILD_DIR" --output=coverage.md

# NEVER MIX: Don't use both approaches simultaneously
# Pick one workflow and stick with it consistently
```

#### Environment-Specific Issues

**Problem**: Different behavior across development environments

**Environmental diagnosis**:
```bash
# Check compiler and tool versions
gfortran --version
gcov --version
fpm --version

# Check file system and permissions
ls -la src/
df -h .  # Check disk space
umask     # Check default permissions

# Test basic gcov functionality
echo 'program test; print *, "hello"; end program' > test.f90
gfortran -fprofile-arcs -ftest-coverage test.f90 -o test
./test
gcov test.f90
ls -la *.gcov
rm test.f90 test test.gc*
```

### Getting Help

- **Built-in help**: `fortcov --help`
- **Validate config**: `fortcov --config=fortcov.nml --verbose`  
- **Debug mode**: `fortcov --source=src --verbose` for detailed output
- **Workflow validation**: Use the test commands above to verify your environment
- **GitHub Issues**: Report bugs at [github.com/lazy-fortran/fortcov/issues](https://github.com/lazy-fortran/fortcov/issues)

## Build System Integration

FortCov integrates seamlessly with all major Fortran build systems. For complete working examples, see [examples/build_systems/](examples/build_systems/).

### FPM Integration

FPM stores coverage files in complex build directories. Use these patterns:

```bash
# Pattern 1: Extract from build directories (recommended)
fpm test --flag "-fprofile-arcs -ftest-coverage"
BUILD_DIR=$(find build -name "*.gcda" | head -1 | xargs dirname)
cd "$BUILD_DIR" && gcov *.gcno && cd -
find build -name "*.gcov" -exec cp {} . \;
fortcov --source=. --exclude='build/*,test/*' --output=coverage.md

# Pattern 2: Use helper script (simpler)
./scripts/fpm_coverage_bridge.sh root coverage.md

# Pattern 3: Direct build directory analysis (fastest)
BUILD_DIR=$(find build -name "*.gcda" | head -1 | xargs dirname)
cd "$BUILD_DIR" && gcov *.gcno
fortcov --source="$BUILD_DIR" --output=coverage.md
```

**Recommended**: Use Pattern 2 (bridge script) for consistent results and automatic error handling.

**Complete working examples**: See [examples/build_systems/fpm/](examples/build_systems/fpm/) for all FPM integration patterns.

### CMake Integration

```cmake
# In CMakeLists.txt
option(ENABLE_COVERAGE "Enable coverage analysis" OFF)
if(ENABLE_COVERAGE)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fprofile-arcs -ftest-coverage")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -lgcov")
endif()

# Custom target for coverage
add_custom_target(fortcov_report
    COMMAND gcov ${CMAKE_SOURCE_DIR}/src/*.f90
    COMMAND fortcov --source=. --output=coverage.html --output-format=html
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)
```

### Makefile Integration

```makefile
# Coverage flags
COVERAGE_FLAGS = -fprofile-arcs -ftest-coverage
COVERAGE_LIBS = -lgcov

coverage: test
	gcov src/*.f90
	fortcov --source=. --exclude='build/*' --output=coverage.md

clean-coverage:
	rm -f *.gcov *.gcda *.gcno
```

### Meson Integration

```meson
# In meson.build
coverage_option = get_option('coverage')
if coverage_option
  add_project_arguments('-fprofile-arcs', '-ftest-coverage', language: 'fortran')
  add_project_link_arguments('-lgcov', language: 'fortran')
endif

# Custom target
run_target('coverage',
  command: ['bash', '-c', 'gcov src/*.f90 && fortcov --source=. --output=coverage.html']
)
```

**Complete Examples**: See [examples/build_systems/](examples/build_systems/) for full working examples with all build systems.

## CI/CD Integration

### GitHub Actions

```yaml
name: Coverage
on: [push, pull_request]
jobs:
  coverage:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3
    - name: Setup Fortran
      uses: fortran-lang/setup-fortran@v1
    - name: Install FPM
      uses: fortran-lang/setup-fpm@v1
    - name: Build with coverage
      run: |
        fpm build --flag "-fprofile-arcs -ftest-coverage"
        fpm test --flag "-fprofile-arcs -ftest-coverage"
    - name: Generate coverage report
      run: |
        # Option 1: Use bridge script (recommended for consistency)
        chmod +x scripts/fpm_coverage_bridge.sh
        ./scripts/fpm_coverage_bridge.sh root coverage.md
        
        # Option 2: Manual FPM build directory extraction
        # BUILD_DIR=$(find build -name "*.gcda" | head -1 | xargs dirname)
        # if [ -n "$BUILD_DIR" ]; then
        #   cd "$BUILD_DIR" && gcov *.gcno && cd -
        #   find build -name "*.gcov" -exec cp {} . \;
        # else
        #   echo "No coverage data found - using fallback"
        #   gcov src/*.f90 2>/dev/null || true
        # fi
        # # Verify .gcov files exist
        # find . -name "*.gcov" -type f | head -5
        # # Run coverage analysis
        # if find build -name "fortcov" -type f -executable | head -1 | xargs test -x; then
        #   $(find build -name "fortcov" -type f -executable | head -1) --source=. --exclude='build/*,test/*' --output=coverage.md --fail-under=80 --quiet
        # else
        #   fpm run fortcov -- --source=. --exclude='build/*,test/*' --output=coverage.md --fail-under=80 --quiet
        # fi
    - name: Upload coverage
      uses: actions/upload-artifact@v3
      with:
        name: coverage-report
        path: coverage.md
```

### GitLab CI

```yaml
coverage:
  script:
    - fpm build --flag "-fprofile-arcs -ftest-coverage"
    - fpm test --flag "-fprofile-arcs -ftest-coverage"  
    # Option 1: Use bridge script (recommended)
    - chmod +x scripts/fpm_coverage_bridge.sh
    - ./scripts/fpm_coverage_bridge.sh root coverage.md
    # Option 2: Manual extraction (fallback)
    # - BUILD_DIR=$(find build -name "*.gcda" | head -1 | xargs dirname)
    # - if [ -n "$BUILD_DIR" ]; then cd "$BUILD_DIR" && gcov *.gcno && cd -; find build -name "*.gcov" -exec cp {} . \;; fi
    # - find . -name "*.gcov" -type f | wc -l
    # - fpm run fortcov -- --source=. --exclude='build/*,test/*' --output=coverage.md --quiet
  artifacts:
    paths:
      - coverage.md
```

**Complete CI/CD Examples**: See [examples/build_systems/ci_cd/](examples/build_systems/ci_cd/) for GitHub Actions, GitLab CI, and Jenkins configurations.

## Contributing

We welcome contributions! For detailed development information see [DESIGN.md](DESIGN.md).

### Quick Development Setup

```bash
git clone https://github.com/lazy-fortran/fortcov.git
cd fortcov

# Build and test
fpm build
fpm test

# Test with coverage
fpm build --flag "-fprofile-arcs -ftest-coverage"  
fpm test --flag "-fprofile-arcs -ftest-coverage"
gcov src/*.f90  
fortcov --source=. --exclude='build/*,test/*' --output=coverage.md
```

### Guidelines

- Follow Test-Driven Development (TDD)
- Keep functions small and focused
- Use existing code style (88 char lines, 4-space indent)
- Update documentation for new features

## Links

- **Documentation**: [DESIGN.md](DESIGN.md) - Detailed architecture and design
- **Issues**: [GitHub Issues](https://github.com/lazy-fortran/fortcov/issues) - Bug reports and feature requests
- **FPM**: [Fortran Package Manager](https://github.com/fortran-lang/fpm) - Build system