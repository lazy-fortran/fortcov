# FortCov

A modern coverage analysis tool specifically designed for Fortran projects. FortCov processes coverage data from gfortran to generate comprehensive, actionable reports that help you understand and improve your code coverage.

## Why FortCov?

- **🎯 Fortran-First Design**: Built specifically for Fortran with deep understanding of modules, interfaces, and modern Fortran constructs
- **⚡ Fast & Reliable**: Processes large codebases efficiently with robust memory management and comprehensive error handling
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
fortcov --exclude='build/*,test/*' --output=coverage.md
```

**Alternative approach** (using provided helper script):
```bash
# Use the FPM coverage bridge script for simplified workflow
# (From FortCov repository root directory)
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
fortcov --exclude='build/*,test/*' --output=coverage.md

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
fortcov --exclude='build/*,test/*' --output=coverage.md

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
fortcov --fail-under=80 --quiet --output=coverage.md

# Check exit code for pipeline control
EXIT_CODE=$?
if [ $EXIT_CODE -eq 0 ]; then
    echo "✅ Coverage analysis successful"
elif [ $EXIT_CODE -eq 1 ]; then
    echo "❌ Configuration error or tool failure"
    exit 1
elif [ $EXIT_CODE -eq 2 ]; then
    echo "⚠️ Coverage below threshold (pipeline decision required)"
    # Configure your pipeline: fail build or continue with warning
    exit 1  # Fail build
    # exit 0  # Continue with warning
elif [ $EXIT_CODE -eq 3 ]; then
    echo "⚠️ No coverage data found"
    # Configure your pipeline behavior for missing coverage
    exit 1  # Fail if coverage required
    # exit 0  # Continue if coverage optional
else
    echo "❌ Unexpected exit code: $EXIT_CODE"
    exit 1
fi
```

**Exit Code Reference:**
- `0` - Success (analysis completed, help/version displayed)
- `1` - Error (invalid configuration, file errors, tool failures)  
- `2` - Coverage threshold not met (configurable CI/CD behavior)
- `3` - No coverage data found (configurable CI/CD behavior)

#### For Large Projects

```bash
# Exclude build artifacts and test files from current directory
fortcov --exclude='build/*' --exclude='test/*' --output=coverage.md

# Process specific directories
fortcov --source=src/core --source=src/utils --output=coverage.md
```

#### For Interactive Analysis

```bash
# Launch terminal user interface for browsing coverage
fortcov --tui

# Generate multiple output formats
fortcov --output-format=json --output=coverage.json
fortcov --output-format=html --output=coverage.html
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
fortcov --output-format=json --output=baseline.json

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
| `--source=PATH` | `-s` | Source directory to search for .gcov files (default: current directory) | `--source=src` |
| `--output=FILE` | `-o` | Output file | `--output=coverage.md` |
| `--fail-under=N` | `-t` | Coverage threshold | `--fail-under=80` |
| `--quiet` | `-q` | Suppress output | `--quiet` |
| `--verbose` | `-v` | Detailed output | `--verbose` |
| `--help` | `-h` | Show help | `--help` |

### All Options

```bash
fortcov --help  # See complete list of options
```

### Exit Codes

FortCov provides standard exit codes for CI/CD integration and automation:

| Exit Code | Meaning | CI/CD Action | Example Scenario |
|-----------|---------|--------------|------------------|
| `0` | **Success** | ✅ Continue pipeline | Coverage analysis completed, help/version displayed |
| `1` | **Error** | ❌ Fail pipeline | Invalid configuration, file access errors, tool failures |
| `2` | **Threshold Not Met** | ⚠️ Configurable | Coverage below `--fail-under` threshold |
| `3` | **No Coverage Data** | ⚠️ Configurable | No .gcov files found (strict mode only) |

**CI/CD Exit Code Handling:**

```bash
# Basic threshold checking
fortcov --fail-under=80 --quiet --output=coverage.md
if [ $? -eq 2 ]; then
    echo "⚠️ Coverage below 80% threshold"
    exit 1  # Fail build
fi

# Advanced exit code handling
fortcov --fail-under=80 --strict --quiet --output=coverage.md
EXIT_CODE=$?
case $EXIT_CODE in
    0) echo "✅ Coverage analysis successful" ;;
    1) echo "❌ Tool error - check configuration"; exit 1 ;;
    2) echo "⚠️ Coverage below threshold"; exit 1 ;;  # Or exit 0 to continue
    3) echo "⚠️ No coverage data found"; exit 1 ;;    # Or exit 0 if optional
    *) echo "❌ Unexpected exit code: $EXIT_CODE"; exit 1 ;;
esac
```

**Exit Code Testing:**

```bash
# Test help flag (should return 0)
fortcov --help; echo "Exit code: $?"

# Test invalid configuration (should return 1)  
fortcov --output-format=invalid; echo "Exit code: $?"

# Test high threshold (should return 2 if coverage exists)
fortcov --fail-under=99.9 --quiet; echo "Exit code: $?"

# Test missing coverage in strict mode (should return 3)
fortcov --strict --source=/nonexistent --quiet; echo "Exit code: $?"
```

## Troubleshooting

### Quick Problem Resolution

| Error Message | Jump to Solution |
|---------------|------------------|
| "No coverage files found" | [→ Missing Coverage Files](#-no-coverage-files-found) |
| "Command not found: fortcov" | [→ Command Not Found](#-command-not-found-fortcov) |
| "Permission denied" | [→ Permission Issues](#-permission-denied) |
| "File too large" / processing large datasets | [→ Large Files](#-file-too-large-or-processing-very-large-datasets) |
| CI/CD pipeline failures | [→ CI/CD Issues](#cicd-troubleshooting) |
| Exit code problems | [→ Exit Code Issues](#-understanding-exit-codes) |

### Understanding Exit Codes

**Quick Exit Code Diagnosis:**

```bash
# Test tool status
fortcov --help; echo "Help exit code: $?"  # Should be 0

# Test with your project
fortcov --source=src --output=coverage.md; echo "Exit code: $?"

# Interpret common exit codes:
# 0 = Success - everything worked
# 1 = Error - check configuration, file permissions, or tool installation  
# 2 = Threshold not met - coverage below --fail-under value
# 3 = No coverage data - missing .gcov files (strict mode only)
```

**Exit Code Problem Resolution:**

| Exit Code | Problem | Solution |
|-----------|---------|----------|
| `1` | Configuration error | Check `--output-format`, file paths, permissions |
| `1` | Tool failure | Verify FortCov installation: `fpm build && fpm run fortcov -- --help` |
| `2` | Threshold not met | Lower `--fail-under` or add more tests |
| `3` | No coverage data | Generate .gcov files: [→ Missing Coverage Files](#-no-coverage-files-found) |

**CI/CD Exit Code Handling:**

```bash
# Option 1: Strict mode (fail on any issue)
fortcov --fail-under=80 --strict --quiet --output=coverage.md || exit 1

# Option 2: Flexible mode (handle specific cases)
fortcov --fail-under=80 --quiet --output=coverage.md
case $? in
    0) echo "Coverage OK" ;;
    1) echo "Tool error"; exit 1 ;;
    2) echo "Low coverage"; exit 1 ;;  # or exit 0 to continue
    3) echo "No data"; exit 0 ;;       # or exit 1 if coverage required
esac
```

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
fortcov --exclude='build/*,test/*' --output=coverage.md

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
    "$EXECUTABLE" --output=coverage.md
else
    fpm run fortcov -- --output=coverage.md
fi
```

#### ❌ "File too large" or processing very large datasets

**Cause**: Extremely large coverage datasets affecting processing performance

**Note**: Core memory allocation bugs have been fixed (Issue #178). This section covers handling very large datasets efficiently.

**Immediate Diagnosis**:
```bash
# Identify largest .gcov files
find . -name "*.gcov" -exec ls -lh {} \; | sort -k5 -hr | head -10

# Check total .gcov file size
du -sh $(find . -name "*.gcov")

# Check available system resources
free -h && df -h .
```

**Solution Strategy**:
```bash
# Strategy 1: Process in smaller batches (for massive projects)
fortcov --source=src/core --output=core-coverage.md
fortcov --source=src/utils --output=utils-coverage.md
fortcov --source=src/tests --output=tests-coverage.md

# Strategy 2: Clean up unnecessarily large files (if problematic)
# Review files larger than 10MB
find . -name "*.gcov" -size +10M -ls
# Remove only if truly problematic
find . -name "*.gcov" -size +50M -delete  # Only extreme cases

# Strategy 3: Use exclude patterns to focus analysis
fortcov --source=. --exclude='**/problematic_module.f90.gcov,build/*,test/*' --output=coverage.md

# Strategy 4: For CI/CD - reasonable timeouts for large projects
timeout 600 fortcov --source=src --output=coverage.md || echo "Coverage generation timed out"
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
        ./build/gfortran_*/app/fortcov --exclude='build/*,test/*' --output=coverage.md
    else
        fpm run fortcov -- --exclude='build/*,test/*' --output=coverage.md
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
    - fpm run fortcov -- --exclude='build/*,test/*' --output=coverage.md --verbose
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
fortcov --exclude='build/*,test/*' --output=coverage.md

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
fortcov --exclude='build/*,test/*' --output=coverage.md

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
    COMMAND fortcov --output=coverage.html --output-format=html
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
	fortcov --exclude='build/*' --output=coverage.md

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
  command: ['bash', '-c', 'gcov src/*.f90 && fortcov --output=coverage.html']
)
```

**Complete Examples**: See [examples/build_systems/](examples/build_systems/) for full working examples with all build systems.

## CI/CD Integration

### Single Platform Coverage

Basic coverage workflow for single-platform validation:

```yaml
# GitHub Actions - Basic Coverage
name: Coverage
on: [push, pull_request]
jobs:
  coverage:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v4
    - name: Setup Fortran
      uses: fortran-lang/setup-fortran@v1
    - name: Install FPM
      uses: fortran-lang/setup-fpm@v5
    - name: Build with coverage
      run: |
        fpm build --flag "-fprofile-arcs -ftest-coverage"
        fpm test --flag "-fprofile-arcs -ftest-coverage"
    - name: Generate coverage report
      run: |
        # Use bridge script for coverage generation
        chmod +x scripts/fpm_coverage_bridge.sh
        ./scripts/fpm_coverage_bridge.sh root coverage.md
    
    - name: Check coverage threshold
      run: |
        # Run coverage analysis with threshold checking
        fpm run fortcov -- --exclude='build/*,test/*' --output=coverage.md --fail-under=80 --quiet
        EXIT_CODE=$?
        
        case $EXIT_CODE in
            0) 
                echo "✅ Coverage analysis successful - threshold met"
                ;;
            1) 
                echo "❌ Coverage tool error - check configuration"
                exit 1
                ;;
            2) 
                echo "⚠️ Coverage below 80% threshold"
                echo "This build fails due to insufficient test coverage"
                exit 1
                ;;
            3) 
                echo "⚠️ No coverage data found"
                echo "Tests may not have generated coverage data"
                exit 1
                ;;
            *) 
                echo "❌ Unexpected exit code: $EXIT_CODE"
                exit 1
                ;;
        esac
    - name: Upload coverage
      uses: actions/upload-artifact@v4
      with:
        name: coverage-report
        path: coverage.md
```

### Multi-Platform Matrix Coverage

**Enterprise-grade matrix coverage** for comprehensive platform validation:

#### GitHub Actions Matrix

```yaml
# Multi-Platform Matrix Coverage
name: Matrix Coverage Analysis
on: [push, pull_request]
jobs:
  coverage-matrix:
    runs-on: ${{ matrix.os }}
    timeout-minutes: 45
    strategy:
      fail-fast: false
      matrix:
        os: [ubuntu-latest, ubuntu-22.04, macos-latest, windows-latest]
        compiler: [gfortran, ifort, nvfortran]
        exclude:
          # Intel Fortran not available on macOS/Windows
          - os: macos-latest
            compiler: ifort
          - os: windows-latest
            compiler: ifort
          # NVIDIA Fortran limited availability
          - os: ubuntu-22.04
            compiler: nvfortran
          - os: macos-latest
            compiler: nvfortran
          - os: windows-latest
            compiler: nvfortran
    
    steps:
    - uses: actions/checkout@v4
    - name: Setup compiler environment
      run: |
        # Set compiler-specific coverage flags
        if [ "${{ matrix.compiler }}" == "gfortran" ]; then
          echo "COVERAGE_FLAGS=-fprofile-arcs -ftest-coverage" >> $GITHUB_ENV
        elif [ "${{ matrix.compiler }}" == "ifort" ]; then
          echo "COVERAGE_FLAGS=-prof-gen=srcpos" >> $GITHUB_ENV
        elif [ "${{ matrix.compiler }}" == "nvfortran" ]; then
          echo "COVERAGE_FLAGS=-Mprof=ccff" >> $GITHUB_ENV
        fi
      shell: bash
      
    - name: Setup Fortran compiler
      uses: fortran-lang/setup-fortran@v1
      with:
        compiler: ${{ matrix.compiler }}
      continue-on-error: true
      
    - name: Build and test matrix combination
      run: |
        # Graceful handling of unavailable compilers
        if ! which ${{ matrix.compiler }} > /dev/null 2>&1; then
          echo "⚠️ Compiler ${{ matrix.compiler }} not available on ${{ matrix.os }}"
          echo "Creating placeholder for matrix completeness"
          exit 0
        fi
        
        # Build with matrix-specific flags
        fpm build --flag "$COVERAGE_FLAGS"
        fpm test --flag "$COVERAGE_FLAGS"
        
        # Generate coverage
        gcov src/*.f90 || true
        fpm run fortcov -- --output=coverage-${{ matrix.compiler }}-${{ matrix.os }}.html
        
    - name: Upload matrix artifacts
      uses: actions/upload-artifact@v4
      with:
        name: matrix-coverage-${{ matrix.compiler }}-${{ matrix.os }}-${{ github.run_id }}
        path: |
          coverage-${{ matrix.compiler }}-${{ matrix.os }}.html
          *.gcov
        retention-days: 30

  # Aggregate all matrix results
  matrix-aggregation:
    runs-on: ubuntu-latest
    needs: coverage-matrix
    if: always()
    steps:
    - name: Download all matrix artifacts
      uses: actions/download-artifact@v4
      with:
        pattern: matrix-coverage-*
        merge-multiple: true
    - name: Create aggregated report
      run: |
        echo "# Matrix Coverage Results" > matrix-summary.md
        echo "Coverage validated across multiple compilers and platforms:" >> matrix-summary.md
        ls -la coverage-*.html | wc -l >> matrix-summary.md
        echo "Individual reports available in artifacts" >> matrix-summary.md
```

#### GitLab CI Matrix

```yaml
# GitLab CI - Matrix Configuration
stages:
  - matrix-coverage
  - aggregation

.matrix-base:
  stage: matrix-coverage
  variables:
    MATRIX_BUILD: "true"
  script:
    - echo "Matrix: $MATRIX_COMPILER on $MATRIX_OS"
    - fpm build --flag "$COVERAGE_FLAGS"
    - fpm test --flag "$COVERAGE_FLAGS"
    - gcov src/*.f90 || true
    - fpm run fortcov -- --output=coverage-${MATRIX_COMPILER}-${MATRIX_OS}.html
  artifacts:
    paths:
      - coverage-${MATRIX_COMPILER}-${MATRIX_OS}.html
    expire_in: 30 days
  allow_failure: true

matrix-gfortran-ubuntu:
  extends: .matrix-base
  image: ubuntu:latest
  variables:
    MATRIX_COMPILER: "gfortran"
    MATRIX_OS: "ubuntu"
    COVERAGE_FLAGS: "-fprofile-arcs -ftest-coverage"

matrix-aggregation:
  stage: aggregation
  script:
    - echo "Aggregating matrix results..."
    - find . -name "coverage-*.html" | wc -l
```

#### Jenkins Matrix Pipeline

```groovy
// Jenkins - Matrix Pipeline  
pipeline {
    agent none
    stages {
        stage('Matrix Coverage') {
            matrix {
                axes {
                    axis {
                        name 'COMPILER'
                        values 'gfortran', 'ifort'
                    }
                    axis {
                        name 'OS_IMAGE'
                        values 'ubuntu:20.04', 'ubuntu:22.04'
                    }
                }
                stages {
                    stage('Matrix Build') {
                        agent {
                            docker "${OS_IMAGE}"
                        }
                        steps {
                            sh '''
                                echo "Matrix: ${COMPILER} on ${OS_IMAGE}"
                                fpm test --flag "-fprofile-arcs -ftest-coverage"
                                fpm run fortcov -- --output=coverage-${COMPILER}-${OS_IMAGE//:/}.html
                            '''
                        }
                    }
                }
            }
        }
    }
}
```

### Matrix Coverage Features

**Multi-Compiler Support:**
- **gfortran**: Full coverage support across all platforms
- **ifort**: Linux support with Intel OneAPI
- **nvfortran**: HPC environments with NVIDIA HPC SDK

**Multi-Platform Support:**
- **Ubuntu**: 20.04, 22.04, latest (comprehensive Linux coverage)
- **macOS**: 12, 13, latest (Apple Silicon and Intel)
- **Windows**: 2019, 2022, latest (MinGW and MSYS2)

**Advanced Features:**
- Automatic exclusion of incompatible compiler/OS combinations
- Graceful degradation for unavailable tools
- Performance monitoring and threshold validation
- Comprehensive artifact aggregation and reporting

### Complete CI/CD Examples

For comprehensive matrix configuration examples with exclusion rules, performance monitoring, and artifact aggregation:

- **[CI/CD Matrix Guide](CI_CD_MATRIX_GUIDE.md)** - Complete matrix implementation guide
- **[examples/build_systems/ci_cd/](examples/build_systems/ci_cd/)** - Working examples for all platforms

**Matrix Validation**: Run `examples/build_systems/test_matrix_coverage_implementation.sh` to validate all configurations.

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
fortcov --exclude='build/*,test/*' --output=coverage.md
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