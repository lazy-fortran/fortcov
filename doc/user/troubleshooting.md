# FortCov Troubleshooting Guide

Quick solutions to common FortCov issues.

## CLI Flag Issues (Recently Fixed)

### ✅ Fixed in v0.4.0: CLI Flag Parsing

Previously broken CLI flags now work correctly:

**Working Commands:**
```bash
# Output formats (all working)
fortcov --format=json --output=coverage.json *.gcov
fortcov --format=xml --output=coverage.xml *.gcov  
fortcov --output=custom.md *.gcov

# Coverage thresholds (working)
fortcov --threshold=80 *.gcov
fortcov --threshold=95 --source=src *.gcov

# Source paths (working)
fortcov --source=src *.gcov

# Invalid flags now properly rejected
fortcov --invalid-flag *.gcov  # Returns error instead of silently ignoring
```

**Partially Working (parsed but implementation incomplete):**
```bash
# These flags are parsed correctly but behavior not fully implemented
fortcov --verbose *.gcov    # Verbose mode parsed but limited output
fortcov --quiet *.gcov      # Quiet mode parsed but suppression incomplete  
fortcov --exclude='test/*'  # Exclude patterns parsed but matching incomplete
```

### ❌ "Unknown flag: --flag-name"

**Solution:** Check flag syntax:
```bash
# ✅ Correct syntax
fortcov --format=json --output=test.json *.gcov

# ❌ Incorrect syntax  
fortcov --format json --output test.json *.gcov  # Missing = for values
```

### ❌ "Unsupported output format: 'xyz'"

**Solution:** Use supported formats:
```bash
# ✅ Supported formats
fortcov --format=markdown  # Default
fortcov --format=json      # JSON output
fortcov --format=xml       # Cobertura XML
fortcov --format=html      # HTML report

# ❌ Invalid format
fortcov --format=xyz       # Unknown format
```

## Quick Diagnosis

```bash
# Check prerequisites
which gfortran && echo "✅ gfortran found" || echo "❌ gfortran missing"
which fpm && echo "✅ fpm found" || echo "❌ fpm missing"  
which gcov && echo "✅ gcov found" || echo "❌ gcov missing"
which fortcov && echo "✅ fortcov found" || echo "❌ fortcov missing"
```

## Installation Issues

### ❌ "Command not found: fpm"

**Solution:**
```bash
# Download FPM binary
curl -fsSL https://github.com/fortran-lang/fpm/releases/latest/download/fpm-0.12.0-linux-x86_64-gcc-12 -o /tmp/fpm
chmod +x /tmp/fpm && sudo mv /tmp/fpm /usr/local/bin/
```

### ❌ "Command not found: gfortran"

**Solution:**
```bash
# Ubuntu/Debian
sudo apt install gcc-gfortran

# macOS
brew install gcc

# Windows (MSYS2)
pacman -S mingw-w64-x86_64-gcc-fortran
```

### ❌ "Command not found: fortcov"

**Solution:**
```bash
# Use fpm run instead
fpm run -- --source=src --output=coverage.md

# Or install system-wide
sudo cp build/gfortran_*/app/fortcov /usr/local/bin/

# Or add to PATH
export PATH="$PATH:$(pwd)/build/gfortran_*/app"
```

## Coverage Generation Issues

### ❌ "No coverage files found" (Zero-Configuration Mode)

**Problem:** Zero-configuration mode can't find coverage files

**Solution:**
```bash
# Complete zero-config workflow
fpm clean
rm -rf build/gcov *.gcov *.gcda *.gcno

# Build and run tests with coverage flags
fpm test --flag "-fprofile-arcs -ftest-coverage"

# Generate .gcov files in standard location
gcov -o build/gcov src/*.f90

# Verify files exist in expected location
ls -la build/gcov/*.gcov

# Run zero-config FortCov
fortcov
```

### ❌ "No coverage files found" (Traditional Mode)

**Problem:** Missing `.gcov` files for traditional usage

**Solution:**
```bash
# Complete traditional workflow
fpm clean
rm -f *.gcov *.gcda *.gcno

# Build with coverage flags
fpm build --flag "-fprofile-arcs -ftest-coverage"

# Run tests to generate .gcda files
fpm test --flag "-fprofile-arcs -ftest-coverage"

# Generate .gcov files
gcov src/*.f90

# Verify files exist
ls -la *.gcov

# Run FortCov with explicit paths
fortcov --source=src --output=coverage.md
```

### ❌ "gcov: command not found"

**Solution:**
```bash
# Install gcov (comes with gcc)
sudo apt install gcc  # Ubuntu/Debian
brew install gcc      # macOS

# Find gcov location
which gcov || find /usr -name "gcov*" 2>/dev/null
```

### ❌ "No runtime data found"

**Problem:** `.gcda` files missing (tests didn't run with coverage)

**Solution:**
```bash
# Ensure tests run with coverage flags
fpm test --flag "-fprofile-arcs -ftest-coverage" --verbose

# Check if .gcda files were created
find . -name "*.gcda" -ls
```

## Zero-Configuration Mode Issues

### ❌ "Auto-discovery found no files in expected locations"

**Problem:** Zero-config mode searched but found no coverage or source files

**Solution:**
```bash
# Check what zero-config is looking for
echo "=== Coverage file search locations ==="
ls -la build/gcov/*.gcov 2>/dev/null || echo "No files in build/gcov/"
ls -la *.gcov 2>/dev/null || echo "No files in current directory"
find build -name "*.gcov" 2>/dev/null || echo "No gcov files in build/"

echo "=== Source file search locations ==="
ls -la src/*.f90 2>/dev/null || echo "No files in src/"
ls -la *.f90 2>/dev/null || echo "No files in current directory"

# Fix: Generate files in expected location
gcov -o build/gcov src/*.f90  # Use build/gcov (preferred)
fortcov  # Should now work
```

### ❌ "Output directory doesn't exist"

**Problem:** Zero-config tries to write to `build/coverage/` but directory missing

**Solution:**
```bash
# Zero-config automatically creates directories, but if permissions issue:
mkdir -p build/coverage
chmod 755 build/coverage
fortcov
```

### ❌ "Zero-config finds wrong files"

**Problem:** Auto-discovery picks up unwanted files

**Solution:**
```bash
# Clean up unwanted files first
rm -f test/*.gcov build/test/*.gcov

# Or override with specific source
fortcov --source=src  # Still uses auto-discovery for coverage files
```

## Runtime Issues

### ❌ "Error: invalid source path"

**Solution:**
```bash
# Use correct path
fortcov --source=./src --output=coverage.md

# Or absolute path
fortcov --source=/full/path/to/src --output=coverage.md

# Check if path exists
ls -ld src/
```

### ❌ "Permission denied" errors

**Solution:**
```bash
# Fix file permissions
chmod 644 *.gcov
chmod 755 .

# Check file ownership
ls -la *.gcov
```

### ❌ "Empty coverage report"

**Solution:**
```bash
# Verify coverage files have content
head -5 *.gcov

# If empty, regenerate
rm *.gcov *.gcda *.gcno
fpm clean
fpm test --flag "-fprofile-arcs -ftest-coverage"
gcov src/*.f90
```

## Build System Issues

### ❌ FPM Integration Problems

**Solution:**
```bash
# Clean FPM cache
rm -rf ~/.local/share/fpm
fpm clean

# Use explicit flags
fpm build --profile debug --flag "-fprofile-arcs -ftest-coverage -O0"
```

### ❌ CMake Integration Problems

**Solution:**
```cmake
# In CMakeLists.txt
set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -fprofile-arcs -ftest-coverage")
```

```bash
# Build with coverage
cmake -DCMAKE_BUILD_TYPE=Debug ..
make && make test
```

## CI/CD Integration Issues

### ❌ GitHub Actions failures

**Solution:**
```yaml
# Add debugging to workflow
- name: Debug coverage setup
  run: |
    which gfortran && gfortran --version
    which fpm && fpm --version
    which gcov && gcov --version
    find . -name "*.gcov" -o -name "*.gcda" -o -name "*.gcno"
```

### ❌ Docker integration issues

**Solution:**
```dockerfile
# Ensure all tools are available
RUN apt-get update && apt-get install -y \
    build-essential gfortran git curl

# Install FPM
RUN curl -fsSL https://github.com/fortran-lang/fpm/releases/latest/download/fpm-0.12.0-linux-x86_64-gcc-12 -o /usr/local/bin/fpm && \
    chmod +x /usr/local/bin/fpm
```

## Debug Information Collection

```bash
#!/bin/bash
# debug-info.sh - Collect debug information

echo "=== System Info ==="
uname -a

echo "=== Tool Versions ==="
gfortran --version | head -1
fpm --version
gcov --version | head -1
fortcov --version 2>/dev/null || echo "fortcov not in PATH"

echo "=== Project Structure ==="
find . -name "*.f90" | head -10

echo "=== Zero-Configuration Diagnostics ==="
echo "Coverage files in build/gcov/:"
ls -la build/gcov/*.gcov 2>/dev/null || echo "None found"
echo "Coverage files in current directory:"
ls -la *.gcov 2>/dev/null || echo "None found"
echo "Source files in src/:"
ls -la src/*.f90 2>/dev/null || echo "None found"
echo "Source files in current directory:"
ls -la *.f90 2>/dev/null || echo "None found"

echo "=== Coverage Files ==="
find . -name "*.gcov" -o -name "*.gcda" -o -name "*.gcno"

echo "=== Recent Build ==="
fpm test --flag "-fprofile-arcs -ftest-coverage" --verbose 2>&1 | tail -10

echo "=== Zero-Config Test ==="
fortcov --help 2>/dev/null | head -5 || echo "fortcov command failed"
```

## Getting Help

1. **Run debug info script** above and save output
2. **Try minimal example** to isolate the issue
3. **Check prerequisites** are properly installed
4. **Search existing issues** on GitHub
5. **Report issues** with complete error messages

For advanced troubleshooting and edge cases, see the complete troubleshooting documentation and community resources.