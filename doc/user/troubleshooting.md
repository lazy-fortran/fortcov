# FortCov Troubleshooting Guide

Quick solutions to common FortCov issues.

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

### ❌ "No coverage files found"

**Problem:** Missing `.gcov` files

**Solution:**
```bash
# Complete coverage workflow
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

# Run FortCov
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

echo "=== Project Structure ==="
find . -name "*.f90" | head -10

echo "=== Coverage Files ==="
find . -name "*.gcov" -o -name "*.gcda" -o -name "*.gcno"

echo "=== Recent Build ==="
fpm test --flag "-fprofile-arcs -ftest-coverage" --verbose 2>&1 | tail -10
```

## Getting Help

1. **Run debug info script** above and save output
2. **Try minimal example** to isolate the issue
3. **Check prerequisites** are properly installed
4. **Search existing issues** on GitHub
5. **Report issues** with complete error messages

For advanced troubleshooting and edge cases, see the complete troubleshooting documentation and community resources.