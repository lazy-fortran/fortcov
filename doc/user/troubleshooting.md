# FortCov Troubleshooting Guide

Quick solutions to common FortCov issues.

## CLI Flag Issues (Issue #228 Fixes)

### ✅ MAJOR FIX: CLI Argument Parsing Restored

**Issue #228 Status**: Significant improvements implemented, 5/16 test cases now pass.

**✅ FULLY WORKING:**
```bash
# Output formats (verified working)
fortcov --format=json --output=coverage.json *.gcov
fortcov --format=xml --output=coverage.xml *.gcov
fortcov --output=custom.md *.gcov

# Coverage thresholds (working with validation)
fortcov --threshold=80 *.gcov     # Exits with code 1 if not met
fortcov --threshold=150 *.gcov    # Error: Invalid threshold (>100%)
fortcov --threshold=-50 *.gcov    # Error: Negative threshold rejected

# Interactive modes (working)
fortcov --tui                     # Launches interactive interface
fortcov --diff --diff-baseline=baseline.json  # Diff analysis mode

# Security improvements (working)
fortcov --invalid-flag *.gcov     # Error: Unknown flag (no longer silently ignored)
```

**🔄 PARTIALLY WORKING:**
```bash
# These work but with limited functionality
fortcov --verbose *.gcov    # Some enhanced output, not complete
fortcov --source=src *.gcov # Parsed but discovery logic incomplete  
fortcov --exclude='test/*'  # Pattern parsed but not fully applied
```

**❌ NOT YET IMPLEMENTED:**
```bash
# These flags are recognized but not functional
fortcov --quiet *.gcov           # Quiet mode not implemented
fortcov --gcov-executable=gcov   # Custom gcov path not implemented
fortcov --threads=4 *.gcov       # Parallel processing not implemented
fortcov --config=fortcov.nml     # Configuration files not implemented
```

### ❌ "Unknown flag: --flag-name"

**Status**: ✅ **FIXED** - Invalid flags now properly rejected (Issue #228)

**Solution:** Check flag syntax and implementation status:
```bash
# ✅ Correct syntax (working flags)
fortcov --format=json --output=test.json *.gcov
fortcov --threshold=80 *.gcov
fortcov --tui

# ❌ Incorrect syntax
fortcov --format json --output test.json *.gcov  # Missing = for values

# ❌ Not yet implemented (recognized but not functional)
fortcov --quiet *.gcov           # Flag recognized, feature pending
fortcov --threads=4 *.gcov       # Flag recognized, feature pending
```

### ❌ "Unsupported output format: 'xyz'"

**Status**: ✅ **FIXED** - Format validation working correctly

**Solution:** Use supported formats:
```bash
# ✅ Verified working formats
fortcov --format=json      # JSON output (verified)
fortcov --format=xml       # Cobertura XML (verified)
fortcov --format=markdown  # Default markdown (verified)

# ❌ Invalid format (properly rejected)
fortcov --format=xyz       # Error: Unsupported format
fortcov --format=html      # Check if implemented
```

## Security Enhancements (Issue #235 - FIXED) ✅

**Security Improvements Applied**: FortCov has been hardened against command injection vulnerabilities.

### ✅ Enhanced Security Features

**Command Execution Security**:
- All shell commands now use proper argument escaping
- Directory creation uses `safe_mkdir()` with input validation
- File operations use `escape_shell_argument()` protection
- Executable validation uses secure temporary files instead of shell redirection

**Path Security Validation**:
- Shell metacharacter detection (`;`, `&`, `|`, `` ` ``, `$`, `>`, `<`, `"`, `'`)
- Directory traversal protection (`../`, `/..`, URL-encoded variants)
- System file access prevention (`/etc/`, `/proc/`, `/sys/`, `/dev/`)
- Windows device name blocking (CON, PRN, NUL, COM, LPT, AUX)
- NULL byte injection detection

**User Impact**: These security improvements are transparent to normal usage but provide robust protection against malicious inputs.

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

# Generate .gcov files from FPM build directories
# This command pattern:
# 1. Finds all .gcda files in nested build directories
# 2. Extracts unique directory paths containing coverage data
# 3. Runs gcov with correct object-directory for each compiler path
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
  gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
done

# Verify .gcov files were created in current directory
ls -la *.gcov

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

## Enhanced Zero-Configuration Mode Issues (Issue #227 Fixed)

### ✅ "Enhanced zero-config mode with automatic gcov generation"

**Status:** **SIGNIFICANTLY IMPROVED** - Issue #227 fixes now provide comprehensive auto-discovery and generation

**Current capabilities:**
```bash
# Enhanced workflow - no manual gcov generation needed
fpm test --flag "-fprofile-arcs -ftest-coverage" 
fortcov  # Handles everything automatically:
         # 1. Discovers existing .gcov files (fast path)
         # 2. Auto-generates from .gcda/.gcno files when needed
         # 3. Handles FPM/CMake/Generic build structures
         # 4. Creates build/coverage/coverage.md automatically
```

### ❌ "Enhanced auto-discovery still finds no coverage data"

**Problem:** Even enhanced zero-config mode found no coverage files or couldn't generate them

**Enhanced Diagnostics:**
```bash
# Check enhanced zero-config discovery phases
echo "=== Phase 1: Existing .gcov files ==="
ls -la build/gcov/*.gcov 2>/dev/null || echo "No files in build/gcov/ (preferred)"
ls -la *.gcov 2>/dev/null || echo "No files in current directory"
find build -name "*.gcov" 2>/dev/null || echo "No gcov files in build/ (recursive)"

echo "=== Phase 2: .gcda/.gcno files for auto-generation ==="
echo "FPM build structure:"
find build -name "gfortran_*/app/*.gcda" -o -name "gfortran_*/test/*.gcda" 2>/dev/null
echo "CMake build structure:"  
find build -name "*.gcda" -o -name "_build" -name "*.gcda" 2>/dev/null
echo "Generic build structure:"
find . -name "*build*/*.gcda" -o -name "obj/*.gcda" -o -name "objects/*.gcda" 2>/dev/null

echo "=== Phase 3: gcov executable availability ==="
which gcov && gcov --version || echo "gcov not found in PATH"

# Enhanced fix with Issue #227 improvements
fpm test --flag "-fprofile-arcs -ftest-coverage"  # Ensure .gcda files exist
fortcov  # Should now auto-generate and process
```

### ✅ "Output directory creation now automatic"

**Status:** **FIXED in Issue #227** - Enhanced zero-config automatically creates output directories

**Current behavior:**
```bash
# Enhanced zero-config now handles directory creation automatically
fortcov  # Creates build/coverage/ directory if needed, with proper permissions

# If still encountering issues, check permissions on parent directory:
ls -ld build/  # Should be writable by user
chmod 755 build/  # Fix if needed
fortcov  # Should now work
```

### ✅ "Intelligent file filtering now prevents wrong files"

**Status:** **IMPROVED in Issue #227** - Enhanced argument classification and filtering

**Current behavior:**
```bash
# Enhanced zero-config now includes intelligent filtering:
# - Executable paths no longer treated as coverage files 
# - Build directories automatically excluded from source discovery
# - Test directories excluded by default
fortcov  # Smart filtering applied automatically

# Manual override still available if needed:
fortcov --source=src  # Explicit source filter with auto-coverage discovery
rm -f unwanted.gcov   # Remove specific unwanted files if necessary
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
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
  gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
done
```

## Security-Related Issues

### ❌ "Path contains dangerous characters"

**Problem**: Enhanced security validation detects potentially malicious path inputs

**Solution:**
```bash
# Use clean, simple paths without special characters
fortcov --source=src --output=coverage.md

# Avoid paths with shell metacharacters
# ❌ Bad: fortcov --source="src;rm -rf /" 
# ❌ Bad: fortcov --output="report.md|malicious"
# ✅ Good: fortcov --source=src --output=report.md
```

### ❌ "Suspicious system path access"

**Problem**: Security validation blocks access to system directories

**Solution:**
```bash
# Avoid system directories (this is intentional security protection)
# ❌ Blocked: fortcov --source=/etc/
# ❌ Blocked: fortcov --source=/proc/
# ❌ Blocked: fortcov --source=/sys/

# Use project directories instead
# ✅ Allowed: fortcov --source=src/
# ✅ Allowed: fortcov --source=./lib/
```

### ❌ Windows device name errors

**Problem**: Security validation blocks Windows device names that could be exploited

**Solution:**
```bash
# Avoid reserved device names on all platforms
# ❌ Blocked: fortcov --output=CON
# ❌ Blocked: fortcov --output=PRN.md
# ❌ Blocked: fortcov --output=NUL.txt

# Use normal filenames
# ✅ Allowed: fortcov --output=coverage.md
# ✅ Allowed: fortcov --output=report.html
```

**Note**: These security features protect against command injection and path traversal attacks. They may occasionally block legitimate but unusual path patterns. Use standard project directory structures for best compatibility.

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

echo "=== Enhanced Zero-Configuration Diagnostics (Issue #227) ==="
echo "Phase 1 - Existing .gcov files:"
ls -la build/gcov/*.gcov 2>/dev/null || echo "None in build/gcov/"
ls -la *.gcov 2>/dev/null || echo "None in current directory" 
find build -name "*.gcov" 2>/dev/null || echo "None in build/ (recursive)"

echo "Phase 2 - .gcda/.gcno files for auto-generation:"
echo "FPM structure:"
find build -path "*/gfortran_*/app/*.gcda" -o -path "*/gfortran_*/test/*.gcda" 2>/dev/null || echo "None"
echo "CMake structure:"
find build -name "*.gcda" -o -path "_build/*.gcda" 2>/dev/null || echo "None"
echo "Generic structure:"
find . -path "*build*/*.gcda" -o -path "obj/*.gcda" -o -path "objects/*.gcda" 2>/dev/null || echo "None"

echo "Phase 3 - gcov availability:"
which gcov && gcov --version | head -1 || echo "gcov not available"

echo "Source file discovery:"
ls -la src/*.f90 2>/dev/null || echo "None in src/"
ls -la *.f90 2>/dev/null || echo "None in current directory"

echo "=== Coverage Files ==="
find . -name "*.gcov" -o -name "*.gcda" -o -name "*.gcno"

echo "=== Recent Build ==="
fpm test --flag "-fprofile-arcs -ftest-coverage" --verbose 2>&1 | tail -10

echo "=== Enhanced Zero-Config Test (Issue #227) ==="
echo "Testing enhanced zero-config capabilities:"
fortcov --help 2>/dev/null | head -5 || echo "fortcov command failed"
echo "Testing enhanced auto-discovery (dry run):"
fortcov --verbose 2>/dev/null | head -10 || echo "Enhanced zero-config test failed"
```

## Getting Help

1. **Run debug info script** above and save output
2. **Try minimal example** to isolate the issue
3. **Check prerequisites** are properly installed
4. **Search existing issues** on GitHub
5. **Report issues** with complete error messages

For advanced troubleshooting and edge cases, see the complete troubleshooting documentation and community resources.