# FortCov Troubleshooting Guide

Complete troubleshooting guide for FortCov issues, from installation to advanced usage problems.

## Quick Diagnosis

Run this diagnostic command first:

```bash
# Quick system check
fortcov --source=src --verbose --output=test.md 2>&1 | tee fortcov-debug.log
```

If FortCov isn't installed yet:

```bash
# Check prerequisites
which fpm && echo "✅ FPM found" || echo "❌ FPM missing"
which gfortran && echo "✅ gfortran found" || echo "❌ gfortran missing"  
which gcov && echo "✅ gcov found" || echo "❌ gcov missing"
```

## Installation Issues

### ❌ "Command not found: fpm"

**Problem**: Fortran Package Manager not installed

**Solution**:

```bash
# Linux (using binary)
curl -fsSL https://github.com/fortran-lang/fpm/releases/latest/download/fpm-linux.tar.gz | tar -xzf - -C /tmp
sudo mv /tmp/fpm /usr/local/bin/

# macOS (using Homebrew)
brew install fortran-lang/tap/fpm

# Windows (MSYS2)
pacman -S mingw-w64-x86_64-fortran-lang-fpm

# From source
git clone https://github.com/fortran-lang/fpm
cd fpm && fpm install --prefix=/usr/local
```

### ❌ "Command not found: gfortran"

**Problem**: Fortran compiler not installed

**Solution**:

```bash
# Ubuntu/Debian
sudo apt update && sudo apt install gfortran

# CentOS/RHEL/Fedora
sudo dnf install gcc-gfortran  # or: sudo yum install gcc-gfortran

# macOS
brew install gcc

# Windows (MSYS2)
pacman -S mingw-w64-x86_64-gcc-fortran
```

### ❌ "FortCov build fails"

**Problem**: FortCov compilation errors

**Diagnosis**:

```bash
# Clean build with verbose output
cd fortcov
fpm clean
fpm build --verbose 2>&1 | tee build.log
```

**Common solutions**:

```bash
# Update FPM to latest version
fpm --version  # Should be 0.8.0+

# Use specific compiler
fpm build --compiler gfortran-11

# Check for missing dependencies
ls -la src/  # Verify all source files present
```

### ❌ "Command not found: fortcov"

**Problem**: FortCov executable not in PATH

**Solutions**:

```bash
# Option 1: Use fpm run (recommended for testing)
fpm run -- --source=src --output=coverage.md

# Option 2: Run directly from build directory
./build/gfortran_*/app/fortcov --source=src --output=coverage.md

# Option 3: Add to PATH temporarily  
export PATH="$PATH:$(pwd)/build/gfortran_*/app"
fortcov --source=src --output=coverage.md

# Option 4: Install system-wide
sudo cp build/gfortran_*/app/fortcov /usr/local/bin/
fortcov --source=src --output=coverage.md

# Option 5: Create symlink
sudo ln -s $(pwd)/build/gfortran_*/app/fortcov /usr/local/bin/fortcov
```

## Coverage Generation Issues

### ❌ "No coverage files found"

**Problem**: Missing `.gcov` files

**Diagnosis**:

```bash
# Check for coverage files
find . -name "*.gcov" -ls
find . -name "*.gcda" -ls  
find . -name "*.gcno" -ls

# Verify build flags were used
grep -r "fprofile-arcs" build/ || echo "No coverage flags found in build"
```

**Solution**:

```bash
# Complete coverage workflow
cd your-project

# 1. Clean previous build
fpm clean
rm -f *.gcov *.gcda *.gcno

# 2. Build with coverage flags
fpm build --flag "-fprofile-arcs -ftest-coverage"

# 3. Run tests to generate .gcda files
fpm test --flag "-fprofile-arcs -ftest-coverage"

# 4. Generate .gcov files  
gcov src/*.f90

# 5. Verify files exist
ls -la *.gcov

# 6. Run FortCov
fortcov --source=src --output=coverage.md
```

### ❌ "gcov: command not found"

**Problem**: gcov tool not available

**Solution**:

```bash
# Install gcov (usually comes with gcc)
# Ubuntu/Debian
sudo apt install gcc

# Find gcov location
which gcov || find /usr -name "gcov*" 2>/dev/null

# Use specific gcov version
fortcov --source=src --gcov=/usr/bin/gcov-11 --output=coverage.md
```

### ❌ "gcov: source file not found"

**Problem**: gcov can't locate source files

**Diagnosis**:

```bash
# Check current directory structure
pwd
ls -la src/

# Check where gcov looks for sources
gcov --help | grep -A5 -B5 "object-directory"
```

**Solution**:

```bash
# Option 1: Run gcov from correct directory
cd src/
gcov *.f90
cd ..
fortcov --source=src --output=coverage.md

# Option 2: Use absolute paths  
gcov src/*.f90 --object-directory=build/
fortcov --source=src --output=coverage.md

# Option 3: Copy .gcda files to source directory
find build/ -name "*.gcda" -exec cp {} src/ \;
cd src/ && gcov *.f90 && cd ..
fortcov --source=src --output=coverage.md
```

### ❌ "Data validation failed"

**Problem**: Corrupted or invalid coverage data

**Diagnosis**:

```bash
# Check gcov file format
head -10 *.gcov
file *.gcov

# Look for obvious corruption
grep -n "^[^0-9#]" *.gcov | head -5
```

**Solution**:

```bash
# Clean regeneration
rm -f *.gcov *.gcda *.gcno
fpm clean

# Fresh build and test
fpm build --flag "-fprofile-arcs -ftest-coverage"
fpm test --flag "-fprofile-arcs -ftest-coverage"  
gcov src/*.f90

# Verify data integrity
fortcov --source=src --verbose --output=coverage.md
```

## Runtime Issues

### ❌ "Permission denied"

**Problem**: File access permissions

**Diagnosis**:

```bash
# Check source directory permissions
ls -la src/

# Check output directory permissions
touch coverage.md && rm coverage.md || echo "Cannot write to current directory"

# Check if running as wrong user
whoami
id
```

**Solution**:

```bash
# Fix source permissions
chmod -R 644 src/
chmod 755 src/  # Directory needs execute permission

# Fix output permissions
chmod 755 $(dirname coverage.md)  # Parent directory
chmod 644 coverage.md             # File (if exists)

# Run with appropriate user (avoid sudo unless necessary)
# If you must use sudo, fix ownership afterward:
sudo chown $USER:$USER coverage.md
```

### ❌ "File too large" or "Memory exhaustion"

**Problem**: Coverage files exceed limits

**Diagnosis**:

```bash
# Check file sizes
find . -name "*.gcov" -exec ls -lh {} \; | sort -k5 -hr | head -10

# Check available memory
free -h

# Check disk space
df -h .
```

**Solution**:

```bash
# Option 1: Process in smaller batches
fortcov --source=src/core --output=core-coverage.md
fortcov --source=src/utils --output=utils-coverage.md

# Option 2: Clean up large files
find . -name "*.gcov" -size +10M -ls  # Find large files
find . -name "*.gcov" -size +50M -delete  # Remove very large files

# Option 3: Regenerate with specific files
rm -f *.gcov
gcov src/specific_module.f90  # Only generate for specific files
fortcov --source=src --output=coverage.md

# Option 4: Increase system limits (if allowed)
ulimit -n 4096  # Increase file descriptor limit
ulimit -v unlimited  # Remove virtual memory limit (careful!)
```

### ❌ "Integer overflow" or "Division by zero"

**Problem**: Extreme values in coverage data

**Diagnosis**:

```bash
# Look for problematic data patterns
grep -n ":" *.gcov | grep -E "(:[0-9]{10,}:|:-[0-9]+:)"

# Check for zero-statement files
grep -l "^        0:" *.gcov
```

**Solution**:

```bash
# Identify problematic files
for file in *.gcov; do
    echo "Checking $file"
    awk -F: '{print $2}' "$file" | sort -n | tail -5
done

# Skip problematic files
fortcov --source=src --exclude="problematic_file.f90" --output=coverage.md

# Report to FortCov maintainers if data appears legitimate
echo "Problematic file found: save for bug report"
cp problematic_file.f90.gcov /tmp/fortcov-bug-report.gcov
```

## Configuration Issues

### ❌ "Configuration file not found"

**Problem**: Config file doesn't exist or wrong path

**Solution**:

```bash
# Check file exists
ls -la fortcov.nml

# Use absolute path
fortcov --config=/full/path/to/fortcov.nml

# Generate example config
cp fortcov.nml.example fortcov.nml
# Edit as needed
```

### ❌ "Invalid namelist format"

**Problem**: Syntax errors in configuration file

**Diagnosis**:

```bash
# Test namelist syntax with gfortran
gfortran -fsyntax-only -x f90 fortcov.nml
```

**Common fixes**:

```fortran
! Wrong: Comments inside namelist
&fortcov_config
    ! This comment breaks parsing
    output_path = 'coverage.md'
/

! Correct: Comments outside namelist
! Configuration for FortCov
&fortcov_config
    output_path = 'coverage.md'
/

! Wrong: Missing quotes
&fortcov_config
    output_path = coverage.md  ! Missing quotes
/

! Correct: Proper quotes
&fortcov_config
    output_path = 'coverage.md'
/

! Wrong: Missing closing slash
&fortcov_config
    output_path = 'coverage.md'
! Missing /

! Correct: Proper closing
&fortcov_config
    output_path = 'coverage.md'
/
```

## Output Issues

### ❌ "Empty coverage report"

**Problem**: Report generated but contains no data

**Diagnosis**:

```bash
# Check if FortCov found any files
fortcov --source=src --verbose --output=coverage.md 2>&1 | grep -i "processing\|found\|file"

# Check source directory contents
find src/ -name "*.f90" -ls
find . -name "*.gcov" -ls
```

**Solution**:

```bash
# Ensure coverage files exist and match source structure
ls -la src/     # Source files
ls -la *.gcov   # Coverage files

# Check that .gcov files correspond to source files
for src in src/*.f90; do
    base=$(basename "$src" .f90)
    if [ ! -f "${base}.f90.gcov" ]; then
        echo "Missing coverage for: $src"
    fi
done

# Regenerate coverage with verbose gcov
gcov --verbose src/*.f90
fortcov --source=src --verbose --output=coverage.md
```

### ❌ "Report shows 0% coverage for all files"

**Problem**: Coverage data exists but shows no coverage

**Diagnosis**:

```bash
# Check gcov file contents
head -20 *.gcov | grep -E "^[[:space:]]*[0-9]+:"

# Look for execution counts
grep -c "^[[:space:]]*[0-9][0-9]*:" *.gcov
```

**Solution**:

```bash
# Verify tests actually ran
fpm test --verbose --flag "-fprofile-arcs -ftest-coverage" 2>&1 | grep -E "(PASSED|FAILED|ERROR)"

# Check if .gcda files were generated (runtime data)
find . -name "*.gcda" -ls

# If no .gcda files, tests didn't run properly:
fpm clean
fpm build --flag "-fprofile-arcs -ftest-coverage"
fpm test --flag "-fprofile-arcs -ftest-coverage"
ls -la *.gcda  # Should exist now

# Regenerate coverage
gcov src/*.f90
fortcov --source=src --output=coverage.md
```

### ❌ "HTML/JSON output malformed"

**Problem**: Output format issues

**Solution**:

```bash
# Test different output formats
fortcov --source=src --output-format=markdown --output=test.md
fortcov --source=src --output-format=json --output=test.json
fortcov --source=src --output-format=html --output=test.html

# Validate JSON output
if command -v jq >/dev/null; then
    jq . test.json >/dev/null && echo "✅ Valid JSON" || echo "❌ Invalid JSON"
fi

# Check HTML syntax
if command -v tidy >/dev/null; then
    tidy -q -e test.html 2>&1 | head -10
fi
```

## Performance Issues

### ❌ "FortCov runs very slowly"

**Problem**: Performance issues with large projects

**Diagnosis**:

```bash
# Profile the run
time fortcov --source=src --verbose --output=coverage.md

# Check project size
find src/ -name "*.f90" | wc -l
find . -name "*.gcov" -exec wc -l {} + | tail -1
```

**Solutions**:

```bash
# Option 1: Process in parallel (if multiple source dirs)
fortcov --source=src/core --output=core.md &
fortcov --source=src/utils --output=utils.md &
wait

# Option 2: Exclude unnecessary files  
fortcov --source=src --exclude='external/*' --exclude='vendor/*' --output=coverage.md

# Option 3: Use faster output format
fortcov --source=src --output-format=json --quiet --output=coverage.json

# Option 4: Limit file size
find . -name "*.gcov" -size +1M -delete  # Remove very large coverage files
gcov src/*.f90  # Regenerate
fortcov --source=src --output=coverage.md
```

## CI/CD Integration Issues

### ❌ "CI pipeline fails with FortCov"

**Problem**: FortCov fails in automated environments

**Diagnosis**:

```bash
# Check CI environment
env | grep -E "(CI|BUILD|PATH)" | sort

# Test locally in similar environment  
docker run --rm -it ubuntu:22.04 bash
# Install dependencies and test...
```

**Solutions**:

#### GitHub Actions

```yaml
- name: Debug environment
  run: |
    echo "PWD: $(pwd)"
    echo "PATH: $PATH" 
    ls -la
    which fpm || echo "fpm not found"
    which gfortran || echo "gfortran not found"
    which gcov || echo "gcov not found"

- name: Install dependencies
  run: |
    sudo apt update
    sudo apt install -y gfortran
    curl -fsSL https://github.com/fortran-lang/fpm/releases/latest/download/fpm-linux.tar.gz | tar -xz -C /tmp
    sudo mv /tmp/fpm /usr/local/bin/

- name: Build FortCov
  run: |
    git clone https://github.com/krystophny/fortcov.git /tmp/fortcov
    cd /tmp/fortcov
    fpm build --profile release
    sudo cp build/gfortran_*/app/fortcov /usr/local/bin/

- name: Generate coverage
  run: |
    fpm build --flag "-fprofile-arcs -ftest-coverage"
    fpm test --flag "-fprofile-arcs -ftest-coverage"
    gcov src/*.f90
    fortcov --source=src --output=coverage.md --quiet --fail-under=80
```

#### Docker Issues

```dockerfile
# Dockerfile for reproducible environment
FROM ubuntu:22.04

RUN apt update && apt install -y \
    gfortran \
    curl \
    git

RUN curl -fsSL https://github.com/fortran-lang/fpm/releases/latest/download/fpm-linux.tar.gz | tar -xz -C /usr/local/bin

COPY . /app
WORKDIR /app

RUN git clone https://github.com/krystophny/fortcov.git /tmp/fortcov && \
    cd /tmp/fortcov && \
    fpm build --profile release && \
    cp build/gfortran_*/app/fortcov /usr/local/bin/

CMD ["bash"]
```

## Getting Help

### Collecting Debug Information

Create a debug report:

```bash
#!/bin/bash
# save as: debug-fortcov.sh

echo "=== FortCov Debug Report ===" > fortcov-debug.txt
echo "Date: $(date)" >> fortcov-debug.txt
echo "PWD: $(pwd)" >> fortcov-debug.txt
echo "" >> fortcov-debug.txt

echo "=== System Information ===" >> fortcov-debug.txt
uname -a >> fortcov-debug.txt
echo "gfortran: $(which gfortran) $(gfortran --version | head -1)" >> fortcov-debug.txt
echo "fpm: $(which fpm) $(fmp --version 2>/dev/null || echo 'version unknown')" >> fortcov-debug.txt
echo "gcov: $(which gcov) $(gcov --version | head -1)" >> fortcov-debug.txt
echo "" >> fortcov-debug.txt

echo "=== Project Structure ===" >> fortcov-debug.txt
find . -maxdepth 3 -type f -name "*.f90" >> fortcov-debug.txt
echo "" >> fortcov-debug.txt

echo "=== Coverage Files ===" >> fortcov-debug.txt
find . -name "*.gcov" -o -name "*.gcda" -o -name "*.gcno" | head -20 >> fortcov-debug.txt
echo "" >> fortcov-debug.txt

echo "=== FortCov Output ===" >> fortcov-debug.txt
fortcov --source=src --verbose --output=/tmp/test-coverage.md 2>&1 | head -50 >> fortcov-debug.txt

echo "Debug report saved to: fortcov-debug.txt"
```

### Support Resources

1. **Check existing issues**: [GitHub Issues](https://github.com/krystophny/fortcov/issues)
2. **Read documentation**: README.md, USER_GUIDE.md, CONFIGURATION.md  
3. **Try minimal example**: Use test project to isolate issue
4. **Submit bug report**: Include debug information and minimal reproduction

### Creating Minimal Reproduction

```bash
# Create test project for bug reports
mkdir fortcov-test && cd fortcov-test

cat > fpm.toml << 'EOF'
name = "test"
version = "0.1.0"
license = "MIT"
author = "Test"
maintainer = "test@example.com"

[build]
auto-executables = true

[dependencies]
[test]
EOF

mkdir -p src app test

cat > src/simple.f90 << 'EOF'
module simple
    implicit none
    public :: add_numbers
contains
    function add_numbers(a, b) result(c)
        integer, intent(in) :: a, b
        integer :: c
        c = a + b
    end function add_numbers
end module simple
EOF

cat > app/main.f90 << 'EOF'
program main
    use simple
    implicit none
    print *, add_numbers(5, 3)
end program main
EOF

cat > test/test_simple.f90 << 'EOF'
program test_simple
    use simple
    implicit none
    
    if (add_numbers(2, 3) /= 5) stop 1
    print *, "Test passed"
end program test_simple
EOF

# Test coverage workflow
fpm build --flag "-fprofile-arcs -ftest-coverage"
fpm test --flag "-fprofile-arcs -ftest-coverage"
gcov src/*.f90
fortcov --source=src --output=coverage.md --verbose
```

This creates a minimal project to reproduce issues and test solutions.