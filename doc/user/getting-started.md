# Getting Started with FortCov

Quick start guide to get your first coverage report working in under 5 minutes.

## Prerequisites

Ensure you have the required tools installed:

```bash
# Check prerequisites
which gfortran && echo "✅ gfortran found" || echo "❌ gfortran missing"
which fpm && echo "✅ fpm found" || echo "❌ fpm missing"
which gcov && echo "✅ gcov found" || echo "❌ gcov missing"
which fortcov && echo "✅ fortcov found" || echo "❌ fortcov missing"
```

If any are missing, see [Installation Guide](installation.md).

## Your First Coverage Report

### Step 1: Create a Simple Project

```bash
# Create new project
mkdir my-calculator && cd my-calculator

# Initialize FPM project
cat > fpm.toml << 'EOF'
name = "calculator"
version = "0.1.0"
EOF

mkdir -p src test
```

### Step 2: Write Source Code

Create `src/calculator.f90`:

```fortran
module calculator
    implicit none
    private
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

Create `test/test_calculator.f90`:

```fortran
program test_calculator
    use calculator
    implicit none
    
    ! Test addition
    if (abs(add(2.0, 3.0) - 5.0) > 1e-6) then
        print *, 'FAIL: add test'
        stop 1
    end if
    
    print *, 'All tests passed!'
end program test_calculator
```

### Step 3: Generate Coverage

**Enhanced Zero-Configuration Mode (Recommended)**

The simplest way to get coverage reports with Issue #227 fixes:

```bash
# Build and test with coverage
fpm test --flag "-fprofile-arcs -ftest-coverage"

# NEW: Zero-configuration mode with automatic gcov generation
fortcov

# That's it! The enhanced zero-config mode now:
# 1. Auto-discovers coverage files in priority locations:
#    - build/gcov/*.gcov (preferred location)
#    - *.gcov (current directory)
#    - build/**/*.gcov (recursive search)
# 2. Auto-generates .gcov files from .gcda/.gcno when needed:
#    - FPM: build/gfortran_*/app/ and build/gfortran_*/test/
#    - CMake: build/ and _build/ directories  
#    - Generic: *build*/, obj/, objects/ directories
# 3. Intelligent argument filtering (no longer treats executable paths as coverage files)
# 4. Smart output directory creation (build/coverage/coverage.md)

# View the auto-generated report
cat build/coverage/coverage.md
```

**Traditional Mode (Custom Workflows)**

For specific configurations or legacy workflows:

```bash
# Build and test with coverage
fpm test --flag "-fprofile-arcs -ftest-coverage"

# Generate coverage data
gcov src/*.f90

# Create coverage report with explicit options
fortcov --source=src --output=coverage.md

# View the report
cat coverage.md
```

**Success!** You've generated your first coverage report.

### Security Notice

FortCov includes comprehensive security protections against command injection and path traversal attacks. The Issue #227 fixes enhance these protections with:

- **Secure gcov execution**: All gcov commands use validated arguments and secure temporary files
- **Path sanitization**: Enhanced validation for coverage file paths and output directories  
- **Argument filtering**: Intelligent classification prevents executable paths from being treated as coverage files
- **Command injection protection**: Shell metacharacter detection and safe command execution

These features work transparently during normal usage but will block potentially malicious inputs for security.

If you encounter "Path contains dangerous characters" or similar security messages, use standard project directory structures and avoid special shell characters in file paths.

## Next Steps

- **Read the [Usage Guide](usage-guide.md)** for advanced options
- **Check [Examples](examples.md)** for real-world scenarios
- **Set up [Configuration](configuration.md)** for your project
- **Explore [Troubleshooting](troubleshooting.md)** for common issues and security features