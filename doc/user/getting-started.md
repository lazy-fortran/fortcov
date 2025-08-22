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

**Zero-Configuration Mode (Recommended)**

The simplest way to get coverage reports:

```bash
# Build and test with coverage
fpm test --flag "-fprofile-arcs -ftest-coverage"

# Generate coverage data from FPM build directories
# This discovers nested build/gfortran_*/fortcov/ directories automatically
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
  gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
done

# Analyze with zero configuration
fortcov

# View the report (auto-generated in build/coverage/)
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

FortCov includes comprehensive security protections against command injection and path traversal attacks. These features work transparently during normal usage but will block potentially malicious inputs for security.

If you encounter "Path contains dangerous characters" or similar security messages, use standard project directory structures and avoid special shell characters in file paths.

## Next Steps

- **Read the [Usage Guide](usage-guide.md)** for advanced options
- **Check [Examples](examples.md)** for real-world scenarios
- **Set up [Configuration](configuration.md)** for your project
- **Explore [Troubleshooting](troubleshooting.md)** for common issues and security features