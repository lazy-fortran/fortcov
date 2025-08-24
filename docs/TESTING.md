# FortCov Test Infinite Loop Prevention Guide

This document explains how to write tests that avoid infinite loops and fork bombs in the FortCov test suite.

## Quick Reference

### ✅ DO
- Test module functionality directly
- Use `.FORK_BOMB_DISABLED` for tests that would cause recursion
- Execute binaries directly: `./build/gfortran_*/app/fortcov`
- Use isolated test environments
- Set proper exit codes (0 = success, non-zero = failure)

### ❌ DON'T  
- Call `fpm test` from within tests
- Create recursive test execution chains
- Depend on full system integration for unit tests
- Leave test artifacts that interfere with other tests

## Problem Background

**Issue #305** documents the infinite loop problem that occurred when tests recursively called `fpm test`:

```bash
# This causes infinite recursion:
fpm test → runs test_script.sh → calls "fpm test" → runs test_script.sh → ...
```

**Root cause**: Tests calling `fpm test` within themselves created "fork bombs" where the test suite would recursively spawn itself infinitely.

**Impact**: Test suite hanging for 3+ minutes, CI/CD pipeline failures, blocked development workflow.

## The .FORK_BOMB_DISABLED Solution

Tests that would cause recursive execution are renamed with `.FORK_BOMB_DISABLED` extension:

```
test/test_documentation_commands_issue_232.sh.FORK_BOMB_DISABLED
test/test_readme_gcov_workflow.sh.FORK_BOMB_DISABLED  
test/test_issue_260_comprehensive.sh.FORK_BOMB_DISABLED
test/test_readme_executable_validation.sh.FORK_BOMB_DISABLED
```

### How It Works

1. **FPM ignores** files without `.f90` extension for auto-tests
2. **Tests are preserved** but not automatically executed  
3. **Documentation remains** for reference and manual testing
4. **Fork bombs are prevented** by breaking the recursive execution chain

## Safe Test Patterns

### Pattern 1: Direct Module Testing

```fortran
program test_coverage_engine
    !! Safe: Tests module functionality directly
    use coverage_engine
    use coverage_model
    implicit none
    
    integer :: total_tests = 0
    integer :: passed_tests = 0
    
    ! Test specific functionality
    call test_coverage_calculation()
    call test_file_parsing() 
    call test_error_handling()
    
    ! Report results
    print *, "Tests passed:", passed_tests, "/", total_tests
    if (passed_tests /= total_tests) stop 1
    
contains
    subroutine test_coverage_calculation()
        type(coverage_data_t) :: coverage
        real :: result
        
        total_tests = total_tests + 1
        
        ! Test the actual function, not the command line
        call calculate_coverage(coverage, result)
        
        if (abs(result - 85.0) < 0.01) then
            passed_tests = passed_tests + 1
            print *, "✅ Coverage calculation test passed"  
        else
            print *, "❌ Coverage calculation test failed"
        end if
    end subroutine
end program
```

### Pattern 2: Direct Binary Execution

```bash
#!/bin/bash
# Safe: Tests binary directly without recursive fpm calls
set -e

echo "Testing FortCov binary execution..."

# Check if binary exists
BINARY="./build/gfortran_*/app/fortcov"
if ! ls $BINARY >/dev/null 2>&1; then
    echo "❌ FortCov binary not found"
    exit 1
fi

# Test help command
echo "Testing --help flag..."
if $BINARY --help >/dev/null 2>&1; then
    echo "✅ Help command works"
else
    echo "❌ Help command failed"
    exit 1
fi

# Test version command  
echo "Testing --version flag..."
if $BINARY --version >/dev/null 2>&1; then
    echo "✅ Version command works"
else
    echo "❌ Version command failed" 
    exit 1
fi

echo "✅ Binary execution tests passed"
```

### Pattern 3: Environment Isolation

```bash
#!/bin/bash
# Safe: Uses environment isolation to prevent recursion
if [ -n "$FORTCOV_TEST_RUNNING" ]; then
    echo "Skipping test - already in test environment"
    exit 0
fi

export FORTCOV_TEST_RUNNING=1
echo "Running isolated integration test..."

# Create temporary test environment
TEST_DIR="temp_test_$$"
mkdir -p "$TEST_DIR"
cd "$TEST_DIR"

# Setup test project
cat > fpm.toml << 'EOF'  
name = "test_project"
version = "0.1.0"
EOF

# Test workflow without recursive calls
fpm build --flag "-fprofile-arcs -ftest-coverage"
find build -name "*.gcda" -exec gcov {} \; 2>/dev/null

# Use parent directory binary (not recursive fpm)
../build/gfortran_*/app/fortcov --source=src --output=coverage.md

# Verify results
if [ -f coverage.md ]; then
    echo "✅ Integration test passed"
    exit_code=0
else
    echo "❌ Integration test failed"  
    exit_code=1
fi

# Cleanup
cd ..
rm -rf "$TEST_DIR"
unset FORTCOV_TEST_RUNNING

exit $exit_code
```

## Dangerous Patterns to Avoid

### ❌ Pattern 1: Recursive FPM Calls
```bash
#!/bin/bash
# DANGEROUS: This causes infinite recursion
echo "Running comprehensive tests..."
fpm test  # This will call this script again infinitely!
echo "Tests complete"
```

### ❌ Pattern 2: Nested Test Execution
```fortran
program test_integration
    ! DANGEROUS: Testing by running the test suite
    call system("fpm test test_coverage_engine")  ! Recursive call
    call system("fpm test test_file_utils")       ! Another recursive call
end program
```

### ❌ Pattern 3: Command Line Testing Without Isolation  
```bash
#!/bin/bash
# DANGEROUS: No protection against recursion
echo "Testing command line..."
fortcov --help  # This might trigger fpm test via some dependency
fpm test         # Direct recursive call
```

## Converting Problematic Tests

When you find a test causing infinite loops:

### Step 1: Disable the Test
```bash
# Rename to prevent execution
mv test/problematic_test.sh test/problematic_test.sh.FORK_BOMB_DISABLED
```

### Step 2: Add Documentation Comment
```bash
#!/bin/bash
# test/problematic_test.sh.FORK_BOMB_DISABLED
# 
# DISABLED: This test causes infinite recursion by calling "fpm test"
# TODO: Rewrite to test functionality directly without recursive calls
#
# Original problem: Line 45 calls "fpm test" which re-runs this script
# Fix needed: Replace fpm test with direct binary execution
```

### Step 3: Fix the Test Logic
```bash
#!/bin/bash
# test/safe_test.sh (fixed version)
echo "Testing documentation examples..."

# OLD (caused recursion): fpm test
# NEW (safe): Direct binary testing
BINARY="./build/gfortran_*/app/fortcov"

if [ -f "$BINARY" ]; then
    echo "✅ Binary exists"
    
    # Test specific functionality directly
    if $BINARY --help >/dev/null 2>&1; then
        echo "✅ Help command works"
    else
        echo "❌ Help command failed"
        exit 1
    fi
else
    echo "❌ Binary not found, run 'fpm build' first"
    exit 1
fi

echo "✅ Documentation tests passed"
```

## FPM Configuration for Safety

The `fpm.toml` includes fork bomb prevention documentation:

```toml
# FORK BOMB PREVENTION: Tests that call "fpm test" are DISABLED  
# to prevent infinite recursion. See .FORK_BOMB_DISABLED files.
[build]
auto-tests = true  # Only .f90 files are executed as tests
```

## Recovery from Infinite Loops

If you encounter an infinite loop:

1. **Stop immediately**: `Ctrl+C` or kill the process
2. **Identify the cause**: Look for recent test changes  
3. **Disable the test**: Add `.FORK_BOMB_DISABLED` extension
4. **Fix the logic**: Remove recursive calls
5. **Test the fix**: Run the individual test before re-enabling
6. **Re-enable**: Remove the `.FORK_BOMB_DISABLED` extension

## Testing Checklist

Before adding new tests, verify:

- [ ] Test does not call `fpm test` 
- [ ] Test does not call `fpm run fortcov` in a way that triggers test execution
- [ ] Shell scripts test binaries directly
- [ ] Fortran tests focus on module functionality  
- [ ] Integration tests use isolated environments
- [ ] Test cleanup removes temporary files
- [ ] Exit codes are set properly (0 = success)

## Examples from FortCov Codebase

### Working Example: test_cli_flag_parsing_issue_231.f90
```fortran
program test_cli_flag_parsing_issue_231
    !! Tests CLI flag parsing directly without external calls
    use fortcov_config
    implicit none
    
    ! Tests specific module functions
    call test_flag_parsing()
    call test_config_validation()
    
contains
    subroutine test_flag_parsing()
        ! Direct module testing - safe
        character(len=256) :: args(10)
        type(config_t) :: config
        
        args(1) = "--output=test.json"
        call parse_arguments(args, config)
        
        if (trim(config%output_file) == "test.json") then
            print *, "✅ Flag parsing test passed"
        else
            print *, "❌ Flag parsing test failed"
            stop 1
        end if
    end subroutine
end program
```

### Disabled Example: test_documentation_commands_issue_232.sh.FORK_BOMB_DISABLED
This file contains comprehensive testing logic but is disabled because it originally called `fmp test` recursively. The test logic is preserved for reference and could be converted to use direct binary execution.

## Integration with CI/CD

The infinite loop prevention ensures:

- **Fast test execution**: Test suite completes in ~1.5 seconds instead of hanging
- **Reliable CI/CD**: GitHub Actions no longer timeout or hang  
- **Predictable results**: Tests run consistently across environments
- **Maintainable codebase**: Clear patterns for writing safe tests

## Future Considerations

When adding new test capabilities:

1. **Maintain the .FORK_BOMB_DISABLED convention**
2. **Document any new anti-patterns discovered** 
3. **Update this guide with additional safe patterns**
4. **Consider test isolation strategies for complex integration scenarios**

This prevention system ensures FortCov maintains a fast, reliable test suite while preserving the ability to test complex integration scenarios safely.