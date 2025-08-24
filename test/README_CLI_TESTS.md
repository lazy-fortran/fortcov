# CLI Flag Testing Documentation

## Issue #231: CLI Flag Parsing Completely Broken

This directory contains comprehensive failing tests for Issue #231, demonstrating that all CLI flags are silently ignored by fortcov.

## Root Cause

The bug is in `config_parser.f90:141`:
```fortran
is_zero_config = .not. has_input_related_arguments(args)
```

Zero-configuration mode incorrectly triggers for output-related flags, overriding correctly parsed CLI values.

## Test Files

### `test_cli_flag_parsing_issue_231.f90`
Comprehensive end-to-end testing of all major CLI flags:
- `--output` flag functionality
- `--format` flag functionality  
- `--verbose` flag functionality
- `--quiet` flag functionality
- `--threshold` flag functionality
- `--exclude` flag functionality
- `--source` flag functionality
- Invalid flag error handling (security issue)

Expected: ALL tests fail (demonstrating broken functionality)

### `test_zero_config_override_bug.f90`
Root cause demonstration tests showing exactly how parsed values get overridden:
- Shows parsed values are correct initially
- Demonstrates zero-config defaults override parsed values
- Confirms the specific bug location

### `run_cli_flag_tests.sh`
Test execution script with manual verification commands.

## Running Tests

```bash
# Run comprehensive CLI flag tests
fpm test test_cli_flag_parsing_issue_231

# Run root cause demonstration
fpm test test_zero_config_override_bug

# Run with script
./test/run_cli_flag_tests.sh
```

## Expected Results (RED Phase)

All tests should FAIL, demonstrating:
1. CLI flags are parsed correctly
2. CLI flag values are overridden by zero-configuration defaults
3. Security issue: invalid flags silently accepted

## Manual Verification

Test these commands to see the bug:

```bash
# Should create test.json but creates default markdown file
fortcov --output=test.json --format=json

# Should show verbose output but shows normal output
fortcov --verbose

# Should fail threshold but passes
fortcov --threshold=99 --strict

# Should show error but silently ignores
fortcov --invalid-flag
```

## GREEN Phase Fix Requirements

The fix should:
1. Only trigger zero-config mode when NO arguments provided
2. Preserve all parsed CLI flag values
3. Generate errors for invalid flags
4. Keep zero-configuration functionality working for no-argument case

## Test Infinite Loop Prevention

⚠️ **IMPORTANT**: Tests in this directory follow the `.FORK_BOMB_DISABLED` convention to prevent infinite recursion.

**Problem**: Tests that call `fpm test` create infinite loops (fork bombs) causing the test suite to hang.

**Solution**: Shell scripts that would cause recursion are renamed with `.FORK_BOMB_DISABLED` extension.

**Safe Testing Patterns**:
- ✅ Test Fortran modules directly in `.f90` files
- ✅ Test binary execution with direct paths: `./build/gfortran_*/app/fortcov`  
- ❌ Never call `fpm test` from within tests

For comprehensive testing guidelines, see [`../docs/TESTING.md`](../docs/TESTING.md).