# Exit Code Classification System

## Overview

The FortCov exit code system provides standardized Unix-compatible exit codes for CI/CD integration and automated tooling. This replaces the previous STOP statement approach with proper exit() calls.

## Exit Code Categories

### Success (0)
- **EXIT_SUCCESS (0)**: Operation completed successfully
  - All tests passed
  - Coverage analysis completed
  - Configuration validation succeeded
  - No errors encountered

### General Failures (1-9)

#### EXIT_FAILURE (1)
General failure - catch-all for unspecified errors
- Unexpected runtime errors
- Unclassified failures
- Generic operation failures

#### EXIT_THRESHOLD_NOT_MET (2)
Coverage threshold requirement not satisfied
- Coverage percentage below configured threshold
- Quality gate failure
- CI/CD pipeline blocker

#### EXIT_NO_COVERAGE_DATA (3)
No coverage data available for analysis
- Missing .gcov files
- Empty coverage results
- No source files processed

#### EXIT_INVALID_CONFIG (4)
Configuration error or invalid parameters
- Invalid command-line arguments
- Malformed configuration file
- Conflicting options

#### EXIT_FILE_ACCESS_ERROR (5)
File system access issues
- Source files not found
- Permission denied
- I/O operation failures

#### EXIT_MEMORY_ERROR (6)
Memory allocation or management errors
- Out of memory conditions
- Allocation failures
- Memory corruption detected

#### EXIT_VALIDATION_ERROR (7)
Validation check failures
- Security validation errors
- Input validation failures
- Constraint violations

## Usage Examples

### Command Line
```bash
# Success case
fortcov --source=src --output=coverage.md
echo $?  # Returns 0

# Threshold not met
fortcov --source=src --threshold=90
echo $?  # Returns 2 if coverage < 90%

# Invalid configuration
fortcov --invalid-flag
echo $?  # Returns 4
```

### CI/CD Integration
```yaml
# GitHub Actions example
- name: Run coverage analysis
  run: |
    fortcov --source=src --threshold=80
    EXIT_CODE=$?
    case $EXIT_CODE in
      0) echo "Coverage passed" ;;
      2) echo "Coverage below threshold" && exit 1 ;;
      3) echo "No coverage data found" && exit 1 ;;
      *) echo "Coverage analysis failed" && exit 1 ;;
    esac
```

### Fortran Implementation
```fortran
use system_exit_handler, only: exit_success, exit_failure, exit_threshold_not_met

! Success case
if (coverage >= threshold) then
    call exit_success("Coverage threshold met")
end if

! Failure case
if (coverage < threshold) then
    call exit_threshold_not_met("Coverage below required threshold")
end if
```

## Migration from STOP Statements

All STOP statements have been replaced with appropriate exit() calls:

| Old Code | New Code |
|----------|----------|
| `stop 0` | `call exit_success()` |
| `stop 1` | `call exit_failure()` or specific exit code |
| `stop 2` | `call exit_threshold_not_met()` |

## Exit Code Handler Module

The `system_exit_handler` module provides:
- Unified exit handling interface
- Consistent error messaging
- Proper Unix exit codes
- CI/CD compatibility

## Best Practices

1. **Use specific exit codes** when the error type is known
2. **Provide error messages** for debugging
3. **Document exit codes** in help text
4. **Test exit codes** in CI/CD pipelines
5. **Maintain backward compatibility** where possible

## Related Documentation

- [Error Handling](./error-handling.md)
- [CI/CD Integration](./ci-integration.md)
- [Testing Guide](./testing.md)