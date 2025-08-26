# FortCov Usage Guide

Advanced usage patterns and workflows. For basic usage, see [README.md](../../README.md).

## Core Usage Patterns

**Manual coverage analysis:**
```bash
cd your-fortran-project
fpm test --flag "-fprofile-arcs -ftest-coverage"
# Generate .gcov files manually:
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
  gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
done
fortcov --source=src *.gcov  # Analyze coverage files
```

**Explicit file specification:**
```bash
# Process specific gcov files
fortcov --source=src file1.gcov file2.gcov

# Process all gcov files in directory
fortcov --source=src *.gcov
```

**Source-based analysis:**
```bash
# Analyze specific source directories with coverage files
fortcov --source src --source lib *.gcov

# With exclusions
fortcov --source . --exclude "test/*" --exclude "*.mod" *.gcov
```

## Output Formats and Analysis

```bash
# Terminal output (current working implementation)
fortcov --source=src *.gcov

# Note: File output formats show 'would be generated' but don't create files
# Available format options: terminal (default), markdown, json, xml
fortcov --source=src *.gcov --format=json
fortcov --source=src *.gcov --format=xml

# Verbose terminal output
fortcov --source=src *.gcov --verbose
```

## Coverage Thresholds and Quality Gates

```bash
# Warning threshold (non-failing)
fortcov --source=src *.gcov --minimum 80

# Fail if below threshold (exit code 1)
fortcov --source=src *.gcov --fail-under 85

# Combined thresholds
fortcov --source=src *.gcov --minimum 80 --fail-under 90
```

## Advanced Analysis Features

**Note**: Advanced features like diff analysis and TUI mode are not yet implemented in the current version. The current implementation provides terminal coverage analysis only.

## Workflow Integration

**Pre-commit Hook:**
```bash
#!/bin/bash
fpm test --flag "-fprofile-arcs -ftest-coverage"
# Generate .gcov files
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
  gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
done
fortcov --source=src *.gcov --fail-under=80 --quiet
```

**CI/CD Pipeline:**
```bash
# Generate coverage for CI (note: file output not yet implemented)
fpm test --flag "-fprofile-arcs -ftest-coverage"
# Generate .gcov files
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
  gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
done
fortcov --source=src *.gcov --fail-under=85 --quiet
```

**Development Workflow:**
```bash
# Quick check during development
fortcov --source=src *.gcov --verbose

# Detailed analysis with exclusions
fortcov --source src --exclude "*.mod" --exclude "test/*" *.gcov
```

## Configuration-Driven Usage

**Environment-specific configs:**
```bash
# Development (lower threshold, verbose)
fortcov --config=dev.nml *.gcov

# Production (high threshold)
fortcov --config=ci.nml *.gcov

# Override config values
fortcov --config=dev.nml *.gcov --fail-under=90
```

## Security and Validation

FortCov includes built-in security validation:

- **Path traversal protection**: Prevents access outside project directory
- **Command injection prevention**: Validates all input parameters
- **File size limits**: Prevents processing of excessively large files
- **Timeout protection**: Prevents long-running operations

All paths are validated and normalized before processing.

## Performance Optimization

**Large project handling:**
```bash
# Use specific source paths (faster)
fortcov --source src --source lib *.gcov

# Parallel processing (note: threads option may not be implemented)
fortcov --source=src *.gcov --threads 4

# Exclude large directories
fortcov --source=src --exclude "vendor/*" --exclude "build/*" *.gcov
```

**Memory management:**
- Processes files in batches to limit memory usage
- Automatic cleanup of temporary files
- Configurable memory limits via configuration files