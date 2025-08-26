# FortCov Usage Guide

Advanced usage patterns and workflows. For basic usage, see [README.md](../../README.md).

## Core Usage Patterns

**Zero-configuration (recommended):**
```bash
cd your-fortran-project
fpm test --flag "-fprofile-arcs -ftest-coverage"
fortcov  # Auto-discovers sources and coverage files
```

**Explicit file specification:**
```bash
# Process specific gcov files
fortcov file1.gcov file2.gcov

# Process all gcov files in directory
fortcov *.gcov
```

**Source-based analysis:**
```bash
# Analyze specific source directories
fortcov --source src --source lib

# With exclusions
fortcov --source . --exclude "test/*" --exclude "*.mod"
```

## Output Formats and Destinations

```bash
# Terminal output (default)
fortcov

# File output with format
fortcov --format=markdown --output=coverage.md
fortcov --format=json --output=coverage.json
fortcov --format=xml --output=coverage.xml

# Combined terminal and file
fortcov --format=json --output=coverage.json --verbose
```

## Coverage Thresholds and Quality Gates

```bash
# Warning threshold (non-failing)
fortcov --minimum 80

# Fail if below threshold (exit code 1)
fortcov --fail-under 85

# Combined thresholds
fortcov --minimum 80 --fail-under 90
```

## Advanced Analysis Features

**Coverage Diff Analysis:**
```bash
# Compare two coverage reports
fortcov --diff baseline.json,current.json

# With custom threshold for warnings
fortcov --diff baseline.json,current.json --diff-threshold 5.0

# Include unchanged files in diff
fortcov --diff base.json,new.json --include-unchanged
```

**Terminal UI Mode:**
```bash
# Interactive exploration
fortcov --tui

# TUI with specific source
fortcov --source src --tui
```

## Workflow Integration

**Pre-commit Hook:**
```bash
#!/bin/bash
fpm test --flag "-fprofile-arcs -ftest-coverage"
fortcov --fail-under=80 --quiet
```

**CI/CD Pipeline:**
```bash
# Generate coverage for CI
fortcov --format=json --output=coverage.json --fail-under=85 --quiet

# Parse results
coverage=$(jq -r '.summary.line_coverage' coverage.json)
echo "Coverage: $coverage%"
```

**Development Workflow:**
```bash
# Quick check during development
fortcov --verbose

# Detailed analysis with exclusions
fortcov --source src --exclude "*.mod" --exclude "test/*" --format=markdown --output=dev-coverage.md
```

## Configuration-Driven Usage

**Environment-specific configs:**
```bash
# Development (lower threshold, verbose)
fortcov --config=dev.nml

# Production (high threshold, JSON output)
fortcov --config=ci.nml

# Override config values
fortcov --config=dev.nml --fail-under=90
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
fortcov --source src --source lib

# Parallel processing
fortcov --threads 4

# Exclude large directories
fortcov --exclude "vendor/*" --exclude "build/*"
```

**Memory management:**
- Processes files in batches to limit memory usage
- Automatic cleanup of temporary files
- Configurable memory limits via configuration files