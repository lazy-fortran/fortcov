# FortCov Usage Guide

**Advanced usage patterns** - see main [README.md](../../README.md) for basic usage.

## Command Reference

```bash
# Zero-configuration (recommended)
fortcov

# Traditional usage  
fortcov --source=src --output=coverage.md

# CI/CD usage
fortcov --fail-under=80 --quiet

# Configuration file
fortcov --config=fortcov.nml
```

## Advanced Options

| Option | Description | Status |
|--------|-------------|--------|
| `--format=FORMAT` | Output format (markdown/json/xml) | ✅ Working |
| `--threshold=N` | Coverage threshold (warning only) | ✅ Working |
| `--fail-under=N` | Fail if coverage is below N% | ✅ Working |
| `--tui` | Interactive analysis mode | ✅ Working |
| `--diff --diff-baseline=FILE` | Coverage comparison | ✅ Working |
| `--verbose` | Enhanced output | 🔄 Partial |
| `--quiet` | Suppress stdout | ❌ Not implemented |
| `--exclude=PATTERN` | Exclude files | 🔄 Partial |

## Configuration Files

**Namelist format** (`fortcov.nml`):
```fortran
&fortcov_config
    source_paths = 'src/', 'lib/'
    exclude_patterns = 'test/*', '*.mod'
    output_format = 'markdown'
    minimum_coverage = 80.0
    verbose = .false.
/
```

## Security Features

FortCov validates all inputs to prevent:

```bash
# These are blocked for security
fortcov --source="path;rm -rf /"        # Command injection
fortcov --source="../../../etc/"        # Path traversal  
fortcov --source="/proc/"                # System access
```

Use clean paths without special characters.

## Environment Integration

**Pre-commit workflow:**
```bash
fortcov --fail-under=80 && echo "Ready to commit" || echo "Add more tests"
```

**Quality gates:**
```bash
fortcov --fail-under=95 --format=json | jq -r '.summary.line_coverage'
```

For complete documentation including CI/CD examples, see main [README.md](../../README.md).