# FortCov Configuration Guide

Complete configuration reference for FortCov. For basic usage, see [README.md](../../README.md).

## Configuration File Format

FortCov uses **Fortran namelist format** (`fortcov.nml`) for configuration:

```fortran
&fortcov_config
    source_paths = 'src/', 'lib/', 'modules/'
    exclude_patterns = 'test/*', '*.mod', 'vendor/*'
    output_format = 'json'
    output_path = 'coverage.json'
    minimum_coverage = 80.0
    verbose = .true.
    quiet = .false.
    gcov_executable = 'gcov'
/
```

## Complete Configuration Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `source_paths` | string array | `'src/'` | Source directories to analyze |
| `exclude_patterns` | string array | `''` | File patterns to exclude (glob syntax) |
| `output_format` | string | `'markdown'` | Output format: text, markdown (default), json, html, xml |
| `output_path` | string | `'-'` | Output file path ('-' for stdout) |
| `minimum_coverage` | real | `0.0` | Minimum coverage threshold (0-100) |
| `verbose` | logical | `.false.` | Enable verbose output |
| `quiet` | logical | `.false.` | Suppress non-essential output |
| `gcov_executable` | string | `'gcov'` | Path to gcov executable |

## Environment-Specific Examples

**Development** (`dev.nml`):
```fortran
&fortcov_config
    source_paths = 'src/', 'lib/'
    minimum_coverage = 70.0
    verbose = .true.
/
```

**Production CI** (`ci.nml`):
```fortran
&fortcov_config
    source_paths = 'src/'
    exclude_patterns = '*.mod', 'test/*', 'vendor/*'
    output_format = 'json'
    output_path = 'coverage.json'
    minimum_coverage = 95.0
    quiet = .true.
/
```

**Diff Analysis** (`diff.nml`):
```fortran
&fortcov_config
    output_format = 'json'
    source_paths = 'src/'
    verbose = .true.
/
```

## Command-Line Usage

```bash
# Use specific config
fortcov --config=ci.nml

# Override config values  
fortcov --config=dev.nml --fail-under=85

# Run with verbose output
fortcov --config=prod.nml --verbose
```

## Configuration Best Practices

**Project Structure Integration:**
- Place `fortcov.nml` in project root
- Use environment-specific config files (dev.nml, ci.nml)
- Version control configuration files

**Performance Optimization:**
- Exclude unnecessary directories (test/, build/, vendor/)
- Use specific source paths instead of wildcards
- Enable quiet mode for CI/CD pipelines

**Current Implementation Status:**
- Coverage analysis and terminal display fully functional
- All file output formats create actual files successfully
- Configuration files and CLI options fully functional

**CI/CD Integration:**
- Terminal output provides coverage statistics for parsing
- Set appropriate coverage thresholds
- Enable fail-under for quality gates