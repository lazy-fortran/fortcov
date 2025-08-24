# FortCov Configuration Guide

**Configuration reference** - see main [README.md](../../README.md) for basic usage.

## Configuration File Format

**Fortran namelist** (`fortcov.nml`):
```fortran
&fortcov_config
    source_paths = 'src/', 'lib/', 'modules/'
    exclude_patterns = 'test/*', '*.mod', 'vendor/*'
    output_format = 'markdown'
    output_path = 'coverage.md'
    minimum_coverage = 80.0
    verbose = .true.
    quiet = .false.
/
```

## Environment-Specific Configs

**Development** (`dev.nml`):
```fortran
&fortcov_config
    source_paths = 'src/', 'lib/'
    minimum_coverage = 70.0
    verbose = .true.
/
```

**Production** (`prod.nml`):
```fortran
&fortcov_config
    source_paths = 'src/'
    exclude_patterns = '*.mod', 'test/*', 'vendor/*'
    minimum_coverage = 95.0
    quiet = .true.
    output_format = 'json'
/
```

## Configuration Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `source_paths` | string array | `'src/'` | Source directories |
| `exclude_patterns` | string array | `''` | Exclusion patterns |
| `output_format` | string | `'markdown'` | Output format |
| `output_path` | string | `'coverage.md'` | Output file |
| `minimum_coverage` | real | `0.0` | Threshold percentage |
| `verbose` | logical | `.false.` | Verbose output |
| `quiet` | logical | `.false.` | Quiet mode |

## Usage Examples

```bash
# Use specific config
fortcov --config=prod.nml

# Override config values
fortcov --config=dev.nml --fail-under=85

# Environment variables
export FORTCOV_CONFIG=prod.nml
fortcov
```

For complete configuration examples, see main [README.md](../../README.md).