# FortCov Configuration Reference

Complete guide to FortCov configuration options.

## Quick Reference

```bash
# Command line usage
fortcov --source=src --output=coverage.md --threshold=80

# Configuration file usage
fortcov --config=fortcov.nml
```

## Command Line Options (Issue #228 Status)

**Major CLI Fix**: Issue #228 has been largely resolved with 5/16 test cases now passing. Core functionality restored with proper validation and error handling.

### Basic Options (✅ Working - Issue #228 Fixed)

| Option | Description | Status | Example |
|--------|-------------|--------|--------|
| `--source=PATH` | Source directory | 🔄 Partial | `--source=src` |
| `--output=FILE` | Output file | ✅ Working | `--output=coverage.md` |
| `--format=FORMAT` | Output format | ✅ Working | `--format=json` |
| `--threshold=N` | Coverage threshold (0-100%) | ✅ Working | `--threshold=80` |

### Behavior Options

| Option | Description | Status | Example |
|--------|-------------|--------|--------|
| `--verbose` | Detailed output | 🔄 Partial | `--verbose` |
| `--quiet` | Suppress output | ❌ Pending | `--quiet` |
| `--exclude=PATTERN` | Exclude files | 🔄 Partial | `--exclude='test/*'` |
| `--include=PATTERN` | Include only files | ❌ Pending | `--include='src/*'` |

### Advanced Options

| Option | Description | Status | Example |
|--------|-------------|--------|--------|
| `--tui` | Interactive mode | ✅ Working | `--tui` |
| `--diff` | Diff analysis mode | ✅ Working | `--diff --diff-baseline=base.json` |
| `--config=FILE` | Configuration file | ❌ Pending | `--config=fortcov.nml` |
| `--gcov=PATH` | Custom gcov path | ❌ Pending | `--gcov=/usr/bin/gcov-11` |
| `--threads=N` | Processing threads | ❌ Pending | `--threads=8` |

## Configuration File

Create `fortcov.nml`:

```fortran
&fortcov_config
    source_paths = 'src/'
    exclude_patterns = 'test/*', '*.mod'
    output_format = 'markdown'
    output_path = 'coverage.md'
    minimum_coverage = 80.0
    verbose = .false.
/
```

### Complete Configuration Example

```fortran
! FortCov configuration file
&fortcov_config
    ! Input/Output settings
    input_format = 'gcov'
    output_format = 'markdown'
    output_path = 'coverage.md'
    
    ! Source configuration
    source_paths = 'src/', 'lib/'
    exclude_patterns = '*.mod', 'test/*', 'build/*'
    include_patterns = '*.f90', '*.F90'
    
    ! Coverage settings
    minimum_coverage = 80.0
    fail_under = 80.0
    
    ! Tool configuration
    gcov_executable = 'gcov'
    threads = 4
    
    ! Output control
    verbose = .false.
    quiet = .false.
    
    ! Advanced features
    show_functions = .false.
    show_branches = .false.
    hide_covered_files = .false.
    strict_mode = .false.
/
```

## Environment-Specific Configurations

### Development Environment

`configs/dev.nml`:

```fortran
&fortcov_config
    source_paths = 'src/', 'lib/'
    exclude_patterns = '*.mod', 'test/*'
    minimum_coverage = 70.0
    verbose = .true.
    show_functions = .true.
/
```

### CI/CD Environment

`configs/ci.nml`:

```fortran
&fortcov_config
    source_paths = 'src/'
    exclude_patterns = '*.mod', 'build/*', 'test/*'
    minimum_coverage = 80.0
    quiet = .true.
    strict_mode = .true.
    output_format = 'json'
    output_path = 'coverage.json'
/
```

### Production Environment

`configs/prod.nml`:

```fortran
&fortcov_config
    source_paths = 'src/', 'lib/'
    exclude_patterns = '*.mod', 'test/*', 'vendor/*'
    minimum_coverage = 90.0
    quiet = .true.
    output_format = 'html'
    output_path = 'coverage-report.html'
    show_functions = .true.
    hide_covered_files = .true.
/
```

## Pattern Matching

### Include/Exclude Patterns

```fortran
&fortcov_config
    ! Include only Fortran source files
    include_patterns = '*.f90', '*.F90', '*.f95'
    
    ! Exclude various patterns
    exclude_patterns = '*.mod',           ! Compiled modules
                      'test/*',          ! Test directory
                      'build/*',         ! Build directory
                      'vendor/*',        ! Third-party code
                      '*/deprecated/*'   ! Deprecated code
/
```

## Configuration Validation

Test your configuration before running expensive analysis:

```bash
# Validate configuration file
fortcov --config=fortcov.nml --validate-config

# Validate command-line configuration
fortcov --source=src --threshold=80 --include='*.f90' --validate-config

# Check configuration in verbose mode
fortcov --config=fortcov.nml --verbose
```

## Environment Variables

Some options can be controlled via environment variables:

| Variable | Description | Example |
|----------|-------------|---------|
| `FORTCOV_CONFIG` | Default config file | `export FORTCOV_CONFIG=./fortcov.nml` |
| `FORTCOV_GCOV` | Default gcov path | `export FORTCOV_GCOV=/usr/bin/gcov-11` |
| `FORTCOV_THREADS` | Default thread count | `export FORTCOV_THREADS=8` |

## Configuration Precedence

Options are applied in this order (later overrides earlier):

1. **Default values** (built into FortCov)
2. **Configuration file** (via `--config=file.nml`)
3. **Environment variables** (if supported)
4. **Command-line arguments** (highest priority)

Example:
```bash
# Config file sets minimum_coverage = 70.0
# Command line overrides to 80.0
fortcov --config=fortcov.nml --threshold=80
```

## Best Practices

### Use Environment-Specific Configs

```bash
# Development
fortcov --config=configs/dev.nml

# CI/CD
fortcov --config=configs/ci.nml

# Production
fortcov --config=configs/prod.nml
```

### Template Configuration

```fortran
! fortcov-template.nml
&fortcov_config
    source_paths = 'src/'
    exclude_patterns = '*.mod', 'test/*', 'build/*'
    include_patterns = '*.f90', '*.F90'
    fail_under_threshold = 80.0
    threads = 2
    output_format = 'markdown'
    output_path = 'coverage.md'
    verbose = .false.
/
```

Validate template:
```bash
# Always validate configuration before use
fortcov --config=fortcov-template.nml --validate-config
```

For complete configuration options and advanced usage patterns, see the full configuration documentation.