# FortCov Configuration Guide

This guide covers all configuration options for FortCov, from simple command-line usage to advanced configuration files.

## Quick Reference

```bash
# Most common usage patterns
fortcov --source=src --output=coverage.md                    # Basic
fortcov --source=src --fail-under=80 --quiet                 # CI/CD
fortcov --config=fortcov.nml                                 # Config file
```

## Command Line Options

### Required Options

| Option | Description | Example |
|--------|-------------|---------|
| `--source=PATH` | Source directory to analyze | `--source=src` |

### Output Options

| Option | Short | Default | Description | Example |
|--------|-------|---------|-------------|---------|
| `--output=FILE` | `-o` | stdout | Output file path | `--output=coverage.md` |
| `--output-format=FORMAT` | - | markdown | Output format | `--output-format=json` |

**Available formats**: `markdown`, `json`, `html`

### Coverage Options

| Option | Short | Default | Description | Example |
|--------|-------|---------|-------------|---------|
| `--fail-under=N` | `-t` | 0.0 | Minimum coverage threshold | `--fail-under=80` |
| `--exclude=PATTERN` | - | None | Exclude files matching pattern | `--exclude='test/*'` |

### Behavior Options

| Option | Short | Default | Description | Example |
|--------|-------|---------|-------------|---------|
| `--verbose` | `-v` | false | Show detailed output | `--verbose` |
| `--quiet` | `-q` | false | Only show errors | `--quiet` |
| `--strict` | - | false | Error if no coverage files | `--strict` |

### Advanced Options

| Option | Default | Description | Example |
|--------|---------|-------------|---------|
| `--config=FILE` | None | Load configuration file | `--config=fortcov.nml` |
| `--gcov=EXECUTABLE` | gcov | Custom gcov path | `--gcov=/usr/bin/gcov-10` |
| `--tui` | false | Interactive terminal interface | `--tui` |
| `--import=FILE` | None | Import JSON coverage data | `--import=baseline.json` |
| `--diff=BASE,CURRENT` | None | Compare two coverage datasets | `--diff=old.json,new.json` |

## Configuration File Format

FortCov uses Fortran namelist format for configuration files.

### Basic Configuration

Create `fortcov.nml`:

```fortran
&fortcov_config
    input_format = 'gcov'
    output_format = 'markdown'
    output_path = 'coverage.md'
    source_paths = 'src/'
    minimum_coverage = 80.0
/
```

### Complete Configuration Example

```fortran
! FortCov configuration file
! Save as: fortcov.nml

&fortcov_config
    ! Input/Output settings
    input_format = 'gcov'              ! gcov, lcov, json  
    output_format = 'markdown'         ! markdown, json, html
    output_path = 'coverage.md'        ! Output file path
    
    ! Source configuration
    source_paths = 'src/', 'lib/', 'app/'      ! Directories to include
    exclude_patterns = '*.mod',                 ! Patterns to exclude
                      '*.o', 
                      'build/*',
                      'test/*',
                      'external/*'
    
    ! Coverage settings
    minimum_coverage = 80.0            ! Threshold percentage (0-100)
    
    ! Tool configuration
    gcov_executable = 'gcov'           ! Path to gcov executable
    
    ! Output control
    verbose = .false.                  ! Detailed output
    quiet = .false.                    ! Suppress non-error output
    
    ! Advanced features
    enable_diff = .false.              ! Enable diff mode
    diff_threshold = 5.0               ! Diff significance threshold
    keep_gcov_files = .false.          ! Preserve .gcov files
    strict_mode = .false.              ! Error if no coverage files
    tui_mode = .false.                 ! Launch interactive browser
/
```

### Configuration Validation

Test your configuration:

```bash
# Check if configuration is valid
fortcov --config=fortcov.nml --verbose

# Dry run (if implemented)
fortcov --config=fortcov.nml --dry-run
```

## Environment-Specific Configurations

### Development Environment

`configs/development.nml`:

```fortran
&fortcov_config
    output_format = 'markdown'
    output_path = 'coverage-dev.md'
    source_paths = 'src/', 'lib/'
    exclude_patterns = '*.mod', 'test/*', 'debug/*'
    minimum_coverage = 70.0
    verbose = .true.
    keep_gcov_files = .true.
/
```

### CI/CD Environment

`configs/ci.nml`:

```fortran
&fortcov_config
    output_format = 'json'
    output_path = 'coverage.json'
    source_paths = 'src/', 'lib/'
    exclude_patterns = '*.mod', 'test/*', 'build/*', 'external/*'
    minimum_coverage = 80.0
    quiet = .true.
    strict_mode = .true.
/
```

### Production Reporting

`configs/production.nml`:

```fortran
&fortcov_config
    output_format = 'html'
    output_path = 'coverage-report.html'
    source_paths = 'src/', 'lib/'
    exclude_patterns = '*.mod', 'test/*', 'build/*', 'external/*', 'vendor/*'
    minimum_coverage = 85.0
    quiet = .true.
/
```

Usage:

```bash
# Environment-specific runs
fortcov --config=configs/development.nml
fortcov --config=configs/ci.nml  
fortcov --config=configs/production.nml
```

## Advanced Configuration Patterns

### Multi-Module Projects

For projects with multiple Fortran modules:

```fortran
&fortcov_config
    source_paths = 'src/core/',
                  'src/utils/', 
                  'src/interfaces/',
                  'lib/external/'
    exclude_patterns = '*.mod',
                      'test/*',
                      'build/*',
                      'src/*/test/*',          ! Module-specific tests
                      'lib/external/vendor/*'  ! Third-party code
    output_path = 'coverage-full.md'
    minimum_coverage = 75.0
/
```

### Library vs Application Coverage

**Library Configuration** (`lib-coverage.nml`):

```fortran
&fortcov_config
    source_paths = 'lib/'
    exclude_patterns = '*.mod', 'examples/*', 'benchmarks/*'
    minimum_coverage = 90.0     ! Higher standard for libraries
    output_path = 'lib-coverage.md'
/
```

**Application Configuration** (`app-coverage.nml`):

```fortran
&fortcov_config
    source_paths = 'src/', 'app/'
    exclude_patterns = '*.mod', 'test/*', 'scripts/*'
    minimum_coverage = 80.0     ! Application-level standard
    output_path = 'app-coverage.md'
/
```

### Incremental Coverage Analysis

For large projects, analyze components separately:

```bash
# Core modules first
fortcov --source=src/core --output=core-coverage.md --config=base.nml

# Utilities  
fortcov --source=src/utils --output=utils-coverage.md --config=base.nml

# Full analysis
fortcov --source=src --output=full-coverage.md --config=full.nml
```

## Configuration Best Practices

### 1. Project-Specific Standards

Set coverage thresholds based on project type:

| Project Type | Minimum Coverage | Reasoning |
|--------------|-----------------|-----------|
| Libraries | 85-95% | High reuse, critical reliability |
| Applications | 75-85% | Business logic focus |
| Prototypes | 60-75% | Rapid development |
| Legacy Code | 50-70% | Incremental improvement |

### 2. Exclusion Patterns

**Always exclude**:
```fortran
exclude_patterns = '*.mod',        ! Compiled modules
                  'build/*',       ! Build artifacts
                  'external/*',    ! Third-party code
                  'vendor/*'       ! Dependencies
```

**Consider excluding**:
```fortran
exclude_patterns = 'test/*',       ! Test files
                  '*_test.f90',    ! Test modules
                  'examples/*',    ! Example code
                  'benchmarks/*',  ! Performance tests
                  'scripts/*'      ! Build scripts
```

### 3. Output Format Selection

**Use Markdown for**:
- Code review comments
- Documentation integration
- Manual review

**Use JSON for**:
- CI/CD integration
- Automated processing
- Data analysis
- Coverage trending

**Use HTML for**:
- Stakeholder reports
- Interactive browsing
- Archive/documentation

### 4. Environment Configuration

**Development**:
```fortran
verbose = .true.           ! Detailed output for debugging
keep_gcov_files = .true.   ! Preserve for analysis
minimum_coverage = 70.0    ! Lower threshold during development
```

**CI/CD**:
```fortran
quiet = .true.             ! Clean CI logs
strict_mode = .true.       ! Fail on missing data
minimum_coverage = 80.0    ! Production standard
```

**Production Reporting**:
```fortran
output_format = 'html'     ! Rich reports
quiet = .true.             ! Clean output
minimum_coverage = 85.0    ! High standard
```

## Troubleshooting Configuration

### Common Issues

#### "Configuration file not found"

```bash
# Check file exists
ls -la fortcov.nml

# Check permissions
chmod 644 fortcov.nml

# Use absolute path
fortcov --config=/full/path/to/fortcov.nml
```

#### "Invalid namelist format"

```bash
# Validate namelist syntax
gfortran -fsyntax-only -x f90 fortcov.nml

# Common issues:
# - Missing closing '/' 
# - Comments inside namelist block
# - Incorrect string quoting
```

#### "Source path not found"

```fortran
! Check your paths are correct
source_paths = 'src/',     ! Note trailing slash
              'lib/'       ! Multiple paths as array
```

#### "Exclude patterns not working"

```fortran
! Use proper glob patterns
exclude_patterns = 'test/*',      ! Directory and contents
                  '*.mod',        ! File extension
                  '*_test.f90',   ! Name pattern
                  'build/**'      ! Recursive (if supported)
```

### Configuration Validation

Create a validation script `validate-config.sh`:

```bash
#!/bin/bash

CONFIG_FILE="${1:-fortcov.nml}"

echo "Validating FortCov configuration: $CONFIG_FILE"

# Check file exists
if [ ! -f "$CONFIG_FILE" ]; then
    echo "❌ Configuration file not found: $CONFIG_FILE"
    exit 1
fi

# Check namelist syntax
if ! gfortran -fsyntax-only -x f90 "$CONFIG_FILE" 2>/dev/null; then
    echo "❌ Invalid Fortran namelist syntax"
    gfortran -fsyntax-only -x f90 "$CONFIG_FILE"
    exit 1
fi

# Test with FortCov
if fortcov --config="$CONFIG_FILE" --help >/dev/null 2>&1; then
    echo "✅ Configuration file is valid"
else
    echo "❌ Configuration rejected by FortCov"
    fortcov --config="$CONFIG_FILE" --help
    exit 1
fi

echo "✅ Configuration validation passed"
```

Usage:

```bash
./validate-config.sh fortcov.nml
./validate-config.sh configs/production.nml
```

### Configuration Templates

Generate configuration from current settings:

```bash
# Export current settings to config file
fortcov --source=src --output=coverage.md --verbose --fail-under=80 --export-config=current.nml
```

*Note: `--export-config` is a suggested feature for future implementation.*

## Configuration Migration

When updating FortCov versions, configuration might need updates:

### Version Compatibility

```fortran
! FortCov 1.0 format
&fortcov_config
    input_format = 'gcov'
    output_format = 'markdown'
/

! Future version might add:
&fortcov_config
    version = '2.0'            ! Configuration version
    input_format = 'gcov'
    output_format = 'markdown'
    ! New options...
/
```

### Migration Strategy

1. **Backup current config**: `cp fortcov.nml fortcov.nml.backup`
2. **Test new version**: `fortcov --config=fortcov.nml --verbose`
3. **Update as needed**: Add new options, remove deprecated ones
4. **Validate**: Use validation script to confirm

For complex migrations, consider version-specific configs:

```
configs/
├── v1.0-fortcov.nml
├── v1.1-fortcov.nml  
└── current-fortcov.nml -> v1.1-fortcov.nml
```