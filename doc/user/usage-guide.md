# FortCov Usage Guide

Comprehensive guide for using FortCov in different scenarios.

## Quick Reference

```bash
# Simple usage
fortcov --source=src --output=coverage.md

# CI/CD usage  
fortcov --source=src --fail-under=80 --quiet

# Configuration file
fortcov --config=fortcov.nml
```

## Command-Line Interface

### Basic Options

| Option | Description | Example |
|--------|-------------|---------|
| `--source=PATH` | Source directory | `--source=src` |
| `--output=FILE` | Output file | `--output=coverage.md` |
| `--output-format=FORMAT` | Format (markdown/html/json) | `--output-format=html` |
| `--fail-under=N` | Coverage threshold | `--fail-under=80` |

### Behavior Options

| Option | Description | Example |
|--------|-------------|---------|
| `--verbose` | Detailed output | `--verbose` |
| `--quiet` | Suppress output | `--quiet` |
| `--exclude=PATTERN` | Exclude files | `--exclude='test/*'` |
| `--include=PATTERN` | Include only files | `--include='src/*'` |

## User Workflows

### For Developers

#### Daily Development

```bash
# Quick coverage check
fortcov --source=src --quiet && echo "✓ Coverage good"

# Detailed analysis
fortcov --source=src --verbose --output=coverage.md
```

#### Pre-commit Workflow

```bash
# Validate configuration first
fortcov --source=src --validate-config || exit 1

# Check coverage meets standards
fortcov --source=src --fail-under=80 --quiet
if [ $? -eq 0 ]; then
    echo "✓ Ready to commit"
else
    echo "⚠ Add more tests"
fi
```

### For DevOps Engineers

#### CI/CD Integration

**GitHub Actions:**
```yaml
- name: Generate Coverage
  run: |
    fpm test --flag "-fprofile-arcs -ftest-coverage"
    gcov src/*.f90
    fortcov --source=src --fail-under=80 --output=coverage.md
```

**GitLab CI:**
```yaml
coverage:
  script:
    - fpm test --flag "-fprofile-arcs -ftest-coverage"
    - gcov src/*.f90
    - fortcov --source=src --output-format=html --output=coverage.html
```

#### Quality Gates

```bash
# Critical components (95% required)
fortcov --source=src/critical --fail-under=95 --quiet

# Normal components (80% required)  
fortcov --source=src --exclude=critical/* --fail-under=80 --quiet

# Performance optimization for large codebases
fortcov --source=src --threads=4 --include='*.f90,*.F90' --fail-under=80
```

### For Project Managers

#### Coverage Monitoring

```bash
# Extract coverage percentage
COVERAGE=$(fortcov --source=src --output-format=json --quiet | jq -r '.summary.line_coverage')
echo "Current coverage: $COVERAGE%"

# Trend tracking
echo "$(date),$COVERAGE" >> coverage-history.csv
```

## Configuration Files

### Basic Configuration

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

### Environment-Specific Configs

**Development** (`configs/dev.nml`):
```fortran
&fortcov_config
    source_paths = 'src/', 'lib/'
    minimum_coverage = 70.0
    verbose = .true.
/
```

**Production** (`configs/prod.nml`):
```fortran
&fortcov_config
    source_paths = 'src/'
    exclude_patterns = '*.mod', 'test/*', 'vendor/*'
    minimum_coverage = 90.0
    quiet = .true.
    output_format = 'html'
/
```

## Output Formats

### Markdown (Default)
```bash
fortcov --source=src --output=coverage.md
```
Perfect for documentation and README files.

### HTML Report
```bash
fortcov --source=src --output-format=html --output=coverage.html
```
Interactive report with syntax highlighting.

### JSON Output
```bash
fortcov --source=src --output-format=json --output=coverage.json
```
Machine-readable format for tool integration.

## Best Practices

### File Organization
```bash
# Good: Consistent source structure
fortcov --source=src --exclude='test/*'

# Good: Multiple source directories
fortcov --source=src --source=lib --exclude='*.mod'
```

### Performance Tips
```bash
# Use threads for large projects
fortcov --source=src --threads=8

# Target only relevant files with include patterns
fortcov --source=src --include='*.f90,*.F90' --exclude='test/*'

# Validate configuration before running expensive analysis
fortcov --source=src --validate-config && fortcov --source=src --threads=4
```

### Integration Examples

**Makefile:**
```makefile
coverage: test
	gcov src/*.f90
	fortcov --source=src --output=coverage.html

.PHONY: coverage
```

For complete examples and advanced usage patterns, see [Examples](examples.md) and [Configuration Reference](configuration.md).