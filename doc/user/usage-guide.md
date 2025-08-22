# FortCov Usage Guide

Comprehensive guide for using FortCov in different scenarios.

## Quick Reference

```bash
# Zero-configuration (recommended)
fortcov

# Traditional usage with explicit options
fortcov --source=src --output=coverage.md

# CI/CD usage  
fortcov --threshold=80 --quiet

# Configuration file
fortcov --config=fortcov.nml
```

## Zero-Configuration Mode

The fastest way to get coverage reports - just run `fortcov` without any arguments:

```bash
# Standard workflow
fpm test --flag "-fprofile-arcs -ftest-coverage"
gcov -o build/gcov src/*.f90
fortcov  # Auto-discovers everything!
```

### How Zero-Configuration Works

**Auto-Discovery Priority Order:**
1. **Coverage files**: `build/gcov/*.gcov` → `./*.gcov` → `build/**/*.gcov`
2. **Source files**: `src/*.f90` → `./*.f90` (excludes `build/*`, `test/*`)
3. **Output location**: `build/coverage/coverage.md` (creates directory if needed)

### Zero-Configuration Examples

```bash
# Works in any project with standard structure
fortcov

# Override just the output location
fortcov --output=my-report.md

# Override coverage threshold while using auto-discovery
fortcov --threshold=90

# Zero-config with quiet mode for CI
fortcov --quiet --threshold=80
```

## Command-Line Interface

### Basic Options

| Option | Description | Example |
|--------|-------------|---------|
| `--source=PATH` | Source directory | `--source=src` |
| `--output=FILE` | Output file | `--output=coverage.md` |
| `--format=FORMAT` | Format (markdown/json/xml) | `--format=json` |
| `--threshold=N` | Coverage threshold percentage | `--threshold=80` |

### Behavior Options

| Option | Description | Example |
|--------|-------------|---------|
| `--verbose` | Enable verbose mode | `--verbose` |
| `--quiet` | Enable quiet mode | `--quiet` |
| `--exclude=PATTERN` | Exclude files by pattern | `--exclude='test/*'` |
| `--include=PATTERN` | Include only files | `--include='src/*'` |

### Working CLI Examples (Tested)

**Output Formats** - All working correctly:
```bash
# JSON output with custom path
fortcov --format=json --output=coverage.json *.gcov

# XML output (Cobertura format) 
fortcov --format=xml --output=coverage.xml *.gcov

# Markdown output (default)
fortcov --output=report.md *.gcov
```

**Coverage Thresholds** - Working correctly:
```bash
# Set threshold and fail if not met
fortcov --threshold=80 *.gcov
echo $?  # Exit code 1 if coverage < 80%

# High threshold for critical code
fortcov --threshold=95 --source=src/critical *.gcov
```

**Source Paths** - Working correctly:
```bash
# Specify source directory
fortcov --source=src *.gcov

# Multiple source paths
fortcov --source=src --source=lib *.gcov
```

**Invalid Flag Handling** - Security fix working:
```bash
# Invalid flags now properly rejected
fortcov --invalid-flag *.gcov
# Returns: Error: Unknown flag: --invalid-flag
```

**Combined Usage** - All combinations work:
```bash
# Production-ready command
fortcov --format=json --output=coverage.json --threshold=80 --source=src *.gcov

# CI/CD usage
fortcov --format=xml --output=coverage.xml --threshold=85 --quiet *.gcov
```

## User Workflows

### For Developers

#### Daily Development

```bash
# Quick coverage check with zero-config
fortcov --quiet && echo "✓ Coverage good"

# Detailed analysis with zero-config
fortcov --verbose

# Traditional approach (if needed)
fortcov --source=src --verbose --output=coverage.md
```

#### Pre-commit Workflow

```bash
# Check coverage meets standards with zero-config
fortcov --threshold=80 --quiet
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
    gcov -o build/gcov src/*.f90
    fortcov --threshold=80 --quiet
```

**GitLab CI:**
```yaml
coverage:
  script:
    - fpm test --flag "-fprofile-arcs -ftest-coverage"
    - gcov -o build/gcov src/*.f90
    - fortcov --format=xml --output=coverage.xml
```

#### Quality Gates

```bash
# Standard quality gate with zero-config
fortcov --threshold=80 --quiet

# High-bar for critical projects
fortcov --threshold=95 --quiet

# Custom source patterns (overrides auto-discovery)
fortcov --source=src/critical --threshold=95 --quiet
```

### For Project Managers

#### Coverage Monitoring

```bash
# Extract coverage percentage with zero-config
COVERAGE=$(fortcov --format=json --quiet | jq -r '.summary.line_coverage')
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
fortcov --source=src --format=html --output=coverage.html
```
Interactive report with syntax highlighting.

### JSON Output
```bash
fortcov --source=src --format=json --output=coverage.json
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