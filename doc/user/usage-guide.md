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
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do gcov --object-directory="$dir" "$dir"/*.gcno; done
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

### CLI Features Status (Issue #228 Fix)

**✅ FULLY WORKING** - Tested and verified:

**Output Formats:**
```bash
# JSON output (verified working)
fortcov --format=json --output=coverage.json *.gcov

# XML output (Cobertura format)
fortcov --format=xml --output=coverage.xml *.gcov

# Markdown output (default)
fortcov --output=report.md *.gcov
```

**Coverage Thresholds:**
```bash
# Threshold enforcement working - exits with code 1 if not met
fortcov --threshold=80 *.gcov
echo $?  # Returns 1 if coverage < 80%

# Invalid thresholds properly rejected
fortcov --threshold=150 *.gcov  # Error: must be 0-100%
fortcov --threshold=-50 *.gcov  # Error: negative values rejected
```

**Interactive and Analysis Modes:**
```bash
# TUI mode working - launches interactive interface
fortcov --tui

# Diff mode working - compares against baseline
fortcov --diff --diff-baseline=baseline.json --output=diff.md
```

**Security Improvements:**
```bash
# Invalid flags now properly rejected (security fix)
fortcov --invalid-flag *.gcov
# Returns: Error: Unknown flag: --invalid-flag

# Missing files properly detected
fortcov --diff --diff-baseline=/nonexistent.json
# Returns: Error: Baseline file not found

# Enhanced security validation protects against malicious inputs
fortcov --source="path;rm -rf /" *.gcov
# Returns: Error: Path contains dangerous characters

# Directory traversal attacks prevented
fortcov --source="../../../etc/" *.gcov  
# Returns: Error: Path contains dangerous characters

# System directory access blocked for security
fortcov --source="/proc/" *.gcov
# Returns: Error: Suspicious system path access
```

**🔄 PARTIALLY WORKING** - Parsed correctly, implementation incomplete:

**Verbose Mode:**
```bash
# Verbose flag parsed and some enhanced output provided
fortcov --verbose *.gcov  # Shows file processing details
```

**Source Path Configuration:**
```bash
# Flag parsed but discovery logic needs improvement
fortcov --source=src *.gcov  # Partially applies source restriction
```

**Exclude Patterns:**
```bash
# Pattern parsed but not fully applied during file discovery
fortcov --exclude='test/*' *.gcov  # Partial pattern matching
```

**❌ NOT YET IMPLEMENTED** - Future features:
```bash
# These flags are recognized but not yet functional
fortcov --quiet *.gcov          # Quiet mode
fortcov --gcov-executable=gcov  # Custom gcov path
fortcov --threads=4 *.gcov      # Parallel processing
fortcov --max-files=100 *.gcov  # File count limits
fortcov --config=fortcov.nml    # Configuration files
```

**Recommended Working Commands:**
```bash
# Production-ready JSON output with threshold
fortcov --format=json --output=coverage.json --threshold=80 *.gcov

# Interactive analysis
fortcov --tui

# Coverage comparison
fortcov --diff --diff-baseline=baseline.json --format=json *.gcov
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
fortcov --threshold=80  # Note: --quiet not yet implemented
if [ $? -eq 0 ]; then
    echo "✓ Ready to commit - coverage threshold met"
else
    echo "⚠ Add more tests - coverage below threshold"
fi
```

### For DevOps Engineers

#### CI/CD Integration

**GitHub Actions:**
```yaml
- name: Generate Coverage
  run: |
    fmp test --flag "-fprofile-arcs -ftest-coverage"
    find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do gcov --object-directory="$dir" "$dir"/*.gcno; done
    fortcov --threshold=80  # --quiet flag not yet implemented
```

**GitLab CI:**
```yaml
coverage:
  script:
    - fpm test --flag "-fprofile-arcs -ftest-coverage"
    - find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do gcov --object-directory="$dir" "$dir"/*.gcno; done
    - fortcov --format=xml --output=coverage.xml
```

#### Quality Gates

```bash
# Standard quality gate with zero-config
fortcov --threshold=80  # Exits with code 1 if not met

# High-bar for critical projects
fortcov --threshold=95  # Validated range: 0-100%

# Source filtering (partial implementation)
fortcov --source=src/critical --threshold=95
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

### Security Best Practices

```bash
# ✅ Good: Use clean, simple paths
fortcov --source=src --output=coverage.md

# ✅ Good: Standard project structure
fortcov --source=src --source=lib --output=reports/coverage.html

# ❌ Avoid: Special shell characters in paths
# fortcov --source="src;malicious_command"
# fortcov --output="report.md|dangerous"

# ❌ Avoid: Directory traversal patterns
# fortcov --source="../../../etc"
# fortcov --output="../../../../tmp/bad.md"

# ❌ Avoid: System directories (automatically blocked)
# fortcov --source=/proc --source=/sys --source=/etc
```

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