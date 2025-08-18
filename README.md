# FortCov

A modern coverage analysis tool specifically designed for Fortran projects. FortCov processes coverage data from gfortran to generate comprehensive, actionable reports that help you understand and improve your code coverage.

## Why FortCov?

- **🎯 Fortran-First Design**: Built specifically for Fortran with deep understanding of modules, interfaces, and modern Fortran constructs
- **⚡ Fast & Reliable**: Processes large codebases efficiently with comprehensive error handling
- **🔒 Secure by Default**: Built-in protections against common security vulnerabilities and malformed input
- **📊 Multiple Output Formats**: Generate Markdown, JSON, HTML reports that integrate seamlessly with your workflow
- **🛠️ Developer Friendly**: Clear error messages, progress indicators, and extensive configuration options

## Quick Start

Get up and running with FortCov in under 2 minutes:

```bash
# 1. Build your Fortran project with coverage flags
fpm build --flag "-fprofile-arcs -ftest-coverage"
fpm test --flag "-fprofile-arcs -ftest-coverage"

# 2. Generate .gcov files from source  
gcov src/*.f90

# 3. Create coverage report
fortcov --source=. --exclude='build/*' --exclude='test/*' --output=coverage.md
```

That's it! Open `coverage.md` to see your coverage report.

## Installation

### From Source (Recommended)

```bash
git clone https://github.com/lazy-fortran/fortcov.git
cd fortcov
fpm build --profile release

# Optional: Add to PATH
sudo ln -s $(pwd)/build/gfortran_*/app/fortcov /usr/local/bin/fortcov
```

### Prerequisites

- **Fortran compiler**: gfortran 9.0+ recommended
- **Fortran Package Manager**: [Install fpm](https://github.com/fortran-lang/fpm)
- **gcov**: Usually included with gcc/gfortran

## Usage

### Basic Usage

The most common workflow:

```bash
# Generate coverage report for your src/ directory
fortcov --source=src --output=coverage.md
```

### Common Use Cases

#### For CI/CD Pipelines

```bash
# Set coverage threshold and use quiet mode
fortcov --source=src --fail-under=80 --quiet --output=coverage.md

# Exit codes: 0=success, 1=error, 2=coverage below threshold
echo "Coverage check result: $?"
```

#### For Large Projects

```bash
# Exclude build artifacts and test files
fortcov --source=src --exclude='build/*' --exclude='test/*' --output=coverage.md

# Process specific directories
fortcov --source=src/core --source=src/utils --output=coverage.md
```

#### For Interactive Analysis

```bash
# Launch terminal user interface for browsing coverage
fortcov --source=src --tui

# Generate multiple output formats
fortcov --source=src --output-format=json --output=coverage.json
fortcov --source=src --output-format=html --output=coverage.html
```

### Configuration File

For complex projects, use a configuration file:

```bash
# Generate a sample configuration
cp fortcov.nml.example fortcov.nml

# Edit fortcov.nml to match your project
# Then run:
fortcov --config=fortcov.nml
```

### Import/Export Workflow

```bash
# Export coverage data to JSON for processing
fortcov --source=src --output-format=json --output=baseline.json

# Compare coverage between releases  
fortcov --diff=baseline.json,current.json --threshold=5.0 --output=diff.md
```

### Example Output

```markdown
| Filename                          |   Stmts |   Miss | Cover   | Missing            |
|-----------------------------------|---------|--------|---------|--------------------|
| src/module_a.f90                  |     100 |     10 | 90.00%  | 45-48, 72-77       |
| src/module_b.f90                  |      50 |      0 | 100.00% |                    |
| TOTAL                             |     150 |     10 | 93.33%  |                    |
```

## Command Reference

### Essential Options

| Option | Short | Description | Example |
|--------|-------|-------------|---------|
| `--source=PATH` | `-s` | Source directory | `--source=src` |
| `--output=FILE` | `-o` | Output file | `--output=coverage.md` |
| `--fail-under=N` | `-t` | Coverage threshold | `--fail-under=80` |
| `--quiet` | `-q` | Suppress output | `--quiet` |
| `--verbose` | `-v` | Detailed output | `--verbose` |
| `--help` | `-h` | Show help | `--help` |

### All Options

```bash
fortcov --help  # See complete list of options
```

## Troubleshooting

### Common Issues

#### ❌ "No coverage files found"

**Cause**: Missing `.gcov` files or wrong source path

**Solution**:
```bash
# 1. Verify you built with coverage flags
fpm build --flag "-fprofile-arcs -ftest-coverage"

# 2. Run your tests to generate .gcda files  
fpm test --flag "-fprofile-arcs -ftest-coverage"

# 3. Generate .gcov files  
gcov src/*.f90

# 4. Run fortcov
fortcov --source=. --exclude='build/*' --exclude='test/*' --output=coverage.md
```

#### ❌ "Command not found: fortcov"

**Cause**: FortCov not in PATH

**Solution**:
```bash
# Option 1: Use fpm run
fpm run -- --source=src --output=coverage.md

# Option 2: Run directly  
./build/gfortran_*/app/fortcov --source=src

# Option 3: Add to PATH (expand glob pattern first)
export PATH="$PATH:$(echo $(pwd)/build/gfortran_*/app)"
```

#### ❌ "Permission denied"

**Cause**: Insufficient file permissions

**Solution**:
```bash
# Check source directory permissions
ls -la src/

# Check output directory is writable
touch coverage.md && rm coverage.md

# Fix permissions if needed
chmod -R 644 src/
chmod 755 $(dirname coverage.md)
```

#### ❌ "File too large" or "Memory exhaustion"

**Cause**: Very large coverage files

**Solution**:
```bash
# Check file sizes
find . -name "*.gcov" -exec ls -lh {} \; | sort -k5 -hr | head -5

# Process in smaller batches
fortcov --source=src/core --output=core.md
fortcov --source=src/utils --output=utils.md

# Or clean up and regenerate coverage data  
find . -name "*.gcov" -size +10M -delete
fpm test --flag "-fprofile-arcs -ftest-coverage"
```

### Getting Help

- **Built-in help**: `fortcov --help`
- **Validate config**: `fortcov --config=fortcov.nml --verbose`  
- **Debug mode**: `fortcov --source=src --verbose` for detailed output
- **GitHub Issues**: Report bugs at [github.com/lazy-fortran/fortcov/issues](https://github.com/lazy-fortran/fortcov/issues)

## CI/CD Integration

### GitHub Actions

```yaml
name: Coverage
on: [push, pull_request]
jobs:
  coverage:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3
    - name: Setup Fortran
      uses: fortran-lang/setup-fortran@v1
    - name: Install FPM
      uses: fortran-lang/setup-fpm@v1
    - name: Build with coverage
      run: |
        fpm build --flag "-fprofile-arcs -ftest-coverage"
        fpm test --flag "-fprofile-arcs -ftest-coverage"
    - name: Generate coverage report
      run: |
        gcov src/*.f90
        fortcov --source=. --exclude='build/*' --exclude='test/*' --output=coverage.md --fail-under=80 --quiet
    - name: Upload coverage
      uses: actions/upload-artifact@v3
      with:
        name: coverage-report
        path: coverage.md
```

### GitLab CI

```yaml
coverage:
  script:
    - fpm build --flag "-fprofile-arcs -ftest-coverage"
    - fpm test --flag "-fprofile-arcs -ftest-coverage"  
    - gcov src/*.f90
    - fortcov --source=. --exclude='build/*' --exclude='test/*' --output=coverage.md --quiet
  artifacts:
    paths:
      - coverage.md
```

## Contributing

We welcome contributions! For detailed development information see [DESIGN.md](DESIGN.md).

### Quick Development Setup

```bash
git clone https://github.com/lazy-fortran/fortcov.git
cd fortcov

# Build and test
fpm build
fpm test

# Test with coverage
fpm build --flag "-fprofile-arcs -ftest-coverage"  
fpm test --flag "-fprofile-arcs -ftest-coverage"
gcov src/*.f90  
fortcov --source=. --exclude='build/*' --exclude='test/*' --output=coverage.md
```

### Guidelines

- Follow Test-Driven Development (TDD)
- Keep functions small and focused
- Use existing code style (88 char lines, 4-space indent)
- Update documentation for new features

## Links

- **Documentation**: [DESIGN.md](DESIGN.md) - Detailed architecture and design
- **Issues**: [GitHub Issues](https://github.com/lazy-fortran/fortcov/issues) - Bug reports and feature requests
- **FPM**: [Fortran Package Manager](https://github.com/fortran-lang/fpm) - Build system