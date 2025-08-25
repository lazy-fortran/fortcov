# FortCov - Fortran Coverage Analysis Tool

Fortran code coverage analysis tool that generates markdown reports from gfortran gcov data.

## Quick Start

```bash
# Install prerequisites 
sudo apt install gfortran  # or: brew install gcc (macOS)

# Install FortCov
git clone https://github.com/lazy-fortran/fortcov.git
cd fortcov && fpm build --profile release
sudo cp build/gfortran_*/app/fortcov /usr/local/bin/

# Generate coverage (zero-configuration)
cd your-fortran-project
fpm test --flag "-fprofile-arcs -ftest-coverage"
find build -name "*.gcda" | while read gcda_file; do
  gcov -b "$gcda_file" 2>/dev/null || true
done
fortcov --source=.  # Discovers .gcov files, creates coverage.md
```

## Example Usage

**Simple Project:**

```bash
# Create project
mkdir calculator && cd calculator
cat > fpm.toml << 'EOF'
name = "calculator"
version = "0.1.0"
EOF
mkdir -p src test

# Source code (src/calculator.f90)
cat > src/calculator.f90 << 'EOF'
module calculator
    implicit none
    public :: add
contains
    function add(a, b) result(c)
        real, intent(in) :: a, b
        real :: c
        c = a + b
    end function add
end module calculator
EOF

# Test code (test/test_calculator.f90)  
cat > test/test_calculator.f90 << 'EOF'
program test_calculator
    use calculator
    implicit none
    if (abs(add(2.0, 3.0) - 5.0) > 1e-6) stop 1
    print *, 'Test passed!'
end program test_calculator
EOF

# Generate coverage
fpm test --flag "-fprofile-arcs -ftest-coverage"
find build -name "*.gcda" | while read gcda_file; do
  gcov -b "$gcda_file" 2>/dev/null || true
done
fortcov --source=. --fail-under=80
```

**Output:**
```markdown
| Filename | Stmts | Covered | Cover | Missing |
|----------|-------|---------|-------|---------|
| src/calculator.f90 | 3 | 3 | 100.00% | |
| TOTAL | 3 | 3 | 100.00% | |
```

## Command Line Options

```bash
# Zero-configuration (recommended)
fortcov --source=.                   # Auto-discovers .gcov files in current directory

# Traditional mode
fortcov --source=src --output=coverage.md

# CI/CD integration
fortcov --source=. --fail-under=80 --format=json --output=coverage.json

# Interactive analysis
fortcov --source=. --tui

# Coverage comparison
fortcov --source=. --diff=baseline.json,current.json --output=changes.md
```

## Installation

**Ubuntu/Debian:**
```bash
sudo apt update && sudo apt install -y build-essential gfortran git curl
curl -fsSL https://github.com/fortran-lang/fpm/releases/latest/download/fpm-0.12.0-linux-x86_64-gcc-12 -o /tmp/fpm
chmod +x /tmp/fpm && sudo mv /tmp/fpm /usr/local/bin/
git clone https://github.com/lazy-fortran/fortcov.git
cd fortcov && fpm build --profile release
sudo cp build/gfortran_*/app/fortcov /usr/local/bin/
```

**macOS:**
```bash
xcode-select --install
brew install gcc fortran-lang/tap/fpm
git clone https://github.com/lazy-fortran/fortcov.git  
cd fortcov && fpm build --profile release
sudo cp build/gfortran_*/app/fortcov /usr/local/bin/
```

## Build System Integration

**FPM (Fortran Package Manager):**
```bash
fpm test --flag "-fprofile-arcs -ftest-coverage"
find build -name "*.gcda" | while read gcda_file; do
  gcov -b "$gcda_file" 2>/dev/null || true
done
fortcov --source=.
```

**CMake:**
```cmake
set(CMAKE_Fortran_FLAGS_TESTING "-g -O0 -fprofile-arcs -ftest-coverage")
add_custom_target(coverage
    COMMAND gcov ${CMAKE_BINARY_DIR}/*.gcno
    COMMAND fortcov --source=${CMAKE_SOURCE_DIR} --output=coverage.html
)
```

**Makefile:**
```makefile
COVERAGE_FLAGS = -fprofile-arcs -ftest-coverage

coverage: test
	gcov $(SOURCES)
	fortcov --source=. --output=coverage.html

.PHONY: coverage
```

## CI/CD Integration

**GitHub Actions:**
```yaml
- name: Generate Coverage
  run: |
    fpm test --flag "-fprofile-arcs -ftest-coverage"
    find build -name "*.gcda" | while read gcda_file; do
      gcov -b "$gcda_file" 2>/dev/null || true
    done
    fortcov --source=. --fail-under=80 --format=json --output=coverage.json
```

**GitLab CI:**
```yaml
coverage:
  script:
    - fpm test --flag "-fprofile-arcs -ftest-coverage" 
    - find build -name "*.gcda" | while read gcda_file; do gcov -b "$gcda_file" 2>/dev/null || true; done
    - fortcov --source=. --format=xml --output=coverage.xml --fail-under=80
  artifacts:
    reports:
      coverage_report:
        coverage_format: cobertura
        path: coverage.xml
```

## Configuration

**Namelist file** (`fortcov.nml`):
```fortran
&fortcov_config
    source_paths = 'src/', 'lib/'
    exclude_patterns = 'test/*', '*.mod'  
    output_format = 'markdown'
    output_path = 'coverage.md'
    minimum_coverage = 80.0
    verbose = .false.
/
```

**Usage:**
```bash
fortcov --source=. --config=fortcov.nml
```

## Output Formats

- **Markdown** (default): `--format=markdown` 
- **JSON**: `--format=json`
- **XML** (Cobertura): `--format=xml`
- **HTML**: `--format=html`

## Security Features

FortCov includes comprehensive security protections:

- **Command injection prevention**: Validates all input paths
- **Path traversal protection**: Blocks dangerous directory access
- **System directory protection**: Prevents access to sensitive system paths

Use standard project structures and avoid special characters in file paths.

## Troubleshooting

**Common Issues:**

```bash
# Check prerequisites
which gfortran && echo "✅ gfortran found" || echo "❌ gfortran missing"
which gcov && echo "✅ gcov found" || echo "❌ gcov missing" 
which fortcov && echo "✅ fortcov found" || echo "❌ fortcov missing"

# Build failures
fpm --version    # Ensure FPM 0.8.0+
gfortran --version  # Ensure gfortran 9.0+

# Memory allocation errors (fixed in v2.0+)
fortcov --verbose  # Shows detailed error information

# Security errors
fortcov --source="path;rm -rf /"  # Error: Path contains dangerous characters
fortcov --source="../../../etc/"  # Error: Path contains dangerous characters
```

**Solutions:**
- Install missing prerequisites from your package manager
- Use clean, simple file paths without special characters
- Update to latest FortCov version for memory safety fixes
- Check file permissions in source directories

## Testing

### Running Tests

```bash
# Run all tests
fpm test

# Run specific test
fpm test test_coverage_engine

# Run with coverage
fpm test --flag "-fprofile-arcs -ftest-coverage"
```

### Test Infinite Loop Prevention

⚠️ **Important**: Some tests are marked with `.FORK_BOMB_DISABLED` extension to prevent infinite recursion:

```bash
# These files are DISABLED to prevent infinite loops
test/test_documentation_commands_issue_232.sh.FORK_BOMB_DISABLED
test/test_readme_gcov_workflow.sh.FORK_BOMB_DISABLED
test/test_issue_260_comprehensive.sh.FORK_BOMB_DISABLED
```

**Why**: Tests that call `fpm test` create infinite recursion (fork bombs) causing the test suite to hang.

**Solution**: Problematic tests are renamed with `.FORK_BOMB_DISABLED` extension so FPM ignores them.

### Writing Safe Tests

✅ **Good**: Test functionality directly
```fortran
program test_coverage_engine
    use coverage_engine
    call test_coverage_calculation()  ! Direct module testing
end program
```

❌ **Bad**: Recursive test calls  
```bash
#!/bin/bash
fpm test  # This causes infinite recursion!
```

For detailed testing guidelines, see [`docs/TESTING.md`](docs/TESTING.md).

## Contributing

1. Fork the repository
2. Create feature branch: `git checkout -b feature-name`
3. Write tests: Follow TDD approach (RED/GREEN/REFACTOR)
4. Implement changes: Keep functions <50 lines, files <500 lines  
5. **Ensure tests don't cause infinite loops**: Never call `fpm test` from within tests
6. Submit PR: Include working code examples

## License

MIT License - see LICENSE file for details.

## Architecture

FortCov uses a simple, proven architecture:

1. **Execute gcov** on Fortran source files
2. **Parse .gcov text files** (not binary formats)
3. **Calculate statistics** and generate reports
4. **Output multiple formats** (markdown, JSON, XML, HTML)

This approach follows LCOV methodology: leverage GCC's gcov tool rather than parsing complex binary formats.

For detailed architecture information, see `doc/design/DESIGN.md`.