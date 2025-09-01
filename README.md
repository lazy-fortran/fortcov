# FortCov - Fortran Coverage Analysis Tool

Fortran code coverage analysis tool that generates coverage reports from gcov data. FortCov can auto-discover coverage artifacts and securely invoke `gcov` for you in zero-config/auto-discovery modes, or you can provide `.gcov` files explicitly.

## Quick Start

```bash
# Install prerequisites 
sudo apt install gfortran  # or: brew install gcc (macOS)
# Note: json-fortran dependency is automatically fetched by FPM

# Install FortCov
git clone https://github.com/lazy-fortran/fortcov.git
cd fortcov && fpm build --profile release
# Install the first built binary found (handles multiple build dirs)
sudo install -m 0755 "$(find build -type f -path '*/app/fortcov' | head -n1)" /usr/local/bin/fortcov

# Zero-config (auto) coverage generation
cd your-fortran-project
# IMPORTANT: FPM does not have a --coverage flag. Use --flag with compiler options:
fpm test --flag "-fprofile-arcs -ftest-coverage"

# Then let FortCov auto-discover and run gcov securely
fortcov                        # zero-config mode: discovers build, runs gcov, analyzes

# Or generate a markdown report directly
fortcov --output=coverage.md   # writes coverage.md (markdown report)

# Explicit mode (optional): provide coverage files or source roots if you prefer
# fortcov --source=src *.gcov --output=coverage.md
```

## Manual Workflow (Fallback)

If auto-discovery cannot run `gcov` in your environment (unusual build layouts
or strict sandboxing), generate `.gcov` files yourself and pass them to FortCov:

```bash
# 1) Instrument and run tests
fpm test --flag "-fprofile-arcs -ftest-coverage"

# 2) Generate .gcov files from each build directory (avoids timestamp issues)
project_root="$(pwd)"
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
  (
    cd "$dir" && gcov *.gcno >/dev/null 2>&1 || true
    mv ./*.gcov "$project_root" 2>/dev/null || true
  )
done

# 3) Analyze
fortcov --source=src *.gcov --output=coverage.md
```

## Documentation

Complete documentation is available in the [`doc/`](doc/) directory:

### Getting Started
- **[Installation Guide](doc/user/installation.md)** - Detailed installation instructions for all platforms
- **[Getting Started](doc/user/getting-started.md)** - Step-by-step tutorial and first coverage report
- **[Usage Guide](doc/user/usage-guide.md)** - Command-line options and basic usage patterns

### Integration & Configuration
- **[Configuration](doc/user/configuration.md)** - Configuration files and customization options
- **[Examples](doc/user/examples.md)** - Build system integration examples (FPM, CMake, Make, etc.)
- **[Troubleshooting](doc/user/troubleshooting.md)** - Common issues and solutions

### Developer Documentation
- **[Development Guide](doc/developer/development-guide.md)** - Contributing to FortCov
- **[API Reference](doc/developer/api-reference.md)** - Internal API documentation
- **[Architecture](doc/developer/architecture.md)** - System design and architecture
- **[Testing Guide](doc/developer/testing.md)** - Testing patterns and best practices
- **[Fork Bomb Prevention](doc/developer/fork-bomb-prevention.md)** - Testing safety guidelines

## Features

- **Automatic gcov integration**: discovers `.gcda/.gcno` and securely runs `gcov`
- **Multiple output formats**: text, markdown (default), json, html, xml
- **JSON-fortran integration**: Robust JSON processing using json-fortran library
- **Build system integration**: Works with FPM, CMake, Make, and custom build systems  
- **CI/CD ready**: Designed for automated testing pipelines
- **Security focused**: Path validation, command injection prevention
- **Performance optimized**: Fast analysis of large codebases

## Example Output

```bash
$ fortcov --output=coverage.md
📊 Analyzing coverage...
🔍 Orchestrating coverage file discovery...
📦 Using build system integration for discovery: fpm
✅ Discovered 3 coverage files

Coverage Statistics:
  Line Coverage:  72.86%
  Lines Covered: 51 of 70 lines
```

## Quick Examples

**Basic usage (auto):**
```bash
# NOTE: Use --flag, not --coverage (which doesn't exist in FPM)
fpm test --flag "-fprofile-arcs -ftest-coverage"
fortcov                       # Auto-discover + run gcov + analyze
fortcov --output=coverage.md  # Write markdown report
```

**Generate markdown report (explicit files):**
```bash
fortcov --source=src *.gcov --output=coverage.md  # Creates detailed markdown report
```

**Current Implementation Status**: Coverage analysis and markdown output are fully functional. JSON output uses json-fortran library for robust processing. Other formats (html, xml) are supported with basic implementations.

**CI/CD integration:**
```bash
# Coverage analysis with threshold checking - returns proper exit codes
fortcov --source=src *.gcov --fail-under 80 --output=coverage.md
echo "Exit code: $?"  # Returns 0=success, 2=threshold not met, 3=no coverage data

# Example CI/CD usage
if ! fortcov --source=src *.gcov --fail-under 80 --quiet; then
    echo "Coverage below 80% threshold - build failed"
    exit 1
fi
```

### Tips
- Prefer zero-config in CI when possible: run `fpm test` with coverage flags, then `fortcov --fail-under 80 --output=coverage.md`.
- If your CI restricts running `gcov`, use the Manual Workflow above and pass `.gcov` files explicitly.

## Known Issues

### FPM Coverage Instrumentation
FPM's coverage support has limitations that may prevent proper `.gcda` file generation:
- `.gcno` files may be cleaned or become incompatible during test execution
- Build artifacts may be modified between compilation and testing
- Coverage data may show "No executable lines" errors

**Workaround**: See the [Coverage Workflow Guide](doc/user/coverage-workflow.md) for alternative approaches and troubleshooting steps.

## Contributing

1. Fork the repository
2. Create a feature branch: `git checkout -b feature-name`
3. Make changes and add tests
4. Run the test suite: `fpm test`
5. Submit a pull request

For detailed contribution guidelines, see [`doc/developer/development-guide.md`](doc/developer/development-guide.md).

## License

MIT License. See [LICENSE](LICENSE) file for details.

---

**Need help?** Check the [troubleshooting guide](doc/user/troubleshooting.md) or [open an issue](https://github.com/lazy-fortran/fortcov/issues).
