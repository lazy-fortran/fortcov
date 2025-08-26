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

# Manual coverage generation
cd your-fortran-project
fpm test --flag "-fprofile-arcs -ftest-coverage"

# Manual gcov file generation (if needed)
fpm test --flag "-fprofile-arcs -ftest-coverage"
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
  gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
done
fortcov --source=src *.gcov
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

- **Multiple output formats**: text, markdown (default), json, html, xml
- **Build system integration**: Works with FPM, CMake, Make, and custom build systems  
- **CI/CD ready**: Designed for automated testing pipelines
- **Security focused**: Path validation, command injection prevention
- **Performance optimized**: Fast analysis of large codebases

## Example Output

```bash
$ fortcov --source=src *.gcov
📊 Analyzing coverage...
Found coverage file 1: demo_calculator.f90.gcov
Found coverage file 2: main.f90.gcov
Found coverage file 3: test_demo.f90.gcov
Found 3 coverage files

Coverage Statistics:
  Line Coverage:  72.86%
  Lines Covered: 51 of 70 lines
```

## Quick Examples

**Basic usage:**
```bash
fpm test --flag "-fprofile-arcs -ftest-coverage"
# Generate .gcov files manually:
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
  gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
done
fortcov --source=src *.gcov  # Shows terminal coverage output
```

**Manual file specification:**
```bash
fortcov --source=src *.gcov  # Analyze gcov files with terminal output
```

**Current Implementation Status**: Text coverage analysis is fully functional. File output formats (markdown, json, html, xml) display "would be generated" but do not create actual files yet.

**CI/CD integration:**
```bash
fortcov --source=src *.gcov --fail-under 80  # Fail if coverage < 80%
```

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