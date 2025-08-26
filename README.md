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

# Zero-configuration mode (auto-discovers coverage files)
cd your-fortran-project
fpm test --flag "-fprofile-arcs -ftest-coverage"
fortcov

# Manual mode with specific files
gcov src/*.f90
fortcov --source . --exclude build/* --exclude test/* --output coverage.md
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

- **Zero-configuration mode**: Auto-discovers coverage files and generates reports
- **Multiple output formats**: Markdown, JSON, XML, HTML, LCOV, Cobertura
- **Build system integration**: Works with FPM, CMake, Make, and custom build systems  
- **CI/CD ready**: Designed for automated testing pipelines
- **Security focused**: Path validation, command injection prevention
- **Performance optimized**: Fast analysis of large codebases

## Example Output

```bash
$ fortcov
FortCov: Starting zero-configuration coverage analysis...
FortCov: Auto-discovering coverage files...
FortCov: Found 5 .gcov files in build/gcov/
FortCov: Analyzing coverage data...
FortCov: Coverage report generated: build/coverage/coverage.md

Coverage Summary:
  Total lines: 1,234
  Covered lines: 1,050
  Coverage: 85.1%
```

## Quick Examples

**Zero-configuration (recommended):**
```bash
fpm test --flag "-fprofile-arcs -ftest-coverage"
fortcov  # Auto-discovers coverage files and shows terminal output
```

**Manual file specification:**
```bash
fortcov *.gcov  # Analyze gcov files with terminal output
```

**Note**: File output formats (--output, --format) are currently affected by issue #396 and show "would be generated" instead of creating actual files. Terminal output works correctly.

**CI/CD integration:**
```bash
fortcov --minimum 80 --fail-under 80  # Fail if coverage < 80%
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