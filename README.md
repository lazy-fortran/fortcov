# FortCov - Fortran Code Coverage Analysis Tool

Fast, robust Fortran code coverage analysis with gcov integration, comprehensive reporting, and build system integration.

## Quick Navigation

### For Users
- [Installation](doc/user/installation.md) - Setup procedures and requirements
- [Getting Started](doc/user/getting-started.md) - Quick start with working examples  
- [Usage Guide](doc/user/usage-guide.md) - Comprehensive usage documentation
- [Examples](doc/user/examples.md) - Practical usage examples
- [Troubleshooting](doc/user/troubleshooting.md) - Problem resolution guide
- [Configuration](doc/user/configuration.md) - Configuration reference

### For Developers
- [Architecture](doc/developer/architecture.md) - System design and architecture
- [API Reference](doc/developer/api-reference.md) - Programming interface documentation
- [Build Integration](doc/developer/build-integration.md) - Build system integration patterns
- [Development Guide](doc/developer/development-guide.md) - Contributor guidance
- [Testing](doc/developer/testing.md) - Testing strategies and frameworks

### Implementation Details
- [Design Decisions](doc/implementation/design-decisions.md) - Architectural decision records
- [Performance](doc/implementation/performance.md) - Performance analysis and optimization
- [Security](doc/implementation/security.md) - Security architecture and considerations

## Quick Start

### Zero-Configuration Usage (Recommended)

```bash
# Simplest possible workflow:
fpm test --flag "-fprofile-arcs -ftest-coverage"
gcov -o build/gcov src/*.f90
fortcov  # That's it!

# Find your report in: build/coverage/coverage.md
```

### Traditional Usage (Still Supported)

```bash
# Explicit configuration for custom workflows:
fpm test --flag "-fprofile-arcs -ftest-coverage"
gcov src/*.f90
fortcov --source=. --exclude='build/*' --exclude='test/*' --output=coverage.md
```

## Key Features

- **Zero-Configuration Mode**: Run `fortcov` without arguments for instant analysis
- **Smart Auto-Discovery**: Automatically finds coverage and source files
- **Fast Analysis**: O(n) performance optimized for large codebases
- **Multiple Output Formats**: Markdown, HTML, JSON, XML
- **Build System Integration**: FPM, CMake, Make, Meson support
- **CI/CD Ready**: GitHub Actions, GitLab CI, Jenkins integration
- **Comprehensive Reporting**: Line, function, and branch coverage
- **Differential Analysis**: Coverage comparison between versions
- **Security Hardened**: Input validation and memory safety

## License

MIT License - see LICENSE file for details.

## Contributing

See [Development Guide](doc/developer/development-guide.md) for contributor information.