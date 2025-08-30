# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

FortCov is a comprehensive Fortran code coverage analysis tool that processes gcov data and generates detailed reports in multiple formats (markdown, JSON, HTML, XML). It's designed for modern Fortran projects using FPM, with extensive security features, TUI mode, zero-configuration workflows, and comprehensive CI/CD integration.

## Essential Development Commands

### Build and Test
```bash
# Build the project
fpm build

# Build with coverage instrumentation
fpm build --flag "-fprofile-arcs -ftest-coverage"

# Run all tests (CI-friendly with timeout handling)
./run_ci_tests.sh

# Run specific test by name  
fmp test test_name

# Run tests with verbose output
fpm test --verbose

# Generate coverage for the project itself
fpm test --flag "-fprofile-arcs -ftest-coverage"
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
  gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
done
fortcov --source=src *.gcov --output=coverage.md

# Run integration validation examples
./validate_integration_examples.sh
```

### Application Usage
```bash
# Basic coverage analysis
./build/gfortran_*/app/fortcov --source=src *.gcov

# Generate markdown report
./build/gfortran_*/app/fortcov --source=src *.gcov --output=coverage.md

# Run in Terminal User Interface mode
./build/gfortran_*/app/fortcov --tui

# Zero-configuration mode (auto-discovery)
./build/gfortran_*/app/fortcov --zero-config

# With coverage threshold
./build/gfortran_*/app/fortcov --source=src *.gcov --fail-under=80

# Quiet mode for CI
./build/gfortran_*/app/fortcov --source=src *.gcov --quiet --fail-under=80
```

### Development Utilities
```bash
# Performance testing
./scripts/test_performance_optimization.sh

# Validate build examples
./scripts/validate_commands.sh

# FPM coverage bridge (handle FPM coverage limitations)  
./scripts/fpm_coverage_bridge.sh

# Version management
./scripts/version_manager.sh
```

## Architecture Overview

### Core Modules (src/)
- **config/**: Configuration parsing, validation, and command-line processing
  - `config_core.f90` - Main configuration management
  - `config_parser_*.f90` - Command line and file parsing
  - `config_validation_*.f90` - Validation and security checks

- **core/**: Foundation architecture and system management  
  - `architectural_core.f90` - System architecture enforcement
  - `system_protection.f90` - Security and safety systems
  - `tui_manager_core.f90` - Terminal User Interface coordination
  - `coverage_calculation_utils.f90` - Core coverage math

- **coverage/**: Coverage data processing and analysis
  - `coverage_data_builder.f90` - Coverage data construction 
  - `coverage_file_loader.f90` - File loading and parsing
  - `coverage_processor_file.f90` - File-level coverage processing
  - `coverage_tui.f90` - TUI interface for coverage analysis

- **gcov/**: Gcov file processing and generation
  - `gcov_executor.f90` - Execute gcov commands safely
  - `gcov_file_discovery.f90` - Auto-discover coverage files
  - `gcov_generation_utils.f90` - Coverage data generation utilities

- **utils/**: Cross-cutting utilities and security
  - `auto_discovery_utils.f90` - Project structure auto-detection
  - `command_utils_core.f90` - Safe command execution  
  - `directory_ops_core.f90` - Directory operations with security
  - `file_ops_secure.f90` - Secure file operations
  - `path_security_core.f90` - Path validation and sanitization
  - `path_utils_consolidated.f90` - Path manipulation utilities

- **reporters/**: Output format generation
  - `html_reporter.f90` - HTML report generation

- **syntax/**: Source code syntax processing
  - `syntax_lexer.f90` - Fortran syntax analysis

- **security/**: Security and safety enforcement
  - Advanced security validation and attack prevention

- **zero_config/**: Zero-configuration workflow
  - `zero_config_core.f90` - Auto-discovery and minimal-setup workflows

### Application Entry Point
- **app/main.f90**: Comprehensive main application with:
  - Command-line argument processing
  - Configuration validation and security checks  
  - Fork bomb prevention (critical safety feature)
  - Multiple execution modes (standard, TUI, zero-config)
  - Clean exit handling with proper error codes

### Test Infrastructure
- **test/**: Comprehensive test suite (90+ test files)
  - Unit tests for all core modules
  - Integration tests for build system compatibility
  - Security vulnerability tests
  - Performance and stress tests
  - Fork bomb prevention tests
  - CLI validation tests
  - Memory allocation and error handling tests

## Critical Safety Features

### Fork Bomb Prevention
**⚠️ MANDATORY SAFETY PROTOCOL**: This project includes sophisticated fork bomb prevention due to test-within-test execution risks.

- **Detection**: Automatic detection of recursive test execution
- **Prevention**: `.fortcov_execution_marker` files prevent recursive execution
- **Test Exclusions**: Known problematic tests are excluded in `run_ci_tests.sh`
- **Safety Script**: `run_ci_tests.sh` includes comprehensive test isolation

**Key Prevention Measures:**
- Tests that hang or cause infinite recursion are automatically excluded
- Timeout handling (10 seconds per test) prevents CI hangs  
- Clean execution markers prevent cascading failures
- Safe test runner with robust error handling

### Security Architecture
- **Path Security**: Comprehensive path validation and sanitization
- **Command Injection Prevention**: Safe command execution with validation
- **File Access Control**: Security-first file operations
- **Input Validation**: Extensive input sanitization throughout

## Configuration

### Configuration File (fortcov.nml)
Uses Fortran namelist format for configuration:
```fortran
&fortcov_config
    source_paths = 'src/', 'app/'
    exclude_patterns = '*.mod', '*.o', 'build/*', 'test/*'
    output_format = 'markdown'  ! or 'text', 'json', 'html', 'xml'
    output_path = 'coverage.md'
    minimum_coverage = 80.0
    quiet = .false.
    verbose = .false.
/
```

### Command Line Options
- `--source=PATH`: Source directories to analyze
- `--output=FILE`: Output file path  
- `--format=FORMAT`: Output format (text/markdown/json/html/xml)
- `--fail-under=PERCENT`: Coverage threshold for CI
- `--quiet`: Suppress non-essential output
- `--verbose`: Enable detailed output
- `--tui`: Launch Terminal User Interface
- `--zero-config`: Auto-discovery mode
- `--help`: Show help information
- `--version`: Show version information

## Multi-Format Output Support

### Supported Formats
1. **Markdown** (default): Human-readable reports with tables and statistics
2. **JSON**: Machine-readable for CI/CD integration and tooling
3. **HTML**: Rich web-based reports with syntax highlighting  
4. **XML**: Cobertura-compatible format for universal tool support
5. **Text**: Plain text console output

### Example Outputs
```bash
# Generate different formats
fortcov --source=src *.gcov --format=markdown --output=coverage.md
fortcov --source=src *.gcov --format=json --output=coverage.json  
fortcov --source=src *.gcov --format=html --output=coverage.html
fortcov --source=src *.gcov --format=xml --output=coverage.xml
```

## Build System Integration

FortCov supports multiple build systems with comprehensive examples:

### FPM (Fortran Package Manager) - Primary
```bash
# Standard workflow
fpm test --flag "-fprofile-arcs -ftest-coverage"
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
  gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true  
done
fortcov --source=src *.gcov
```

### CMake Integration
```cmake
# Enable coverage in CMakeLists.txt
set(CMAKE_Fortran_FLAGS_TESTING "-fprofile-arcs -ftest-coverage")
add_custom_target(fortcov_report
    COMMAND fortcov --source=src *.gcov --output=coverage.md
)
```

### Makefile Integration  
```makefile
COVERAGE_FLAGS = -fprofile-arcs -ftest-coverage

coverage:
	$(FC) $(COVERAGE_FLAGS) $(SOURCES) -o $(TARGET)
	./$(TARGET)
	gcov $(SOURCES)
	fortcov --source=. *.gcov
```

## CI/CD Integration

### GitHub Actions
- **Workflow**: `.github/workflows/ci.yml` provides complete CI setup
- **Features**: Multi-compiler testing, timeout handling, detailed reporting
- **Safety**: Includes fork bomb prevention and proper error handling

### Exit Codes  
FortCov returns meaningful exit codes for CI/CD:
- `0`: Success 
- `1`: General failure
- `2`: Coverage threshold not met
- `3`: No coverage data found  
- `4`: Invalid configuration
- `5`: File access errors
- `6`: Memory errors
- `7`: Validation errors

## Development Workflow

### Making Changes
1. **Read existing code**: Always examine current implementation before changes
2. **Follow safety protocols**: Respect fork bomb prevention measures  
3. **Test comprehensively**: Use `./run_ci_tests.sh` for thorough testing
4. **Security first**: All file operations must use secure utilities
5. **Update tests**: Add appropriate unit and integration tests

### Code Organization Principles
- **Modular Design**: Each module has a single, clear responsibility
- **Security by Default**: All operations assume hostile input
- **Error Resilience**: Comprehensive error handling throughout
- **Performance Awareness**: Efficient processing of large codebases
- **Cross-platform Compatibility**: Works on Linux, macOS, Windows

## Testing Strategy

### Test Categories
1. **Unit Tests**: Individual module functionality
2. **Integration Tests**: Multi-module workflows  
3. **Security Tests**: Vulnerability and attack prevention
4. **Performance Tests**: Large file and stress testing
5. **CLI Tests**: Command-line interface validation
6. **Memory Tests**: Allocation and cleanup verification
7. **Fork Bomb Tests**: Recursive execution prevention

### Test Execution
- **Primary**: `./run_ci_tests.sh` (excludes problematic tests)
- **Individual**: `fpm test test_name` for specific tests
- **Full Suite**: `fpm test --verbose` (may include hanging tests)
- **Performance**: `./scripts/test_performance_optimization.sh`

## Dependencies

### Required
- **gfortran**: GNU Fortran compiler with gcov support
- **FPM**: Fortran Package Manager for builds  
- **json-fortran**: JSON processing (auto-fetched by FPM)

### Optional
- **lcov/gcovr**: For comparison testing
- **pycobertura**: For external format validation

## Key Features

### Advanced Capabilities
- **Zero Configuration**: Auto-discovery of project structure and build systems
- **Terminal UI**: Interactive coverage analysis and exploration  
- **Multi-format Output**: Comprehensive reporting in 5+ formats
- **Security Hardened**: Protection against common attacks and vulnerabilities
- **CI/CD Optimized**: Proper exit codes, quiet modes, and threshold checking
- **Performance Scaled**: Handles large codebases efficiently  
- **Cross-platform**: Linux, macOS, Windows compatibility

### Production Ready
- **Memory Safe**: Comprehensive allocation tracking and cleanup
- **Error Resilient**: Graceful handling of all error conditions  
- **Path Secure**: Validation and sanitization of all file paths
- **Command Safe**: Protection against injection attacks
- **Fork Safe**: Prevention of recursive execution bombs
- **CI Safe**: Timeout handling and proper process management