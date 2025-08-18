# Enhanced CLI Guide

This guide documents FortCov's enhanced command-line interface, which provides simplified workflows while maintaining full backward compatibility.

## Quick Reference

### Simplified Usage (Recommended)
```bash
fortcov file.gcno                 # Process single coverage file
fortcov *.gcno                    # Process multiple files with wildcards
```

### Advanced Usage (Full Control)
```bash
fortcov --source=src --output=report.md --fail-under=80
```

## Auto-gcov Mode

FortCov automatically detects when .gcno files are provided as positional arguments and enables **auto-gcov mode**, which:

1. **Detects .gcno files** in the command line arguments
2. **Searches for corresponding .gcda files** in the same directory
3. **Extracts source directories** from .gcno file paths automatically
4. **Executes gcov command** securely for each .gcno/.gcda pair
5. **Processes generated .gcov files** using the standard coverage pipeline

### Example Auto-gcov Workflow
```bash
# After compiling with coverage and running your program:
ls *.gc*
# Output: program.gcno program.gcda

# Auto-gcov mode activates automatically:
fortcov program.gcno

# What happens internally:
# 1. Detects program.gcno -> enables auto-gcov mode
# 2. Finds program.gcda in same directory
# 3. Extracts source directory from file path
# 4. Runs: gcov program.gcno (in appropriate directory)
# 5. Processes generated program.f90.gcov file
# 6. Generates coverage report
```

## Source Directory Auto-detection

When using .gcno files, FortCov automatically discovers source directories:

```bash
# These .gcno files:
build/debug/src/module_a.f90.gcno
build/debug/lib/module_b.f90.gcno

# Auto-detect these source directories:
build/debug/src/
build/debug/lib/

# Equivalent to manually specifying:
fortcov --source=build/debug/src --source=build/debug/lib *.gcov
```

## Backward Compatibility

All existing flag-based workflows continue to work unchanged:

```bash
# Traditional workflows still supported
fortcov --input-format=gcov --output-format=markdown *.gcov
fortcov --source=src --exclude="*.mod" --verbose *.gcov
```

The enhanced CLI detects the input type and chooses the appropriate processing mode:
- **.gcno files**: Auto-gcov mode with automatic gcov execution
- **.gcov files**: Traditional gcov text file processing
- **Flags only**: Directory-based coverage file discovery

## Error Handling

### Missing .gcda Files
When .gcno files lack corresponding .gcda files:
```bash
fortcov program.gcno
# Output: Warning: No runtime data found for program.gcno
#         Expected .gcda file: program.gcda
#         Run your program first to generate runtime coverage data
```

### Gcov Execution Issues
When gcov command fails or produces no output:
```bash
fortcov program.gcno --verbose
# Output: Warning: gcov reported issues for program.gcno
#         Message: No .gcov files were generated
#         Continuing with generated files...
```

### No Coverage Data
When no valid coverage files are found:
```bash
fortcov *.gcno --strict
# Exit code: 3 (EXIT_NO_COVERAGE_DATA)

fortcov *.gcno  # default mode
# Exit code: 0 (EXIT_SUCCESS)
# Output: Warning: No coverage files found
```

## Integration with Atomic Temp Files

The auto-gcov mode integrates seamlessly with FortCov's atomic temporary file system:

- **Secure gcov execution**: Uses secure command executor to prevent injection attacks
- **Atomic file operations**: Generated .gcov files handled through atomic temp file manager
- **Clean resource management**: Automatic cleanup of temporary files using RAII patterns
- **Cross-platform security**: Unix/Linux and Windows secure file handling

## Configuration Examples

### Simple Project
```bash
# Just analyze coverage - auto-detects everything
fortcov *.gcno
```

### Multi-directory Project
```bash
# Auto-detects source directories from .gcno paths
fortcov build/*/src/*.gcno build/*/lib/*.gcno
```

### Quality Gate Integration
```bash
# Fail CI/CD if coverage below threshold
fortcov *.gcno --fail-under=80 --strict --quiet
```

### Custom Gcov Tool
```bash
# Use specific gcov version with additional arguments
fortcov *.gcno --gcov=/usr/bin/gcov-11 --gcov-args="--branch-probabilities"
```

### Multiple Output Formats
```bash
# Generate HTML report with custom filename
fortcov *.gcno --output-format=html --output=coverage_report.html

# Generate JSON for CI/CD integration
fortcov *.gcno --output-format=json --output=coverage.json
```

## Best Practices

### 1. Use Simplified CLI for Common Cases
```bash
# Preferred for most projects
fortcov *.gcno

# Instead of verbose flag-based approach
fortcov --input-format=gcov --source=. *.gcov
```

### 2. Leverage Auto-detection
Let FortCov discover source directories automatically rather than specifying manually:
```bash
# Auto-detection (recommended)
fortcov build/debug/src/*.gcno

# Manual specification (unnecessary)
fortcov --source=build/debug/src build/debug/src/*.gcno
```

### 3. Combine with Quality Gates
```bash
# CI/CD integration
fortcov *.gcno --fail-under=80 --output-format=json --output=coverage.json
```

### 4. Use Verbose Mode for Debugging
```bash
# Troubleshooting coverage issues
fortcov *.gcno --verbose
```

## Migration from Traditional Tools

### From gcovr
```bash
# gcovr approach
gcovr --root . --html --html-details --output coverage.html

# FortCov equivalent
fortcov *.gcno --output-format=html --output=coverage.html
```

### From lcov
```bash
# lcov approach
lcov --capture --directory . --output-file coverage.info
genhtml coverage.info --output-directory coverage_html

# FortCov equivalent
fortcov *.gcno --output-format=html --output=coverage.html
```

## Troubleshooting

### Command Not Found
```bash
# Ensure FortCov is built and in PATH
fpm build --profile release
export PATH=$PATH:./build/gfortran_*/app
```

### No Coverage Data Generated
```bash
# Verify compilation flags
gfortran -fprofile-arcs -ftest-coverage program.f90

# Ensure program executed
./a.out

# Check for .gcda files
ls *.gcda
```

### Gcov Version Mismatch
```bash
# Use matching gcov version
fortcov *.gcno --gcov=/usr/bin/gcov-11  # if compiled with gfortran-11
```

### Permission Issues
```bash
# Ensure write permissions for .gcov files
chmod u+w .
fortcov *.gcno
```