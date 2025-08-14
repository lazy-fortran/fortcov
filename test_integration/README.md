# Integration Tests for fortcov

This directory contains comprehensive integration tests that validate fortcov functionality with real Fortran code and coverage data.

## Structure

```
test_integration/
├── README.md                    # This file
├── fixtures/                    # Sample Fortran programs for testing
│   ├── simple_module/          # Basic module with 100% coverage
│   ├── uncovered_procedure/    # Module with intentionally uncovered code
│   ├── nested_module/          # Complex nested modules with contains
│   ├── generic_interfaces/     # Generic interface blocks
│   ├── select_case/            # Partial branch coverage scenarios
│   ├── do_loops/               # Various do loop constructs
│   ├── array_operations/       # Array assignments and operations
│   ├── submodules/             # Modern Fortran submodules
│   └── mixed_project/          # Multi-file project with varying coverage
├── self_coverage_test.sh       # Self-coverage test script
└── compare_toolchains.py       # Comprehensive comparison with lcov pipeline
```

## Test Scenarios

### 1. Simple Module (100% Coverage)
- **Purpose**: Validate basic line coverage calculation
- **Coverage**: All procedures executed, should show 100%
- **Files**: `simple_module.f90`, `test_simple.f90`

### 2. Uncovered Procedure
- **Purpose**: Test identification of unused procedures
- **Coverage**: Main procedure used, helper unused
- **Expected**: Partial coverage with clear identification of uncovered lines

### 3. Nested Module
- **Purpose**: Test hierarchy maintenance in complex structures
- **Features**: Multiple levels of contains blocks, nested procedures
- **Expected**: Correct coverage attribution across hierarchy levels

### 4. Generic Interfaces
- **Purpose**: Test handling of interface blocks as non-executable
- **Features**: Generic interfaces, procedure overloading
- **Expected**: Interface blocks not counted as executable code

### 5. Select Case Coverage
- **Purpose**: Test partial branch coverage tracking
- **Features**: Select case with some uncovered cases
- **Expected**: Partial branch coverage with missing case identification

### 6. Do Loops
- **Purpose**: Test various loop construct coverage
- **Features**: Standard do, do while, do concurrent, named loops
- **Expected**: Proper tracking of loop body execution

### 7. Array Operations
- **Purpose**: Test array assignments and operations as executable
- **Features**: Array constructors, where constructs, slicing
- **Expected**: Array operations counted as executable lines

### 8. Submodules
- **Purpose**: Test modern Fortran submodule support
- **Features**: Parent module with implementing submodules
- **Expected**: Correct association with parent modules

### 9. Mixed Project
- **Purpose**: Test aggregation across multiple files
- **Features**: Multiple modules with varying coverage levels
- **Expected**: Accurate project-wide coverage aggregation

## Running Tests

### Basic Integration Tests
```bash
# Run the integration test suite
fmp test test_integration
```

### Self-Coverage Test
```bash
# Test fortcov analyzing its own coverage
./test_integration/self_coverage_test.sh
```

### Comprehensive Toolchain Comparison
```bash
# Compare fortcov against lcov+cobertura+pycobertura pipeline
python3 test_integration/compare_toolchains.py
```

## Test Requirements

### Dependencies
- `gfortran` with coverage support (`-fprofile-arcs -ftest-coverage`)
- `fpm` (Fortran Package Manager)

### Optional (for comparison tests)
- `lcov` - Line coverage tool
- `lcov_cobertura` - LCOV to Cobertura converter
- `pycobertura` - Cobertura XML to markdown converter

### Installation of comparison tools:
```bash
# Ubuntu/Debian
sudo apt-get install lcov

# Install Python tools
pip install lcov_cobertura pycobertura
```

## Coverage Data Handling

**Important**: No binary coverage files (*.gcno, *.gcda, *.gcov) are committed to git. All coverage data is generated at test runtime and cleaned up automatically.

The `.gitignore` rules ensure binary files are never accidentally committed:
```
*.gcno
*.gcda
*.gcov
build/
```

## Expected Outcomes

### Successful Integration Test
- All 9 test scenarios should pass
- Coverage data generated and analyzed correctly
- Reports produced in expected markdown format
- No crashes or error conditions

### Self-Coverage Validation
- fortcov can analyze its own source code
- Coverage data compatible with standard toolchain
- Reasonable coverage percentages for fortcov modules

### Toolchain Comparison
- fortcov results should be comparable to lcov pipeline
- Minor differences acceptable due to different analysis approaches
- Major discrepancies indicate potential issues

## Troubleshooting

### No Coverage Data Generated
- Ensure gfortran supports coverage (`gfortran --version`)
- Check that `-fprofile-arcs -ftest-coverage` flags are applied
- Verify test programs actually execute (not just compile)

### Tool Comparison Failures
- Install missing tools (`lcov`, `lcov_cobertura`, `pycobertura`)
- Check tool versions for compatibility
- Some differences between tools are expected and normal

### Test Failures
- Check fixture programs compile correctly
- Verify fortcov executable is available
- Review test output for specific error messages

## Contributing

When adding new integration tests:

1. Create fixture program in appropriate subdirectory
2. Include both source and test files
3. Document expected coverage behavior
4. Add test case to `test_integration.f90`
5. Update this README with new scenario

Follow the TDD approach:
1. **RED**: Write failing test first
2. **GREEN**: Implement minimal fixture to pass
3. **REFACTOR**: Clean up code while keeping tests green