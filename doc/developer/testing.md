# FortCov Testing Guide

Testing strategies and frameworks for FortCov development.

## Test Structure

### Test Organization

FortCov uses a comprehensive testing approach:

```
test/
├── unit/                           # Unit tests for individual modules
│   ├── test_coverage_engine.f90
│   ├── test_coverage_model.f90
│   └── test_fortcov_config.f90
├── integration/                    # Integration tests
│   ├── test_cli_integration.f90
│   └── test_build_system_integration.f90
├── system/                         # System-level tests
│   ├── test_end_to_end_workflow.f90
│   └── test_documentation_validation.f90
└── fixtures/                       # Test data and fixtures
    ├── sample_coverage_data/
    └── test_projects/
```

### Test Types

**Unit Tests**: Test individual modules and functions in isolation
```fortran
program test_coverage_model
    ! Tests for coverage_model.f90 module
end program
```

**Integration Tests**: Test interaction between modules
```fortran
program test_cli_integration
    ! Tests for command-line interface with config system
end program
```

**System Tests**: End-to-end testing of complete workflows
```fortran
program test_end_to_end_workflow
    ! Complete coverage analysis workflow testing
end program
```

## Running Tests

### Basic Test Execution

```bash
# Run all tests
fpm test

# Run specific test
fpm test test_coverage_engine

# Verbose test output
fpm test --verbose

# Run tests with specific flags
fpm test --flag "-DDEBUG"
```

### Coverage Testing

```bash
# Generate test coverage
fpm test --flag "-fprofile-arcs -ftest-coverage"

# Extract coverage data from test runs
find build -name "*.gcda" -path "*/test/*" -execdir gcov {} \;

# Analyze test coverage
gcov test/*.f90
fortcov --source=test --output=test-coverage.md
```

### Performance Testing

```bash
# Run performance benchmarks
fmp test test_performance_benchmarks

# Profile test execution
fpm build --flag "-pg"
fpm test test_performance_benchmarks
gprof build/gfortran_*/test/test_performance_benchmarks gmon.out
```

## Writing Tests

### Unit Test Template

```fortran
program test_module_name
    !! Unit tests for module_name module
    use module_name
    implicit none
    
    ! Test execution tracking
    integer :: total_tests = 0
    integer :: passed_tests = 0
    logical :: all_tests_passed = .true.
    
    ! Run all test subroutines
    call test_basic_functionality()
    call test_edge_cases()
    call test_error_conditions()
    
    ! Report results
    call report_test_results()
    
    ! Exit with appropriate code
    if (.not. all_tests_passed) stop 1
    
contains
    
    subroutine test_basic_functionality()
        !! Test normal operation
        
        total_tests = total_tests + 1
        
        ! Test implementation
        if (basic_function(5) == 10) then
            passed_tests = passed_tests + 1
            print *, "✅ Basic functionality test passed"
        else
            print *, "❌ Basic functionality test failed"
            all_tests_passed = .false.
        end if
    end subroutine test_basic_functionality
    
    subroutine test_edge_cases()
        !! Test boundary conditions
        
        total_tests = total_tests + 1
        
        ! Test edge case
        if (edge_case_function(0) == 0) then
            passed_tests = passed_tests + 1
            print *, "✅ Edge case test passed"
        else
            print *, "❌ Edge case test failed"
            all_tests_passed = .false.
        end if
    end subroutine test_edge_cases
    
    subroutine test_error_conditions()
        !! Test error handling
        
        total_tests = total_tests + 1
        
        ! Test error condition
        if (error_function(-1) == ERROR_INVALID_INPUT) then
            passed_tests = passed_tests + 1
            print *, "✅ Error condition test passed"
        else
            print *, "❌ Error condition test failed"
            all_tests_passed = .false.
        end if
    end subroutine test_error_conditions
    
    subroutine report_test_results()
        print *, ""
        print *, "=== Test Results ==="
        print *, "Total tests:", total_tests
        print *, "Passed:", passed_tests
        print *, "Failed:", total_tests - passed_tests
        
        if (all_tests_passed) then
            print *, "✅ All tests passed!"
        else
            print *, "❌ Some tests failed"
        end if
    end subroutine report_test_results
    
end program test_module_name
```

### Integration Test Example

```fortran
program test_cli_integration
    !! Integration tests for CLI and configuration system
    use cli_parser
    use fortcov_config
    use file_utils
    implicit none
    
    integer :: total_tests = 0
    integer :: passed_tests = 0
    
    call test_config_file_loading()
    call test_command_line_overrides()
    
    print *, "Integration tests:", passed_tests, "/", total_tests
    if (passed_tests /= total_tests) stop 1
    
contains
    
    subroutine test_config_file_loading()
        type(fortcov_config_t) :: config
        character(len=256) :: test_config_file
        integer :: status
        
        total_tests = total_tests + 1
        
        ! Create temporary config file
        test_config_file = create_temp_config_file()
        
        ! Load configuration
        call load_config_from_file(test_config_file, config, status)
        
        if (status == STATUS_SUCCESS .and. config%minimum_coverage == 85.0) then
            passed_tests = passed_tests + 1
            print *, "✅ Config file loading test passed"
        else
            print *, "❌ Config file loading test failed"
        end if
        
        ! Cleanup
        call delete_file(test_config_file)
    end subroutine test_config_file_loading
    
    function create_temp_config_file() result(filename)
        character(len=256) :: filename
        integer :: unit
        
        filename = "test_config.nml"
        open(newunit=unit, file=filename, status='replace')
        write(unit, '(A)') "&fortcov_config"
        write(unit, '(A)') "    minimum_coverage = 85.0"
        write(unit, '(A)') "/"
        close(unit)
    end function create_temp_config_file
    
end program test_cli_integration
```

### System Test Example

```fortran
program test_end_to_end_workflow
    !! System-level end-to-end testing
    use file_utils
    use system_utils
    implicit none
    
    character(len=256) :: test_project_dir
    character(len=256) :: coverage_output
    integer :: status
    
    ! Setup test environment
    call setup_test_project(test_project_dir)
    
    ! Execute complete workflow
    call execute_coverage_workflow(test_project_dir, status)
    
    ! Verify results
    coverage_output = trim(test_project_dir) // "/coverage.md"
    
    if (status == 0 .and. file_exists(coverage_output)) then
        print *, "✅ End-to-end workflow test passed"
    else
        print *, "❌ End-to-end workflow test failed"
        stop 1
    end if
    
    ! Cleanup
    call cleanup_test_project(test_project_dir)
    
end program test_end_to_end_workflow
```

## Test Quality Standards

### Test Requirements

- **Comprehensive Coverage**: All public interfaces must have tests
- **Edge Cases**: Test boundary conditions and invalid inputs
- **Error Handling**: Verify proper error responses
- **Performance**: Include performance regression tests for critical paths
- **Deterministic**: Tests must produce consistent results
- **Isolated**: Tests should not depend on external state

### Test Data Management

```fortran
! Good: Use test fixtures
subroutine setup_test_data()
    ! Create known test data
end subroutine

! Good: Clean up after tests
subroutine cleanup_test_data()
    ! Remove temporary files
end subroutine

! Bad: Depend on external files that might change
! Don't: Read user's actual config files in tests
```

### Assertion Patterns

```fortran
! Good: Clear assertion with context
if (result /= expected) then
    print *, "❌ Test failed: expected", expected, "got", result
    test_failed = .true.
end if

! Good: Floating point comparison
if (abs(result - expected) > tolerance) then
    print *, "❌ Floating point test failed"
    test_failed = .true.
end if

! Good: String comparison
if (trim(result) /= trim(expected)) then
    print *, "❌ String test failed"
    print *, "Expected: '", trim(expected), "'"
    print *, "Got:      '", trim(result), "'"
    test_failed = .true.
end if
```

## Advanced Testing

### Memory Leak Testing

```bash
# Build with debug symbols
fpm build --profile debug

# Run tests under valgrind
valgrind --leak-check=full --track-origins=yes \
         --error-exitcode=1 \
         ./build/gfortran_*/test/test_coverage_model

# Check for memory errors
if [ $? -eq 0 ]; then
    echo "✅ No memory leaks detected"
else
    echo "❌ Memory issues found"
fi
```

### Thread Safety Testing

```fortran
program test_thread_safety
    !! Test thread-safe operations
    use omp_lib
    use coverage_processor
    implicit none
    
    integer, parameter :: NUM_THREADS = 4
    integer :: i
    logical :: all_passed = .true.
    
    !$OMP PARALLEL NUM_THREADS(NUM_THREADS) PRIVATE(i) SHARED(all_passed)
    !$OMP DO
    do i = 1, 100
        if (.not. test_concurrent_operation(i)) then
            !$OMP CRITICAL
            all_passed = .false.
            !$OMP END CRITICAL
        end if
    end do
    !$OMP END DO
    !$OMP END PARALLEL
    
    if (all_passed) then
        print *, "✅ Thread safety tests passed"
    else
        print *, "❌ Thread safety tests failed"
        stop 1
    end if
    
end program test_thread_safety
```

### Property-Based Testing

```fortran
subroutine test_coverage_properties()
    !! Test mathematical properties of coverage calculations
    integer :: covered, total
    real :: percentage
    
    ! Property: Coverage percentage is always between 0 and 100
    do covered = 0, 100
        do total = covered, 100
            percentage = calculate_coverage_percentage(covered, total)
            
            if (percentage < 0.0 .or. percentage > 100.0) then
                print *, "❌ Coverage percentage out of range:", percentage
                stop 1
            end if
        end do
    end do
    
    print *, "✅ Coverage percentage property tests passed"
end subroutine test_coverage_properties
```

## Continuous Integration Testing

### GitHub Actions Integration

```yaml
# .github/workflows/test.yml
name: Tests
on: [push, pull_request]

jobs:
  test:
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]
        
    steps:
    - uses: actions/checkout@v4
    
    - name: Setup Fortran
      uses: fortran-lang/setup-fortran@v1
      with:
        compiler: gfortran
        
    - name: Install FPM
      uses: fortran-lang/setup-fpm@v5
      
    - name: Run tests
      run: fpm test
      
    - name: Run coverage tests
      run: |
        fpm test --flag "-fprofile-arcs -ftest-coverage"
        gcov src/*.f90
        fortcov --source=src --output=coverage.md
        
    - name: Upload test results
      uses: actions/upload-artifact@v4
      with:
        name: test-results-${{ matrix.os }}
        path: coverage.md
```

### Test Automation

```bash
#!/bin/bash
# test-automation.sh - Comprehensive test runner

set -e

echo "=== FortCov Test Suite ==="

# Build tests
echo "Building tests..."
fpm build

# Unit tests
echo "Running unit tests..."
fpm test

# Integration tests  
echo "Running integration tests..."
for test_file in test/integration/test_*.f90; do
    test_name=$(basename "$test_file" .f90)
    echo "Running $test_name..."
    fpm test "$test_name"
done

# System tests
echo "Running system tests..."
for test_file in test/system/test_*.f90; do
    test_name=$(basename "$test_file" .f90)
    echo "Running $test_name..."
    fpm test "$test_name"
done

# Coverage analysis
echo "Generating test coverage..."
fpm test --flag "-fprofile-arcs -ftest-coverage"
gcov test/*.f90
fortcov --source=test --output=test-coverage.md

echo "✅ All tests completed successfully!"
```

## Test Documentation

### Test Plan Documentation

Each major feature should have a test plan:

```markdown
# Test Plan: Coverage Analysis Engine

## Test Objectives
- Verify correct coverage percentage calculations
- Test handling of various gcov file formats  
- Validate error handling for malformed inputs

## Test Cases

### TC001: Basic Coverage Calculation
**Objective**: Verify coverage percentage calculation
**Input**: 80 covered lines out of 100 total
**Expected**: 80.0% coverage
**Priority**: High

### TC002: Edge Case - Zero Lines
**Objective**: Handle edge case of zero total lines
**Input**: 0 covered, 0 total
**Expected**: 0.0% coverage, no division by zero
**Priority**: Medium
```

This comprehensive testing approach ensures FortCov maintains high quality and reliability across all supported platforms and use cases.