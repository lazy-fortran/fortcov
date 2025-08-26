# FortCov Development Guide

Guide for contributing to FortCov development.

## Getting Started

### Prerequisites

- gfortran 9.0+
- FPM 0.8.0+
- Git
- Basic understanding of Fortran and coverage analysis

### Setup Development Environment

```bash
# Clone repository
git clone https://github.com/lazy-fortran/fortcov.git
cd fortcov

# Build and verify
fpm build
fpm test

# Install locally for testing
fpm install --prefix=~/.local
```

### Development Workflow

```bash
# Create feature branch
git checkout -b feature/my-feature

# Make changes and test
fpm build
fpm test

# Generate coverage for your changes  
fpm test --flag "-fprofile-arcs -ftest-coverage"
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
  gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
done
fortcov --source=src *.gcov

# Commit changes
git add .
git commit -m "feat: add new feature"

# Submit pull request
git push origin feature/my-feature
```

## Code Standards

### Architecture Guidelines

- Follow **SOLID principles**: Single responsibility, dependency injection
- Use **foundation layer utilities** for common patterns
- Maintain **backward compatibility** in public APIs
- **Document all public interfaces** with inline comments

### Code Style

```fortran
! Good: Descriptive names and consistent style
module coverage_processor
    implicit none
    private
    
    integer, parameter :: MAX_LINE_LENGTH = 2048
    
    type, public :: coverage_result_t
        character(len=:), allocatable :: filename
        integer :: total_lines
        integer :: covered_lines
        real :: percentage
    end type
    
    public :: process_coverage_file
    
contains
    
    subroutine process_coverage_file(filename, result, status)
        character(len=*), intent(in) :: filename
        type(coverage_result_t), intent(out) :: result
        integer, intent(out) :: status
        
        ! Implementation here
    end subroutine process_coverage_file
    
end module coverage_processor
```

### Formatting Rules

- **Line length**: 88 characters (90 for Fortran with `&` continuation)
- **Indentation**: 4 spaces (no tabs)
- **Naming**: `snake_case` for variables and procedures
- **Constants**: `UPPER_SNAKE_CASE` 
- **Types**: `typename_t` suffix

### Memory Management

```fortran
! Good: Use allocatable arrays
integer, allocatable :: data(:)
allocate(data(size))

! Good: Automatic deallocation
! (data is automatically deallocated when going out of scope)

! Bad: Manual deallocation (not needed for allocatables)
! deallocate(data)
```

## Testing Strategy

### Test Structure

Every module must have corresponding tests:

```
src/
├── coverage_engine.f90
├── coverage_model.f90
└── coverage_reporter.f90

test/
├── test_coverage_engine.f90
├── test_coverage_model.f90
└── test_coverage_reporter.f90
```

### Writing Unit Tests

```fortran
program test_coverage_model
    use coverage_model
    implicit none
    
    integer :: tests_run = 0
    integer :: tests_passed = 0
    
    call test_coverage_calculation()
    call test_edge_cases()
    
    print *, "Tests run:", tests_run, "Passed:", tests_passed
    if (tests_passed /= tests_run) stop 1
    
contains
    
    subroutine test_coverage_calculation()
        type(coverage_result_t) :: result
        
        tests_run = tests_run + 1
        
        ! Test normal case
        result = calculate_coverage(80, 100)
        
        if (abs(result%percentage - 80.0) < 1e-6) then
            tests_passed = tests_passed + 1
            print *, "✅ Coverage calculation test passed"
        else
            print *, "❌ Coverage calculation test failed"
        end if
    end subroutine test_coverage_calculation
    
    subroutine test_edge_cases()
        type(coverage_result_t) :: result
        
        tests_run = tests_run + 1
        
        ! Test division by zero
        result = calculate_coverage(0, 0)
        
        if (result%percentage == 0.0) then
            tests_passed = tests_passed + 1
            print *, "✅ Edge case test passed"
        else
            print *, "❌ Edge case test failed"
        end if
    end subroutine test_edge_cases
    
end program test_coverage_model
```

### Test Coverage Requirements

- **All public interfaces** must have tests
- **Edge cases and error conditions** must be tested
- **Tests should be deterministic** and isolated
- **Test names should clearly describe** what is being tested
- **Maintain high test coverage** for your own contributions

## Architecture Principles

### Modular Design

FortCov follows a decomposed architecture:

```
Foundation Layer:
├── foundation_constants.f90    (System-wide constants)
├── foundation_layer_utils.f90  (Common utilities)
└── architectural_patterns.f90  (Design patterns)

Core Layer:
├── coverage_engine.f90         (Main orchestration)
├── coverage_model.f90          (Data structures)
├── coverage_reporter.f90       (Output generation)
└── fortcov_config.f90          (Configuration)
```

### Security-First Development

All external inputs must be validated:

```fortran
use input_validation

subroutine process_file(filename)
    character(len=*), intent(in) :: filename
    type(validation_result_t) :: validation
    
    ! Always validate inputs first
    call validate_file_path(filename, validation)
    if (.not. validation%is_valid) then
        print *, "Error:", trim(validation%error_message)
        return
    end if
    
    ! Process validated input
end subroutine
```

### Performance Considerations

Use O(n) algorithms and pre-allocation patterns:

```fortran
! Good: Pre-allocate for known size
integer, allocatable :: results(:)
allocate(results(estimated_size))

! Good: Use streaming for large data
call process_file_streaming(filename, buffer_size=4096)

! Bad: O(n²) concatenation
! results = [results, new_item]  ! Avoid this pattern
```

## Contribution Process

### Before Starting

1. **Check existing issues** and discussions
2. **Create or comment on relevant issue** to discuss approach
3. **Fork repository** and create feature branch
4. **Follow coding standards** and architecture principles

### During Development

1. **Write tests first** (TDD approach when possible)
2. **Keep commits focused** and atomic
3. **Update documentation** for public API changes
4. **Run full test suite** before committing

```bash
# Complete testing workflow
fpm build
fpm test
fpm test --flag "-fprofile-arcs -ftest-coverage"
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
  gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
done
fortcov --source=src *.gcov --fail-under=90
```

### Pull Request Guidelines

#### PR Title Format
```
<type>: <description>

Examples:
feat: add XML output format
fix: resolve memory leak in coverage parser
docs: update API reference for new module
test: add edge case tests for configuration parser
refactor: decompose large coverage_engine module
```

#### PR Description Template
```markdown
## Summary
Brief description of changes and motivation.

## Changes
- [ ] Added/modified functionality
- [ ] Updated tests
- [ ] Updated documentation
- [ ] Verified backward compatibility

## Test Results
- All existing tests pass: ✅/❌
- New tests added and pass: ✅/❌
- Coverage maintained/improved: ✅/❌

## Breaking Changes
List any breaking changes or migration steps needed.
```

### Review Process

1. **Automated CI** runs all tests and checks
2. **Code review** by maintainers
3. **Documentation review** for public API changes
4. **Integration testing** with real projects
5. **Merge** after all checks pass

## Advanced Development

### Performance Profiling

```bash
# Profile performance with gprof
fpm build --flag "-pg"
./your_test_program
gprof your_test_program gmon.out > profile.txt
```

### Memory Safety Testing

```bash
# Use valgrind for memory leak detection
fpm build --flag "-g -O0"
valgrind --leak-check=full --track-origins=yes ./your_test_program
```

### Cross-Platform Testing

Test on multiple platforms:
- Ubuntu/Debian (gfortran)
- CentOS/RHEL (gfortran)
- macOS (brew gcc)
- Windows/MSYS2 (mingw-w64)

## Release Process

### Version Numbering

FortCov follows semantic versioning (SemVer):
- **MAJOR**: Breaking changes
- **MINOR**: New features (backward compatible)
- **PATCH**: Bug fixes (backward compatible)

### Release Checklist

- [ ] All tests pass on all platforms
- [ ] Documentation updated
- [ ] CHANGELOG.md updated  
- [ ] Version bumped in fpm.toml
- [ ] Git tag created
- [ ] Release notes written

## Getting Help

### Resources

- **Architecture docs**: See DESIGN.md for system overview
- **API docs**: Inline documentation in source code
- **Examples**: See examples/ directory
- **Tests**: Test files show usage patterns

### Community

- **GitHub Issues**: Bug reports and feature requests
- **GitHub Discussions**: General questions and ideas
- **Code Reviews**: Learning opportunity through PR reviews

### Maintainer Contact

For architectural questions or major contributions, reach out to maintainers through GitHub issues or discussions.

Happy coding! 🚀