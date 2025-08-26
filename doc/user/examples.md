# FortCov Examples and Tutorials

**Working examples** - see main [README.md](../../README.md) for complete tutorials.

## Basic Example

**Complete working example:**

```bash
mkdir calc && cd calc && fpm init calc

# Source (src/calc.f90)
cat > src/calc.f90 << 'EOF'
module calc
    implicit none
    public :: add, subtract
contains
    function add(a, b) result(c)
        real, intent(in) :: a, b
        real :: c
        c = a + b
    end function
    
    function subtract(a, b) result(c)
        real, intent(in) :: a, b  
        real :: c
        c = a - b
    end function
end module
EOF

# Test (test/test_calc.f90)
cat > test/test_calc.f90 << 'EOF'
program test_calc
    use calc
    implicit none
    if (abs(add(2.0, 3.0) - 5.0) > 1e-6) stop 1
    if (abs(subtract(5.0, 2.0) - 3.0) > 1e-6) stop 1
    print *, 'All tests passed!'
end program
EOF

# Generate coverage
fpm test --flag "-fprofile-arcs -ftest-coverage"
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
  gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
done
fortcov --source . --output coverage.md
```

**Expected output:**
```markdown
| Filename | Stmts | Covered | Cover | Missing |
|----------|-------|---------|-------|---------|  
| src/calc.f90 | 6 | 6 | 100.00% | |
| TOTAL | 6 | 6 | 100.00% | |
```

## Build Integration Examples

**FPM (recommended):**
```bash
fpm test --flag "-fprofile-arcs -ftest-coverage"
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
  gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
done
fortcov --source .
```

**CMake:**
```cmake
set(CMAKE_Fortran_FLAGS_TESTING "-fprofile-arcs -ftest-coverage")
add_custom_target(coverage
    COMMAND gcov *.gcno
    COMMAND fortcov --source ${CMAKE_SOURCE_DIR}
)
```

**Makefile:**
```makefile
coverage: test
	gcov $(SOURCES) 
	fortcov --source . --output coverage.md
```

## CI/CD Integration

**GitHub Actions:**
```yaml
- run: |
    fpm test --flag "-fprofile-arcs -ftest-coverage"
    find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
      gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
    done
    fortcov --source . --fail-under 80 --format json --output coverage.json
```

**GitLab CI:**
```yaml
coverage:
  script:
    - fpm test --flag "-fprofile-arcs -ftest-coverage" 
    - find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true; done
    - fortcov --source . --format xml --output coverage.xml --fail-under 80
  artifacts:
    reports:
      coverage_report:
        coverage_format: cobertura
        path: coverage.xml
```

For comprehensive build system examples, see `examples/build_systems/` directory and main [README.md](../../README.md).