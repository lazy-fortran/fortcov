# Getting Started with FortCov

**Condensed 5-minute guide** - for complete build system examples see [examples.md](examples.md).

## Prerequisites Check

```bash
which gfortran gcov fpm fortcov  # All must be installed
```

## Quick Tutorial

```bash
# 1. Create project
mkdir hello && cd hello && fpm new hello

# 2. Update source to add testable function (src/hello.f90)
cat > src/hello.f90 << 'EOF'
module hello
    implicit none
    private

    public :: say_hello, add_numbers
contains
    subroutine say_hello
        print *, "Hello, hello!"
    end subroutine say_hello

    function add_numbers(a, b) result(c)
        real, intent(in) :: a, b
        real :: c
        c = a + b
    end function add_numbers
end module
EOF

# 3. Add test (test/test_hello.f90)
cat > test/test_hello.f90 << 'EOF'
program test_hello
    use hello
    implicit none
    real :: result
    
    result = add_numbers(2.0, 3.0)
    if (abs(result - 5.0) > 1e-6) stop 1
    print *, 'OK'
end program
EOF

# 4. Generate coverage
# NOTE: FPM does not have a --coverage flag. Use --flag with compiler options:
fpm test --flag "-fprofile-arcs -ftest-coverage"
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
  gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
done
fortcov --source=src *.gcov --output=coverage.md  # Creates detailed markdown report
```

## Next Steps

See main [README.md](../../README.md) for:
- Complete installation guide
- Build system integration (CMake, Make, Meson)
- CI/CD examples (GitHub Actions, GitLab)  
- Configuration files and advanced options
- Security features and troubleshooting