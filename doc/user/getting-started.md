# Getting Started with FortCov

**Condensed 5-minute guide** - for complete examples see main [README.md](../../README.md).

## Prerequisites Check

```bash
which gfortran gcov fpm fortcov  # All must be installed
```

## Quick Tutorial

```bash
# 1. Create project
mkdir calc && cd calc && fpm init calc

# 2. Add source (src/calc.f90)
cat > src/calc.f90 << 'EOF'
module calc
    implicit none
    public :: add
contains
    function add(a, b) result(c)
        real, intent(in) :: a, b
        real :: c
        c = a + b
    end function
end module
EOF

# 3. Add test (test/test_calc.f90)
cat > test/test_calc.f90 << 'EOF'
program test_calc
    use calc
    implicit none
    if (abs(add(2.0, 3.0) - 5.0) > 1e-6) stop 1
    print *, 'OK'
end program
EOF

# 4. Generate coverage
fpm test --flag "-fprofile-arcs -ftest-coverage"
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
  gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
done
fortcov --source=. --output=coverage.md  # Creates coverage report
```

## Next Steps

See main [README.md](../../README.md) for:
- Complete installation guide
- Build system integration (CMake, Make, Meson)
- CI/CD examples (GitHub Actions, GitLab)  
- Configuration files and advanced options
- Security features and troubleshooting