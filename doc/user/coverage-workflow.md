# FortCov Coverage Workflow Guide

This guide explains how to generate code coverage for Fortran projects using FortCov.

## The Coverage Generation Problem

When using FPM with coverage flags, you may encounter an issue where:
- `.gcno` files (compile-time coverage data) are generated during build
- `.gcda` files (runtime coverage data) are generated during test execution
- But the `.gcno` files disappear or become incompatible when tests run

This happens because FPM may clean or rebuild parts of the project between the build and test phases, causing a mismatch between the coverage instrumentation files.

## Working Coverage Workflow

### For FPM Projects

The most reliable way to generate coverage with FPM is to use a combined build-test approach:

```bash
# 1. Clean previous coverage data
rm -rf build *.gcov *.gcda *.gcno

# 2. Build AND test in a single command to maintain consistency
fpm test --flag "-fprofile-arcs -ftest-coverage"

# 3. Generate .gcov files from the coverage data
# Note: This step may fail with "No executable lines" due to the gcno/gcda mismatch
find build -name "*.gcda" | while read gcda_file; do
    dir=$(dirname "$gcda_file")
    base=$(basename "$gcda_file" .gcda)
    gcno_file="$dir/$base.gcno"
    
    if [ -f "$gcno_file" ]; then
        (cd "$dir" && gcov -o . "$base.gcno" 2>/dev/null)
    fi
done

# Move .gcov files to project root
find build -name "*.gcov" -exec mv {} . \; 2>/dev/null

# 4. Run fortcov analysis
fortcov --source=src *.gcov
```

### Alternative: Manual Compilation

If the FPM workflow doesn't generate proper coverage data, you can compile and test manually:

```bash
# 1. Compile source files with coverage
gfortran -c src/*.f90 -fprofile-arcs -ftest-coverage -J build

# 2. Compile test files  
gfortran -c test/*.f90 -fprofile-arcs -ftest-coverage -J build -I build

# 3. Link and create test executable
gfortran *.o -o run_tests -fprofile-arcs -ftest-coverage

# 4. Run tests to generate .gcda files
./run_tests

# 5. Generate .gcov files
gcov *.gcda

# 6. Analyze with fortcov
fortcov --source=src *.gcov
```

### For CMake Projects

CMake handles coverage instrumentation more reliably:

```bash
# 1. Configure with coverage flags
cmake -B build -DCMAKE_Fortran_FLAGS="-fprofile-arcs -ftest-coverage"

# 2. Build
cmake --build build

# 3. Run tests
cd build && ctest

# 4. Generate coverage
find . -name "*.gcda" | xargs dirname | sort -u | while read dir; do
    gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
done

# 5. Analyze
fortcov --source=../src *.gcov
```

### For Makefile Projects

```makefile
# In your Makefile
FCFLAGS += -fprofile-arcs -ftest-coverage
LDFLAGS += -fprofile-arcs -ftest-coverage

coverage: test
	find . -name "*.gcda" -exec gcov {} \;
	fortcov --source=src *.gcov
```

## Troubleshooting Coverage Generation

### Issue: "No executable lines" error from gcov

**Cause**: The `.gcno` and `.gcda` files are out of sync, usually because:
- Files were recompiled after `.gcda` generation
- Different compilation flags were used
- Build directory was partially cleaned

**Solution**: 
- Always run `fpm test` with coverage flags in one command
- Don't run `fpm build` separately before `fpm test`
- Clean all artifacts before starting

### Issue: No .gcda files generated

**Cause**: Tests didn't actually execute the instrumented code

**Solution**:
- Verify tests are running: check for test output
- Ensure test executables are linked with `-fprofile-arcs -ftest-coverage`
- Check that the code being tested is actually executed

### Issue: .gcno files missing

**Cause**: Build artifacts were cleaned or not generated with coverage flags

**Solution**:
- Use `--flag "-fprofile-arcs -ftest-coverage"` on both build and test
- Don't clean between build and test phases

## Best Practices

1. **Always clean before coverage runs**: Remove old `.gcda`, `.gcno`, and `.gcov` files
2. **Use consistent flags**: Apply coverage flags to all compilation and linking
3. **Process immediately**: Generate `.gcov` files right after tests complete
4. **Single command**: With FPM, prefer `fpm test --flag "..."` over separate build/test

## Example Coverage Script

Save this as `coverage.sh` in your project root:

```bash
#!/bin/bash
set -e

echo "Generating coverage report..."

# Clean
rm -rf build *.gcov coverage_report.md

# Build and test with coverage
fpm test --flag "-fprofile-arcs -ftest-coverage" || true

# Try to generate .gcov files
find build -name "*.gcda" 2>/dev/null | while read gcda; do
    dir=$(dirname "$gcda")
    base=$(basename "$gcda" .gcda)
    if [ -f "$dir/$base.gcno" ]; then
        (cd "$dir" && gcov "$base.gcno" 2>/dev/null) || true
    fi
done

# Move any generated .gcov files
find build -name "*.gcov" -exec mv {} . \; 2>/dev/null || true

# Run fortcov if we have .gcov files
if ls *.gcov >/dev/null 2>&1; then
    fortcov --source=src *.gcov
else
    echo "Warning: No .gcov files generated. Coverage data may be incomplete."
    echo "This is a known issue with FPM's coverage instrumentation."
fi
```

Make it executable and run:
```bash
chmod +x coverage.sh
./coverage.sh
```

## Known Limitations

- **FPM coverage support is incomplete**: The `--flag` approach may not always work correctly
- **Build hash changes**: FPM may use different build directories for build vs test
- **Instrumentation mismatch**: `.gcno` and `.gcda` files may become incompatible

For critical coverage needs, consider using CMake or manual compilation until FPM's coverage support improves.