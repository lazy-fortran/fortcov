#!/bin/bash
# Test script to validate Meson configuration fixes for issue #259
# Tests that reserved target names and deprecated API issues are resolved

set -e  # Exit on error

echo "=== Testing Meson Configuration Fixes for Issue #259 ==="
echo

# Test 1: Configuration should succeed without errors
echo "Test 1: Meson configuration should succeed without errors or deprecation warnings"
rm -rf builddir
if meson setup builddir -Dcoverage=true 2>&1 | grep -E "(ERROR|WARNING.*deprecated)"; then
    echo "FAIL: Found configuration errors or deprecation warnings"
    exit 1
else
    echo "PASS: Configuration completed without errors or deprecation warnings"
fi
echo

# Test 2: Build should succeed
echo "Test 2: Project should build successfully"
if meson compile -C builddir > /dev/null 2>&1; then
    echo "PASS: Build completed successfully"
else
    echo "FAIL: Build failed"
    exit 1
fi
echo

# Test 3: Tests should pass
echo "Test 3: Tests should pass"
if meson test -C builddir > /dev/null 2>&1; then
    echo "PASS: All tests passed"
else
    echo "FAIL: Tests failed"
    exit 1
fi
echo

# Test 4: Coverage target should exist and run
echo "Test 4: Coverage target 'fortcov_coverage' should exist and run successfully"
if meson compile fortcov_coverage -C builddir > /dev/null 2>&1; then
    echo "PASS: Coverage target executed successfully"
else
    echo "FAIL: Coverage target failed"
    exit 1
fi
echo

# Test 5: Coverage data should be generated
echo "Test 5: Coverage data should be generated"
if find builddir -name "*.gcov" | grep -q .; then
    echo "PASS: Coverage files (.gcov) were generated"
    echo "Generated coverage files:"
    find builddir -name "*.gcov" -exec basename {} \; | sed 's/^/  - /'
else
    echo "FAIL: No coverage files found"
    exit 1
fi
echo

echo "=== All Tests Passed! ==="
echo "Issue #259 fixes verified:"
echo "  ✓ Reserved target name 'coverage' renamed to 'fortcov_coverage'"
echo "  ✓ Deprecated meson.source_root() replaced with meson.project_source_root()"
echo "  ✓ Deprecated meson.build_root() replaced with meson.project_build_root()"
echo "  ✓ Full Meson workflow works: setup → build → test → coverage"