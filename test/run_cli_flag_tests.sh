#!/bin/bash
#
# CLI Flag Testing Script for Issue #231
# 
# Given-When-Then: Demonstrate broken CLI flag functionality
# 
# GIVEN: A compiled fortcov with working test coverage files
# WHEN: Various CLI flags are tested through the test suite
# THEN: All tests should fail, demonstrating CLI flags are ignored
#

echo "=============================================="
echo "CLI Flag Testing for Issue #231"
echo "=============================================="
echo ""
echo "This script demonstrates that ALL CLI flags are"
echo "silently ignored by fortcov (Issue #231)."
echo ""

# Check if we're in the project root
if [ ! -f "fpm.toml" ]; then
    echo "❌ Error: Run this script from the fortcov project root"
    exit 1
fi

echo "Building test suite..."
echo ""

# Build the test program using FPM
if ! fpm build test:test_cli_flag_parsing_issue_231 2>/dev/null; then
    echo "❌ Failed to build test suite"
    echo "   This may be due to missing dependencies or compilation errors"
    echo ""
    echo "Manual compilation attempt:"
    echo "gfortran -I build/gfortran_*/fortcov -o test/test_cli_flag_parsing_issue_231 test/test_cli_flag_parsing_issue_231.f90 build/gfortran_*/fortcov/*.o"
    exit 1
fi

echo "✓ Test suite built successfully"
echo ""

echo "Running CLI flag tests..."
echo "Expected result: ALL tests should FAIL (demonstrating Issue #231)"
echo ""

# Run the test executable
if ! ./build/gfortran_*/test/test_cli_flag_parsing_issue_231; then
    echo ""
    echo "⚠️  Test execution encountered errors"
    echo "   This is expected - the tests are designed to fail"
    echo "   demonstrating that CLI flags are not working"
fi

echo ""
echo "=============================================="
echo "CLI Flag Test Verification"
echo "=============================================="
echo ""
echo "To manually verify CLI flag issues:"
echo ""
echo "1. Test output path flag:"
echo "   fortcov --output=custom.json --format=json"
echo "   → Should create custom.json but creates default file"
echo ""
echo "2. Test format flag:"
echo "   fortcov --format=xml --output=test.xml"
echo "   → Should create XML but creates markdown"
echo ""
echo "3. Test verbose flag:"
echo "   fortcov --verbose"
echo "   → Should show detailed output but shows normal output"
echo ""
echo "4. Test invalid flag:"
echo "   fortcov --invalid-flag"
echo "   → Should show error but silently ignores"
echo ""
echo "All these demonstrate Issue #231: CLI flags are parsed"
echo "but not applied during coverage analysis execution."