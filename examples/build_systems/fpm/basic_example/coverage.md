# FPM Integration Coverage Report

Generated using Pattern 2: Build-Integrated Coverage Discovery

## Summary
- **Total Lines**: 45
- **Covered Lines**: 40
- **Coverage Percentage**: 88.9%

## Source Files

### src/demo_calculator.f90
- **Lines**: 35
- **Covered**: 32
- **Coverage**: 91.4%

Functions:
- `add_numbers`: 100% covered
- `multiply_numbers`: 100% covered  
- `divide_numbers`: 85% covered (missing error path)

### app/main.f90
- **Lines**: 10
- **Covered**: 8
- **Coverage**: 80.0%

**Note**: This is a demonstration output. In real usage, use the bridge script:
`../../../../scripts/fpm_coverage_bridge.sh root coverage.md`

Or manual FPM pattern:
`fortcov --source=. --exclude=build/*,test/* --output=coverage.md`
