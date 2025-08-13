# Issue #7: Implement Coverage Statistics Calculator

## User Story
As a developer, I want to calculate coverage statistics so that I can understand the test coverage of my codebase.

## Acceptance Criteria
- [ ] Calculate line coverage percentage
- [ ] Calculate branch coverage percentage
- [ ] Calculate function coverage percentage
- [ ] Identify uncovered line ranges
- [ ] Aggregate statistics across files

## Test Specifications (RED Phase)

### Test 1: Calculate 100% line coverage
**Given**: coverage_data_t with 10 lines, all executed
**When**: Calling calculate_line_coverage()
**Then**: Should return coverage_stats_t with percentage=100.0

### Test 2: Calculate partial line coverage
**Given**: coverage_data_t with 10 lines, 7 executed
**When**: Calling calculate_line_coverage()
**Then**: Should return percentage=70.0, missing_ranges="4-5, 8"

### Test 3: Calculate 0% coverage
**Given**: coverage_data_t with all execution_count=0
**When**: Calling calculate_line_coverage()
**Then**: Should return percentage=0.0

### Test 4: Calculate branch coverage
**Given**: 4 branches: 2 fully covered, 1 partial, 1 uncovered
**When**: Calling calculate_branch_coverage()
**Then**: Should return percentage=50.0 (2/4 fully covered)

### Test 5: Calculate function coverage
**Given**: 5 functions: 3 called, 2 never called
**When**: Calling calculate_function_coverage()
**Then**: Should return percentage=60.0

### Test 6: Aggregate multiple files
**Given**: File A (80% coverage), File B (60% coverage)
**When**: Calling aggregate_file_coverage()
**Then**: Should return weighted average based on line counts

### Test 7: Handle non-executable lines
**Given**: File with 20 lines, 10 executable, 8 covered
**When**: Calculating coverage
**Then**: Should return 80.0% (8/10, not 8/20)

### Test 8: Compress missing line ranges
**Given**: Uncovered lines [3,4,5,10,11,20]
**When**: Generating missing_ranges string
**Then**: Should return "3-5, 10-11, 20"

### Test 9: Module-level statistics
**Given**: Module with multiple procedures
**When**: Calculating module coverage
**Then**: Should aggregate all procedures within module

### Test 10: Empty file handling
**Given**: coverage_data_t with no executable lines
**When**: Calculating statistics
**Then**: Should return 100.0% (no lines to cover)

## Implementation Notes (GREEN Phase)
- Count only executable lines in totals
- Use real(8) for percentage calculations
- Build range strings efficiently
- Handle divide-by-zero cases

## Technical Details
- File: `src/coverage_statistics.f90`
- Test file: `test/test_coverage_statistics.f90`
- Dependencies: coverage_model, string_utils