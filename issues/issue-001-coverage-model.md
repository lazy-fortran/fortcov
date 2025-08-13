# Issue #1: Implement Coverage Data Model ✅ COMPLETED

## User Story
As a coverage analysis tool developer, I want a unified internal representation of coverage data so that all parsers and reporters can work with a consistent data structure.

## Acceptance Criteria
- [x] Define all coverage data types in `src/coverage_model.f90`
- [x] Support line, branch, and function coverage representations
- [x] Include Fortran-specific constructs (modules, contains blocks)
- [x] Provide serialization/deserialization for testing

**Status**: COMPLETED - Merged in PR #1

## Test Specifications (RED Phase)

### Test 1: Create and access line coverage data
**Given**: A `coverage_line_t` object
**When**: Setting execution count to 5 and line number to 42
**Then**: The object should return execution_count=5, line_number=42, and is_covered()=.true.

### Test 2: Create uncovered line
**Given**: A `coverage_line_t` object
**When**: Setting execution count to 0
**Then**: The object should return is_covered()=.false.

### Test 3: Branch coverage calculation
**Given**: A `coverage_branch_t` with taken_count=3, not_taken_count=0
**When**: Querying branch coverage
**Then**: Should return is_partially_covered()=.true. and is_fully_covered()=.false.

### Test 4: Function coverage with Fortran module info
**Given**: A `coverage_function_t` for a module procedure
**When**: Setting parent_module="my_module" and is_module_procedure=.true.
**Then**: Should correctly identify as module procedure with parent

### Test 5: File coverage aggregation
**Given**: A `coverage_file_t` with 10 lines (7 covered, 3 uncovered)
**When**: Calculating coverage percentage
**Then**: Should return 70.0% coverage

### Test 6: Coverage data serialization round-trip
**Given**: A complete `coverage_data_t` structure
**When**: Serializing to string and deserializing back
**Then**: The deserialized data should match the original

## Implementation Notes (GREEN Phase)
- Start with simple types, add methods incrementally
- Use allocatable arrays for dynamic collections
- Implement percentage calculation as type-bound procedures

## Technical Details
- File: `src/coverage_model.f90`
- Test file: `test/test_coverage_model.f90`
- Dependencies: None (foundational module)