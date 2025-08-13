# Issue #6: Implement GCov Parser ✅ COMPLETED

## User Story
As a Fortran developer using gfortran, I want to parse gcov coverage data so that I can generate coverage reports for my code.

## Acceptance Criteria
- [x] Implement coverage_parser_t interface for gcov
- [x] Parse .gcno/.gcda file pairs
- [x] Map coverage to source lines
- [x] Handle Fortran-specific constructs

## Test Specifications (RED Phase)

### Test 1: Parse simple subroutine coverage
**Given**: Coverage data for a simple subroutine with 5 lines
**When**: Parsing with gcov_parser
**Then**: Should return coverage_data_t with correct line counts

### Test 2: Handle module procedures
**Given**: Coverage for procedures within a Fortran module
**When**: Parsing module source
**Then**: Should correctly identify parent module for each procedure

### Test 3: Parse contains block
**Given**: A module with contained procedures
**When**: Parsing coverage
**Then**: Should maintain hierarchy: module -> contains -> procedures

### Test 4: Handle interface blocks
**Given**: Coverage for generic interfaces
**When**: Parsing interface definitions
**Then**: Should not count interface lines as executable

### Test 5: Process use statements
**Given**: Module with use statements
**When**: Parsing coverage
**Then**: Should mark use statements as non-executable

### Test 6: Parse branch coverage in if-then-else
**Given**: If-then-else block with coverage
**When**: Parsing branch data
**Then**: Should identify both branches and their execution counts

### Test 7: Handle do loop coverage
**Given**: Do loop with coverage data
**When**: Parsing loop execution
**Then**: Should show loop body execution counts

### Test 8: Parse select case coverage
**Given**: Select case with multiple cases
**When**: Parsing case branches
**Then**: Should track each case branch separately

### Test 9: Handle implicit none statements
**Given**: Source with implicit none
**When**: Parsing coverage
**Then**: Should mark as non-executable line

### Test 10: Parse array constructors
**Given**: Array constructor expressions
**When**: Parsing coverage
**Then**: Should count as single executable statement

## Implementation Notes (GREEN Phase)
- Build on gcov_binary_format module
- Map binary data to source lines
- Special handling for Fortran keywords
- Track module/procedure hierarchy

## Technical Details
- File: `src/gcov_parser.f90`
- Test file: `test/test_gcov_parser.f90`
- Dependencies: coverage_parser, gcov_binary_format, coverage_model
- Test data: Create sample Fortran programs with known coverage