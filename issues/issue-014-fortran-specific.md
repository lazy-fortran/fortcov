# Issue #14: Enhance Fortran-Specific Coverage Support

## User Story
As a Fortran developer, I want fortcov to correctly handle all Fortran language constructs so that coverage reports accurately reflect my code structure.

## Acceptance Criteria
- [ ] Correctly identify non-executable Fortran statements
- [ ] Handle module structure and hierarchy
- [ ] Process Fortran 2008+ features
- [ ] Support preprocessor directives
- [ ] Handle line continuations

## Test Specifications (RED Phase)

### Test 1: Module use statements
**Given**: Module with multiple use statements
**When**: Parsing coverage
**Then**: Use statements should be marked non-executable

### Test 2: Implicit none handling
**Given**: Various implicit none forms (module/procedure level)
**When**: Processing coverage
**Then**: All should be marked non-executable

### Test 3: Type definitions
**Given**: Derived type definitions with procedures
**When**: Analyzing coverage
**Then**: Type definition lines non-executable, type-bound procedures executable

### Test 4: Parameter declarations
**Given**: Parameter (constant) declarations
**When**: Processing coverage
**Then**: Should be marked non-executable

### Test 5: Data statements
**Given**: DATA statements for initialization
**When**: Parsing coverage
**Then**: Should handle as non-executable

### Test 6: Format statements
**Given**: FORMAT statements for I/O
**When**: Processing coverage
**Then**: Should be marked non-executable

### Test 7: Line continuations
**Given**: Statement continued with & across lines
**When**: Calculating coverage
**Then**: Should count as single executable statement

### Test 8: Block constructs
**Given**: BLOCK/END BLOCK constructs
**When**: Processing coverage
**Then**: Should handle scope correctly

### Test 9: Associate constructs
**Given**: ASSOCIATE blocks
**When**: Analyzing coverage
**Then**: Should track execution within associate

### Test 10: Coarray declarations
**Given**: Coarray variables and operations
**When**: Processing coverage
**Then**: Should handle coarray syntax correctly

### Test 11: Abstract interfaces
**Given**: Abstract interface definitions
**When**: Parsing coverage
**Then**: Should mark as non-executable

### Test 12: Procedure pointers
**Given**: Procedure pointer assignments
**When**: Processing coverage
**Then**: Should count assignments as executable

### Test 13: Forall constructs
**Given**: FORALL statements and blocks
**When**: Analyzing coverage
**Then**: Should track as executable statements

### Test 14: Where constructs
**Given**: WHERE statements for array masking
**When**: Processing coverage
**Then**: Should handle as executable

### Test 15: Preprocessor directives
**Given**: #ifdef, #include directives
**When**: Parsing coverage
**Then**: Should handle or skip appropriately

## Implementation Notes (GREEN Phase)
- Build comprehensive keyword list
- Handle statement classification
- Track scope and hierarchy
- Consider preprocessing effects

## Technical Details
- File: `src/fortran_syntax.f90`
- Test file: `test/test_fortran_syntax.f90`
- Dependencies: Integrates with gcov_parser
- Reference: Fortran 2018 standard for completeness