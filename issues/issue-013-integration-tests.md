# Issue #13: Create Integration Test Suite

## User Story
As a maintainer, I want comprehensive integration tests so that I can ensure fortcov works correctly with real Fortran projects.

## Acceptance Criteria
- [ ] Create sample Fortran programs with known coverage
- [ ] Generate reference coverage data
- [ ] Test end-to-end workflow
- [ ] Verify report accuracy
- [ ] Test edge cases

## Test Specifications (RED Phase)

### Test 1: Simple module with 100% coverage
**Given**: A Fortran module with all lines executed
**When**: Running fortcov on its coverage data
**Then**: Report should show 100.00% coverage

### Test 2: Module with uncovered procedure
**Given**: Module with one procedure never called
**When**: Analyzing coverage
**Then**: Should correctly identify uncovered procedure

### Test 3: Nested modules with contains
**Given**: Module with contained procedures
**When**: Processing coverage
**Then**: Should maintain correct hierarchy in report

### Test 4: Generic interfaces
**Given**: Module with generic interface blocks
**When**: Analyzing coverage
**Then**: Should handle interface blocks as non-executable

### Test 5: Select case coverage
**Given**: Program with select case, some cases uncovered
**When**: Processing coverage
**Then**: Should show partial branch coverage

### Test 6: Do loop variations
**Given**: Various do loop constructs (do while, do concurrent)
**When**: Analyzing coverage
**Then**: Should correctly track loop body execution

### Test 7: Array operations
**Given**: Array assignments and operations
**When**: Processing coverage
**Then**: Should count as appropriate executable lines

### Test 8: Submodules
**Given**: Program using submodules
**When**: Analyzing coverage
**Then**: Should correctly associate with parent module

### Test 9: Mixed coverage project
**Given**: Multi-file project with varying coverage
**When**: Running fortcov
**Then**: Should aggregate correctly in TOTAL row

### Test 10: Large project performance
**Given**: Project with 100+ source files
**When**: Processing coverage
**Then**: Should complete within reasonable time (<5 seconds)

## Implementation Notes (GREEN Phase)
- Create test/fixtures/ directory with sample programs
- Use FPM to build test programs with coverage
- Compare output against expected reports
- Test both small and large codebases

## Technical Details
- Directory: `test/integration/`
- Test programs: `test/fixtures/`
- Expected outputs: `test/expected/`
- Script: `test/run_integration_tests.sh`
- Dependencies: All modules

## Sample Test Programs to Create

1. **simple_module**: Basic module with procedures
2. **nested_module**: Module with contains block
3. **interface_module**: Generic interfaces and overloading
4. **branching_program**: If-then-else, select case
5. **loops_program**: Various loop constructs
6. **array_program**: Array operations and constructors
7. **submodule_program**: Parent module with submodules
8. **mixed_project**: Multiple interconnected modules