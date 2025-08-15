# Issue #5: Implement GCov Command Execution 

## User Story
As a GCC/gfortran user, I want to execute the gcov command to generate text coverage files so that I can analyze coverage data from my Fortran programs without complex binary parsing.

## Acceptance Criteria
- [ ] Execute gcov command on source files
- [ ] Handle gcov command failures gracefully
- [ ] Support different gcov options (branch coverage, etc.)
- [ ] Manage temporary .gcov files

## ARCHITECTURAL DECISION
**We DO NOT parse binary .gcno/.gcda files directly.** Instead, we follow the proven LCOV approach:
1. Execute `gcov source.f90` to generate `source.f90.gcov` text files
2. Parse the well-documented text format
3. This eliminates 1000+ lines of complex binary parsing code

## Test Specifications (RED Phase)

### Test 1: Execute basic gcov command
**Given**: A source file with coverage data (test.f90, test.gcno, test.gcda)
**When**: Executing `gcov test.f90`
**Then**: Should generate test.f90.gcov file successfully

### Test 2: Handle missing GCDA file
**Given**: Only .gcno file present (no execution data)
**When**: Executing `gcov -n test.f90` (no execution data)
**Then**: Should generate .gcov file showing zero execution counts

### Test 3: Handle gcov command failure
**Given**: Invalid or missing source file
**When**: Executing gcov command
**Then**: Should return error status and clear error message

### Test 4: Execute gcov with branch coverage
**Given**: Source file compiled with branch coverage
**When**: Executing `gcov -b test.f90`
**Then**: Should generate .gcov file with branch information

### Test 5: Handle working directory changes
**Given**: Coverage files in different directory than source
**When**: Executing gcov with proper path resolution
**Then**: Should find and process coverage files correctly

### Test 6: Cleanup temporary files
**Given**: Generated .gcov files after processing
**When**: Completing coverage analysis
**Then**: Should optionally clean up temporary .gcov files

### Test 7: Handle gcov version differences
**Given**: Different versions of gcov tool
**When**: Executing gcov command
**Then**: Should work with standard gcov text output format

### Test 8: Process multiple source files
**Given**: Multiple Fortran source files with coverage
**When**: Executing gcov on each file
**Then**: Should generate corresponding .gcov files for each

## Implementation Notes (GREEN Phase)
- Use execute_command_line() for gcov execution
- Implement proper error handling for command failures
- Support configurable gcov options
- Handle file path resolution for coverage data discovery

## Technical Details
- File: `src/gcov_command_executor.f90` (NEW: replaces binary parser)
- Test file: `test/test_gcov_command_executor.f90`
- Dependencies: file_utils (for path resolution)
- Test data: Real Fortran programs with gfortran-generated coverage
- **Benefit**: ~50 lines instead of 1000+ lines of binary parsing code