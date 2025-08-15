# Issue #11: Implement Coverage Engine ✅ COMPLETED

## User Story
As a fortcov user, I want a central engine that orchestrates the coverage analysis workflow so that I can process coverage data with a single command.

## Acceptance Criteria
- [x] Coordinate parser selection based on input files
- [x] Process coverage data through statistics module
- [x] Generate reports using selected reporter
- [x] Handle errors gracefully
- [x] Return appropriate exit codes

## Test Specifications (RED Phase)

### Test 1: End-to-end coverage analysis  
**Given**: Source files with .gcno/.gcda coverage data
**When**: Running analyze_coverage() with gcov text parsing
**Then**: Should execute gcov commands and produce markdown report

### Test 2: Gcov command execution workflow
**Given**: Directory with Fortran source files and coverage data  
**When**: Processing coverage with simplified pipeline
**Then**: Should run gcov, parse .gcov text files, generate markdown

### Test 3: Multiple source directories
**Given**: Config with source_paths=["src", "lib"]
**When**: Processing coverage
**Then**: Should find coverage files in both directories

### Test 4: Exclude pattern filtering
**Given**: Config with exclude_patterns=["test/*"]
**When**: Processing files
**Then**: Should skip files matching exclude patterns

### Test 5: Coverage threshold enforcement
**Given**: Config with minimum_coverage=80.0, actual=75.0
**When**: Completing analysis
**Then**: Should return non-zero exit code

### Test 6: Handle missing coverage data
**Given**: No .gcda files (code not executed) 
**When**: Running gcov analysis with -n flag
**Then**: Should generate .gcov files showing 0% coverage

### Test 7: Gcov command error handling
**Given**: gcov command fails or missing gcov tool
**When**: Executing coverage analysis
**Then**: Should report clear error and continue with other files

### Test 8: Reporter error handling
**Given**: Invalid output path (no permissions)
**When**: Generating report
**Then**: Should report error with clear message

### Test 9: Verbose output
**Given**: Config with verbose=.true.
**When**: Processing coverage
**Then**: Should print progress messages

### Test 10: Quiet mode
**Given**: Config with quiet=.true.
**When**: Processing coverage
**Then**: Should suppress all non-error output

## Implementation Notes (GREEN Phase)
- Implement as orchestrator, not business logic
- Delegate to appropriate modules
- Collect and report all errors
- Clean separation of concerns

## Technical Details
- File: `src/coverage_engine.f90`
- Test file: `test/test_coverage_engine.f90`
- Dependencies: All other modules
- This is the main coordination module