# Issue #12: Implement Command-Line Application ✅ COMPLETED

## User Story
As an end user, I want a command-line interface to fortcov so that I can generate coverage reports from my terminal.

## Acceptance Criteria
- [x] Parse command-line arguments
- [x] Initialize configuration
- [x] Invoke coverage engine
- [x] Handle errors with proper messages
- [x] Return appropriate exit codes

## Test Specifications (RED Phase)

### Test 1: Basic execution with gcov files
**Given**: Directory with .gcda/.gcno files
**When**: Running `fortcov`
**Then**: Should generate markdown report to stdout

### Test 2: Specify output file
**Given**: Command `fortcov --output=report.md`
**When**: Executing
**Then**: Should write report to report.md file

### Test 3: Help message
**Given**: Command `fortcov --help`
**When**: Executing
**Then**: Should display usage and exit with code 0

### Test 4: Version information
**Given**: Command `fortcov --version`
**When**: Executing
**Then**: Should display version and exit with code 0

### Test 5: Invalid arguments
**Given**: Command `fortcov --invalid-option`
**When**: Executing
**Then**: Should display error and exit with code 1

### Test 6: No coverage files found
**Given**: Directory with no coverage files
**When**: Running `fortcov`
**Then**: Should display warning and generate empty report

### Test 7: Coverage below threshold
**Given**: Command `fortcov --fail-under=90` with 80% coverage
**When**: Executing
**Then**: Should exit with code 2

### Test 8: Successful threshold check
**Given**: Command `fortcov --fail-under=70` with 80% coverage
**When**: Executing
**Then**: Should exit with code 0

### Test 9: Multiple source directories
**Given**: Command `fortcov --source=src --source=lib`
**When**: Executing
**Then**: Should process files from both directories

### Test 10: Verbose output
**Given**: Command `fortcov --verbose`
**When**: Executing
**Then**: Should display detailed processing information

## Implementation Notes (GREEN Phase)
- Minimal logic in main program
- Delegate to config and engine modules
- Clear error messages for users
- Consistent exit codes for CI/CD

## Technical Details
- File: `app/main.f90`
- Test file: `test/test_cli.f90`
- Dependencies: fortcov_config, coverage_engine
- Exit codes: 0=success, 1=error, 2=threshold_failure