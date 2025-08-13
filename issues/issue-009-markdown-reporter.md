# Issue #9: Implement Markdown Reporter

## User Story
As a developer, I want markdown-formatted coverage reports so that I can view coverage in GitHub PRs and documentation.

## Acceptance Criteria
- [ ] Generate markdown table with coverage data
- [ ] Format percentages with colors/indicators
- [ ] Compress missing line ranges
- [ ] Include summary row
- [ ] Sort files by name or coverage

## Test Specifications (RED Phase)

### Test 1: Generate basic coverage table
**Given**: Coverage data for 2 files
**When**: Generating markdown report
**Then**: Should produce table with columns: Filename, Stmts, Miss, Cover, Missing

### Test 2: Format coverage percentages
**Given**: File with 85.5% coverage
**When**: Formatting percentage
**Then**: Should display "85.50%"

### Test 3: Compress missing line ranges
**Given**: Uncovered lines [10,11,12,20,21,30]
**When**: Formatting missing column
**Then**: Should display "10-12, 20-21, 30"

### Test 4: Generate TOTAL row
**Given**: Multiple files with coverage
**When**: Generating report
**Then**: Last row should show "TOTAL" with aggregate statistics

### Test 5: Handle 100% coverage
**Given**: File with complete coverage
**When**: Generating row
**Then**: Missing column should be empty

### Test 6: Handle 0% coverage
**Given**: File with no coverage
**When**: Generating row
**Then**: Should show "0.00%" and list all lines as missing

### Test 7: Sort files alphabetically
**Given**: Files in random order
**When**: Generating report with sort=name
**Then**: Files should appear alphabetically

### Test 8: Column alignment
**Given**: Files with varying name lengths
**When**: Generating table
**Then**: Columns should align properly with padding

### Test 9: Escape special markdown characters
**Given**: Filename with | character
**When**: Generating table row
**Then**: Should escape as \|

### Test 10: Module hierarchy display
**Given**: Fortran modules with procedures
**When**: Generating report
**Then**: Should show indented procedure names under modules

### Test 11: Table header formatting
**Given**: Any coverage data
**When**: Generating report
**Then**: Should include header row with separator line

### Test 12: Large line range handling
**Given**: File with ranges >1000 lines
**When**: Formatting missing column
**Then**: Should truncate with "..." if too long

## Implementation Notes (GREEN Phase)
- Build table row by row with string concatenation
- Use string_utils for range compression
- Calculate column widths dynamically
- Format with fixed-width columns

## Technical Details
- File: `src/markdown_reporter.f90`
- Test file: `test/test_markdown_reporter.f90`
- Dependencies: coverage_reporter, coverage_model, coverage_statistics, string_utils
- Output format must match the specification exactly