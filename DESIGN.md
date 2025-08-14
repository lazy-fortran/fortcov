# FortCov Design Document

## Executive Summary

FortCov is a coverage analysis tool specifically designed for Fortran projects that leverages GCC's gcov tool to generate markdown coverage reports. The architecture follows the proven approach used by LCOV: **run gcov command to generate text files, then parse those**. This eliminates complex binary format parsing and provides a fast path to working coverage reports.

## Architecture Overview - SIMPLIFIED APPROACH

```
┌─────────────────────────────────────────────────────────────────┐
│                         Command Line Interface                   │
├─────────────────────────────────────────────────────────────────┤
│                         Coverage Engine                          │
├──────────────────┬──────────────────┬──────────────────────────┤
│   Parser Layer   │  Analysis Core   │    Reporter Layer         │
├──────────────────┼──────────────────┼──────────────────────────┤
│ GCov Text Parser │ Coverage Model   │ Markdown Reporter         │
│ (Future: LCOV)   │ Statistics       │ (Future: JSON)            │
│ (Future: Intel)  │ Diff Engine      │ (Future: Cobertura)       │
│                  │                  │                          │
└──────────────────┴──────────────────┴──────────────────────────┘

PROVEN PIPELINE: .gcda/.gcno → gcov command → .gcov text → parse → markdown
```

## Key Architectural Decision: Use gcov Tool, Not Binary Parsing

**CRITICAL**: We do NOT parse binary .gcda/.gcno files directly. Instead, we follow the mature approach used by LCOV and other successful coverage tools:

1. **Execute gcov command** on source files
2. **Parse generated .gcov text files** (well-documented format)
3. **Transform to internal model** for analysis and reporting
4. **Generate markdown output** quickly and reliably

This approach is:
- **Simpler**: Text parsing vs. complex binary format engineering
- **Maintainable**: Leverages GCC's own gcov tool for format handling
- **Proven**: Used successfully by LCOV for decades
- **Fast**: Gets us working coverage reports immediately

## Core Modules

### 1. Coverage Model (`coverage_model`)

**Purpose**: Define the unified internal representation of coverage data.

**Types**:
- `source_location_t`: Represents a location in source code
  - `filename`: Source file path
  - `line_number`: Line number
  - `column_start`, `column_end`: Optional column range

- `coverage_line_t`: Line coverage information
  - `location`: Source location
  - `execution_count`: Number of times executed
  - `is_executable`: Whether line contains executable code

- `coverage_branch_t`: Branch coverage information
  - `location`: Source location
  - `taken_count`: Times branch taken
  - `not_taken_count`: Times branch not taken
  - `branch_id`: Unique identifier

- `coverage_function_t`: Function/subroutine coverage
  - `name`: Function/subroutine name
  - `location`: Source location
  - `execution_count`: Call count
  - `lines`: Array of covered lines
  - `branches`: Array of branches
  - `is_module_procedure`: Fortran-specific flag
  - `parent_module`: Optional parent module name

- `coverage_file_t`: File-level coverage
  - `filename`: File path
  - `lines`: Array of line coverage
  - `functions`: Array of function coverage
  - `modules`: Array of module coverage (Fortran-specific)

- `coverage_data_t`: Complete coverage dataset
  - `files`: Array of file coverage
  - `metadata`: Compilation/execution metadata

### 2. Parser Abstraction (`coverage_parser`)

**Purpose**: Abstract interface for all coverage data parsers.

**Abstract Type**: `coverage_parser_t`
- `parse(path) -> coverage_data_t`: Parse coverage files
- `can_parse(path) -> logical`: Check if parser supports format
- `get_required_files() -> string array`: List required file extensions

### 3. GCov Text Parser (`gcov_parser`)

**Purpose**: Execute gcov command and parse generated .gcov text files.

**Implementation Details**:
- **Execute gcov command**: `gcov source.f90` generates `source.f90.gcov`
- **Parse gcov text format**: Well-documented format `count:line_num:source_code`
- **Handle gcov output patterns**: `-` (non-executable), `#####` (uncovered), numeric (executed)
- **Extract line-by-line coverage**: Direct mapping from gcov output to coverage model
- **Support branch info**: Parse gcov branch coverage if available

**Gcov Text Format**:
```
        -:    0:Source:test.f90
        -:    0:Graph:test.gcno  
        -:    0:Data:test.gcda
        1:    1:program test
        2:    2:    print *, "hello"
    #####:    3:    if (.false.) print *, "never"
        -:    4:end program
```

**Fortran-Specific Handling**:
- Module procedure recognition from gcov function sections
- Proper handling of Fortran line continuation
- Interface block detection (marked as non-executable)
- Contains block processing
- Generic procedure mapping via function names

### 4. Coverage Statistics (`coverage_statistics`)

**Purpose**: Calculate coverage metrics from the unified model.

**Functions**:
- `calculate_line_coverage(coverage_data_t) -> coverage_stats_t`
- `calculate_branch_coverage(coverage_data_t) -> coverage_stats_t`
- `calculate_function_coverage(coverage_data_t) -> coverage_stats_t`
- `aggregate_file_coverage(files) -> coverage_summary_t`

**Types**:
- `coverage_stats_t`: Coverage statistics
  - `total_items`: Total countable items
  - `covered_items`: Covered items
  - `percentage`: Coverage percentage
  - `missing_ranges`: Uncovered line ranges

### 5. Reporter Abstraction (`coverage_reporter`)

**Purpose**: Abstract interface for all report generators.

**Abstract Type**: `coverage_reporter_t`
- `generate_report(coverage_data, output_path)`
- `get_format_name() -> string`
- `supports_diff() -> logical`

### 6. Markdown Reporter (`markdown_reporter`)

**Purpose**: Generate markdown-formatted coverage reports.

**Output Format**:
```markdown
| Filename                          |   Stmts |   Miss | Cover   | Missing            |
|-----------------------------------|---------|--------|---------|--------------------|
| src/module_a.f90                  |     100 |     10 | 90.00%  | 45-48, 72-77       |
| src/module_b.f90                  |      50 |      0 | 100.00% |                    |
| TOTAL                             |     150 |     10 | 93.33%  |                    |
```

**Features**:
- Sortable columns
- Missing line range compression
- Module/subroutine hierarchy display
- Summary statistics

### 7. Diff Engine (`coverage_diff`)

**Purpose**: Compare coverage between two datasets.

**Functions**:
- `diff_coverage(old, new) -> coverage_diff_t`
- `generate_diff_report(diff, reporter)`

**Types**:
- `coverage_diff_t`: Coverage difference
  - `added_coverage`: Newly covered lines
  - `removed_coverage`: Previously covered, now uncovered
  - `unchanged_coverage`: Unchanged coverage
  - `delta_percentage`: Change in coverage percentage

### 8. Configuration (`fortcov_config`)

**Purpose**: Manage configuration and command-line options.

**Types**:
- `config_t`: Configuration
  - `input_format`: Parser selection
  - `output_format`: Reporter selection
  - `source_paths`: Source file search paths
  - `exclude_patterns`: Files to exclude
  - `minimum_coverage`: Threshold for failure

### 9. File System Utilities (`file_utils`)

**Purpose**: Abstract file system operations for portability.

**Functions**:
- `find_files(pattern) -> string array`
- `resolve_path(path) -> absolute path`
- `read_binary_file(path) -> byte array`
- `write_text_file(path, content)`

### 10. String Utilities (`string_utils`)

**Purpose**: String manipulation utilities.

**Functions**:
- `compress_ranges(numbers) -> string`: "1,2,3,5,6" -> "1-3,5-6"
- `format_percentage(value) -> string`
- `trim(string) -> string`
- `split(string, delimiter) -> string array`

## Data Flow - SIMPLIFIED PIPELINE

1. **Input Discovery Phase**:
   - CLI parses arguments into `config_t`
   - Discover .gcno/.gcda files in source directories
   - Identify corresponding Fortran source files

2. **Gcov Execution Phase**:
   - Execute `gcov source.f90` for each source file
   - Generate .gcov text files with coverage data
   - Handle cases where .gcda missing (no execution data)

3. **Text Parsing Phase**:
   - Parse .gcov text files line by line
   - Extract execution counts and source lines
   - Build unified `coverage_data_t` from text format

4. **Analysis Phase**:
   - Calculate coverage percentages from execution counts
   - Compress missing line ranges for reporting
   - Compute file and project-level statistics

5. **Reporting Phase**:
   - Generate markdown tables with coverage data
   - Format percentages, missing ranges, and totals
   - Output to file or stdout

**Key Benefit**: Steps 2-3 replace complex binary parsing with simple text processing!

## Extension Points

### Adding New Input Formats

1. Implement `coverage_parser_t` interface
2. Register parser in parser factory
3. Add file extension mappings

### Adding New Output Formats

1. Implement `coverage_reporter_t` interface
2. Register reporter in reporter factory
3. Add CLI option for format selection

### Multi-Compiler Support

- Intel Fortran: Implement `intel_parser` module
- Flang/LLVM: Implement `llvm_parser` module
- Each parser translates to unified `coverage_data_t`

## Testing Strategy - FOCUSED ON FAST DELIVERY

### Unit Testing - MVP Focus

Priority modules for immediate implementation:
- `test_gcov_parser`: **TEXT parsing only** (much simpler!)
- `test_markdown_reporter`: Report generation with actual data
- `test_coverage_statistics`: Metric calculations from parsed data
- `test_string_utils`: Range compression and formatting

### Integration Testing - Real Coverage Data

- Test with actual .gcov files generated by gfortran
- Sample Fortran projects with known coverage results
- Compare our markdown output to expected results
- Edge cases: no coverage, 100% coverage, missing files

### Test Data - Use Real gcov Output

Instead of creating minimal binary files:
- **Use actual .gcov text files** from gfortran runs
- Create sample Fortran programs with various coverage scenarios
- Generate reference markdown reports for comparison
- Test error cases: missing files, malformed gcov output

**Key Advantage**: Testing text parsing is much easier than binary format validation!

## Performance Considerations - SIMPLIFIED

- **Text file processing**: Much faster than binary parsing
- **gcov command caching**: Reuse .gcov files if newer than source
- String buffer reuse for markdown generation  
- **Parallel gcov execution**: Run gcov commands in parallel (future)
- **Memory efficiency**: Process files one at a time, not batch loading

## Error Handling - ROBUST AND CLEAR

- **gcov command failures**: Clear error messages when gcov fails
- **Missing coverage files**: Graceful handling of .gcda absence  
- **Malformed gcov output**: Validation and error reporting
- **Source file access**: Clear messages for file permission issues
- **Non-zero exit codes**: Proper CI/CD integration
- **Incremental processing**: Skip files that cause errors, continue with others

**Benefit**: Text parsing errors are much easier to diagnose than binary corruption!

## SOLID Compliance

### Single Responsibility
- Each module has one clear purpose
- Parser modules only parse
- Reporter modules only report
- Statistics module only calculates

### Open/Closed
- Parser/Reporter abstractions allow extension
- New formats added without modifying core
- Configuration extensible via new options

### Liskov Substitution
- All parsers interchangeable via interface
- All reporters interchangeable via interface
- Consistent behavior across implementations

### Interface Segregation
- Small, focused interfaces
- Optional capabilities via separate interfaces
- No forced implementation of unused methods

### Dependency Inversion
- Core depends on abstractions, not implementations
- Parsers/Reporters depend on model abstraction
- Factory pattern for instantiation

## KISS Principle - DRAMATICALLY SIMPLIFIED

- **Use gcov tool**: Eliminate complex binary format engineering
- **Text parsing only**: Single-pass line-by-line processing
- **Direct markdown output**: No intermediate template processing
- **Minimal dependencies**: Just gcov command and text I/O
- **Proven approach**: Follow LCOV's successful methodology

## Summary: Why This Approach Wins

### Before (Binary Parsing Complexity)
- 1000+ lines of binary format parsing code
- Endianness handling, checksum validation, arc graph reconstruction
- Fragile parsing that breaks with GCC version changes  
- Difficult testing with mock binary data
- Slow development cycle due to complexity

### After (Text Parsing Simplicity)  
- ~200 lines of text parsing code
- Simple line-by-line parsing with well-documented format
- Robust: leverages GCC's own gcov tool for format handling
- Easy testing with real .gcov files
- **Fast path to working markdown reports**

### Implementation Priority
1. **gcov command execution** (execute_command_line)
2. **Text parsing** (parse .gcov format) 
3. **Markdown generation** (working coverage reports)
4. **Statistics and formatting** (percentages, ranges)
5. **Error handling and edge cases**

**Result**: Working coverage tool in days, not weeks!