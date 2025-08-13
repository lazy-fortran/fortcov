# FortCov Design Document

## Executive Summary

FortCov is a coverage analysis tool specifically designed for Fortran projects. It processes coverage data from various compilers and generates comprehensive reports in multiple formats. The architecture prioritizes extensibility, clean abstractions, and exceptional Fortran language support.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                         Command Line Interface                   │
├─────────────────────────────────────────────────────────────────┤
│                         Coverage Engine                          │
├──────────────────┬──────────────────┬──────────────────────────┤
│   Parser Layer   │  Analysis Core   │    Reporter Layer         │
├──────────────────┼──────────────────┼──────────────────────────┤
│ GCov Parser      │ Coverage Model   │ Markdown Reporter         │
│ Cobertura Parser │ Statistics       │ Cobertura Reporter        │
│ LCOV Parser      │ Diff Engine      │ LCOV Reporter             │
│ (Future: Intel)  │ Aggregation      │ JSON Reporter             │
└──────────────────┴──────────────────┴──────────────────────────┘
```

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

### 3. GCov Parser (`gcov_parser`)

**Purpose**: Parse GCC/gfortran coverage data (.gcda/.gcno files).

**Implementation Details**:
- Binary format parsing for .gcno (compile-time graph)
- Binary format parsing for .gcda (runtime data)
- Arc graph reconstruction
- Block execution count calculation
- Line mapping from debug information

**Fortran-Specific Handling**:
- Module boundary detection
- Contains block processing
- Interface block handling
- Submodule support
- Generic procedure mapping

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

## Data Flow

1. **Input Phase**:
   - CLI parses arguments into `config_t`
   - File discovery based on configuration
   - Parser selection based on file extensions

2. **Parsing Phase**:
   - Selected parser reads coverage files
   - Binary/text data decoded
   - Unified `coverage_data_t` constructed

3. **Analysis Phase**:
   - Statistics calculated
   - Coverage percentages computed
   - Missing line ranges identified

4. **Reporting Phase**:
   - Reporter selected based on configuration
   - Report generated from unified model
   - Output written to file/stdout

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

## Testing Strategy

### Unit Testing

Each module has corresponding test module:
- `test_coverage_model`: Model serialization/deserialization
- `test_gcov_parser`: Binary format parsing
- `test_markdown_reporter`: Report generation
- `test_coverage_statistics`: Metric calculations
- `test_string_utils`: String manipulation

### Integration Testing

- End-to-end coverage workflow
- Sample Fortran projects with known coverage
- Regression tests against reference outputs

### Test Data

- Minimal coverage files for unit tests
- Real-world Fortran project samples
- Edge cases (empty files, 100% coverage, etc.)

## Performance Considerations

- Lazy file reading for large projects
- Efficient binary parsing without full file loads
- String buffer reuse for report generation
- Parallel file processing capability (future)

## Error Handling

- Graceful degradation for corrupt coverage files
- Clear error messages with file locations
- Validation of coverage data consistency
- Non-zero exit codes for CI/CD integration

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

## KISS Principle

- Unified internal model simplifies processing
- Single-pass parsing where possible
- Direct markdown generation without templates
- Minimal external dependencies