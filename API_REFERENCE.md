# FortCov API Reference

Complete API documentation for FortCov modules, interfaces, and types. This guide is for developers extending FortCov or integrating it into other tools.

## Core Architecture

FortCov follows a modular architecture with clean separation of concerns:

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   CLI/Config    │───▶│  Coverage Core  │───▶│    Reporters    │
│                 │    │                 │    │                 │
│ - Arguments     │    │ - Model         │    │ - Markdown      │
│ - Configuration │    │ - Statistics    │    │ - JSON          │
│ - Validation    │    │ - Engine        │    │ - HTML          │
└─────────────────┘    └─────────────────┘    └─────────────────┘
        │                       │                       │
        ▼                       ▼                       ▼
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│ Error Handling  │    │     Parsers     │    │   Utilities     │
│                 │    │                 │    │                 │
│ - Error Types   │    │ - GCOV Parser   │    │ - File Utils    │
│ - Context       │    │ - LCOV Parser   │    │ - String Utils  │
│ - Recovery      │    │ - JSON Parser   │    │ - Validation    │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

## Module Reference

### Core Modules

#### `fortcov` - Main Interface Module

**Purpose**: Main entry point and high-level interface

```fortran
module fortcov
    use fortcov_config, only: config_t, parse_config, show_help, show_version
    use coverage_engine, only: analyze_coverage, EXIT_SUCCESS, EXIT_FAILURE
    
    public :: run_coverage_analysis
    public :: config_t, parse_config, show_help, show_version
    public :: EXIT_SUCCESS, EXIT_FAILURE
```

**Public Interface**:

```fortran
! Main analysis function
function run_coverage_analysis(config) result(exit_code)
    type(config_t), intent(in) :: config
    integer :: exit_code
```

**Exit Codes**:
- `EXIT_SUCCESS` (0): Analysis completed successfully
- `EXIT_FAILURE` (1): Error during analysis
- `EXIT_THRESHOLD_NOT_MET` (2): Coverage below threshold
- `EXIT_NO_COVERAGE_DATA` (3): No coverage data found

#### `fortcov_config` - Configuration Management

**Purpose**: Command-line parsing and configuration management

```fortran
type :: config_t
    character(len=:), allocatable :: input_format      ! gcov, lcov, json
    character(len=:), allocatable :: output_format     ! markdown, json, html
    character(len=:), allocatable :: output_path       ! Output file path
    character(len=:), allocatable :: source_paths(:)   ! Source directories
    character(len=:), allocatable :: exclude_patterns(:) ! Exclude patterns
    character(len=:), allocatable :: gcov_executable   ! gcov command path
    real :: minimum_coverage                            ! Coverage threshold
    logical :: verbose                                  ! Verbose output
    logical :: quiet                                    ! Suppress output
    logical :: show_help                               ! Show help flag
    logical :: show_version                            ! Show version flag
    logical :: tui_mode                                ! Interactive mode
    logical :: strict_mode                             ! Strict validation
    ! ... additional fields
end type config_t
```

**Public Procedures**:

```fortran
! Parse command line arguments
subroutine parse_config(args, config, success, error_message)
    character(len=*), intent(in) :: args(:)
    type(config_t), intent(out) :: config
    logical, intent(out) :: success
    character(len=*), intent(out) :: error_message

! Initialize configuration with defaults
subroutine initialize_config(config)
    type(config_t), intent(out) :: config

! Validate configuration
subroutine validate_config(config, error_ctx)
    type(config_t), intent(in) :: config
    type(error_context_t), intent(out) :: error_ctx

! Load configuration from file
subroutine load_config_file(filename, config, success)
    character(len=*), intent(in) :: filename
    type(config_t), intent(inout) :: config
    logical, intent(out) :: success

! Display help message
subroutine show_help()

! Display version information
subroutine show_version()
```

#### `coverage_model` - Data Structures

**Purpose**: Core data structures for coverage information

```fortran
! Line coverage information
type :: line_coverage_t
    integer :: line_number          ! Source line number
    integer :: execution_count      ! Times executed
    logical :: executable          ! Is this line executable?
    logical :: covered             ! Was this line covered?
end type line_coverage_t

! Function coverage information
type :: function_coverage_t
    character(len=:), allocatable :: name      ! Function name
    integer :: start_line                      ! First line
    integer :: end_line                        ! Last line
    integer :: statements                      ! Total statements
    integer :: covered_statements              ! Covered statements
    real :: coverage_percentage                ! Coverage %
end type function_coverage_t

! File coverage information
type :: file_coverage_t
    character(len=:), allocatable :: filename         ! Source file path
    character(len=:), allocatable :: relative_path    ! Relative path
    type(line_coverage_t), allocatable :: lines(:)    ! Line coverage
    type(function_coverage_t), allocatable :: functions(:) ! Function coverage
    integer :: total_lines                             ! Total lines
    integer :: executable_lines                       ! Executable lines
    integer :: covered_lines                          ! Covered lines
    real :: coverage_percentage                       ! File coverage %
end type file_coverage_t

! Overall coverage summary
type :: coverage_summary_t
    type(file_coverage_t), allocatable :: files(:)    ! All files
    integer :: total_files                             ! File count
    integer :: total_statements                        ! Total statements
    integer :: covered_statements                      ! Covered statements
    real :: overall_coverage                          ! Overall %
    character(len=:), allocatable :: timestamp       ! Analysis time
end type coverage_summary_t
```

#### `coverage_engine` - Analysis Engine

**Purpose**: Main coverage analysis orchestration

```fortran
! Main analysis function
function analyze_coverage(config) result(exit_code)
    type(config_t), intent(in) :: config
    integer :: exit_code

! Discover coverage files
subroutine discover_coverage_files(source_paths, exclude_patterns, files, count)
    character(len=*), intent(in) :: source_paths(:)
    character(len=*), intent(in) :: exclude_patterns(:)
    character(len=:), allocatable, intent(out) :: files(:)
    integer, intent(out) :: count

! Parse coverage data
subroutine parse_coverage_data(files, summary, error_ctx)
    character(len=*), intent(in) :: files(:)
    type(coverage_summary_t), intent(out) :: summary
    type(error_context_t), intent(out) :: error_ctx

! Generate reports
subroutine generate_reports(summary, config, error_ctx)
    type(coverage_summary_t), intent(in) :: summary
    type(config_t), intent(in) :: config
    type(error_context_t), intent(out) :: error_ctx
```

### Parser Modules

#### `coverage_parser` - Parser Interface

**Purpose**: Abstract interface for coverage parsers

```fortran
! Abstract parser interface
abstract interface
    subroutine parse_coverage_file_interface(filename, file_coverage, error_ctx)
        character(len=*), intent(in) :: filename
        type(file_coverage_t), intent(out) :: file_coverage
        type(error_context_t), intent(out) :: error_ctx
    end subroutine parse_coverage_file_interface
end interface

! Parser registry type
type :: parser_registry_t
    procedure(parse_coverage_file_interface), pointer :: gcov_parser => null()
    procedure(parse_coverage_file_interface), pointer :: lcov_parser => null()
    procedure(parse_coverage_file_interface), pointer :: json_parser => null()
end type parser_registry_t
```

### Reporter Modules

#### `coverage_reporter` - Reporter Interface

**Purpose**: Abstract interface for coverage reporters

```fortran
! Abstract reporter interface
abstract interface
    subroutine generate_report_interface(summary, output_path, error_ctx)
        type(coverage_summary_t), intent(in) :: summary
        character(len=*), intent(in) :: output_path
        type(error_context_t), intent(out) :: error_ctx
    end subroutine generate_report_interface
end interface

! Reporter registry
type :: reporter_registry_t
    procedure(generate_report_interface), pointer :: markdown_reporter => null()
    procedure(generate_report_interface), pointer :: json_reporter => null()
    procedure(generate_report_interface), pointer :: html_reporter => null()
end type reporter_registry_t
```

#### `markdown_reporter` - Markdown Output

**Purpose**: Generate Markdown coverage reports

```fortran
! Generate markdown report
subroutine generate_markdown_report(summary, output_path, error_ctx)
    type(coverage_summary_t), intent(in) :: summary
    character(len=*), intent(in) :: output_path
    type(error_context_t), intent(out) :: error_ctx

! Format coverage table
subroutine format_coverage_table(files, table_content)
    type(file_coverage_t), intent(in) :: files(:)
    character(len=:), allocatable, intent(out) :: table_content

! Format missing lines
subroutine format_missing_lines(line_coverage, missing_ranges)
    type(line_coverage_t), intent(in) :: line_coverage(:)
    character(len=:), allocatable, intent(out) :: missing_ranges
```

### Utility Modules

#### `error_handling` - Error Management

**Purpose**: Comprehensive error handling and user feedback

```fortran
! Error context type
type :: error_context_t
    integer :: error_code = ERROR_SUCCESS
    character(len=512) :: message = ""
    character(len=512) :: suggestion = ""
    character(len=256) :: context = ""
    character(len=2048) :: stack_trace = ""
    logical :: recoverable = .false.
    logical :: logged = .false.
end type error_context_t
```

**Error Codes**:
```fortran
integer, parameter :: ERROR_SUCCESS = 0
integer, parameter :: ERROR_MISSING_SOURCE_FILE = 1004
integer, parameter :: ERROR_PERMISSION_DENIED = 1005
integer, parameter :: ERROR_OUT_OF_MEMORY = 1006
integer, parameter :: ERROR_INVALID_CONFIG = 1007
integer, parameter :: ERROR_PARTIAL_PROCESSING = 1008
integer, parameter :: ERROR_THRESHOLD_NOT_MET = 1011
integer, parameter :: ERROR_FILE_ACCESS = 1020
integer, parameter :: ERROR_MISSING_FILE = 1021
integer, parameter :: ERROR_FILE_TOO_LARGE = 1022
integer, parameter :: ERROR_INVALID_DATA = 1024
```

**Error Handlers**:
```fortran
! Common error scenarios
subroutine handle_missing_source(source_file, error_ctx)
subroutine handle_permission_denied(file_path, error_ctx)
subroutine handle_out_of_memory(requested_size, error_ctx)
subroutine handle_invalid_config(config_file, line_number, error_ctx)
subroutine handle_no_coverage_files(source_path, error_ctx)
subroutine handle_gcov_not_found(gcov_path, error_ctx)
subroutine handle_invalid_arguments(arg_name, error_ctx)
subroutine handle_threshold_not_met(current_coverage, required_coverage, error_ctx)
```

#### `file_utils` - File Operations

**Purpose**: File system utilities and operations

```fortran
! Check if file exists and is readable
function file_exists(filename) result(exists)
    character(len=*), intent(in) :: filename
    logical :: exists

! Get file size
function get_file_size(filename) result(size_bytes)
    character(len=*), intent(in) :: filename
    integer :: size_bytes

! Read file contents
subroutine read_file_contents(filename, contents, error_ctx)
    character(len=*), intent(in) :: filename
    character(len=:), allocatable, intent(out) :: contents
    type(error_context_t), intent(out) :: error_ctx

! Write file contents
subroutine write_file_contents(filename, contents, error_ctx)
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: contents
    type(error_context_t), intent(out) :: error_ctx

! Create directory if it doesn't exist
subroutine ensure_directory_exists(path, error_ctx)
    character(len=*), intent(in) :: path
    type(error_context_t), intent(out) :: error_ctx
```

#### `string_utils` - String Manipulation

**Purpose**: String processing utilities

```fortran
! Split string by delimiter
subroutine split_string(input, delimiter, parts, count)
    character(len=*), intent(in) :: input
    character(len=*), intent(in) :: delimiter
    character(len=:), allocatable, intent(out) :: parts(:)
    integer, intent(out) :: count

! Trim whitespace and control characters
function trim_string(input) result(output)
    character(len=*), intent(in) :: input
    character(len=:), allocatable :: output

! Check if string matches pattern
function matches_pattern(string, pattern) result(matches)
    character(len=*), intent(in) :: string
    character(len=*), intent(in) :: pattern
    logical :: matches

! Escape special characters for output format
function escape_string(input, format) result(output)
    character(len=*), intent(in) :: input
    character(len=*), intent(in) :: format  ! 'markdown', 'json', 'html'
    character(len=:), allocatable :: output
```

#### `input_validation` - Input Validation

**Purpose**: Validate user inputs and data integrity

```fortran
! Validate file path
function is_valid_path(path) result(valid)
    character(len=*), intent(in) :: path
    logical :: valid

! Validate coverage threshold
function is_valid_threshold(threshold) result(valid)
    real, intent(in) :: threshold
    logical :: valid

! Validate output format
function is_valid_output_format(format) result(valid)
    character(len=*), intent(in) :: format
    logical :: valid

! Validate configuration
subroutine validate_configuration(config, error_ctx)
    type(config_t), intent(in) :: config
    type(error_context_t), intent(out) :: error_ctx
```

## Usage Examples

### Basic Coverage Analysis

```fortran
program example_basic
    use fortcov
    use fortcov_config
    use error_handling
    implicit none
    
    type(config_t) :: config
    integer :: exit_code
    
    ! Initialize configuration
    call initialize_config(config)
    
    ! Set basic options
    config%source_paths = ['src/']
    config%output_path = 'coverage.md'
    config%output_format = 'markdown'
    config%minimum_coverage = 80.0
    
    ! Run analysis
    exit_code = run_coverage_analysis(config)
    
    select case (exit_code)
    case (EXIT_SUCCESS)
        print *, 'Coverage analysis completed successfully'
    case (EXIT_THRESHOLD_NOT_MET)
        print *, 'Coverage below threshold'
    case (EXIT_NO_COVERAGE_DATA)
        print *, 'No coverage data found'
    case default
        print *, 'Analysis failed with error code:', exit_code
    end select
end program example_basic
```

### Custom Error Handling

```fortran
program example_error_handling
    use fortcov_config
    use error_handling
    implicit none
    
    type(config_t) :: config
    type(error_context_t) :: error_ctx
    character(len=:), allocatable :: args(:)
    logical :: success
    character(len=256) :: error_message
    
    ! Example command line arguments
    args = ['--source=src', '--output=coverage.md', '--fail-under=85']
    
    ! Parse configuration with error handling
    call parse_config(args, config, success, error_message)
    
    if (.not. success) then
        if (config%show_help) then
            call show_help()
        else
            ! Handle parsing error
            call handle_invalid_arguments(error_message, error_ctx)
            
            print *, 'Error:', trim(error_ctx%message)
            print *, 'Suggestions:'
            print *, trim(error_ctx%suggestion)
        end if
        stop 1
    end if
    
    ! Validate configuration
    call validate_config(config, error_ctx)
    if (error_ctx%error_code /= ERROR_SUCCESS) then
        print *, 'Configuration error:', trim(error_ctx%message)
        print *, 'Suggestion:', trim(error_ctx%suggestion)
        stop 1
    end if
    
    print *, 'Configuration validated successfully'
end program example_error_handling
```

### Custom Reporter Implementation

```fortran
module custom_reporter
    use coverage_model
    use error_handling
    implicit none
    
contains
    
    subroutine generate_csv_report(summary, output_path, error_ctx)
        type(coverage_summary_t), intent(in) :: summary
        character(len=*), intent(in) :: output_path
        type(error_context_t), intent(out) :: error_ctx
        
        integer :: unit, i
        
        ! Initialize error context
        error_ctx%error_code = ERROR_SUCCESS
        
        ! Open output file
        open(newunit=unit, file=output_path, status='replace', iostat=error_ctx%error_code)
        if (error_ctx%error_code /= 0) then
            call handle_permission_denied(output_path, error_ctx)
            return
        end if
        
        ! Write CSV header
        write(unit, '(A)') 'filename,statements,covered,coverage_percent'
        
        ! Write data for each file
        do i = 1, size(summary%files)
            associate(file => summary%files(i))
                write(unit, '(A,A,I0,A,I0,A,F0.2)') &
                    trim(file%filename), ',', &
                    file%executable_lines, ',', &
                    file%covered_lines, ',', &
                    file%coverage_percentage
            end associate
        end do
        
        ! Write summary
        write(unit, '(A,I0,A,I0,A,F0.2)') &
            'TOTAL,', summary%total_statements, ',', &
            summary%covered_statements, ',', summary%overall_coverage
        
        close(unit)
    end subroutine generate_csv_report
    
end module custom_reporter
```

### Data Structure Access

```fortran
program example_data_access
    use coverage_model
    use coverage_engine
    use fortcov_config
    implicit none
    
    type(config_t) :: config
    type(coverage_summary_t) :: summary
    type(error_context_t) :: error_ctx
    integer :: i, j
    
    ! Configure and run analysis
    call initialize_config(config)
    config%source_paths = ['src/']
    
    ! This would typically be called internally by run_coverage_analysis
    ! call parse_coverage_data(['src/module.f90.gcov'], summary, error_ctx)
    
    ! Access coverage data
    print *, 'Overall coverage:', summary%overall_coverage, '%'
    print *, 'Total files:', summary%total_files
    
    ! Iterate through files
    do i = 1, size(summary%files)
        associate(file => summary%files(i))
            print *, 'File:', trim(file%filename)
            print *, '  Coverage:', file%coverage_percentage, '%'
            print *, '  Lines:', file%covered_lines, '/', file%executable_lines
            
            ! Access function-level coverage
            do j = 1, size(file%functions)
                associate(func => file%functions(j))
                    print *, '  Function:', trim(func%name), '(', func%coverage_percentage, '%)'
                end associate
            end do
        end associate
    end do
end program example_data_access
```

## Extension Points

### Adding New Input Formats

1. **Create Parser Module**:
```fortran
module my_format_parser
    use coverage_model
    use error_handling
    
contains
    
    subroutine parse_my_format(filename, file_coverage, error_ctx)
        character(len=*), intent(in) :: filename
        type(file_coverage_t), intent(out) :: file_coverage
        type(error_context_t), intent(out) :: error_ctx
        
        ! Implementation here
    end subroutine parse_my_format
    
end module my_format_parser
```

2. **Register Parser**:
```fortran
! In coverage_engine or main initialization
parser_registry%my_format_parser => parse_my_format
```

### Adding New Output Formats

1. **Create Reporter Module**:
```fortran
module my_reporter
    use coverage_model
    use error_handling
    
contains
    
    subroutine generate_my_report(summary, output_path, error_ctx)
        type(coverage_summary_t), intent(in) :: summary
        character(len=*), intent(in) :: output_path
        type(error_context_t), intent(out) :: error_ctx
        
        ! Implementation here
    end subroutine generate_my_report
    
end module my_reporter
```

2. **Register Reporter**:
```fortran
! In coverage_engine or main initialization
reporter_registry%my_reporter => generate_my_report
```

## Best Practices

### Error Handling

1. **Always check error contexts**:
```fortran
call some_operation(input, output, error_ctx)
if (error_ctx%error_code /= ERROR_SUCCESS) then
    ! Handle error appropriately
    return
end if
```

2. **Provide helpful error messages**:
```fortran
call handle_missing_source(filename, error_ctx)
! This provides structured error message and suggestions
```

3. **Use appropriate error codes**:
```fortran
error_ctx%error_code = ERROR_MISSING_FILE  ! For missing files
error_ctx%recoverable = .false.            ! If operation cannot continue
```

### Memory Management

1. **Always deallocate allocatable arrays**:
```fortran
! Fortran automatically deallocates when variables go out of scope
! But be explicit for clarity in long procedures
if (allocated(summary%files)) deallocate(summary%files)
```

2. **Use `move_alloc` for performance**:
```fortran
! Transfer ownership without copying
call move_alloc(temp_array, final_array)
```

### Configuration

1. **Initialize configurations properly**:
```fortran
call initialize_config(config)  ! Always initialize first
! Then modify specific fields
config%minimum_coverage = 85.0
```

2. **Validate all user inputs**:
```fortran
call validate_config(config, error_ctx)
if (error_ctx%error_code /= ERROR_SUCCESS) then
    ! Handle validation error
end if
```

## Thread Safety

FortCov modules are **not thread-safe**. Each analysis should run in a separate process or be serialized if multiple analyses are needed.

## Compilation Requirements

- **Fortran Standard**: Fortran 2008 or later
- **Compiler**: gfortran 9.0+, ifort 19.0+, or equivalent
- **Dependencies**: None (self-contained)
- **Build System**: Fortran Package Manager (fpm)

For complete compilation instructions, see [INSTALLATION.md](INSTALLATION.md).

## Version Compatibility

API compatibility is maintained within major versions. Breaking changes are documented in release notes and migration guides are provided.

**Current Version**: 1.0.0  
**API Stability**: Stable for 1.x releases