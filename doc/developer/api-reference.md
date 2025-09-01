# FortCov API Reference

Programming interface documentation for FortCov modules.

## Core Modules

### coverage_engine

Main orchestration module for coverage analysis.

```fortran
use coverage_engine
```

**Primary Interface:**
```fortran
function execute_coverage_analysis(config) result(status)
    type(fortcov_config_t), intent(in) :: config
    integer :: status
end function
```

### fortcov_config

Configuration management module.

```fortran
use fortcov_config
```

**Configuration Type:**
```fortran
type :: fortcov_config_t
    character(len=:), allocatable :: source_paths(:)
    character(len=:), allocatable :: exclude_patterns(:)
    character(len=256) :: output_path
    character(len=32) :: output_format
    real :: minimum_coverage
    logical :: verbose
    logical :: quiet
end type
```

**Key Procedures:**
```fortran
! Load configuration from file
subroutine load_config_from_file(filename, config, status)

! Validate configuration parameters
function validate_config(config) result(is_valid)

! Apply command-line overrides
subroutine apply_cli_overrides(config, cli_args)
```

### coverage_model

Data structures for coverage information.

```fortran
use coverage_model
```

**Coverage Data Type:**
```fortran
type :: coverage_info_t
    character(len=256) :: filename
    integer :: total_lines
    integer :: covered_lines
    real :: coverage_percentage
    integer, allocatable :: missing_lines(:)
end type
```

**Coverage Summary Type:**
```fortran
type :: coverage_summary_t
    integer :: total_files
    integer :: total_lines
    integer :: covered_lines
    real :: overall_percentage
    type(coverage_info_t), allocatable :: files(:)
end type
```

### json_coverage_io

JSON input/output operations.

```fortran
use json_coverage_io
```

**Key Procedures:**
```fortran
! Export coverage data to JSON
subroutine export_coverage_json(coverage, filename, status)

! Import coverage data from JSON
subroutine import_coverage_json(filename, coverage, status)

! Validate JSON schema
function validate_json_schema(json_data) result(is_valid)
```

### coverage_reporter

Report generation module.

```fortran
use coverage_reporter
```

**Reporter Interface:**
```fortran
! Generate markdown report
subroutine generate_markdown_report(coverage, output_path, status)

! Generate HTML report
subroutine generate_html_report(coverage, output_path, status)

! Generate XML report (Cobertura format)
subroutine generate_xml_report(coverage, output_path, status)
```

## Error Handling

All API functions return status codes:

```fortran
integer, parameter :: STATUS_SUCCESS = 0
integer, parameter :: STATUS_ERROR_FILE_NOT_FOUND = 1
integer, parameter :: STATUS_ERROR_INVALID_CONFIG = 2
integer, parameter :: STATUS_ERROR_MEMORY_ALLOCATION = 3
integer, parameter :: STATUS_ERROR_IO = 4
```

## Usage Examples

### Basic Coverage Analysis

```fortran
program basic_example
    use coverage_engine
    use fortcov_config
    
    implicit none
    
    type(fortcov_config_t) :: config
    integer :: status
    
    ! Initialize configuration
    call initialize_default_config(config)
    config%source_paths = ['src/']
    config%output_path = 'coverage.md'
    config%minimum_coverage = 80.0
    
    ! Execute analysis
    status = execute_coverage_analysis(config)
    
    if (status /= STATUS_SUCCESS) then
        print *, 'Coverage analysis failed with status:', status
        stop 1
    end if
end program
```

### Configuration-Based Analysis

```fortran
program config_example
    use coverage_engine
    use fortcov_config
    
    implicit none
    
    type(fortcov_config_t) :: config
    integer :: status
    
    ! Load from configuration file
    call load_config_from_file('fortcov.nml', config, status)
    if (status /= STATUS_SUCCESS) then
        print *, 'Failed to load configuration'
        stop 1
    end if
    
    ! Execute analysis
    status = execute_coverage_analysis(config)
end program
```

## Thread Safety

Parallel processing is not implemented. FortCov is not thread-safe at this time:

- Do not run FortCov concurrently within the same process
- The `--threads` flag is reserved and currently ignored (single-threaded)
- File operations aim for safety (e.g., atomic writes) but are not a
  substitute for full synchronization primitives

## Memory Management

FortCov uses automatic memory management:

- Allocatable arrays are automatically deallocated
- No manual memory management required
- Resource cleanup on error conditions
- Memory pool optimization for frequent allocations

For complete API documentation with detailed parameter descriptions, see inline documentation in the source code modules.
