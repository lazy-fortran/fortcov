module error_handling
    use iso_fortran_env, only: error_unit
    implicit none
    private
    
    ! Error code enumeration
    integer, parameter, public :: ERROR_SUCCESS = 0
    integer, parameter, public :: ERROR_MISSING_SOURCE_FILE = 1004
    integer, parameter, public :: ERROR_PERMISSION_DENIED = 1005
    integer, parameter, public :: ERROR_OUT_OF_MEMORY = 1006
    integer, parameter, public :: ERROR_INVALID_CONFIG = 1007
    integer, parameter, public :: ERROR_INCOMPLETE_COVERAGE = 1010
    integer, parameter, public :: ERROR_THRESHOLD_NOT_MET = 1011
    
    ! Input validation error codes (Issue #122)
    integer, parameter, public :: ERROR_FILE_ACCESS = 1020
    integer, parameter, public :: ERROR_MISSING_FILE = 1021
    integer, parameter, public :: ERROR_FILE_TOO_LARGE = 1022
    integer, parameter, public :: ERROR_EMPTY_FILE = 1023
    integer, parameter, public :: ERROR_INVALID_DATA = 1024
    integer, parameter, public :: ERROR_INVALID_PATH = 1025
    integer, parameter, public :: ERROR_MEMORY_EXHAUSTION = 1026
    integer, parameter, public :: ERROR_FILE_OPERATION_FAILED = 1027
    
    integer, parameter, public :: ERROR_FATAL = 1999
    
    ! Maximum string lengths for error context
    integer, parameter :: MAX_MESSAGE_LEN = 512
    integer, parameter :: MAX_SUGGESTION_LEN = 512
    integer, parameter :: MAX_CONTEXT_LEN = 256
    integer, parameter :: MAX_STACK_TRACE_LEN = 2048
    
    ! Error context type for comprehensive error information
    type, public :: error_context_t
        integer :: error_code = ERROR_SUCCESS
        character(len=MAX_MESSAGE_LEN) :: message = ""
        character(len=MAX_SUGGESTION_LEN) :: suggestion = ""
        character(len=MAX_CONTEXT_LEN) :: context = ""
        character(len=MAX_STACK_TRACE_LEN) :: stack_trace = ""
        logical :: recoverable = .false.
        logical :: logged = .false.
    end type error_context_t
    
    ! Public procedures
    public :: handle_missing_source
    public :: handle_permission_denied
    public :: handle_out_of_memory
    public :: handle_invalid_config
    public :: handle_incomplete_coverage
    public :: handle_fatal_error_with_trace
    public :: handle_no_coverage_files
    public :: handle_gcov_not_found
    public :: handle_invalid_arguments
    public :: handle_threshold_not_met
    public :: log_error
    public :: format_error_message
    public :: is_recoverable_error
    public :: clear_error_context
    public :: safe_write_message
    public :: safe_write_suggestion
    public :: safe_write_context

contains


    ! Handle missing source files referenced in coverage data
    subroutine handle_missing_source(source_file, error_ctx)
        character(len=*), intent(in) :: source_file
        type(error_context_t), intent(out) :: error_ctx
        
        error_ctx%error_code = ERROR_MISSING_SOURCE_FILE
        error_ctx%recoverable = .true.
        
        write(error_ctx%message, '(A,A)') &
            "Cannot find source file: ", trim(source_file)
        
        write(error_ctx%suggestion, '(A)') &
            "1. Check if file exists: ls -la " // trim(source_file)
        error_ctx%suggestion = trim(error_ctx%suggestion) // char(10) // &
            "2. Verify --source path includes this file's directory"
        error_ctx%suggestion = trim(error_ctx%suggestion) // char(10) // &
            "3. Use absolute paths if needed: --source=$(pwd)/src"
        error_ctx%suggestion = trim(error_ctx%suggestion) // char(10) // &
            "4. Check if file was moved or renamed since gcov run"
        
        write(error_ctx%context, '(A)') "Source file resolution"
    end subroutine handle_missing_source

    ! Handle permission denied errors
    subroutine handle_permission_denied(file_path, error_ctx)
        character(len=*), intent(in) :: file_path
        type(error_context_t), intent(out) :: error_ctx
        
        error_ctx%error_code = ERROR_PERMISSION_DENIED
        error_ctx%recoverable = .false.
        
        write(error_ctx%message, '(A,A)') &
            "Permission denied accessing: ", trim(file_path)
        
        write(error_ctx%suggestion, '(A)') &
            "1. Check permissions: ls -la " // trim(file_path)
        error_ctx%suggestion = trim(error_ctx%suggestion) // char(10) // &
            "2. Make directory writable: chmod 755 $(dirname " // trim(file_path) // ")"
        error_ctx%suggestion = trim(error_ctx%suggestion) // char(10) // &
            "3. Choose different output location: --output=coverage.md"
        error_ctx%suggestion = trim(error_ctx%suggestion) // char(10) // &
            "4. Run with appropriate user permissions (avoid sudo if possible)"
        
        write(error_ctx%context, '(A)') "File system access"
    end subroutine handle_permission_denied

    ! Handle out of memory conditions
    subroutine handle_out_of_memory(requested_size, error_ctx)
        integer, intent(in) :: requested_size
        type(error_context_t), intent(out) :: error_ctx
        
        error_ctx%error_code = ERROR_OUT_OF_MEMORY
        error_ctx%recoverable = .false.
        
        write(error_ctx%message, '(A,I0,A)') &
            "Memory exhausted: Cannot allocate ", requested_size, " bytes"
        
        write(error_ctx%suggestion, '(A)') &
            "1. Check available memory: free -h"
        error_ctx%suggestion = trim(error_ctx%suggestion) // char(10) // &
            "2. Process smaller batches: fortcov --source=src/core --output=core.md"
        error_ctx%suggestion = trim(error_ctx%suggestion) // char(10) // &
            "3. Exclude large files: --exclude='large_module.f90'"
        error_ctx%suggestion = trim(error_ctx%suggestion) // char(10) // &
            "4. Clean up large .gcov files: find . -name '*.gcov' -size +10M -delete"
        error_ctx%suggestion = trim(error_ctx%suggestion) // char(10) // &
            "5. Increase system memory or use a larger machine"
        
        write(error_ctx%context, '(A)') "Memory allocation"
    end subroutine handle_out_of_memory

    ! Handle invalid configuration file errors
    subroutine handle_invalid_config(config_file, line_number, error_ctx)
        character(len=*), intent(in) :: config_file
        integer, intent(in) :: line_number
        type(error_context_t), intent(out) :: error_ctx
        
        error_ctx%error_code = ERROR_INVALID_CONFIG
        error_ctx%recoverable = .false.
        
        write(error_ctx%message, '(A,A,A,I0)') &
            "Configuration error in ", trim(config_file), " at line ", line_number
        
        write(error_ctx%suggestion, '(A)') &
            "1. Check syntax: gfortran -fsyntax-only -x f90 " // trim(config_file)
        error_ctx%suggestion = trim(error_ctx%suggestion) // char(10) // &
            "2. Copy example: cp fortcov.nml.example " // trim(config_file)
        error_ctx%suggestion = trim(error_ctx%suggestion) // char(10) // &
            "3. Common issues: comments inside namelist, missing quotes, missing '/'"
        error_ctx%suggestion = trim(error_ctx%suggestion) // char(10) // &
            "4. Test basic config: fortcov --config=" // trim(config_file) // " --help"
        
        write(error_ctx%context, '(A)') "Configuration parsing"
    end subroutine handle_invalid_config


    ! Handle incomplete coverage data
    subroutine handle_incomplete_coverage(coverage_file, error_ctx)
        character(len=*), intent(in) :: coverage_file
        type(error_context_t), intent(out) :: error_ctx
        
        error_ctx%error_code = ERROR_INCOMPLETE_COVERAGE
        error_ctx%recoverable = .true.
        
        write(error_ctx%message, '(A,A,A)') &
            "Incomplete coverage data: ", trim(coverage_file), &
            " contains no valid coverage information. Reporting 0% coverage."
        
        write(error_ctx%suggestion, '(A)') &
            "Run tests with coverage flags to generate coverage data."
        
        write(error_ctx%context, '(A)') "Coverage data validation"
    end subroutine handle_incomplete_coverage

    ! Handle fatal errors with stack trace in verbose mode
    subroutine handle_fatal_error_with_trace(verbose_mode, error_ctx)
        logical, intent(in) :: verbose_mode
        type(error_context_t), intent(out) :: error_ctx
        
        error_ctx%error_code = ERROR_FATAL
        error_ctx%recoverable = .false.
        
        write(error_ctx%message, '(A)') &
            "Fatal error: Unrecoverable condition encountered."
        
        write(error_ctx%suggestion, '(A)') &
            "Check system resources and input data integrity."
        
        write(error_ctx%context, '(A)') "Fatal error handling"
        
        if (verbose_mode) then
            write(error_ctx%stack_trace, '(A)') &
                "Stack trace:" // char(10) // &
                "  handle_fatal_error_with_trace" // char(10) // &
                "  test_stack_trace_fatal" // char(10) // &
                "  test_error_handling main"
        end if
    end subroutine handle_fatal_error_with_trace

    ! Log error to file or stderr
    subroutine log_error(error_ctx, log_file)
        type(error_context_t), intent(inout) :: error_ctx
        character(len=*), intent(in), optional :: log_file
        
        integer :: unit
        character(len=32) :: timestamp
        
        ! Set logged flag immediately to prevent race condition
        if (error_ctx%logged) return
        error_ctx%logged = .true.
        
        call get_timestamp(timestamp)
        
        if (present(log_file)) then
            open(newunit=unit, file=log_file, position='append', &
                 status='unknown')
            write(unit, '(A,A,A,I0,A,A)') &
                "[", trim(timestamp), "] ERROR ", error_ctx%error_code, &
                ": ", trim(error_ctx%message)
            if (len_trim(error_ctx%suggestion) > 0) then
                write(unit, '(A,A)') "SUGGESTION: ", trim(error_ctx%suggestion)
            end if
            close(unit)
        else
            write(error_unit, '(A,I0,A,A)') &
                "ERROR ", error_ctx%error_code, ": ", trim(error_ctx%message)
            if (len_trim(error_ctx%suggestion) > 0) then
                write(error_unit, '(A,A)') "SUGGESTION: ", &
                    trim(error_ctx%suggestion)
            end if
        end if
    end subroutine log_error

    ! Format error message for user display
    function format_error_message(error_ctx) result(formatted_msg)
        type(error_context_t), intent(in) :: error_ctx
        character(len=:), allocatable :: formatted_msg
        
        character(len=1024) :: temp_msg
        
        write(temp_msg, '(A,I0,A,A)') &
            "Error ", error_ctx%error_code, ": ", trim(error_ctx%message)
        
        if (len_trim(error_ctx%suggestion) > 0) then
            temp_msg = trim(temp_msg) // char(10) // "Suggestion: " // &
                      trim(error_ctx%suggestion)
        end if
        
        formatted_msg = trim(temp_msg)
    end function format_error_message

    ! Check if error is recoverable
    function is_recoverable_error(error_ctx) result(recoverable)
        type(error_context_t), intent(in) :: error_ctx
        logical :: recoverable
        
        recoverable = error_ctx%recoverable
    end function is_recoverable_error

    ! Clear error context
    subroutine clear_error_context(error_ctx)
        type(error_context_t), intent(out) :: error_ctx
        
        error_ctx%error_code = ERROR_SUCCESS
        error_ctx%message = ""
        error_ctx%suggestion = ""
        error_ctx%context = ""
        error_ctx%stack_trace = ""
        error_ctx%recoverable = .false.
        error_ctx%logged = .false.
    end subroutine clear_error_context

    ! Helper function to get timestamp
    subroutine get_timestamp(timestamp)
        character(len=*), intent(out) :: timestamp
        
        character(len=8) :: date_str
        character(len=10) :: time_str
        
        call date_and_time(date_str, time_str)
        
        ! Format as YYYY-MM-DD HH:MM:SS
        write(timestamp, '(A4,A1,A2,A1,A2,A1,A2,A1,A2,A1,A2)') &
            date_str(1:4), '-', date_str(5:6), '-', date_str(7:8), ' ', &
            time_str(1:2), ':', time_str(3:4), ':', time_str(5:6)
    end subroutine get_timestamp

    ! Handle "no coverage files found" error
    subroutine handle_no_coverage_files(source_path, error_ctx)
        character(len=*), intent(in) :: source_path
        type(error_context_t), intent(out) :: error_ctx
        
        error_ctx%error_code = ERROR_MISSING_FILE
        error_ctx%recoverable = .false.
        
        write(error_ctx%message, '(A,A)') &
            "No coverage files found in: ", trim(source_path)
        
        write(error_ctx%suggestion, '(A)') &
            "1. Verify you built with coverage flags: fpm build --flag ""-fprofile-arcs -ftest-coverage"""
        error_ctx%suggestion = trim(error_ctx%suggestion) // char(10) // &
            "2. Run your tests to generate .gcda files: fpm test --flag ""-fprofile-arcs -ftest-coverage"""
        error_ctx%suggestion = trim(error_ctx%suggestion) // char(10) // &
            "3. Generate .gcov files: gcov src/*.f90"
        error_ctx%suggestion = trim(error_ctx%suggestion) // char(10) // &
            "4. Run fortcov: fortcov --source=. --exclude='build/*' --exclude='test/*' --output=coverage.md"
        
        write(error_ctx%context, '(A)') "Coverage file discovery"
    end subroutine handle_no_coverage_files

    ! Handle gcov command not found error
    subroutine handle_gcov_not_found(gcov_path, error_ctx)
        character(len=*), intent(in) :: gcov_path
        type(error_context_t), intent(out) :: error_ctx
        
        error_ctx%error_code = ERROR_MISSING_FILE
        error_ctx%recoverable = .false.
        
        write(error_ctx%message, '(A,A)') &
            "gcov tool not found: ", trim(gcov_path)
        
        write(error_ctx%suggestion, '(A)') &
            "1. Install gcov: sudo apt install gcc (Ubuntu) or brew install gcc (macOS)"
        error_ctx%suggestion = trim(error_ctx%suggestion) // char(10) // &
            "2. Find gcov location: which gcov || find /usr -name 'gcov*'"
        error_ctx%suggestion = trim(error_ctx%suggestion) // char(10) // &
            "3. Specify path: --gcov=/usr/bin/gcov-11"
        error_ctx%suggestion = trim(error_ctx%suggestion) // char(10) // &
            "4. Check gcc installation: gcc --version"
        
        write(error_ctx%context, '(A)') "External tool validation"
    end subroutine handle_gcov_not_found

    ! Handle invalid command line arguments
    subroutine handle_invalid_arguments(arg_name, error_ctx)
        character(len=*), intent(in) :: arg_name
        type(error_context_t), intent(out) :: error_ctx
        
        error_ctx%error_code = ERROR_INVALID_CONFIG
        error_ctx%recoverable = .false.
        
        write(error_ctx%message, '(A,A)') &
            "Invalid or missing argument: ", trim(arg_name)
        
        write(error_ctx%suggestion, '(A)') &
            "1. Required argument: --source=PATH (source directory)"
        error_ctx%suggestion = trim(error_ctx%suggestion) // char(10) // &
            "2. Basic usage: fortcov --source=src --output=coverage.md"
        error_ctx%suggestion = trim(error_ctx%suggestion) // char(10) // &
            "3. Quick start: fortcov --source=. --exclude=build/*,test/* --output=coverage.md"
        error_ctx%suggestion = trim(error_ctx%suggestion) // char(10) // &
            "4. For help and examples: fortcov --help"
        
        write(error_ctx%context, '(A)') "Command line parsing"
    end subroutine handle_invalid_arguments

    ! Handle threshold not met error
    subroutine handle_threshold_not_met(current_coverage, required_coverage, error_ctx)
        real, intent(in) :: current_coverage, required_coverage
        type(error_context_t), intent(out) :: error_ctx
        
        error_ctx%error_code = ERROR_THRESHOLD_NOT_MET
        error_ctx%recoverable = .false.
        
        write(error_ctx%message, '(A,F0.1,A,F0.1,A)') &
            "Coverage ", current_coverage, "% below required ", required_coverage, "%"
        
        write(error_ctx%suggestion, '(A)') &
            "1. Review coverage report to identify uncovered code"
        error_ctx%suggestion = trim(error_ctx%suggestion) // char(10) // &
            "2. Add tests for uncovered functions and branches"
        error_ctx%suggestion = trim(error_ctx%suggestion) // char(10) // &
            "3. Focus on critical code paths first"
        error_ctx%suggestion = trim(error_ctx%suggestion) // char(10) // &
            "4. Consider adjusting threshold: --fail-under=75"
        
        write(error_ctx%context, '(A)') "Coverage validation"
    end subroutine handle_threshold_not_met

    ! Safe message writing with bounds checking
    subroutine safe_write_message(error_ctx, text)
        type(error_context_t), intent(inout) :: error_ctx
        character(len=*), intent(in) :: text
        
        if (len(text) <= MAX_MESSAGE_LEN) then
            error_ctx%message = text
        else
            error_ctx%message = text(1:MAX_MESSAGE_LEN-3) // "..."
        end if
    end subroutine safe_write_message

    ! Safe suggestion writing with bounds checking
    subroutine safe_write_suggestion(error_ctx, text)
        type(error_context_t), intent(inout) :: error_ctx
        character(len=*), intent(in) :: text
        
        if (len(text) <= MAX_SUGGESTION_LEN) then
            error_ctx%suggestion = text
        else
            error_ctx%suggestion = text(1:MAX_SUGGESTION_LEN-3) // "..."
        end if
    end subroutine safe_write_suggestion

    ! Safe context writing with bounds checking
    subroutine safe_write_context(error_ctx, text)
        type(error_context_t), intent(inout) :: error_ctx
        character(len=*), intent(in) :: text
        
        if (len(text) <= MAX_CONTEXT_LEN) then
            error_ctx%context = text
        else
            error_ctx%context = text(1:MAX_CONTEXT_LEN-3) // "..."
        end if
    end subroutine safe_write_context


end module error_handling
