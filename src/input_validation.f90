module input_validation
    !! Comprehensive input validation module for Issue #122
    !! 
    !! Provides centralized validation of all external inputs with 
    !! performance-optimized checks and graceful error handling.
    !! 
    !! Security Features:
    !! - File size limits before memory allocation
    !! - Line number bounds checking (1 to MAX_LINES)
    !! - Execution count overflow protection (INT32_MAX boundaries)
    !! - Division by zero protection in percentage calculations
    !! - Path validation against injection attacks
    !! - Memory allocation safety limits
    
    use error_handling
    use iso_fortran_env, only: int32, int64, real32
    use string_utilities, only: int_to_string
    implicit none
    private
    
    ! System limits - optimized for performance and security
    integer(int64), parameter, public :: MAX_FILE_SIZE = 1073741824_int64  ! 1GB
    integer, parameter, public :: MAX_LINE_NUMBER = 100000      ! Reasonable maximum for source file line numbers
    integer, parameter, public :: MAX_EXECUTION_COUNT = 2147483647   ! INT32_MAX for coverage data
    integer, parameter, public :: MAX_PATH_LENGTH = 4096
    integer, parameter, public :: MAX_FILENAME_LENGTH = 255
    integer, parameter, public :: MAX_LINES_PER_FILE = 1000000
    integer(int64), parameter, public :: DEFAULT_FILE_LIMIT = 104857600_int64  ! 100MB default
    
    ! Validation result type
    type, public :: validation_result_t
        logical :: is_valid = .false.
        integer :: error_code = ERROR_SUCCESS
        character(len=512) :: error_message = ""
        character(len=256) :: suggested_fix = ""
    contains
        procedure :: init => validation_result_init
        procedure :: clear => validation_result_clear
    end type validation_result_t
    
    ! Validate line data structure bounds - overloaded interface
    interface validate_line_data_bounds
        module procedure validate_line_data_bounds_simple
        module procedure validate_line_data_bounds_detailed
    end interface
    
    ! Public validation interfaces
    public :: validate_file_constraints
    public :: validate_coverage_data_bounds
    public :: validate_line_data_bounds
    public :: validate_path_safety
    public :: validate_memory_allocation_request
    public :: safe_percentage_calculation
    public :: safe_integer_calculation
    public :: normalize_execution_count
    public :: clamp_line_number
    public :: is_safe_file_size
    public :: get_validation_limits
    
contains

    ! Initialize validation result
    subroutine validation_result_init(this, is_valid, error_code, error_message, suggested_fix)
        class(validation_result_t), intent(inout) :: this
        logical, intent(in) :: is_valid
        integer, intent(in) :: error_code
        character(len=*), intent(in) :: error_message
        character(len=*), intent(in) :: suggested_fix
        
        this%is_valid = is_valid
        this%error_code = error_code
        this%error_message = error_message
        this%suggested_fix = suggested_fix
    end subroutine validation_result_init
    
    ! Clear validation result
    subroutine validation_result_clear(this)
        class(validation_result_t), intent(inout) :: this
        
        this%is_valid = .false.
        this%error_code = ERROR_SUCCESS
        this%error_message = ""
        this%suggested_fix = ""
    end subroutine validation_result_clear

    ! Comprehensive file constraint validation
    subroutine validate_file_constraints(filename, result, size_limit)
        character(len=*), intent(in) :: filename
        type(validation_result_t), intent(out) :: result
        integer(int64), intent(in), optional :: size_limit
        
        integer(int64) :: file_size, actual_limit
        integer :: stat
        logical :: file_exists
        
        ! Set size limit (default 100MB, max 1GB)
        if (present(size_limit)) then
            actual_limit = min(size_limit, MAX_FILE_SIZE)
        else
            actual_limit = DEFAULT_FILE_LIMIT
        end if
        
        call result%clear()
        
        ! Path safety validation first
        call validate_path_safety(filename, result)
        if (.not. result%is_valid) return
        
        ! Check file existence and get size
        inquire(file=filename, exist=file_exists, size=file_size, iostat=stat)
        
        if (stat /= 0) then
            call result%init(.false., ERROR_FILE_ACCESS, &
                           "Cannot access file: " // trim(filename), &
                           "Check file permissions and path")
            return
        end if
        
        if (.not. file_exists) then
            call result%init(.false., ERROR_MISSING_FILE, &
                           "File does not exist: " // trim(filename), &
                           "Verify file path and ensure file was generated")
            return
        end if
        
        ! Validate file size against memory limits
        if (file_size > actual_limit) then
            call result%init(.false., ERROR_FILE_TOO_LARGE, &
                           "File exceeds size limit: " // trim(filename) // &
                           " (" // trim(int64_to_string(file_size)) // " bytes > " // &
                           trim(int64_to_string(actual_limit)) // " limit)", &
                           "Use smaller coverage file or increase size limit")
            return
        end if
        
        if (file_size <= 0) then
            call result%init(.false., ERROR_EMPTY_FILE, &
                           "File is empty: " // trim(filename), &
                           "Ensure coverage data was generated properly")
            return
        end if
        
        ! Success
        call result%init(.true., ERROR_SUCCESS, "", "")
    end subroutine validate_file_constraints

    ! Validate coverage data bounds and structure
    subroutine validate_coverage_data_bounds(line_number, execution_count, result)
        integer, intent(in) :: line_number
        integer, intent(in) :: execution_count
        type(validation_result_t), intent(out) :: result
        
        call result%clear()
        
        ! Validate line number bounds
        if (line_number <= 0) then
            call result%init(.false., ERROR_INVALID_DATA, &
                           "Invalid line number: " // trim(int_to_string(line_number)), &
                           "Line numbers must be positive")
            return
        end if
        
        if (line_number > MAX_LINE_NUMBER) then
            call result%init(.false., ERROR_INVALID_DATA, &
                           "Line number exceeds maximum: " // trim(int_to_string(line_number)) // &
                           " > " // trim(int_to_string(MAX_LINE_NUMBER)), &
                           "Use smaller source files or increase line limit")
            return
        end if
        
        ! Validate execution count bounds
        if (execution_count < 0) then
            call result%init(.false., ERROR_INVALID_DATA, &
                           "Invalid execution count: " // trim(int_to_string(execution_count)), &
                           "Execution counts cannot be negative")
            return
        end if
        
        if (execution_count > MAX_EXECUTION_COUNT) then
            call result%init(.false., ERROR_INVALID_DATA, &
                           "Execution count exceeds safe maximum: " // &
                           trim(int_to_string(execution_count)) // " > " // &
                           trim(int_to_string(MAX_EXECUTION_COUNT)), &
                           "Large execution counts will be clamped to prevent overflow")
            return
        end if
        
        ! Success
        call result%init(.true., ERROR_SUCCESS, "", "")
    end subroutine validate_coverage_data_bounds

    ! Validate line data structure bounds (simple version)
    subroutine validate_line_data_bounds_simple(lines_array_size, result)
        integer, intent(in) :: lines_array_size
        type(validation_result_t), intent(out) :: result
        
        call result%clear()
        
        if (lines_array_size < 0) then
            call result%init(.false., ERROR_INVALID_DATA, &
                           "Invalid array size: " // trim(int_to_string(lines_array_size)), &
                           "Array sizes must be non-negative")
            return
        end if
        
        if (lines_array_size > MAX_LINES_PER_FILE) then
            call result%init(.false., ERROR_INVALID_DATA, &
                           "Array size exceeds maximum: " // trim(int_to_string(lines_array_size)) // &
                           " > " // trim(int_to_string(MAX_LINES_PER_FILE)), &
                           "Reduce array size or increase system limits")
            return
        end if
        
        ! Success
        call result%init(.true., ERROR_SUCCESS, "", "")
    end subroutine validate_line_data_bounds_simple
    
    ! Validate line data bounds (detailed version for security tests)
    subroutine validate_line_data_bounds_detailed(line_number, execution_count, filename, result)
        integer, intent(in) :: line_number
        integer, intent(in) :: execution_count  
        character(len=*), intent(in) :: filename
        type(validation_result_t), intent(out) :: result
        
        call result%clear()
        
        ! Validate filename first
        call validate_path_safety(filename, result)
        if (.not. result%is_valid) return
        
        ! Validate line number bounds
        if (line_number <= 0) then
            call result%init(.false., ERROR_INVALID_DATA, &
                           "Invalid line number: " // trim(int_to_string(line_number)) // &
                           " in file: " // trim(filename), &
                           "Line numbers must be positive")
            return
        end if
        
        if (line_number > MAX_LINE_NUMBER) then
            call result%init(.false., ERROR_INVALID_DATA, &
                           "Line number exceeds maximum: " // trim(int_to_string(line_number)) // &
                           " > " // trim(int_to_string(MAX_LINE_NUMBER)) // &
                           " in file: " // trim(filename), &
                           "Use smaller source files or increase line limit")
            return
        end if
        
        ! Validate execution count bounds
        if (execution_count < 0) then
            call result%init(.false., ERROR_INVALID_DATA, &
                           "Invalid execution count: " // trim(int_to_string(execution_count)) // &
                           " for line " // trim(int_to_string(line_number)) // &
                           " in file: " // trim(filename), &
                           "Execution counts cannot be negative")
            return
        end if
        
        if (execution_count > MAX_EXECUTION_COUNT) then
            call result%init(.false., ERROR_INVALID_DATA, &
                           "Execution count exceeds safe maximum: " // &
                           trim(int_to_string(execution_count)) // " > " // &
                           trim(int_to_string(MAX_EXECUTION_COUNT)) // &
                           " for line " // trim(int_to_string(line_number)) // &
                           " in file: " // trim(filename), &
                           "Large execution counts will be clamped to prevent overflow")
            return
        end if
        
        ! Success
        call result%init(.true., ERROR_SUCCESS, "", "")
    end subroutine validate_line_data_bounds_detailed

    ! Comprehensive path safety validation
    subroutine validate_path_safety(path, result)
        character(len=*), intent(in) :: path
        type(validation_result_t), intent(out) :: result
        
        integer :: i, path_len
        character :: ch
        
        call result%clear()
        path_len = len_trim(path)
        
        ! Check for empty or whitespace-only paths
        if (path_len == 0) then
            call result%init(.false., ERROR_INVALID_PATH, &
                           "Empty file path", &
                           "Provide a valid file path")
            return
        end if
        
        ! Check for excessively long paths
        if (path_len > MAX_PATH_LENGTH) then
            call result%init(.false., ERROR_INVALID_PATH, &
                           "Path too long: " // trim(int_to_string(path_len)) // &
                           " > " // trim(int_to_string(MAX_PATH_LENGTH)), &
                           "Use shorter file paths")
            return
        end if
        
        ! Check for dangerous characters that could enable command injection
        do i = 1, path_len
            ch = path(i:i)
            select case (ch)
            case (';', '|', '&', '$', '`', '(', ')', '{', '}', '[', ']', '<', '>', '"', "'")
                call result%init(.false., ERROR_INVALID_PATH, &
                               "Unsafe character in path: '" // ch // "'", &
                               "Remove special characters from file path")
                return
            case (char(0), char(10), char(13)) ! null, newline, carriage return
                call result%init(.false., ERROR_INVALID_PATH, &
                               "Control character in path", &
                               "Remove control characters from file path")
                return
            end select
        end do
        
        ! Check for whitespace-only path
        if (len_trim(adjustl(path)) == 0) then
            call result%init(.false., ERROR_INVALID_PATH, &
                           "Whitespace-only path", &
                           "Provide a valid file path")
            return
        end if
        
        ! Success
        call result%init(.true., ERROR_SUCCESS, "", "")
    end subroutine validate_path_safety

    ! Validate memory allocation request safety
    subroutine validate_memory_allocation_request(requested_size, result)
        integer(int64), intent(in) :: requested_size
        type(validation_result_t), intent(out) :: result
        
        call result%clear()
        
        if (requested_size < 0) then
            call result%init(.false., ERROR_INVALID_DATA, &
                           "Negative allocation size: " // trim(int64_to_string(requested_size)), &
                           "Memory allocation size must be non-negative")
            return
        end if
        
        if (requested_size > MAX_FILE_SIZE) then
            call result%init(.false., ERROR_MEMORY_EXHAUSTION, &
                           "Allocation size exceeds memory limit: " // &
                           trim(int64_to_string(requested_size)) // " > " // &
                           trim(int64_to_string(MAX_FILE_SIZE)), &
                           "Reduce allocation size or process file in chunks")
            return
        end if
        
        ! Success
        call result%init(.true., ERROR_SUCCESS, "", "")
    end subroutine validate_memory_allocation_request

    ! Safe percentage calculation with division by zero protection
    function safe_percentage_calculation(covered, total) result(percentage)
        integer, intent(in) :: covered, total
        real :: percentage
        
        ! Handle division by zero
        if (total <= 0) then
            percentage = 0.0  ! No total means 0% coverage
            return
        end if
        
        ! Handle invalid covered count
        if (covered < 0) then
            percentage = 0.0
            return
        end if
        
        ! Clamp covered to total (cannot exceed 100%)
        if (covered > total) then
            percentage = 100.0
            return
        end if
        
        ! Safe calculation
        percentage = real(covered) / real(total) * 100.0
        
        ! Clamp to valid range [0.0, 100.0]
        percentage = max(0.0, min(100.0, percentage))
    end function safe_percentage_calculation

    ! Safe integer calculation to prevent overflow
    function safe_integer_calculation(value1, value2, operation) result(safe_result)
        integer, intent(in) :: value1, value2
        character(len=*), intent(in) :: operation
        integer :: safe_result
        
        integer(int64) :: temp_result
        
        select case (trim(operation))
        case ("add", "+")
            temp_result = int(value1, int64) + int(value2, int64)
        case ("multiply", "*")
            temp_result = int(value1, int64) * int(value2, int64)
        case ("subtract", "-")
            temp_result = int(value1, int64) - int(value2, int64)
        case default
            safe_result = value1  ! Default to first value for unknown operations
            return
        end select
        
        ! Clamp to safe integer range
        if (temp_result > MAX_EXECUTION_COUNT) then
            safe_result = MAX_EXECUTION_COUNT
        else if (temp_result < 0) then
            safe_result = 0
        else
            safe_result = int(temp_result)
        end if
    end function safe_integer_calculation

    ! Normalize execution counts to prevent overflow
    pure function normalize_execution_count(exec_count) result(normalized)
        integer, intent(in) :: exec_count
        integer :: normalized
        
        ! Handle negative counts (invalid data)
        if (exec_count < 0) then
            normalized = 0  ! Treat as unexecuted
        ! Cap extremely large values to prevent overflow
        else if (exec_count > MAX_EXECUTION_COUNT) then
            normalized = MAX_EXECUTION_COUNT
        else
            normalized = exec_count
        end if
    end function normalize_execution_count

    ! Clamp line numbers to valid range
    pure function clamp_line_number(line_num) result(clamped)
        integer, intent(in) :: line_num
        integer :: clamped
        
        if (line_num <= 0) then
            clamped = 1  ! Minimum valid line number
        else if (line_num > MAX_LINE_NUMBER) then
            clamped = MAX_LINE_NUMBER
        else
            clamped = line_num
        end if
    end function clamp_line_number

    ! Check if file size is safe for memory allocation
    pure function is_safe_file_size(file_size, limit) result(is_safe)
        integer(int64), intent(in) :: file_size
        integer(int64), intent(in), optional :: limit
        logical :: is_safe
        
        integer(int64) :: actual_limit
        
        if (present(limit)) then
            actual_limit = min(limit, MAX_FILE_SIZE)
        else
            actual_limit = DEFAULT_FILE_LIMIT
        end if
        
        is_safe = (file_size > 0 .and. file_size <= actual_limit)
    end function is_safe_file_size

    ! Get current validation limits for configuration
    subroutine get_validation_limits(max_file_size_out, max_line_number_out, &
                                   max_execution_count_out, max_path_length_out)
        integer(int64), intent(out), optional :: max_file_size_out
        integer, intent(out), optional :: max_line_number_out
        integer, intent(out), optional :: max_execution_count_out
        integer, intent(out), optional :: max_path_length_out
        
        if (present(max_file_size_out)) max_file_size_out = MAX_FILE_SIZE
        if (present(max_line_number_out)) max_line_number_out = MAX_LINE_NUMBER
        if (present(max_execution_count_out)) max_execution_count_out = MAX_EXECUTION_COUNT
        if (present(max_path_length_out)) max_path_length_out = MAX_PATH_LENGTH
    end subroutine get_validation_limits


    ! Helper function to convert int64 to string
    function int64_to_string(int_val) result(str_val)
        integer(int64), intent(in) :: int_val
        character(len=:), allocatable :: str_val
        character(len=32) :: temp_str
        
        write(temp_str, '(I0)') int_val
        str_val = trim(temp_str)
    end function int64_to_string

end module input_validation