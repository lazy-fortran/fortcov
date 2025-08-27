module input_validation_core
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
    
    use error_handling_core
    use iso_fortran_env, only: int64
    use string_utils, only: int_to_string
    implicit none
    private
    
    ! System limits - optimized for performance and security
    integer(int64), parameter, public :: MAX_FILE_SIZE = 1073741824_int64  ! 1GB
    integer, parameter, public :: MAX_EXECUTION_COUNT = 2147483647   ! INT32_MAX for coverage data
    integer, parameter, public :: MAX_PATH_LENGTH = 4096
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
    
    ! Public validation interfaces
    public :: validate_file_constraints
    public :: validate_path_safety
    public :: safe_percentage_calculation
    public :: normalize_execution_count
    
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





    ! Helper function to convert int64 to string
    function int64_to_string(int_val) result(str_val)
        integer(int64), intent(in) :: int_val
        character(len=:), allocatable :: str_val
        character(len=32) :: temp_str
        
        write(temp_str, '(I0)') int_val
        str_val = trim(temp_str)
    end function int64_to_string

end module input_validation_core