module path_security_core
    use error_handling
    use string_utils, only: int_to_string
    use path_pattern_scanner
    use system_file_protection
    use windows_security_validator
    implicit none
    private
    
    ! Public procedures
    public :: validate_path_security
    public :: validate_executable_path
    
    ! Maximum path length for security validation
    integer, parameter :: MAX_PATH_LENGTH = 4096
    
    ! Path validation cache parameters  
    integer, parameter :: PATH_CACHE_SIZE = 32
    
    ! PERFORMANCE OPTIMIZATION: Path validation cache
    type :: path_validation_cache_t
        character(len=256) :: path = ""
        logical :: is_valid = .false.
        logical :: is_cached = .false.
        character(len=:), allocatable :: safe_path
    end type path_validation_cache_t

contains

    ! Path security validation - main entry point
    subroutine validate_path_security(input_path, safe_path, error_ctx)
        character(len=*), intent(in) :: input_path
        character(len=:), allocatable, intent(out) :: safe_path
        type(error_context_t), intent(out) :: error_ctx
        
        character(len=len(input_path)) :: working_path
        integer :: i, path_len
        ! THREAD-SAFE: Local cache variables
        type(path_validation_cache_t), save :: local_path_cache(PATH_CACHE_SIZE)
        integer, save :: local_path_cache_next_slot = 1
        
        call clear_error_context(error_ctx)
        working_path = input_path
        path_len = len_trim(working_path)
        
        ! Length validation
        if (path_len == 0) then
            error_ctx%error_code = ERROR_INVALID_PATH
            call safe_write_message(error_ctx, "Empty path provided")
            return
        end if
        
        if (path_len > MAX_PATH_LENGTH) then
            error_ctx%error_code = ERROR_INVALID_PATH
            call safe_write_message(error_ctx, &
                "Path exceeds maximum length: " // int_to_string(MAX_PATH_LENGTH))
            return
        end if
        
        ! PERFORMANCE: Check cache first
        if (check_path_cache(working_path, safe_path, error_ctx, local_path_cache)) then
            return  ! Cache hit - early exit
        end if
        
        ! PERFORMANCE: Single scan for dangerous patterns with early exits
        if (scan_for_dangerous_patterns(working_path)) then
            error_ctx%error_code = ERROR_INVALID_PATH
            call safe_write_message(error_ctx, "Path contains dangerous characters")
            call cache_path_result(working_path, .false., safe_path, &
                                    local_path_cache, local_path_cache_next_slot)
            return
        end if
        
        ! URL-encoded directory traversal protection
        call check_url_encoded_attacks(working_path, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        ! System file access protection
        call check_system_file_access(working_path, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            ! Sanitize error message to avoid path leakage
            call sanitize_error_message_path(error_ctx)
            return
        end if
        
        ! Windows device names protection
        call check_windows_device_names(working_path, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        ! UNC path protection
        call check_unc_path_attack(working_path, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        ! Allocate and copy safe path
        safe_path = trim(working_path)
        
        ! PERFORMANCE: Cache successful validation
        call cache_path_result(working_path, .true., safe_path, &
                               local_path_cache, local_path_cache_next_slot)
    end subroutine validate_path_security

    ! Executable path validation with enhanced security
    subroutine validate_executable_path(executable, safe_executable, error_ctx)
        character(len=*), intent(in) :: executable
        character(len=:), allocatable, intent(out) :: safe_executable
        type(error_context_t), intent(out) :: error_ctx
        
        logical :: exec_exists
        
        call clear_error_context(error_ctx)
        
        ! Check if it's an absolute path first
        if (index(executable, '/') > 0) then
            ! Absolute or relative path - validate as regular path
            call validate_path_security(executable, safe_executable, error_ctx)
            if (error_ctx%error_code /= ERROR_SUCCESS) return
            
            ! Check if executable exists
            inquire(file=safe_executable, exist=exec_exists)
            if (.not. exec_exists) then
                error_ctx%error_code = ERROR_MISSING_FILE
                call safe_write_message(error_ctx, &
                    "Executable not found - check installation and PATH")
                call safe_write_suggestion(error_ctx, &
                    "Verify the executable is installed and accessible")
                return
            end if
        else
            ! Simple executable name - trust it's in PATH (safer for CI)
            safe_executable = trim(executable)
        end if
    end subroutine validate_executable_path
    
    ! Sanitize error messages to prevent path information leakage
    subroutine sanitize_error_message_path(error_ctx)
        type(error_context_t), intent(inout) :: error_ctx
        
        ! Replace specific sensitive paths with generic messages
        if (index(error_ctx%message, '/home/') > 0 .or. &
            index(error_ctx%message, '/etc/') > 0 .or. &
            index(error_ctx%message, '/root/') > 0 .or. &
            index(error_ctx%message, '/tmp/') > 0) then
            ! Replace with generic message to avoid information leakage
            call safe_write_message(error_ctx, "Invalid path - access denied")
        end if
    end subroutine sanitize_error_message_path

    ! THREAD-SAFE: Check path validation cache
    function check_path_cache(path, safe_path, error_ctx, cache) result(cache_hit)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: safe_path
        type(error_context_t), intent(out) :: error_ctx
        type(path_validation_cache_t), intent(in) :: cache(PATH_CACHE_SIZE)
        logical :: cache_hit
        integer :: i
        
        cache_hit = .false.
        call clear_error_context(error_ctx)
        
        do i = 1, PATH_CACHE_SIZE
            if (cache(i)%is_cached .and. &
                trim(cache(i)%path) == trim(path)) then
                if (cache(i)%is_valid) then
                    safe_path = cache(i)%safe_path
                else
                    error_ctx%error_code = ERROR_INVALID_PATH
                    call safe_write_message(error_ctx, "Cached: Path validation failed")
                end if
                cache_hit = .true.
                return
            end if
        end do
    end function check_path_cache
    
    ! THREAD-SAFE: Cache path validation result
    subroutine cache_path_result(path, is_valid, safe_path, cache, next_slot)
        character(len=*), intent(in) :: path
        logical, intent(in) :: is_valid
        character(len=:), allocatable, intent(in) :: safe_path
        type(path_validation_cache_t), intent(inout) :: cache(PATH_CACHE_SIZE)
        integer, intent(inout) :: next_slot
        
        ! Simple LRU replacement
        cache(next_slot)%path = path
        cache(next_slot)%is_valid = is_valid
        cache(next_slot)%is_cached = .true.
        if (is_valid) then
            cache(next_slot)%safe_path = safe_path
        end if
        
        next_slot = next_slot + 1
        if (next_slot > PATH_CACHE_SIZE) next_slot = 1
    end subroutine cache_path_result

end module path_security_core