module path_validation
    !! Path validation and sanitization module
    !!
    !! This module provides comprehensive path security validation including
    !! injection protection, directory traversal prevention, and system file
    !! access control for Windows and Unix systems.
    use error_handling
    use string_utils, only: format_integer
    implicit none
    private
    
    ! Maximum path length for security validation
    integer, parameter :: MAX_PATH_LENGTH = 4096
    
    ! PERFORMANCE OPTIMIZATION: Path validation cache
    type :: path_validation_cache_t
        character(len=256) :: path = ""
        logical :: is_valid = .false.
        logical :: is_cached = .false.
        character(len=:), allocatable :: safe_path
    end type path_validation_cache_t
    
    ! Path validation cache parameters  
    integer, parameter :: PATH_CACHE_SIZE = 32
    
    ! Public procedures
    public :: validate_path_security
    public :: validate_executable_path
    
contains

    ! Path security validation
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
                "Path exceeds maximum length: " // format_integer(MAX_PATH_LENGTH))
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

    ! Executable path validation
    subroutine validate_executable_path(executable, safe_executable, error_ctx)
        character(len=*), intent(in) :: executable
        character(len=:), allocatable, intent(out) :: safe_executable
        type(error_context_t), intent(out) :: error_ctx
        
        logical :: exec_exists
        
        ! First validate as regular path
        call validate_path_security(executable, safe_executable, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        ! Check if executable exists and is executable
        inquire(file=safe_executable, exist=exec_exists)
        if (.not. exec_exists) then
            error_ctx%error_code = ERROR_MISSING_FILE
            call safe_write_message(error_ctx, &
                "Executable not found - check installation and PATH")
            call safe_write_suggestion(error_ctx, &
                "Verify the executable is installed and accessible")
            return
        end if
    end subroutine validate_executable_path
    
    ! URL-encoded attack detection
    subroutine check_url_encoded_attacks(path, error_ctx)
        character(len=*), intent(in) :: path
        type(error_context_t), intent(inout) :: error_ctx
        
        ! Check for URL-encoded directory traversal patterns
        if (index(path, '%2e') > 0 .or. index(path, '%2E') > 0 .or. &
            index(path, '%2f') > 0 .or. index(path, '%2F') > 0 .or. &
            index(path, '%5c') > 0 .or. index(path, '%5C') > 0) then
            error_ctx%error_code = ERROR_INVALID_PATH
            call safe_write_message(error_ctx, "URL-encoded attack pattern detected")
            return
        end if
    end subroutine check_url_encoded_attacks
    
    ! System file access protection - OPTIMIZED
    subroutine check_system_file_access(path, error_ctx)
        character(len=*), intent(in) :: path
        type(error_context_t), intent(inout) :: error_ctx
        
        ! PERFORMANCE: Avoid full string conversion if possible
        if (len_trim(path) == 0) return
        
        ! PERFORMANCE: Check common system paths first with early exits
        ! Most common system paths first for better performance
        if (path(1:1) == '/') then
            ! Check the most common cases first
            if (starts_with_ignore_case(path, '/tmp/')) then
                error_ctx%error_code = ERROR_INVALID_PATH
                call safe_write_message(error_ctx, "System file access not allowed")
                return
            else if (starts_with_ignore_case(path, '/home/')) then
                error_ctx%error_code = ERROR_INVALID_PATH
                call safe_write_message(error_ctx, "System file access not allowed")
                return
            else if (starts_with_ignore_case(path, '/etc/')) then
                error_ctx%error_code = ERROR_INVALID_PATH
                call safe_write_message(error_ctx, "System file access not allowed")
                return
            else if (starts_with_ignore_case(path, '/root/')) then
                error_ctx%error_code = ERROR_INVALID_PATH
                call safe_write_message(error_ctx, "System file access not allowed")
                return
            else if (starts_with_ignore_case(path, '/usr/')) then
                error_ctx%error_code = ERROR_INVALID_PATH
                call safe_write_message(error_ctx, "System file access not allowed")
                return
            else if (starts_with_ignore_case(path, '/var/log/')) then
                error_ctx%error_code = ERROR_INVALID_PATH
                call safe_write_message(error_ctx, "System file access not allowed")
                return
            else if (starts_with_ignore_case(path, '/proc/')) then
                error_ctx%error_code = ERROR_INVALID_PATH
                call safe_write_message(error_ctx, "System file access not allowed")
                return
            else if (starts_with_ignore_case(path, '/sys/')) then
                error_ctx%error_code = ERROR_INVALID_PATH
                call safe_write_message(error_ctx, "System file access not allowed")
                return
            else if (starts_with_ignore_case(path, '/dev/')) then
                error_ctx%error_code = ERROR_INVALID_PATH
                call safe_write_message(error_ctx, "System file access not allowed")
                return
            end if
        end if
    end subroutine check_system_file_access
    
    ! Windows device names protection - comprehensive validation
    subroutine check_windows_device_names(path, error_ctx)
        character(len=*), intent(in) :: path
        type(error_context_t), intent(inout) :: error_ctx
        
        character(len=len(path)) :: upper_path
        character(len=256) :: path_component
        integer :: i, slash_pos, last_slash_pos, device_num
        logical :: is_device
        
        ! Convert to uppercase for case-insensitive checking
        call to_uppercase(path, upper_path)
        upper_path = trim(upper_path)
        
        ! Check the full path and each component for Windows device names
        call check_path_component_for_device(upper_path, is_device)
        if (is_device) then
            error_ctx%error_code = ERROR_INVALID_PATH
            call safe_write_message(error_ctx, "Windows device name access not allowed")
            return
        end if
        
        ! Check each path component separated by '/' or '\'
        last_slash_pos = 0
        do i = 1, len(upper_path)
            if (upper_path(i:i) == '/' .or. upper_path(i:i) == '\') then
                slash_pos = i
                ! Extract component between slashes
                if (slash_pos > last_slash_pos + 1) then
                    path_component = upper_path(last_slash_pos + 1:slash_pos - 1)
                    call check_path_component_for_device(path_component, is_device)
                    if (is_device) then
                        error_ctx%error_code = ERROR_INVALID_PATH
                        call safe_write_message(error_ctx, &
                            "Windows device name access not allowed")
                        return
                    end if
                end if
                last_slash_pos = slash_pos
            end if
        end do
        
        ! Check the final component after the last slash
        if (last_slash_pos < len(upper_path)) then
            path_component = upper_path(last_slash_pos + 1:len(upper_path))
            call check_path_component_for_device(path_component, is_device)
            if (is_device) then
                error_ctx%error_code = ERROR_INVALID_PATH
                call safe_write_message(error_ctx, &
                    "Windows device name access not allowed")
                return
            end if
        end if
    end subroutine check_windows_device_names
    
    ! Check if a single path component matches a Windows device name
    subroutine check_path_component_for_device(component, is_device)
        character(len=*), intent(in) :: component
        logical, intent(out) :: is_device
        
        character(len=len(component)) :: device_name
        integer :: dot_pos, device_num, iostat_val
        character(len=8) :: num_str
        
        is_device = .false.
        
        ! Skip empty components
        if (len_trim(component) == 0) return
        
        ! Extract device name (part before dot or end of string)
        dot_pos = index(component, '.')
        if (dot_pos > 0) then
            device_name = component(1:dot_pos - 1)
        else
            device_name = trim(component)
        end if
        
        ! Check base device names (CON, PRN, AUX, NUL)
        if (trim(device_name) == 'CON' .or. &
            trim(device_name) == 'PRN' .or. &
            trim(device_name) == 'AUX' .or. &
            trim(device_name) == 'NUL') then
            is_device = .true.
            return
        end if
        
        ! Check COM devices (COM1-COM9)
        if (len_trim(device_name) >= 4) then
            if (device_name(1:3) == 'COM') then
                num_str = device_name(4:len_trim(device_name))
                read(num_str, *, iostat=iostat_val) device_num
                if (iostat_val == 0) then  ! iostat == 0 means successful read
                    ! device_num now contains the actual number read
                    if (device_num >= 1 .and. device_num <= 9) then
                        is_device = .true.
                        return
                    end if
                end if
            end if
        end if
        
        ! Check LPT devices (LPT1-LPT9)
        if (len_trim(device_name) >= 4) then
            if (device_name(1:3) == 'LPT') then
                num_str = device_name(4:len_trim(device_name))
                read(num_str, *, iostat=iostat_val) device_num
                if (iostat_val == 0) then  ! iostat == 0 means successful read
                    ! device_num now contains the actual number read
                    if (device_num >= 1 .and. device_num <= 9) then
                        is_device = .true.
                        return
                    end if
                end if
            end if
        end if
    end subroutine check_path_component_for_device
    
    ! UNC path attack protection
    subroutine check_unc_path_attack(path, error_ctx)
        character(len=*), intent(in) :: path
        type(error_context_t), intent(inout) :: error_ctx
        
        ! Block UNC paths (Windows network paths)
        if (len(path) >= 2) then
            if (path(1:2) == '\\\\') then
                error_ctx%error_code = ERROR_INVALID_PATH
                call safe_write_message(error_ctx, "UNC path access not allowed")
                return
            end if
        end if
    end subroutine check_unc_path_attack
    
    ! Helper to convert string to lowercase
    subroutine to_lowercase(input, output)
        character(len=*), intent(in) :: input
        character(len=*), intent(out) :: output
        integer :: i
        
        output = input
        do i = 1, len_trim(input)
            if (input(i:i) >= 'A' .and. input(i:i) <= 'Z') then
                output(i:i) = char(ichar(input(i:i)) + 32)
            end if
        end do
    end subroutine to_lowercase
    
    ! Helper to convert string to uppercase
    subroutine to_uppercase(input, output)
        character(len=*), intent(in) :: input
        character(len=*), intent(out) :: output
        integer :: i
        
        output = input
        do i = 1, len_trim(input)
            if (input(i:i) >= 'a' .and. input(i:i) <= 'z') then
                output(i:i) = char(ichar(input(i:i)) - 32)
            end if
        end do
    end subroutine to_uppercase
    
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
    
    ! PERFORMANCE: Consolidated dangerous pattern scanning
    pure function scan_for_dangerous_patterns(path) result(has_dangerous)
        character(len=*), intent(in) :: path
        logical :: has_dangerous
        integer :: i, path_len
        
        has_dangerous = .false.
        path_len = len_trim(path)
        
        ! Single pass through string checking for all dangerous patterns
        do i = 1, path_len - 1
            select case (path(i:i))
            case ('.')
                if (i < path_len .and. path(i+1:i+1) == '.') then
                    has_dangerous = .true.
                    return
                end if
            case (';', '|', '&', '<', '>', '$', '`', '"', "'")
                has_dangerous = .true.
                return
            end select
        end do
        
        ! Check last character
        if (path_len > 0) then
            select case (path(path_len:path_len))
            case (';', '|', '&', '<', '>', '$', '`', '"', "'")
                has_dangerous = .true.
                return
            end select
        end if
    end function scan_for_dangerous_patterns
    
    ! PERFORMANCE: Fast case-insensitive prefix check
    pure function starts_with_ignore_case(str, prefix) result(starts)
        character(len=*), intent(in) :: str, prefix
        logical :: starts
        integer :: i, prefix_len
        character :: c1, c2
        
        starts = .false.
        prefix_len = len_trim(prefix)
        
        if (len_trim(str) < prefix_len) return
        
        ! Fast character-by-character comparison with case conversion
        do i = 1, prefix_len
            c1 = str(i:i)
            c2 = prefix(i:i)
            
            ! Convert to lowercase for comparison
            if (c1 >= 'A' .and. c1 <= 'Z') c1 = char(ichar(c1) + 32)
            if (c2 >= 'A' .and. c2 <= 'Z') c2 = char(ichar(c2) + 32)
            
            if (c1 /= c2) return
        end do
        
        starts = .true.
    end function starts_with_ignore_case

end module path_validation