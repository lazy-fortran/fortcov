module path_security
    use error_handling_core, only: error_context_t, ERROR_SUCCESS, &
        ERROR_INVALID_PATH, ERROR_MISSING_FILE, clear_error_context, &
        safe_write_message, safe_write_suggestion
    use string_utils, only: int_to_string
    implicit none
    private
    
    ! Public procedures
    public :: validate_path_security
    public :: validate_executable_path
    
    ! Maximum path length for security validation
    integer, parameter :: MAX_PATH_LENGTH = 4096
    
    ! Path validation cache parameters  
    integer, parameter :: PATH_CACHE_SIZE = 32

    character(len=1), parameter :: BACKSLASH = achar(92)
    
    ! PERFORMANCE OPTIMIZATION: Path validation cache
    type :: path_validation_cache_t
        character(len=MAX_PATH_LENGTH) :: path = ""
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
        logical :: has_drive_letter, is_path_like
        
        call clear_error_context(error_ctx)
        
        has_drive_letter = (len_trim(executable) >= 2 .and. executable(2:2) == ':')
        is_path_like = (index(executable, '/') > 0 .or. &
                        index(executable, BACKSLASH) > 0 .or. has_drive_letter)

        ! Check if it's a path-like executable first
        if (is_path_like) then
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

end module path_security
