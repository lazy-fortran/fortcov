module system_protection_impl
    use error_handling_core, only: error_context_t, ERROR_INVALID_PATH, safe_write_message
    use path_string_utils, only: starts_with_ignore_case
    implicit none
    private
    
    ! Public procedures
    public :: check_system_file_access
    
contains

    ! System file access protection - optimized for performance
    subroutine check_system_file_access(path, error_ctx)
        character(len=*), intent(in) :: path
        type(error_context_t), intent(inout) :: error_ctx
        
        ! PERFORMANCE: Avoid full string conversion if possible
        if (len_trim(path) == 0) return
        
        ! PERFORMANCE: Check absolute paths first with early exits
        if (path(1:1) == '/') then
            ! Block all absolute system paths for security
            ! Only allow specific whitelisted patterns if needed
            call block_absolute_system_path(path, error_ctx)
        end if
    end subroutine check_system_file_access

    ! Block absolute system paths for security
    subroutine block_absolute_system_path(path, error_ctx)
        character(len=*), intent(in) :: path
        type(error_context_t), intent(inout) :: error_ctx
        integer :: i
        
        ! Array of blocked system paths (data-driven approach)
        character(len=*), parameter :: BLOCKED_PATHS(*) = [ &
            '/tmp/ ', '/home/', '/etc/ ', '/root/', '/usr/ ', '/var/ ', &
            '/proc/', '/sys/ ', '/dev/ ' &
        ]
        
        ! Check against blocked system directories
        do i = 1, size(BLOCKED_PATHS)
            if (starts_with_ignore_case(path, trim(BLOCKED_PATHS(i)))) then
                error_ctx%error_code = ERROR_INVALID_PATH
                call safe_write_message(error_ctx, &
                    "System file access not allowed")
                return
            end if
        end do
        
        ! Block any other root-level directory creation
        error_ctx%error_code = ERROR_INVALID_PATH
        call safe_write_message(error_ctx, &
            "Root-level directory access not allowed")
    end subroutine block_absolute_system_path

end module system_protection_impl