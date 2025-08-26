module system_file_protection
    use error_handling
    use path_string_utils
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
        
        ! Check for common system directories first (performance)
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
        else if (starts_with_ignore_case(path, '/var/')) then
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
        else
            ! Block any other root-level directory creation
            ! Allow only relative paths or specific whitelisted paths
            error_ctx%error_code = ERROR_INVALID_PATH
            call safe_write_message(error_ctx, "Root-level directory access not allowed")
            return
        end if
    end subroutine block_absolute_system_path

end module system_file_protection