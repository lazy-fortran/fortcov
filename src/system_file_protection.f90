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

end module system_file_protection