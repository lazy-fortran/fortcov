module path_scanner_impl
    use error_handling_core
    use path_string_utils
    implicit none
    private
    
    ! Public procedures
    public :: scan_for_dangerous_patterns
    public :: check_url_encoded_attacks
    
contains

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

end module path_scanner_impl