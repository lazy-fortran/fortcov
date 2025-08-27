module security_validator_win
    use error_handling_core
    use path_string_utils
    implicit none
    private
    
    ! Public procedures
    public :: check_windows_device_names
    public :: check_unc_path_attack
    
contains

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
            if (path(1:2) == '\\') then
                error_ctx%error_code = ERROR_INVALID_PATH
                call safe_write_message(error_ctx, "UNC path access not allowed")
                return
            end if
        end if
    end subroutine check_unc_path_attack

end module security_validator_win