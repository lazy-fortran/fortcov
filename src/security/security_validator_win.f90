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
        logical :: contains_device
        
        call prepare_path_for_validation(path, upper_path)
        call validate_path_components_for_devices(upper_path, contains_device)
        
        if (contains_device) then
            error_ctx%error_code = ERROR_INVALID_PATH
            call safe_write_message(error_ctx, "Windows device name access not allowed")
        end if
        
    end subroutine check_windows_device_names

    subroutine prepare_path_for_validation(path, upper_path)
        !! Prepare path for device name validation
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: upper_path
        
        ! Convert to uppercase for case-insensitive checking
        call to_uppercase(path, upper_path)
        upper_path = trim(upper_path)
        
    end subroutine prepare_path_for_validation

    subroutine validate_path_components_for_devices(upper_path, contains_device)
        !! Validate all path components for Windows device names
        character(len=*), intent(in) :: upper_path
        logical, intent(out) :: contains_device
        
        character(len=256) :: path_component
        integer :: i, slash_pos, last_slash_pos
        logical :: is_device
        
        contains_device = .false.
        
        ! Check the full path first
        call check_path_component_for_device(upper_path, is_device)
        if (is_device) then
            contains_device = .true.
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
                        contains_device = .true.
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
                contains_device = .true.
                return
            end if
        end if

    end subroutine validate_path_components_for_devices
    
    ! Check if a single path component matches a Windows device name
    subroutine check_path_component_for_device(component, is_device)
        character(len=*), intent(in) :: component
        logical, intent(out) :: is_device
        
        character(len=len(component)) :: device_name
        
        is_device = .false.
        
        ! Skip empty components
        if (len_trim(component) == 0) return
        
        call extract_device_name(component, device_name)
        
        if (is_base_device_name(device_name)) then
            is_device = .true.
        else if (is_numbered_device_name(device_name)) then
            is_device = .true.
        end if
        
    end subroutine check_path_component_for_device

    subroutine extract_device_name(component, device_name)
        !! Extract device name portion from path component (before dot)
        character(len=*), intent(in) :: component
        character(len=*), intent(out) :: device_name
        
        integer :: dot_pos
        
        ! Extract device name (part before dot or end of string)
        dot_pos = index(component, '.')
        if (dot_pos > 0) then
            device_name = component(1:dot_pos - 1)
        else
            device_name = trim(component)
        end if

    end subroutine extract_device_name

    pure function is_base_device_name(device_name) result(is_base_device)
        !! Check for base Windows device names (CON, PRN, AUX, NUL)
        character(len=*), intent(in) :: device_name
        logical :: is_base_device
        
        is_base_device = trim(device_name) == 'CON' .or. &
                         trim(device_name) == 'PRN' .or. &
                         trim(device_name) == 'AUX' .or. &
                         trim(device_name) == 'NUL'

    end function is_base_device_name

    function is_numbered_device_name(device_name) result(is_numbered_device)
        !! Check for numbered Windows device names (COM1-9, LPT1-9)
        character(len=*), intent(in) :: device_name
        logical :: is_numbered_device
        
        is_numbered_device = is_com_device(device_name) .or. &
                             is_lpt_device(device_name)

    end function is_numbered_device_name

    function is_com_device(device_name) result(is_com)
        !! Check for COM devices (COM1-COM9)
        character(len=*), intent(in) :: device_name
        logical :: is_com
        
        integer :: device_num, iostat_val
        character(len=8) :: num_str
        
        is_com = .false.
        if (len_trim(device_name) >= 4) then
            if (device_name(1:3) == 'COM') then
                num_str = device_name(4:len_trim(device_name))
                read(num_str, *, iostat=iostat_val) device_num
                if (iostat_val == 0) then  ! iostat == 0 means successful read
                    if (device_num >= 1 .and. device_num <= 9) then
                        is_com = .true.
                    end if
                end if
            end if
        end if

    end function is_com_device

    function is_lpt_device(device_name) result(is_lpt)
        !! Check for LPT devices (LPT1-LPT9)
        character(len=*), intent(in) :: device_name
        logical :: is_lpt
        
        integer :: device_num, iostat_val
        character(len=8) :: num_str
        
        is_lpt = .false.
        if (len_trim(device_name) >= 4) then
            if (device_name(1:3) == 'LPT') then
                num_str = device_name(4:len_trim(device_name))
                read(num_str, *, iostat=iostat_val) device_num
                if (iostat_val == 0) then  ! iostat == 0 means successful read
                    if (device_num >= 1 .and. device_num <= 9) then
                        is_lpt = .true.
                    end if
                end if
            end if
        end if

    end function is_lpt_device
    
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