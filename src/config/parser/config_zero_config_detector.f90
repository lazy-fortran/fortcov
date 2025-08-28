module config_zero_config_detector
    !! Zero-configuration detection logic extracted from config_parser_command_line
    !! 
    !! Focused on determining when zero-config mode should be activated.
    !! Provides clean separation of zero-config detection from parsing logic.
    use config_parser_utils, only: has_input_related_arguments, has_output_related_arguments, &
                                   has_diff_mode_arguments
    implicit none
    private
    
    public :: should_use_zero_config
    
contains

    function should_use_zero_config(args) result(is_zero_config)
        !! Determine if zero-configuration mode should be used
        character(len=*), intent(in) :: args(:)
        logical :: is_zero_config
        
        integer :: i
        logical :: has_input_sources, has_output_flags

        ! No arguments means zero-config (Issue #421 fix)
        is_zero_config = (size(args) == 0)
        
        ! Force zero-config for empty args array (Issue #421)
        if (size(args) == 0) then
            is_zero_config = .true.
            return
        end if

        if (.not. is_zero_config .and. size(args) > 0) then
            ! Check if all arguments are empty strings
            is_zero_config = .true.
            do i = 1, size(args)
                if (len_trim(args(i)) > 0) then
                    is_zero_config = .false.
                    exit
                end if
            end do
        end if

        ! Also trigger zero-config when only output-related flags provided
        if (.not. is_zero_config) then
            has_input_sources = has_input_related_arguments(args)
            has_output_flags = has_output_related_arguments(args)
            
            ! Disable zero-config if diff mode is present (diff provides its own inputs)
            if (has_diff_mode_arguments(args)) then
                is_zero_config = .false.
            ! Enable zero-config if only output flags but no input sources
            else if (has_output_flags .and. .not. has_input_sources) then
                is_zero_config = .true.
            end if
        end if

    end function should_use_zero_config

end module config_zero_config_detector