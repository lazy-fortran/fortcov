module config_parser_complex_utils
    !! Complex parsing utilities for configuration values
    !!
    !! This module handles advanced parsing operations for complex configuration
    !! formats, including diff file specifications and multi-part values.

    use config_types, only: config_t
    use constants_core, only: MEDIUM_STRING_LEN

    implicit none
    private

    ! Complex parsing utilities
    public :: parse_diff_files

contains

    subroutine parse_diff_files(value, config, success, error_message)
        !! Parse diff file specification (baseline.json,current.json)
        character(len=*), intent(in) :: value
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        integer :: comma_pos
        character(len=:), allocatable :: baseline_file, current_file
        integer :: baseline_len, current_len

        success = .true.
        error_message = ""

        ! Find comma separator
        comma_pos = index(value, ',')
        if (comma_pos == 0) then
            success = .false.
            error_message = "Invalid --diff format. Expected: --diff=baseline.json,current.json"
            return
        end if

        ! Extract baseline and current files
        baseline_file = trim(adjustl(value(1:comma_pos-1)))
        current_file = trim(adjustl(value(comma_pos+1:)))

        ! Validate that both files are specified
        if (len_trim(baseline_file) == 0) then
            success = .false.
            error_message = "Baseline file not specified in --diff format"
            return
        end if

        if (len_trim(current_file) == 0) then
            success = .false.
            error_message = "Current file not specified in --diff format"
            return
        end if

        ! Validate file path lengths to prevent buffer overflow
        baseline_len = len_trim(baseline_file)
        current_len = len_trim(current_file)
        if (baseline_len > MEDIUM_STRING_LEN) then
            success = .false.
            error_message = "Baseline file path too long (max 512 characters)"
            return
        end if
        if (current_len > MEDIUM_STRING_LEN) then
            success = .false.
            error_message = "Current file path too long (max 512 characters)"
            return
        end if

        ! Set the configuration - safe assignment after validation
        config%diff_baseline_file = baseline_file
        config%diff_current_file = current_file

    end subroutine parse_diff_files

end module config_parser_complex_utils