module config_parser_flags
    !! Flag processing for configuration parsing
    !!
    !! This module handles processing of command-line flags and their values,
    !! including control flags, value flags, boolean flags, and complex flags.

    use config_types, only: config_t
    use config_parser_utils, only: add_source_path, add_exclude_pattern, &
                                          add_include_pattern, parse_threshold_with_error, &
                                          parse_integer_with_error, parse_diff_files, &
                                          flag_requires_value

    implicit none
    private

    ! Flag processing interface
    public :: process_flag_arguments
    public :: process_single_flag

contains

    subroutine process_flag_arguments(flags, flag_count, config, success, error_message)
        !! Process flag arguments
        character(len=*), intent(in) :: flags(:)
        integer, intent(in) :: flag_count
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        integer :: i
        character(len=:), allocatable :: flag, value

        success = .true.
        error_message = ""

        i = 1
        do while (i <= flag_count)
            flag = trim(adjustl(flags(i)))

            if (flag_requires_value(flag)) then
                if (i + 1 <= flag_count) then
                    value = trim(adjustl(flags(i + 1)))
                    i = i + 2
                else
                    success = .false.
                    error_message = "Missing value for flag: " // trim(flag) // &
                        ". This flag requires an argument. " // &
                        "Example: " // trim(flag) // " <value>"
                    return
                end if
            else
                value = ""
                i = i + 1
            end if

            call process_single_flag(flag, value, config, success, error_message)
            if (.not. success) return
        end do

    end subroutine process_flag_arguments

    subroutine process_single_flag(flag, value, config, success, error_message)
        !! Process a single flag with its value
        character(len=*), intent(in) :: flag
        character(len=*), intent(in) :: value
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        character(len=:), allocatable :: normalized_flag

        success = .true.
        error_message = ""
        normalized_flag = trim(flag)

        ! Process flags by category
        call process_control_flags(normalized_flag, config, success)
        if (success) return
        
        call process_value_flags(normalized_flag, value, config, success, error_message)
        if (success) return
        
        call process_boolean_flags(normalized_flag, config, success)
        if (success) return
        
        call process_complex_flags(normalized_flag, value, config, success, error_message)
        if (success) return
        
        ! If we have an error message from flag processing, don't override it
        if (len_trim(error_message) == 0) then
            ! Unknown flag
            success = .false.
            error_message = "Unknown flag: " // trim(flag) // &
                ". Run 'fortcov --help' to see available options."
        end if

    end subroutine process_single_flag
    
    subroutine process_control_flags(flag, config, success)
        !! Process control flags (help, version, validate)
        character(len=*), intent(in) :: flag
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        
        success = .true.
        
        select case (flag)
        case ("--help", "-h")
            config%show_help = .true.
        case ("--version", "-V")
            config%show_version = .true.
        case ("--validate")
            config%validate_config_only = .true.
        case ("--validate-architecture")
            config%validate_architecture = .true.
        case default
            success = .false.
        end select
    end subroutine process_control_flags
    
    subroutine process_value_flags(flag, value, config, success, error_message)
        !! Process flags that require string values
        character(len=*), intent(in) :: flag, value
        type(config_t), intent(inout) :: config
        logical, intent(inout) :: success
        character(len=*), intent(out) :: error_message
        
        success = .true.
        
        select case (flag)
        case ("--source", "-s")
            call add_source_path(value, config, success, error_message)
            ! Flag was recognized even if there was an error
        case ("--exclude", "-e")
            call add_exclude_pattern(value, config, success, error_message)
            ! Flag was recognized even if there was an error
        case ("--include", "-i")
            call add_include_pattern(value, config, success, error_message)
            ! Flag was recognized even if there was an error
        case ("--output", "-o")
            config%output_path = value
        case ("--format", "-f")
            config%output_format = value
        case ("--config", "-c")
            config%config_file = value
        case ("--import")
            config%import_file = value
        case ("--gcov-executable")
            config%gcov_executable = value
        case ("--gcov-args")
            config%gcov_args = value
        case ("--architecture-format")
            config%architecture_output_format = value
        case default
            success = .false.
        end select
    end subroutine process_value_flags
    
    subroutine process_boolean_flags(flag, config, success)
        !! Process boolean flags
        character(len=*), intent(in) :: flag
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        
        success = .true.
        
        select case (flag)
        case ("--verbose", "-v")
            config%verbose = .true.
        case ("--quiet", "-q")
            config%quiet = .true.
        case ("--include-unchanged")
            config%include_unchanged = .true.
        case ("--keep-gcov-files")
            config%keep_gcov_files = .true.
        case ("--tui")
            config%tui_mode = .true.
        case ("--strict")
            config%strict_mode = .true.
        case ("--auto-discovery")
            config%auto_discovery = .true.
        case ("--no-auto-discovery")
            config%auto_discovery = .false.
        case ("--auto-test")
            config%auto_test_execution = .true.
        case ("--no-auto-test")
            config%auto_test_execution = .false.
        case ("--fail-on-size-warnings")
            config%fail_on_size_warnings = .true.
        case default
            success = .false.
        end select
    end subroutine process_boolean_flags
    
    subroutine process_complex_flags(flag, value, config, success, error_message)
        !! Process complex flags requiring complex parsing
        character(len=*), intent(in) :: flag, value
        type(config_t), intent(inout) :: config
        logical, intent(inout) :: success
        character(len=*), intent(out) :: error_message
        
        select case (flag)
        case ("--minimum", "-m", "--threshold")
            call parse_threshold_with_error(value, config%minimum_coverage, &
                                            "minimum coverage", success, error_message)
        case ("--fail-under")
            call parse_threshold_with_error(value, config%fail_under_threshold, &
                                            "fail threshold", success, error_message)
        case ("--threads", "-t")
            call parse_integer_with_error(value, config%threads, &
                                          "thread count", success, error_message)
        case ("--diff")
            config%enable_diff = .true.
            call parse_diff_files(value, config, success, error_message)
            ! Flag is recognized even if value parsing fails - error will be caught in validation
            if (.not. success) then
                ! Store raw value for later validation instead of failing flag recognition
                config%diff_baseline_file = "PARSE_ERROR"
                config%diff_current_file = trim(error_message)
                success = .true.
            end if
        case ("--diff-threshold")
            call parse_threshold_with_error(value, config%diff_threshold, &
                                            "diff threshold", success, error_message)
        case ("--test-timeout")
            call parse_integer_with_error(value, config%test_timeout_seconds, &
                                          "test timeout", success, error_message)
            ! Flag was recognized even if there was an error
        case default
            success = .false.
        end select
    end subroutine process_complex_flags

end module config_parser_flags