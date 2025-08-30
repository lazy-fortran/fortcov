module config_parser_flags
    !! Flag processing utilities for command-line argument parsing
    !!
    !! Provides comprehensive flag detection, validation, and processing
    !! for all supported command-line options and argument patterns.
    
    implicit none
    private
    
    public :: is_flag_argument
    public :: flag_requires_value
    public :: get_long_form_option
    public :: has_input_related_arguments
    public :: has_output_related_arguments
    public :: has_diff_mode_arguments
    public :: process_flag_arguments
    public :: process_single_flag
    
contains

    function is_flag_argument(arg) result(is_flag)
        !! Check if argument is a flag (starts with -)
        character(len=*), intent(in) :: arg
        logical :: is_flag
        is_flag = (len_trim(arg) > 1 .and. arg(1:1) == "-")
    end function is_flag_argument

    function flag_requires_value(flag) result(requires_value)
        !! Check if flag requires a value
        character(len=*), intent(in) :: flag
        logical :: requires_value
        
        requires_value = .false.
        
        ! Flags that require values
        select case (trim(flag))
        case ("--source", "-s", "--exclude", "--include", "--output", "-o", &
              "--format", "-f", "--minimum", "-m", "--threshold", &
              "--fail-under", "--diff-threshold", "--import", "--config", &
              "--test-timeout", "--threads", "-t", "--architecture-format", &
              "--gcov-executable", "--gcov-args", "--diff-baseline", "--diff-current")
            requires_value = .true.
        case ("--help", "-h", "--version", "-V", "--quiet", "-q", &
              "--verbose", "-v", "--validate", "--diff", "--lcov", &
              "--auto-test", "--no-auto-test", "--auto-discovery", &
              "--no-auto-discovery", "--zero-config", "--tui")
            requires_value = .false.
        end select
    end function flag_requires_value

    function get_long_form_option(short_flag) result(long_flag)
        !! Convert short flag to long form
        character(len=*), intent(in) :: short_flag
        character(len=:), allocatable :: long_flag
        
        select case (trim(short_flag))
        case ("-s")
            long_flag = "--source"
        case ("-o")
            long_flag = "--output"
        case ("-f")
            long_flag = "--format"
        case ("-m")
            long_flag = "--minimum"
        case ("-t")
            long_flag = "--threads"
        case ("-h")
            long_flag = "--help"
        case ("-V")
            long_flag = "--version"
        case ("-q")
            long_flag = "--quiet"
        case ("-v")
            long_flag = "--verbose"
        case default
            long_flag = short_flag
        end select
    end function get_long_form_option

    function has_input_related_arguments(args) result(has_input)
        !! Check if arguments contain input-related flags
        character(len=*), intent(in) :: args(:)
        logical :: has_input
        integer :: i
        
        has_input = .false.
        do i = 1, size(args)
            if (trim(args(i)) == "--source" .or. trim(args(i)) == "-s" .or. &
                trim(args(i)) == "--include" .or. trim(args(i)) == "--exclude") then
                has_input = .true.
                return
            end if
        end do
    end function has_input_related_arguments

    function has_output_related_arguments(args) result(has_output)
        !! Check if arguments contain output-related flags
        character(len=*), intent(in) :: args(:)
        logical :: has_output
        integer :: i
        
        has_output = .false.
        do i = 1, size(args)
            if (trim(args(i)) == "--output" .or. trim(args(i)) == "-o" .or. &
                trim(args(i)) == "--format" .or. trim(args(i)) == "-f") then
                has_output = .true.
                return
            end if
        end do
    end function has_output_related_arguments

    function has_diff_mode_arguments(args) result(has_diff)
        !! Check if arguments contain diff mode flags
        character(len=*), intent(in) :: args(:)
        logical :: has_diff
        integer :: i
        
        has_diff = .false.
        do i = 1, size(args)
            if (trim(args(i)) == "--diff" .or. trim(args(i)) == "--diff-threshold") then
                has_diff = .true.
                return
            end if
        end do
    end function has_diff_mode_arguments

    subroutine process_flag_arguments(flags, flag_count, config, success, error_message)
        !! Process array of flag arguments and update configuration
        use config_types, only: config_t
        character(len=*), intent(in) :: flags(:)
        integer, intent(in) :: flag_count
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        integer :: i
        character(len=1024) :: combined_flag
        
        success = .true.
        error_message = ""
        
        ! Process each flag in sequence, handling space-separated flag-value pairs
        i = 1
        do while (i <= flag_count)
            if (len_trim(flags(i)) > 0) then
                ! Check if this flag requires a value and the next element is the value
                if (is_flag_argument(flags(i)) .and. flag_requires_value(flags(i))) then
                    if (i < flag_count .and. len_trim(flags(i+1)) > 0 .and. &
                        .not. is_flag_argument(flags(i+1))) then
                        ! Combine flag and value: "flag=value"
                        combined_flag = trim(flags(i)) // "=" // trim(flags(i+1))
                        call process_single_flag(combined_flag, config, success, error_message)
                        if (.not. success) return
                        i = i + 2  ! Skip both flag and value
                    else
                        ! Flag without value - process as is
                        call process_single_flag(flags(i), config, success, error_message)
                        if (.not. success) return
                        i = i + 1
                    end if
                else
                    ! Regular flag (no value required) or already combined
                    call process_single_flag(flags(i), config, success, error_message)
                    if (.not. success) return
                    i = i + 1
                end if
            else
                i = i + 1
            end if
        end do
    end subroutine process_flag_arguments
    
    subroutine process_single_flag(flag_with_value, config, success, error_message)
        !! Process a single flag with its value and update configuration
        use config_types, only: config_t
        use config_parser_string, only: parse_real_with_error, parse_integer_with_error, parse_threshold_with_error
        use config_parser_arrays, only: add_source_path, add_exclude_pattern, add_include_pattern
        character(len=*), intent(in) :: flag_with_value
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        character(len=:), allocatable :: flag, value
        integer :: equals_pos
        
        success = .true.
        error_message = ""
        
        ! Parse flag=value format or separate flag and value
        equals_pos = index(flag_with_value, '=')
        if (equals_pos > 0) then
            flag = trim(flag_with_value(1:equals_pos-1))
            value = trim(flag_with_value(equals_pos+1:))
        else
            flag = trim(flag_with_value)
            value = ""
        end if
        
        ! Process specific flags
        select case (trim(flag))
        case ("--source", "-s")
            if (len_trim(value) > 0) then
                call add_source_path(config, value)
            end if
        case ("--exclude")
            if (len_trim(value) > 0) then
                call add_exclude_pattern(config, value)
            end if
        case ("--include")
            if (len_trim(value) > 0) then
                call add_include_pattern(config, value)
            end if
        case ("--output", "-o")
            if (len_trim(value) > 0) then
                config%output_path = value
            end if
        case ("--format", "-f")
            if (len_trim(value) > 0) then
                config%output_format = value
            end if
        case ("--minimum", "-m", "--threshold")
            if (len_trim(value) > 0) then
                call parse_threshold_with_error(value, config%minimum_coverage, "minimum coverage", success, error_message)
            end if
        case ("--threads", "-t")
            if (len_trim(value) > 0) then
                call parse_integer_with_error(value, config%threads, "thread count", success, error_message)
            end if
        case ("--quiet", "-q")
            config%quiet = .true.
        case ("--verbose", "-v")
            config%verbose = .true.
        case ("--help", "-h")
            config%show_help = .true.
        case ("--version", "-V")
            config%show_version = .true.
        case ("--diff")
            config%enable_diff = .true.
        case ("--diff-baseline")
            if (len_trim(value) > 0) then
                config%diff_baseline_file = value
            end if
        case ("--diff-current")
            if (len_trim(value) > 0) then
                config%diff_current_file = value
            end if
        case ("--fail-under")
            if (len_trim(value) > 0) then
                call parse_threshold_with_error(value, config%fail_under_threshold, "fail-under threshold", success, error_message)
            end if
        case ("--diff-threshold")
            if (len_trim(value) > 0) then
                call parse_threshold_with_error(value, config%diff_threshold, "diff threshold", success, error_message)
            end if
        case ("--import")
            if (len_trim(value) > 0) then
                config%import_file = value
            end if
        case ("--config")
            if (len_trim(value) > 0) then
                config%config_file = value
            end if
        case ("--tui")
            config%tui_mode = .true.
        case ("--auto-discovery")
            config%auto_discovery = .true.
        case ("--no-auto-discovery")
            config%auto_discovery = .false.
        case ("--auto-test")
            config%auto_test_execution = .true.
        case ("--no-auto-test")
            config%auto_test_execution = .false.
        case ("--test-timeout")
            if (len_trim(value) > 0) then
                call parse_integer_with_error(value, config%test_timeout_seconds, "test timeout", success, error_message)
            end if
        case ("--architecture-format")
            if (len_trim(value) > 0) then
                config%architecture_output_format = value
            end if
        case default
            ! Unknown flag - reject with error message
            success = .false.
            error_message = "Unknown flag: '" // trim(flag) // "'"
        end select
    end subroutine process_single_flag

end module config_parser_flags