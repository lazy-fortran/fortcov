module config_parser
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use config_classifier_args, only: classify_command_arguments, flag_requires_value, &
                                      get_long_form_option, is_flag_argument
    use config_defaults_core, only: apply_default_output_filename, &
                                    apply_default_output_path_for_coverage_files, &
                                    ensure_zero_config_output_directory, &
                                    handle_zero_configuration_mode, &
                                    initialize_default_config
    use config_positional_args, only: add_source_path, add_string_to_array, &
                                      process_positional_arguments
    use config_types, only: config_t
    implicit none
    private
    public :: add_exclude_pattern
    public :: add_include_pattern
    public :: add_source_path
    public :: add_string_to_array
    public :: detect_zero_config_mode
    public :: disable_auto_tests_for_manual_inputs
    public :: flag_requires_value
    public :: get_long_form_option
    public :: is_flag_argument
    public :: parse_command_line_config
    public :: parse_config_file
    public :: parse_integer_with_error
    public :: parse_real_with_error
    public :: parse_threshold_with_error
    public :: process_flag_arguments
    public :: process_single_flag
    interface parse_real_with_error
        module procedure parse_real_with_error_real32
        module procedure parse_real_with_error_real64
    end interface parse_real_with_error
    interface parse_threshold_with_error
        module procedure parse_threshold_with_error_real32
        module procedure parse_threshold_with_error_real64
    end interface parse_threshold_with_error
contains
    subroutine parse_command_line_config(args, config, success, error_message)
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(out) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        logical :: is_zero_config
        call initialize_default_config(config)
        success = .true.
        error_message = ""
        call process_special_flags(args, config)
        if (config%show_help .or. config%show_version) return
        is_zero_config = detect_zero_config_mode(args)
        config%zero_configuration_mode = is_zero_config
        if (is_zero_config) then
            call handle_zero_configuration_with_overrides(args, config, success, &
                                                          error_message)
            return
        end if
        call handle_normal_configuration(args, config, success, error_message)
    end subroutine parse_command_line_config
    subroutine apply_command_line_overrides(args, config, success, error_message)
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        character(len=1024), allocatable :: flags(:)
        character(len=1024), allocatable :: positionals(:)
        integer :: flag_count, positional_count
        success = .true.
        error_message = ""
        call classify_command_arguments(args, flags, flag_count, positionals, &
                                        positional_count, success, error_message)
        if (.not. success) return
        if (flag_count > 0) then
            call process_flag_arguments(flags, flag_count, config, success, &
                                        error_message)
            if (.not. success) return
        end if
        if (positional_count > 0) then
            call process_positional_arguments(positionals, positional_count, config, &
                                              success, error_message)
        end if
    end subroutine apply_command_line_overrides
    subroutine handle_zero_configuration_with_overrides(args, config, success, &
                                                        error_message)
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        call apply_command_line_overrides(args, config, success, error_message)
        if (.not. success) return
        call handle_zero_configuration_mode(config)
        call disable_auto_tests_for_manual_inputs(config)
        call apply_default_output_filename(config)
        call ensure_zero_config_output_directory(config)
    end subroutine handle_zero_configuration_with_overrides
    subroutine handle_normal_configuration(args, config, success, error_message)
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        call apply_command_line_overrides(args, config, success, error_message)
        if (.not. success) return
        call disable_auto_tests_for_manual_inputs(config)
        call apply_default_output_filename(config)
        call apply_default_output_path_for_coverage_files(config)
    end subroutine handle_normal_configuration
    subroutine process_special_flags(args, config)
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(inout) :: config
        integer :: i
        character(len=:), allocatable :: arg
        do i = 1, size(args)
            arg = trim(adjustl(args(i)))
            if (len_trim(arg) == 0) cycle
            if (arg == "--help" .or. arg == "-h") then
                config%show_help = .true.
                return
            end if
            if (arg == "--version" .or. arg == "-V") then
                config%show_version = .true.
                return
            end if
            if (arg == "--quiet" .or. arg == "-q") then
                config%quiet = .true.
            end if
            if (arg == "--verbose" .or. arg == "-v") then
                config%verbose = .true.
            end if
            if (arg == "--validate") then
                config%validate_config_only = .true.
            end if
        end do
    end subroutine process_special_flags
    subroutine disable_auto_tests_for_manual_inputs(config)
        type(config_t), intent(inout) :: config
        logical :: has_manual_inputs
        has_manual_inputs = .false.
        if (allocated(config%coverage_files)) then
            if (size(config%coverage_files) > 0) has_manual_inputs = .true.
        end if
        if (.not. has_manual_inputs) then
            if (allocated(config%import_file)) then
                if (len_trim(config%import_file) > 0) has_manual_inputs = .true.
            end if
        end if
        if (.not. has_manual_inputs) then
            call has_non_default_source_paths(config, has_manual_inputs)
        end if
        if (has_manual_inputs) config%auto_test_execution = .false.
    end subroutine disable_auto_tests_for_manual_inputs
    subroutine has_non_default_source_paths(config, has_manual_inputs)
        type(config_t), intent(in) :: config
        logical, intent(out) :: has_manual_inputs
        has_manual_inputs = .false.
        if (.not. allocated(config%source_paths)) return
        if (size(config%source_paths) == 0) return
        if (size(config%source_paths) == 1) then
            if (trim(config%source_paths(1)) /= ".") has_manual_inputs = .true.
            return
        end if
        has_manual_inputs = .true.
    end subroutine has_non_default_source_paths
    function detect_zero_config_mode(args) result(zero_config_mode)
        character(len=*), intent(in) :: args(:)
        logical :: zero_config_mode
        integer :: i
        character(len=:), allocatable :: arg
        logical :: has_non_empty_args
        zero_config_mode = .true.
        has_non_empty_args = .false.
        i = 1
        do while (i <= size(args))
            arg = trim(adjustl(args(i)))
            if (len_trim(arg) == 0) then
                i = i + 1
                cycle
            end if
            has_non_empty_args = .true.
            if (arg == "--zero-config") then
                zero_config_mode = .true.
                return
            end if
            if (arg == "--gcov" .or. arg == "--discover-and-gcov") then
                zero_config_mode = .false.
                return
            end if
            if (is_flag_argument(arg)) then
                if (flag_requires_value(arg)) then
                    i = i + 2
                else
                    i = i + 1
                end if
            else
                zero_config_mode = .false.
                return
            end if
        end do
        if (.not. has_non_empty_args) zero_config_mode = .true.
    end function detect_zero_config_mode
    subroutine process_flag_arguments(flags, flag_count, config, success, &
                                      error_message)
        character(len=*), intent(in) :: flags(:)
        integer, intent(in) :: flag_count
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        integer :: i
        character(len=1024) :: combined_flag
        success = .true.
        error_message = ""
        i = 1
        do while (i <= flag_count)
            if (len_trim(flags(i)) == 0) then
                i = i + 1
                cycle
            end if
            if (is_flag_argument(flags(i)) .and. flag_requires_value(flags(i))) then
                if (i < flag_count) then
                    if (len_trim(flags(i + 1)) > 0) then
                        if (.not. is_flag_argument(flags(i + 1))) then
                            combined_flag = trim(flags(i))//"="//trim(flags(i + 1))
                            call process_single_flag(combined_flag, config, success, &
                                                     error_message)
                            if (.not. success) return
                            i = i + 2
                            cycle
                        end if
                    end if
                end if
            end if
            call process_single_flag(flags(i), config, success, error_message)
            if (.not. success) return
            i = i + 1
        end do
    end subroutine process_flag_arguments
    subroutine process_single_flag(flag_with_value, config, success, error_message)
        character(len=*), intent(in) :: flag_with_value
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        character(len=:), allocatable :: flag
        character(len=:), allocatable :: value
        call parse_flag_value(flag_with_value, flag, value)
        call apply_flag_to_config(flag, value, config, success, error_message)
    end subroutine process_single_flag
    subroutine parse_flag_value(flag_with_value, flag, value)
        character(len=*), intent(in) :: flag_with_value
        character(len=:), allocatable, intent(out) :: flag
        character(len=:), allocatable, intent(out) :: value
        integer :: equals_pos
        equals_pos = index(flag_with_value, "=")
        if (equals_pos > 0) then
            flag = trim(flag_with_value(1:equals_pos - 1))
            value = trim(flag_with_value(equals_pos + 1:))
            return
        end if
        flag = trim(flag_with_value)
        value = ""
    end subroutine parse_flag_value
    subroutine apply_flag_to_config(flag, value, config, success, error_message)
        character(len=*), intent(in) :: flag
        character(len=*), intent(in) :: value
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        success = .true.
        error_message = ""
        select case (trim(flag))
        case ("--source", "-s", "--output", "-o", "--format", "-f", &
              "--gcov-output-dir", "--gcov-executable", "--gcov-cmd")
            call apply_string_flag(flag, value, config)
        case ("--minimum", "-m", "--threshold", "--fail-under", "--threads", "-t")
            call apply_numeric_flag(flag, value, config, success, error_message)
        case ("--quiet", "-q", "--verbose", "-v", "--help", "-h", "--version", "-V", &
              "--auto-test", "--no-auto-test", "--gcov", "--discover-and-gcov", &
              "--validate", "--zero-config")
            call apply_toggle_flag(flag, config)
        case ("--exclude", "--include", "--diff", "--diff-baseline", &
              "--diff-current", "--diff-threshold", "--import", &
              "--auto-discovery", "--no-auto-discovery", "--test-timeout")
            call set_unsupported_flag_error(flag, success, error_message)
        case ("--config")
            success = .false.
            error_message = "Configuration files are no longer supported; use CLI flags"
        case default
            success = .false.
            error_message = "Unknown flag: "//trim(flag)
        end select
    end subroutine apply_flag_to_config
    subroutine apply_toggle_flag(flag, config)
        character(len=*), intent(in) :: flag
        type(config_t), intent(inout) :: config
        select case (trim(flag))
        case ("--quiet", "-q")
            config%quiet = .true.
        case ("--verbose", "-v")
            config%verbose = .true.
        case ("--help", "-h")
            config%show_help = .true.
        case ("--version", "-V")
            config%show_version = .true.
        case ("--auto-test")
            config%auto_test_execution = .true.
        case ("--no-auto-test")
            config%auto_test_execution = .false.
        case ("--gcov", "--discover-and-gcov")
            config%auto_discovery = .false.
        case ("--validate", "--zero-config")
            continue
        end select
    end subroutine apply_toggle_flag
    subroutine apply_string_flag(flag, value, config)
        character(len=*), intent(in) :: flag
        character(len=*), intent(in) :: value
        type(config_t), intent(inout) :: config
        if (len_trim(value) == 0) return
        select case (trim(flag))
        case ("--source", "-s")
            call add_source_path(config, value)
        case ("--output", "-o")
            config%output_path = value
        case ("--format", "-f")
            config%output_format = value
        case ("--gcov-output-dir")
            config%gcov_output_dir = value
        case ("--gcov-executable", "--gcov-cmd")
            config%gcov_executable = value
        end select
    end subroutine apply_string_flag
    subroutine apply_numeric_flag(flag, value, config, success, error_message)
        character(len=*), intent(in) :: flag
        character(len=*), intent(in) :: value
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        if (len_trim(value) == 0) then
            success = .true.
            error_message = ""
            return
        end if
        select case (trim(flag))
        case ("--minimum", "-m", "--threshold")
            call parse_threshold_with_error(value, config%minimum_coverage, &
                                            "minimum coverage", success, error_message)
        case ("--threads", "-t")
            call parse_integer_with_error(value, config%threads, "thread count", &
                                          success, error_message)
        case ("--fail-under")
            call parse_threshold_with_error(value, config%fail_under_threshold, &
                                            "fail-under threshold", success, &
                                            error_message)
        case default
            success = .true.
            error_message = ""
        end select
    end subroutine apply_numeric_flag
    subroutine set_unsupported_flag_error(flag, success, error_message)
        character(len=*), intent(in) :: flag
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        success = .false.
        error_message = "Flag no longer supported: "//trim(flag)
    end subroutine set_unsupported_flag_error
    subroutine parse_real_with_error_real32(str, value, value_name, success, &
                                            error_message)
        character(len=*), intent(in) :: str
        real, intent(out) :: value
        character(len=*), intent(in) :: value_name
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        integer :: iostat
        real(dp) :: parsed
        read (str, *, iostat=iostat) parsed
        success = (iostat == 0)
        if (.not. success) then
            value = 0.0
            error_message = "Invalid "//trim(value_name)//": "//trim(str)
            return
        end if
        value = real(parsed, kind(value))
        error_message = ""
    end subroutine parse_real_with_error_real32
    subroutine parse_real_with_error_real64(str, value, value_name, success, &
                                            error_message)
        character(len=*), intent(in) :: str
        real(dp), intent(out) :: value
        character(len=*), intent(in) :: value_name
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        integer :: iostat
        read (str, *, iostat=iostat) value
        success = (iostat == 0)
        if (.not. success) then
            value = 0.0d0
            error_message = "Invalid "//trim(value_name)//": "//trim(str)
            return
        end if
        error_message = ""
    end subroutine parse_real_with_error_real64
    subroutine parse_integer_with_error(str, value, value_name, success, &
                                        error_message)
        character(len=*), intent(in) :: str
        integer, intent(out) :: value
        character(len=*), intent(in) :: value_name
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        integer :: iostat
        read (str, *, iostat=iostat) value
        success = (iostat == 0)
        if (.not. success) then
            value = 0
            error_message = "Invalid "//trim(value_name)//": "//trim(str)
            return
        end if
        error_message = ""
    end subroutine parse_integer_with_error
    subroutine parse_threshold_with_error_real32(str, value, value_name, success, &
                                                 error_message)
        character(len=*), intent(in) :: str
        real, intent(out) :: value
        character(len=*), intent(in) :: value_name
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        real(dp) :: tmp
        call parse_real_with_error(str, tmp, value_name, success, error_message)
        if (.not. success) then
            value = 0.0
            return
        end if
        if (tmp < 0.0d0 .or. tmp > 100.0d0) then
            success = .false.
            error_message = "Invalid "//trim(value_name)//": "//trim(str)// &
                            " (must be between 0.0 and 100.0)"
            value = 0.0
            return
        end if
        value = real(tmp, kind(value))
        error_message = ""
    end subroutine parse_threshold_with_error_real32
    subroutine parse_threshold_with_error_real64(str, value, value_name, success, &
                                                 error_message)
        character(len=*), intent(in) :: str
        real(dp), intent(out) :: value
        character(len=*), intent(in) :: value_name
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        call parse_real_with_error(str, value, value_name, success, error_message)
        if (.not. success) return
        if (value < 0.0d0 .or. value > 100.0d0) then
            success = .false.
            error_message = "Invalid "//trim(value_name)//": "//trim(str)// &
                            " (must be between 0.0 and 100.0)"
            return
        end if
        error_message = ""
    end subroutine parse_threshold_with_error_real64
    subroutine add_exclude_pattern(config, pattern)
        type(config_t), intent(inout) :: config
        character(len=*), intent(in) :: pattern
        call add_string_to_array(config%exclude_patterns, pattern)
    end subroutine add_exclude_pattern
    subroutine add_include_pattern(config, pattern)
        type(config_t), intent(inout) :: config
        character(len=*), intent(in) :: pattern
        call add_string_to_array(config%include_patterns, pattern)
    end subroutine add_include_pattern
    subroutine parse_config_file(config, success, error_message)
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        if (len_trim(config%config_file) == 0) then
            success = .true.
            error_message = ""
            return
        end if
        success = .false.
        error_message = "Configuration files are no longer supported; use CLI flags"
    end subroutine parse_config_file
end module config_parser
