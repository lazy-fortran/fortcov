module config_parser
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use config_classifier_args, only: classify_command_arguments
    use config_defaults_core, only: apply_default_output_filename, &
                                    apply_default_output_path_for_coverage_files, &
                                    ensure_zero_config_output_directory, &
                                    handle_zero_configuration_mode, &
                                    initialize_default_config
    use config_positional_args, only: process_positional_arguments
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

    subroutine handle_zero_configuration_with_overrides(args, config, success, &
                                                        error_message)
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
            if (.not. success) return
        end if

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
            if (.not. success) return
        end if

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
        logical :: has_non_empty_args

        zero_config_mode = .false.
        has_non_empty_args = .false.

        do i = 1, size(args)
            if (len_trim(args(i)) > 0) then
                has_non_empty_args = .true.
                exit
            end if
        end do

        if (size(args) == 0 .or. .not. has_non_empty_args) then
            zero_config_mode = .true.
            return
        end if

        do i = 1, size(args)
            if (trim(args(i)) == "--zero-config") then
                zero_config_mode = .true.
                return
            end if
        end do

        do i = 1, size(args)
            if (trim(args(i)) == "--gcov" .or. &
                trim(args(i)) == "--discover-and-gcov") then
                zero_config_mode = .false.
                return
            end if
        end do

        call check_zero_config_with_flags(args, zero_config_mode)

    end function detect_zero_config_mode

    subroutine check_zero_config_with_flags(args, zero_config_mode)
        character(len=*), intent(in) :: args(:)
        logical, intent(out) :: zero_config_mode

        integer :: i

        zero_config_mode = .true.

        i = 1
        do while (i <= size(args))
            if (len_trim(args(i)) == 0) then
                i = i + 1
                cycle
            end if

            if (is_flag_argument(args(i))) then
                if (flag_requires_value(args(i))) then
                    i = i + 2
                else
                    i = i + 1
                end if
            else
                zero_config_mode = .false.
                return
            end if
        end do

    end subroutine check_zero_config_with_flags

    function is_flag_argument(arg) result(is_flag)
        character(len=*), intent(in) :: arg
        logical :: is_flag

        integer :: arg_len
        character(len=1) :: second_char

        arg_len = len_trim(arg)
        if (arg_len < 2) then
            is_flag = .false.
            return
        end if

        if (arg(1:1) /= "-") then
            is_flag = .false.
            return
        end if

        second_char = arg(2:2)
        if (is_digit(second_char)) then
            is_flag = .false.
            return
        end if

        is_flag = .true.

    end function is_flag_argument

    pure function is_digit(c) result(res)
        character(len=1), intent(in) :: c
        logical :: res

        res = (c >= "0" .and. c <= "9")

    end function is_digit

    function flag_requires_value(flag) result(requires_value)
        character(len=*), intent(in) :: flag
        logical :: requires_value

        requires_value = .false.

        select case (trim(flag))
        case ("--source", "-s", "--exclude", "--include", "--output", "-o", &
              "--format", "-f", "--minimum", "-m", "--threshold", &
              "--fail-under", "--diff-threshold", "--import", "--config", &
              "--test-timeout", "--threads", "-t", "--gcov-output-dir", &
              "--diff-baseline", "--diff-current", "--gcov-executable", &
              "--gcov-cmd")
            requires_value = .true.
        case ("--help", "-h", "--version", "-V", "--quiet", "-q", &
              "--verbose", "-v", "--validate", "--diff", "--lcov", &
              "--auto-test", "--no-auto-test", "--auto-discovery", &
              "--no-auto-discovery", "--zero-config", &
              "--gcov", "--discover-and-gcov")
            requires_value = .false.
        end select

    end function flag_requires_value

    function get_long_form_option(short_flag) result(long_flag)
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
        case ("--source", "-s")
            if (len_trim(value) > 0) call add_source_path(config, value)
        case ("--exclude", "--include")
            call set_unsupported_flag_error(flag, success, error_message)
        case ("--output", "-o")
            if (len_trim(value) > 0) config%output_path = value
        case ("--format", "-f")
            if (len_trim(value) > 0) config%output_format = value
        case ("--minimum", "-m", "--threshold")
            if (len_trim(value) > 0) then
                call parse_threshold_with_error(value, config%minimum_coverage, &
                                                "minimum coverage", success, &
                                                error_message)
            end if
        case ("--threads", "-t")
            if (len_trim(value) > 0) then
                call parse_integer_with_error(value, config%threads, "thread count", &
                                              success, error_message)
            end if
        case ("--quiet", "-q")
            config%quiet = .true.
        case ("--verbose", "-v")
            config%verbose = .true.
        case ("--help", "-h")
            config%show_help = .true.
        case ("--version", "-V")
            config%show_version = .true.
        case ("--diff", "--diff-baseline", "--diff-current", "--diff-threshold")
            call set_unsupported_flag_error(flag, success, error_message)
        case ("--fail-under")
            if (len_trim(value) > 0) then
                call parse_threshold_with_error(value, config%fail_under_threshold, &
                                                "fail-under threshold", success, &
                                                error_message)
            end if
        case ("--gcov-output-dir")
            if (len_trim(value) > 0) config%gcov_output_dir = value
        case ("--gcov-executable", "--gcov-cmd")
            if (len_trim(value) > 0) config%gcov_executable = value
        case ("--import")
            call set_unsupported_flag_error(flag, success, error_message)
        case ("--config")
            success = .false.
            error_message = "Configuration files are no longer supported; use CLI flags"
        case ("--auto-test")
            config%auto_test_execution = .true.
        case ("--no-auto-test")
            config%auto_test_execution = .false.
        case ("--auto-discovery", "--no-auto-discovery", "--test-timeout", &
              "--validate", "--zero-config")
            call set_unsupported_flag_error(flag, success, error_message)
        case ("--gcov", "--discover-and-gcov")
            config%auto_discovery = .false.
        case default
            success = .false.
            error_message = "Unknown flag: "//trim(flag)
        end select

    end subroutine apply_flag_to_config

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

    subroutine add_string_to_array(array, new_string)
        character(len=:), allocatable, intent(inout) :: array(:)
        character(len=*), intent(in) :: new_string

        character(len=:), allocatable :: temp_array(:)
        integer :: current_size
        integer :: new_size
        integer :: i
        integer :: new_len

        if (.not. allocated(array)) then
            new_len = max(256, len_trim(new_string))
            allocate (character(len=new_len) :: array(1))
            array(1) = new_string
            return
        end if

        current_size = size(array)
        new_size = current_size + 1
        new_len = max(256, len(array), len_trim(new_string))

        allocate (character(len=new_len) :: temp_array(new_size))
        do i = 1, current_size
            temp_array(i) = array(i)
        end do
        temp_array(new_size) = new_string

        call move_alloc(temp_array, array)

    end subroutine add_string_to_array

    subroutine add_source_path(config, path)
        type(config_t), intent(inout) :: config
        character(len=*), intent(in) :: path

        call add_string_to_array(config%source_paths, path)

    end subroutine add_source_path

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
