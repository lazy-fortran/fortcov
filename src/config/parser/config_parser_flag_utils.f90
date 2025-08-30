module config_parser_flag_utils
    !! Command-line flag processing utilities
    !!
    !! This module provides specialized functions for processing command-line
    !! flags, including validation, conversion, and detection of flag types.

    implicit none
    private

    ! Flag processing utilities
    public :: is_flag_argument
    public :: flag_requires_value
    public :: get_long_form_option
    public :: has_input_related_arguments
    public :: has_output_related_arguments
    public :: has_diff_mode_arguments

contains

    function is_flag_argument(arg) result(is_flag)
        !! Check if argument is a flag (starts with - or --)
        character(len=*), intent(in) :: arg
        logical :: is_flag

        ! Check for empty string before substring access to prevent crash
        if (len_trim(arg) == 0) then
            is_flag = .false.
        else
            is_flag = (len_trim(arg) > 1 .and. arg(1:1) == '-')
        end if

    end function is_flag_argument

    function flag_requires_value(flag) result(requires_value)
        !! Check if flag requires a value
        character(len=*), intent(in) :: flag
        logical :: requires_value

        select case (trim(flag))
        ! Flags that do NOT require values
        case ("--help", "-h", "--version", "-V", "--verbose", "-v", &
              "--quiet", "-q", "--validate", "--keep-gcov-files", &
              "--tui", "--strict", "--include-unchanged", &
              "--auto-discovery", "--no-auto-discovery", &
              "--auto-test", "--no-auto-test", "--validate-architecture", &
              "--fail-on-size-warnings")
            requires_value = .false.
        ! Flags that DO require values
        case ("--source", "-s", "--exclude", "-e", "--include", "-i", &
              "--output", "-o", "--format", "-f", "--config", "-c", &
              "--import", "--gcov-executable", "--gcov-args", &
              "--minimum", "-m", "--threshold", "--fail-under", &
              "--threads", "-t", "--diff", "--diff-threshold", &
              "--test-timeout", "--architecture-format")
            requires_value = .true.
        ! Unknown flags do not require values (will be caught as invalid later)
        case default
            requires_value = .false.
        end select

    end function flag_requires_value

    function get_long_form_option(short_arg) result(long_form)
        !! Convert short option to long form
        character(len=*), intent(in) :: short_arg
        character(len=:), allocatable :: long_form

        select case (trim(short_arg))
        case ("-h")
            long_form = "--help"
        case ("-v")
            long_form = "--verbose"
        case ("-V")
            long_form = "--version"
        case ("-q")
            long_form = "--quiet"
        case ("-o")
            long_form = "--output"
        case ("-f")
            long_form = "--format"
        case ("-s")
            long_form = "--source"
        case ("-e")
            long_form = "--exclude"
        case ("-i")
            long_form = "--include"
        case ("-m")
            long_form = "--minimum"
        case ("-t")
            long_form = "--threads"
        case ("-c")
            long_form = "--config"
        case default
            long_form = short_arg
        end select

    end function get_long_form_option

    function has_input_related_arguments(args) result(has_input_args)
        !! Check if arguments contain input-related flags
        character(len=*), intent(in) :: args(:)
        logical :: has_input_args

        integer :: i, equals_pos
        character(len=:), allocatable :: arg, flag_part

        has_input_args = .false.

        do i = 1, size(args)
            arg = trim(adjustl(args(i)))

            ! Handle flags with equals signs (--flag=value)
            equals_pos = index(arg, '=')
            if (equals_pos > 0) then
                flag_part = arg(1:equals_pos-1)
            else
                flag_part = arg
            end if

            ! Check for input-related flags
            if (flag_part == "--source" .or. flag_part == "-s" .or. &
                flag_part == "--import" .or. &
                flag_part == "--config" .or. flag_part == "-c" .or. &
                flag_part == "--gcov-executable" .or. &
                flag_part == "--gcov-args" .or. &
                flag_part == "--include" .or. flag_part == "-i" .or. &
                flag_part == "--exclude" .or. flag_part == "-e") then
                has_input_args = .true.
                return
            end if

            ! Check for positional arguments (coverage files)
            if (.not. is_flag_argument(arg)) then
                if (index(arg, ".gcov") > 0 .or. &
                    index(arg, ".json") > 0 .or. &
                    index(arg, ".xml") > 0 .or. &
                    index(arg, ".info") > 0) then
                    has_input_args = .true.
                    return
                end if
            end if
        end do

    end function has_input_related_arguments

    function has_output_related_arguments(args) result(has_output_args)
        !! Check if arguments contain output-related flags
        character(len=*), intent(in) :: args(:)
        logical :: has_output_args

        integer :: i, equals_pos
        character(len=:), allocatable :: arg, flag_part

        has_output_args = .false.

        do i = 1, size(args)
            arg = trim(adjustl(args(i)))

            ! Handle flags with equals signs (--flag=value)
            equals_pos = index(arg, '=')
            if (equals_pos > 0) then
                flag_part = arg(1:equals_pos-1)
            else
                flag_part = arg
            end if

            ! Check for output-related flags (including boolean output flags)
            if (flag_part == "--output" .or. flag_part == "-o" .or. &
                flag_part == "--format" .or. flag_part == "-f" .or. &
                flag_part == "--verbose" .or. flag_part == "-v" .or. &
                flag_part == "--quiet" .or. flag_part == "-q" .or. &
                flag_part == "--tui" .or. &
                flag_part == "--diff" .or. &
                flag_part == "--diff-threshold" .or. &
                flag_part == "--include-unchanged" .or. &
                flag_part == "--threshold" .or. flag_part == "-m" .or. &
                flag_part == "--minimum" .or. &
                flag_part == "--strict" .or. &
                flag_part == "--validate") then
                has_output_args = .true.
                return
            end if
        end do

    end function has_output_related_arguments

    function has_diff_mode_arguments(args) result(has_diff_args)
        !! Check if arguments contain diff mode flags
        character(len=*), intent(in) :: args(:)
        logical :: has_diff_args

        integer :: i, equals_pos
        character(len=:), allocatable :: arg, flag_part

        has_diff_args = .false.

        do i = 1, size(args)
            arg = trim(adjustl(args(i)))

            ! Handle flags with equals signs (--flag=value)
            equals_pos = index(arg, '=')
            if (equals_pos > 0) then
                flag_part = arg(1:equals_pos-1)
            else
                flag_part = arg
            end if

            ! Check for diff-related flags
            if (flag_part == "--diff") then
                has_diff_args = .true.
                return
            end if
        end do

    end function has_diff_mode_arguments

end module config_parser_flag_utils