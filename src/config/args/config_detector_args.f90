module config_detector_args
    !! Argument type detection for command-line parsing
    !!
    !! This module provides utilities to detect different types of arguments
    !! including input-related, output-related, and diff mode arguments.

    use config_parser, only: is_flag_argument
    implicit none
    private

    public :: has_input_related_arguments
    public :: has_output_related_arguments
    public :: has_diff_mode_arguments

contains

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
                flag_part == "--include" .or. flag_part == "-i" .or. &
                flag_part == "--exclude" .or. flag_part == "-e" .or. &
                flag_part == "--gcov-executable" .or. &
                flag_part == "--gcov-cmd") then
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

end module config_detector_args
