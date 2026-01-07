module config_parser_special
    !! Special configuration processing utilities
    !!
    !! Handles special flags (help, version, quiet), manual input detection,
    !! and zero-config mode detection with comprehensive flag analysis.
    
    use config_types, only: config_t
    use config_parser_flags, only: is_flag_argument, flag_requires_value
    implicit none
    private
    
    public :: process_special_flags
    public :: disable_auto_tests_for_manual_inputs
    public :: detect_zero_config_mode
    
contains

    subroutine process_special_flags(args, config)
        !! Process help, version, and quiet flags early
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(inout) :: config

        integer :: i
        character(len=:), allocatable :: arg

        do i = 1, size(args)
            arg = trim(adjustl(args(i)))

            ! Skip empty arguments
            if (len_trim(arg) == 0) cycle

            ! Check for help flag
            if (arg == "--help" .or. arg == "-h") then
                config%show_help = .true.
                return
            end if

            ! Check for version flag
            if (arg == "--version" .or. arg == "-V") then
                config%show_version = .true.
                return
            end if

            ! Check for quiet flag
            if (arg == "--quiet" .or. arg == "-q") then
                config%quiet = .true.
            end if

            ! Check for verbose flag
            if (arg == "--verbose" .or. arg == "-v") then
                config%verbose = .true.
            end if

            ! Check for validate flag
            if (arg == "--validate") then
                config%validate_config_only = .true.
            end if
        end do
    end subroutine process_special_flags

    subroutine disable_auto_tests_for_manual_inputs(config)
        !! Disable auto-test execution when manual inputs are provided
        type(config_t), intent(inout) :: config
        logical :: has_manual_inputs

        has_manual_inputs = (allocated(config%coverage_files) .and. &
                             size(config%coverage_files) > 0) .or. &
                            (allocated(config%import_file) .and. &
                             len_trim(config%import_file) > 0) .or. &
                            (allocated(config%source_paths) .and. &
                             size(config%source_paths) > 0 .and. &
                             .not. (size(config%source_paths) == 1 .and. &
                                    trim(config%source_paths(1)) == "."))

        if (has_manual_inputs) then
            config%auto_test_execution = .false.
        end if
    end subroutine disable_auto_tests_for_manual_inputs

    function detect_zero_config_mode(args) result(zero_config_mode)
        !! Detect if zero config mode should be enabled
        character(len=*), intent(in) :: args(:)
        logical :: zero_config_mode
        integer :: i
        logical :: has_non_empty_args
        
        zero_config_mode = .false.
        has_non_empty_args = .false.
        
        ! Count non-empty arguments
        do i = 1, size(args)
            if (len_trim(args(i)) > 0) then
                has_non_empty_args = .true.
                exit
            end if
        end do
        
        ! No arguments or only empty strings means zero-config
        if (size(args) == 0 .or. .not. has_non_empty_args) then
            zero_config_mode = .true.
            return
        end if
        
        ! Check for explicit zero-config flag
        do i = 1, size(args)
            if (trim(args(i)) == "--zero-config") then
                zero_config_mode = .true.
                return
            end if
        end do

        ! If explicit gcov mode is requested, do NOT use zero-config
        do i = 1, size(args)
            if (trim(args(i)) == "--gcov" .or. trim(args(i)) == "--discover-and-gcov") then
                zero_config_mode = .false.
                return
            end if
        end do
        
        ! Check if we have only flags (ignoring their values for zero-config detection)
        ! This allows constructs like --output custom.md to still be zero-config
        call check_zero_config_with_flags(args, zero_config_mode)
    end function detect_zero_config_mode

    subroutine check_zero_config_with_flags(args, zero_config_mode)
        !! Check if arguments represent zero-config mode considering flag-value pairs
        character(len=*), intent(in) :: args(:)
        logical, intent(out) :: zero_config_mode
        integer :: i
        logical :: has_coverage_file
        
        zero_config_mode = .true.
        has_coverage_file = .false.
        
        i = 1
        do while (i <= size(args))
            if (len_trim(args(i)) == 0) then
                ! Skip empty strings
                i = i + 1
                cycle
            end if
            
            if (is_flag_argument(args(i))) then
                ! This is a flag - check if it requires a value
                if (flag_requires_value(args(i))) then
                    ! Skip the next argument (the value for this flag)
                    i = i + 2
                else
                    ! Boolean flag
                    i = i + 1
                end if
            else
                ! This is a positional argument (likely a coverage file)
                has_coverage_file = .true.
                zero_config_mode = .false.
                return
            end if
        end do
        
        ! If we get here, all arguments were flags (with their values)
        ! This means zero-config with overrides
        zero_config_mode = .true.
    end subroutine check_zero_config_with_flags

end module config_parser_special
