module config_parser_consolidated
    !! Consolidated configuration parser module
    !!
    !! Combines all configuration parsing functionality including:
    !! - String parsing utilities and type conversion  
    !! - Array management utilities
    !! - Flag processing utilities
    !! - Special flags (help, version, quiet)
    !! - Fork bomb prevention
    !! - Zero config detection
    !! - Complex parsing utilities
    
    use constants_core, only: MEDIUM_STRING_LEN
    use config_types, only: config_t
    implicit none
    private
    
    ! String parsing utilities
    public :: parse_real_with_error
    public :: parse_integer_with_error
    public :: parse_threshold_with_error
    
    ! Array management utilities
    public :: add_string_to_array
    public :: add_source_path
    public :: add_exclude_pattern
    public :: add_include_pattern
    public :: transfer_string_array
    
    ! Flag processing utilities
    public :: is_flag_argument
    public :: flag_requires_value
    public :: get_long_form_option
    public :: has_input_related_arguments
    public :: has_output_related_arguments
    public :: has_diff_mode_arguments
    
    ! Special flags processing
    public :: process_special_flags
    
    ! Fork bomb prevention
    public :: prevent_fork_bomb_with_manual_files
    
    ! Zero config detection
    public :: detect_zero_config_mode
    
    ! Complex parsing utilities
    public :: parse_diff_files
    
contains

    ! ========================================================================
    ! String Parsing Utilities
    ! ========================================================================
    
    subroutine parse_real_with_error(str, value, value_name, success, error_message)
        !! Parse real value with error handling
        character(len=*), intent(in) :: str
        real, intent(out) :: value
        character(len=*), intent(in) :: value_name
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        integer :: iostat

        read(str, *, iostat=iostat) value
        success = (iostat == 0)

        if (.not. success) then
            value = 0.0
            error_message = "Invalid " // trim(value_name) // ": '" // trim(str) // "'"
        else
            error_message = ""
        end if
    end subroutine parse_real_with_error

    subroutine parse_integer_with_error(str, value, value_name, success, error_message)
        !! Parse integer value with error handling
        character(len=*), intent(in) :: str
        integer, intent(out) :: value
        character(len=*), intent(in) :: value_name
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        integer :: iostat

        read(str, *, iostat=iostat) value
        success = (iostat == 0)

        if (.not. success) then
            value = 0
            error_message = "Invalid " // trim(value_name) // ": '" // trim(str) // "'"
        else
            error_message = ""
        end if
    end subroutine parse_integer_with_error

    subroutine parse_threshold_with_error(str, value, value_name, success, error_message)
        !! Parse threshold value (0.0-100.0) with error handling and validation
        character(len=*), intent(in) :: str
        real, intent(out) :: value
        character(len=*), intent(in) :: value_name
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        call parse_real_with_error(str, value, value_name, success, error_message)
        
        if (success) then
            ! Validate threshold range (0.0 to 100.0)
            if (value < 0.0 .or. value > 100.0) then
                success = .false.
                error_message = "Invalid " // trim(value_name) // ": '" // trim(str) // &
                    "' (must be between 0.0 and 100.0)"
            end if
        end if
    end subroutine parse_threshold_with_error

    ! ========================================================================
    ! Array Management Utilities
    ! ========================================================================
    
    subroutine add_string_to_array(array, new_string)
        !! Add string to dynamically sized array
        character(len=:), allocatable, intent(inout) :: array(:)
        character(len=*), intent(in) :: new_string
        character(len=:), allocatable :: temp_array(:)
        integer :: current_size, new_size, i
        integer :: str_len

        str_len = max(len(new_string), 256)  ! Minimum length for safety
        
        if (.not. allocated(array)) then
            allocate(character(len=str_len) :: array(1))
            array(1) = new_string
        else
            current_size = size(array)
            new_size = current_size + 1
            
            ! Create temporary array with new size
            allocate(character(len=str_len) :: temp_array(new_size))
            
            ! Copy existing strings
            do i = 1, current_size
                temp_array(i) = array(i)
            end do
            
            ! Add new string
            temp_array(new_size) = new_string
            
            ! Replace array with temp array
            call move_alloc(temp_array, array)
        end if
    end subroutine add_string_to_array

    subroutine add_source_path(config, path)
        !! Add source path to configuration
        type(config_t), intent(inout) :: config
        character(len=*), intent(in) :: path
        call add_string_to_array(config%source_paths, path)
    end subroutine add_source_path

    subroutine add_exclude_pattern(config, pattern)
        !! Add exclude pattern to configuration
        type(config_t), intent(inout) :: config
        character(len=*), intent(in) :: pattern
        call add_string_to_array(config%exclude_patterns, pattern)
    end subroutine add_exclude_pattern

    subroutine add_include_pattern(config, pattern)
        !! Add include pattern to configuration
        type(config_t), intent(inout) :: config
        character(len=*), intent(in) :: pattern
        call add_string_to_array(config%include_patterns, pattern)
    end subroutine add_include_pattern

    subroutine transfer_string_array(from_array, to_array)
        !! Transfer string array ownership using move_alloc
        character(len=:), allocatable, intent(inout) :: from_array(:)
        character(len=:), allocatable, intent(out) :: to_array(:)
        call move_alloc(from_array, to_array)
    end subroutine transfer_string_array

    ! ========================================================================
    ! Flag Processing Utilities
    ! ========================================================================
    
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
              "--auto-test", "--zero-config", "--tui")
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

    ! ========================================================================
    ! Special Flags Processing
    ! ========================================================================
    
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

    ! ========================================================================
    ! Fork Bomb Prevention
    ! ========================================================================
    
    subroutine prevent_fork_bomb_with_manual_files(config)
        !! Prevent fork bomb by disabling auto-test execution when manual coverage files are provided
        type(config_t), intent(inout) :: config
        logical :: has_manual_coverage_files
        
        has_manual_coverage_files = .false.
        
        ! Check for manually specified coverage files (positional arguments)
        if (allocated(config%coverage_files) .and. size(config%coverage_files) > 0) then
            has_manual_coverage_files = .true.
        end if
        
        ! Check for import file specification
        if (allocated(config%import_file) .and. len_trim(config%import_file) > 0) then
            has_manual_coverage_files = .true.
        end if
        
        ! Check for manually specified source paths (also indicates manual mode)
        ! Exclude zero-config default source path "." from triggering fork bomb prevention
        if (allocated(config%source_paths) .and. size(config%source_paths) > 0) then
            ! Only consider it manual if it's not just the zero-config default "."
            if (.not. (size(config%source_paths) == 1 .and. &
                      trim(config%source_paths(1)) == ".")) then
                has_manual_coverage_files = .true.
            end if
        end if
        
        ! If manual coverage files detected, disable auto-test execution to prevent fork bomb
        if (has_manual_coverage_files) then
            config%auto_test_execution = .false.
        end if
    end subroutine prevent_fork_bomb_with_manual_files

    ! ========================================================================
    ! Zero Config Detection
    ! ========================================================================
    
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
        
        ! Check if we have only flags (ignoring their values for zero-config detection)
        ! This allows constructs like "--output custom.md" to still be zero-config
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

    ! ========================================================================
    ! Complex Parsing Utilities
    ! ========================================================================
    
    subroutine parse_diff_files(diff_arg, before_file, after_file, success, error_message)
        !! Parse diff files argument in format "before,after"
        character(len=*), intent(in) :: diff_arg
        character(len=:), allocatable, intent(out) :: before_file, after_file
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        integer :: comma_pos
        
        success = .false.
        error_message = ""
        
        ! Find comma separator
        comma_pos = index(diff_arg, ",")
        if (comma_pos == 0) then
            error_message = "Invalid diff format: '" // trim(diff_arg) // &
                "' (expected: before_file,after_file)"
            return
        end if
        
        ! Extract before and after filenames
        before_file = trim(diff_arg(1:comma_pos-1))
        after_file = trim(diff_arg(comma_pos+1:))
        
        ! Validate both filenames are non-empty
        if (len_trim(before_file) == 0 .or. len_trim(after_file) == 0) then
            error_message = "Invalid diff format: empty filename in '" // trim(diff_arg) // "'"
            return
        end if
        
        success = .true.
    end subroutine parse_diff_files

end module config_parser_consolidated