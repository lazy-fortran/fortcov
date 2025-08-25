module config_command_parser
    !! Command-line argument parsing
    !! 
    !! This module handles parsing of command-line arguments, including
    !! flag processing, positional arguments, and zero-configuration mode detection.

    use config_types, only: config_t, MAX_ARRAY_SIZE
    use config_parser_utils
    use config_defaults
    use config_file_parser
    use foundation_constants
    use foundation_layer_utils
    use string_utils
    use zero_configuration_manager

    implicit none
    private

    public :: parse_command_line_config
    public :: process_special_flags
    public :: handle_zero_configuration_with_overrides
    public :: handle_normal_configuration
    public :: classify_command_arguments
    public :: process_flag_arguments
    public :: process_positional_arguments
    public :: has_input_related_arguments
    public :: has_output_related_arguments

contains

    function should_use_zero_config(args) result(is_zero_config)
        !! Determine if zero-configuration mode should be used
        character(len=*), intent(in) :: args(:)
        logical :: is_zero_config
        
        integer :: i
        logical :: has_input_sources, has_output_flags

        ! No arguments means zero-config
        is_zero_config = (size(args) == 0)

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
            
            ! Enable zero-config if only output flags but no input sources
            if (has_output_flags .and. .not. has_input_sources) then
                is_zero_config = .true.
            end if
        end if

    end function should_use_zero_config

    subroutine parse_command_line_config(args, config, success, error_message)
        !! Main command-line configuration parsing function
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(out) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        logical :: is_zero_config

        ! Initialize config with defaults
        call initialize_default_config(config)

        success = .true.
        error_message = ""

        ! Process help/version/quiet flags first
        call process_special_flags(args, config)

        ! If help or version requested, skip all other parsing
        if (config%show_help .or. config%show_version) then
            return
        end if

        ! Check for zero-configuration mode
        is_zero_config = should_use_zero_config(args)

        if (is_zero_config) then
            ! Process CLI flags FIRST, then apply zero-config defaults for unset values
            call handle_zero_configuration_with_overrides(args, config, success, error_message)
            return
        end if

        ! Normal argument parsing for non-zero-configuration mode
        call handle_normal_configuration(args, config, success, error_message)

    end subroutine parse_command_line_config

    subroutine process_special_flags(args, config)
        !! Process help, version, and quiet flags early
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(inout) :: config

        integer :: i
        character(len=:), allocatable :: arg, flag

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

    subroutine handle_zero_configuration_with_overrides(args, config, success, error_message)
        !! Handle zero-configuration mode with CLI overrides
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        character(len=1024), allocatable :: flags(:)
        character(len=1024), allocatable :: positionals(:)
        integer :: flag_count, positional_count

        success = .true.
        error_message = ""

        ! First, classify arguments
        call classify_command_arguments(args, flags, flag_count, positionals, &
                                         positional_count, success, error_message)
        if (.not. success) return

        ! Process flag arguments to get CLI overrides
        if (flag_count > 0) then
            call process_flag_arguments(flags, flag_count, config, success, error_message)
            if (.not. success) return
        end if

        ! Process positional arguments if any
        if (positional_count > 0) then
            call process_positional_arguments(positionals, positional_count, &
                                               config, success, error_message)
            if (.not. success) return
        end if

        ! Now apply zero-configuration defaults for unset fields
        call handle_zero_configuration_mode(config)

        ! Load config file if specified
        if (len_trim(config%config_file) > 0) then
            call parse_config_file(config, success, error_message)
            if (.not. success) return
        end if

        ! Apply final defaults
        call apply_html_default_filename(config)
        call ensure_zero_config_output_directory(config)

    end subroutine handle_zero_configuration_with_overrides

    subroutine handle_normal_configuration(args, config, success, error_message)
        !! Handle normal (non-zero-config) command-line parsing
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        character(len=1024), allocatable :: flags(:)
        character(len=1024), allocatable :: positionals(:)
        integer :: flag_count, positional_count

        success = .true.
        error_message = ""

        ! Classify arguments into flags and positionals
        call classify_command_arguments(args, flags, flag_count, positionals, &
                                         positional_count, success, error_message)
        if (.not. success) return

        ! Process flag arguments
        if (flag_count > 0) then
            call process_flag_arguments(flags, flag_count, config, success, error_message)
            if (.not. success) return
        end if

        ! Process positional arguments
        if (positional_count > 0) then
            call process_positional_arguments(positionals, positional_count, &
                                               config, success, error_message)
            if (.not. success) return
        end if

        ! Load config file if specified
        if (len_trim(config%config_file) > 0) then
            call parse_config_file(config, success, error_message)
            if (.not. success) return
        end if

        ! Apply defaults for HTML output
        call apply_html_default_filename(config)
        call apply_default_output_path_for_coverage_files(config)

    end subroutine handle_normal_configuration

    subroutine classify_command_arguments(args, flags, flag_count, positionals, &
                                           positional_count, success, error_message)
        !! Classify arguments into flags and positionals
        character(len=*), intent(in) :: args(:)
        character(len=1024), allocatable, intent(out) :: flags(:)
        integer, intent(out) :: flag_count
        character(len=1024), allocatable, intent(out) :: positionals(:)
        integer, intent(out) :: positional_count
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        logical :: expecting_value

        ! Initialize classification state
        call initialize_argument_classification(flags, positionals, flag_count, &
                                               positional_count, expecting_value, &
                                               success, error_message)

        ! Process all arguments
        call process_all_arguments(args, flags, flag_count, positionals, &
                                  positional_count, expecting_value, success, error_message)
        if (.not. success) return

        ! Final validation
        call validate_argument_completion(flags, flag_count, expecting_value, &
                                         success, error_message)

    end subroutine classify_command_arguments
    
    ! Initialize argument classification arrays and state
    subroutine initialize_argument_classification(flags, positionals, flag_count, &
                                                 positional_count, expecting_value, &
                                                 success, error_message)
        character(len=1024), allocatable, intent(out) :: flags(:)
        character(len=1024), allocatable, intent(out) :: positionals(:)
        integer, intent(out) :: flag_count, positional_count
        logical, intent(out) :: expecting_value, success
        character(len=*), intent(out) :: error_message
        
        success = .true.
        error_message = ""
        flag_count = 0
        positional_count = 0
        expecting_value = .false.
        
        allocate(character(len=1024) :: flags(MAX_ARRAY_SIZE))
        allocate(character(len=1024) :: positionals(MAX_ARRAY_SIZE))
    end subroutine initialize_argument_classification
    
    ! Process all command line arguments
    subroutine process_all_arguments(args, flags, flag_count, positionals, &
                                    positional_count, expecting_value, success, error_message)
        character(len=*), intent(in) :: args(:)
        character(len=1024), allocatable, intent(inout) :: flags(:)
        integer, intent(inout) :: flag_count
        character(len=1024), allocatable, intent(inout) :: positionals(:)
        integer, intent(inout) :: positional_count
        logical, intent(inout) :: expecting_value
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        integer :: i
        character(len=:), allocatable :: arg
        
        success = .true.
        error_message = ""
        
        do i = 1, size(args)
            arg = trim(adjustl(args(i)))
            if (len_trim(arg) == 0) cycle
            
            call process_single_argument(arg, flags, flag_count, positionals, &
                                        positional_count, expecting_value, success, error_message)
            if (.not. success) return
        end do
    end subroutine process_all_arguments
    
    ! Process a single command line argument
    subroutine process_single_argument(arg, flags, flag_count, positionals, &
                                      positional_count, expecting_value, success, error_message)
        character(len=*), intent(in) :: arg
        character(len=1024), allocatable, intent(inout) :: flags(:)
        integer, intent(inout) :: flag_count
        character(len=1024), allocatable, intent(inout) :: positionals(:)
        integer, intent(inout) :: positional_count
        logical, intent(inout) :: expecting_value
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        if (expecting_value) then
            call add_flag_value(arg, flags, flag_count, expecting_value, success, error_message)
        else if (is_flag_argument(arg)) then
            call process_flag_argument(arg, flags, flag_count, expecting_value, &
                                      success, error_message)
        else
            call add_positional_argument(arg, positionals, positional_count, &
                                        success, error_message)
        end if
    end subroutine process_single_argument
    
    ! Add flag value to flags array
    subroutine add_flag_value(value, flags, flag_count, expecting_value, success, error_message)
        character(len=*), intent(in) :: value
        character(len=1024), allocatable, intent(inout) :: flags(:)
        integer, intent(inout) :: flag_count
        logical, intent(inout) :: expecting_value
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        flag_count = flag_count + 1
        if (flag_count > MAX_ARRAY_SIZE) then
            success = .false.
            error_message = "Too many flag arguments"
            return
        end if
        
        flags(flag_count) = value
        expecting_value = .false.
        success = .true.
        error_message = ""
    end subroutine add_flag_value
    
    ! Process flag argument (with or without equals)
    subroutine process_flag_argument(arg, flags, flag_count, expecting_value, &
                                    success, error_message)
        character(len=*), intent(in) :: arg
        character(len=1024), allocatable, intent(inout) :: flags(:)
        integer, intent(inout) :: flag_count
        logical, intent(inout) :: expecting_value
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        integer :: equals_pos
        
        equals_pos = index(arg, '=')
        if (equals_pos > 0) then
            call process_flag_with_equals(arg, equals_pos, flags, flag_count, &
                                         success, error_message)
        else
            call process_regular_flag(arg, flags, flag_count, expecting_value, &
                                     success, error_message)
        end if
    end subroutine process_flag_argument
    
    ! Process flag with equals sign (--flag=value)
    subroutine process_flag_with_equals(arg, equals_pos, flags, flag_count, &
                                       success, error_message)
        character(len=*), intent(in) :: arg
        integer, intent(in) :: equals_pos
        character(len=1024), allocatable, intent(inout) :: flags(:)
        integer, intent(inout) :: flag_count
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        character(len=:), allocatable :: flag, value
        
        flag = arg(1:equals_pos-1)
        value = arg(equals_pos+1:)
        
        call add_flag_to_array(flag, flags, flag_count, success, error_message)
        if (.not. success) return
        
        call add_flag_to_array(value, flags, flag_count, success, error_message)
    end subroutine process_flag_with_equals
    
    ! Process regular flag without value
    subroutine process_regular_flag(arg, flags, flag_count, expecting_value, &
                                   success, error_message)
        character(len=*), intent(in) :: arg
        character(len=1024), allocatable, intent(inout) :: flags(:)
        integer, intent(inout) :: flag_count
        logical, intent(inout) :: expecting_value
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        character(len=:), allocatable :: flag
        
        flag = get_long_form_option(arg)
        call add_flag_to_array(flag, flags, flag_count, success, error_message)
        if (.not. success) return
        
        if (flag_requires_value(flag)) then
            expecting_value = .true.
        end if
    end subroutine process_regular_flag
    
    ! Add flag to flags array with bounds checking
    subroutine add_flag_to_array(flag, flags, flag_count, success, error_message)
        character(len=*), intent(in) :: flag
        character(len=1024), allocatable, intent(inout) :: flags(:)
        integer, intent(inout) :: flag_count
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        flag_count = flag_count + 1
        if (flag_count > MAX_ARRAY_SIZE) then
            success = .false.
            error_message = "Too many flag arguments"
            return
        end if
        
        flags(flag_count) = flag
        success = .true.
        error_message = ""
    end subroutine add_flag_to_array
    
    ! Add positional argument with bounds checking
    subroutine add_positional_argument(arg, positionals, positional_count, &
                                      success, error_message)
        character(len=*), intent(in) :: arg
        character(len=1024), allocatable, intent(inout) :: positionals(:)
        integer, intent(inout) :: positional_count
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        positional_count = positional_count + 1
        if (positional_count > MAX_ARRAY_SIZE) then
            success = .false.
            error_message = "Too many positional arguments"
            return
        end if
        
        positionals(positional_count) = arg
        success = .true.
        error_message = ""
    end subroutine add_positional_argument
    
    ! Validate that argument processing completed correctly
    subroutine validate_argument_completion(flags, flag_count, expecting_value, &
                                           success, error_message)
        character(len=1024), allocatable, intent(in) :: flags(:)
        integer, intent(in) :: flag_count
        logical, intent(in) :: expecting_value
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        if (expecting_value) then
            success = .false.
            error_message = "Missing value for flag: " // trim(flags(flag_count))
        else
            success = .true.
            error_message = ""
        end if
    end subroutine validate_argument_completion

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
                    error_message = "Missing value for flag: " // trim(flag)
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
        
        ! Unknown flag
        success = .false.
        error_message = "Unknown flag: " // trim(flag)

    end subroutine process_single_flag
    
    ! Process control flags (help, version, validate)
    subroutine process_control_flags(flag, config, success)
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
        case default
            success = .false.
        end select
    end subroutine process_control_flags
    
    ! Process flags that require string values
    subroutine process_value_flags(flag, value, config, success, error_message)
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
        case default
            success = .false.
        end select
    end subroutine process_value_flags
    
    ! Process boolean flags
    subroutine process_boolean_flags(flag, config, success)
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
        case default
            success = .false.
        end select
    end subroutine process_boolean_flags
    
    ! Process complex flags requiring complex parsing
    subroutine process_complex_flags(flag, value, config, success, error_message)
        character(len=*), intent(in) :: flag, value
        type(config_t), intent(inout) :: config
        logical, intent(inout) :: success
        character(len=*), intent(out) :: error_message
        
        select case (flag)
        case ("--minimum", "-m", "--threshold")
            call parse_real_with_error(value, config%minimum_coverage, &
                                       "minimum coverage", success, error_message)
        case ("--fail-under")
            call parse_real_with_error(value, config%fail_under_threshold, &
                                       "fail threshold", success, error_message)
        case ("--threads", "-t")
            call parse_integer_with_error(value, config%threads, &
                                          "thread count", success, error_message)
        case ("--diff")
            config%enable_diff = .true.
            call parse_diff_files(value, config, success, error_message)
        case ("--diff-threshold")
            call parse_real_with_error(value, config%diff_threshold, &
                                       "diff threshold", success, error_message)
        case ("--test-timeout")
            call parse_integer_with_error(value, config%test_timeout_seconds, &
                                          "test timeout", success, error_message)
        case default
            success = .false.
        end select
    end subroutine process_complex_flags

    subroutine process_positional_arguments(positionals, positional_count, &
                                             config, success, error_message)
        !! Process positional arguments (coverage files)
        character(len=*), intent(in) :: positionals(:)
        integer, intent(in) :: positional_count
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        integer :: i
        logical :: is_valid_coverage_file
        logical :: is_source_path

        success = .true.
        error_message = ""

        ! Allocate coverage files array
        if (allocated(config%coverage_files)) deallocate(config%coverage_files)
        allocate(character(len=0) :: config%coverage_files(0))

        do i = 1, positional_count
            call classify_positional_argument(positionals(i), is_valid_coverage_file, &
                                               is_source_path)

            if (is_valid_coverage_file) then
                ! Add to coverage files
                call add_string_to_array(positionals(i), config%coverage_files, &
                                         MAX_ARRAY_SIZE, "coverage files", &
                                         success, error_message)
                if (.not. success) return

            else if (is_source_path) then
                ! Add to source paths
                call add_source_path(positionals(i), config, success, error_message)
                if (.not. success) return

            else
                ! Unknown positional argument
                if (config%strict_mode) then
                    success = .false.
                    error_message = "Unknown positional argument: " // trim(positionals(i))
                    return
                else if (config%verbose) then
                    print '(A)', "Warning: Ignoring unknown argument: " // trim(positionals(i))
                end if
            end if
        end do

    end subroutine process_positional_arguments

    subroutine classify_positional_argument(arg, is_valid_coverage_file, is_source_path)
        !! Classify a positional argument
        character(len=*), intent(in) :: arg
        logical, intent(out) :: is_valid_coverage_file
        logical, intent(out) :: is_source_path

        logical :: file_exists, is_directory
        character(len=:), allocatable :: extension
        integer :: dot_pos

        is_valid_coverage_file = .false.
        is_source_path = .false.

        ! Check if file/directory exists
        inquire(file=trim(arg), exist=file_exists)
        if (.not. file_exists) return

        ! Check if it's a directory
        call check_if_directory(arg, is_directory)
        if (is_directory) then
            is_source_path = .true.
            return
        end if

        ! Check file extension
        dot_pos = index(arg, '.', back=.true.)
        if (dot_pos > 0) then
            extension = arg(dot_pos+1:)

            select case (trim(adjustl(extension)))
            case ("gcov")
                is_valid_coverage_file = .true.
            case ("json")
                ! Could be coverage JSON
                if (index(arg, ".gcov.json") > 0 .or. &
                    index(arg, "coverage") > 0) then
                    is_valid_coverage_file = .true.
                end if
            case ("xml")
                ! Could be Cobertura XML
                if (index(arg, "coverage") > 0 .or. &
                    index(arg, "cobertura") > 0) then
                    is_valid_coverage_file = .true.
                end if
            case ("info")
                ! LCOV info file
                is_valid_coverage_file = .true.
            case ("f90", "f95", "f03", "f08", "f", "for")
                ! Fortran source file
                is_source_path = .true.
            end select
        end if

    contains

        subroutine check_if_directory(path, is_dir)
            !! Check if path is a directory
            character(len=*), intent(in) :: path
            logical, intent(out) :: is_dir

            integer :: unit, iostat

            is_dir = .false.

            ! Try to open as directory (will fail for files)
            open(newunit=unit, file=trim(path)//'/..', status='old', &
                 action='read', iostat=iostat)
            if (iostat == 0) then
                is_dir = .true.
                close(unit)
            end if

        end subroutine check_if_directory

    end subroutine classify_positional_argument

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
                flag_part == "--strict") then
                has_output_args = .true.
                return
            end if
        end do

    end function has_output_related_arguments

end module config_command_parser