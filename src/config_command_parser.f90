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

        character(len=:), allocatable :: flags(:)
        character(len=:), allocatable :: positionals(:)
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

        character(len=:), allocatable :: flags(:)
        character(len=:), allocatable :: positionals(:)
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
        character(len=:), allocatable, intent(out) :: flags(:)
        integer, intent(out) :: flag_count
        character(len=:), allocatable, intent(out) :: positionals(:)
        integer, intent(out) :: positional_count
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        integer :: i, max_len
        character(len=:), allocatable :: arg, flag, value
        logical :: expecting_value
        integer :: equals_pos

        success = .true.
        error_message = ""
        flag_count = 0
        positional_count = 0
        expecting_value = .false.

        ! Find maximum argument length
        max_len = 0
        do i = 1, size(args)
            max_len = max(max_len, len_trim(args(i)))
        end do

        ! Allocate arrays
        allocate(character(len=max_len) :: flags(MAX_ARRAY_SIZE))
        allocate(character(len=max_len) :: positionals(MAX_ARRAY_SIZE))

        ! Process each argument
        do i = 1, size(args)
            arg = trim(adjustl(args(i)))

            if (len_trim(arg) == 0) cycle

            if (expecting_value) then
                ! This is a value for the previous flag
                flag_count = flag_count + 1
                if (flag_count > MAX_ARRAY_SIZE) then
                    success = .false.
                    error_message = "Too many flag arguments"
                    return
                end if
                flags(flag_count) = arg
                expecting_value = .false.

            else if (is_flag_argument(arg)) then
                ! Check for equals sign (--flag=value syntax)
                equals_pos = index(arg, '=')
                if (equals_pos > 0) then
                    ! Split flag and value
                    flag = arg(1:equals_pos-1)
                    value = arg(equals_pos+1:)

                    flag_count = flag_count + 1
                    if (flag_count > MAX_ARRAY_SIZE) then
                        success = .false.
                        error_message = "Too many flag arguments"
                        return
                    end if
                    flags(flag_count) = flag

                    flag_count = flag_count + 1
                    if (flag_count > MAX_ARRAY_SIZE) then
                        success = .false.
                        error_message = "Too many flag arguments"
                        return
                    end if
                    flags(flag_count) = value

                else
                    ! Regular flag
                    flag = get_long_form_option(arg)

                    flag_count = flag_count + 1
                    if (flag_count > MAX_ARRAY_SIZE) then
                        success = .false.
                        error_message = "Too many flag arguments"
                        return
                    end if
                    flags(flag_count) = flag

                    if (flag_requires_value(flag)) then
                        expecting_value = .true.
                    end if
                end if

            else
                ! Positional argument
                positional_count = positional_count + 1
                if (positional_count > MAX_ARRAY_SIZE) then
                    success = .false.
                    error_message = "Too many positional arguments"
                    return
                end if
                positionals(positional_count) = arg
            end if
        end do

        if (expecting_value) then
            success = .false.
            error_message = "Missing value for flag: " // trim(flags(flag_count))
            return
        end if

    end subroutine classify_command_arguments

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

        success = .true.
        error_message = ""

        select case (trim(flag))
        case ("--help", "-h")
            config%show_help = .true.

        case ("--version", "-V")
            config%show_version = .true.

        case ("--verbose", "-v")
            config%verbose = .true.

        case ("--quiet", "-q")
            config%quiet = .true.

        case ("--validate")
            config%validate_config_only = .true.

        case ("--source", "-s")
            call add_source_path(value, config, success, error_message)

        case ("--exclude", "-e")
            call add_exclude_pattern(value, config, success, error_message)

        case ("--include", "-i")
            call add_include_pattern(value, config, success, error_message)

        case ("--output", "-o")
            config%output_path = value

        case ("--format", "-f")
            config%output_format = value

        case ("--minimum", "-m")
            call parse_real_with_error(value, config%minimum_coverage, &
                                       "minimum coverage", success, error_message)

        case ("--fail-under")
            call parse_real_with_error(value, config%fail_under_threshold, &
                                       "fail threshold", success, error_message)

        case ("--threads", "-t")
            call parse_integer_with_error(value, config%threads, &
                                          "thread count", success, error_message)

        case ("--config", "-c")
            config%config_file = value

        case ("--diff")
            config%enable_diff = .true.
            call parse_diff_files(value, config, success, error_message)

        case ("--diff-threshold")
            call parse_real_with_error(value, config%diff_threshold, &
                                       "diff threshold", success, error_message)

        case ("--include-unchanged")
            config%include_unchanged = .true.

        case ("--import")
            config%import_file = value

        case ("--gcov-executable")
            config%gcov_executable = value

        case ("--gcov-args")
            config%gcov_args = value

        case ("--keep-gcov-files")
            config%keep_gcov_files = .true.

        case ("--tui")
            config%tui_mode = .true.

        case ("--strict")
            config%strict_mode = .true.

        case default
            success = .false.
            error_message = "Unknown flag: " // trim(flag)
        end select

    end subroutine process_single_flag

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

        integer :: i
        character(len=:), allocatable :: arg

        has_input_args = .false.

        do i = 1, size(args)
            arg = trim(adjustl(args(i)))

            ! Check for input-related flags
            if (arg == "--source" .or. arg == "-s" .or. &
                arg == "--import" .or. &
                arg == "--gcov-executable" .or. &
                arg == "--gcov-args" .or. &
                arg == "--include" .or. arg == "-i" .or. &
                arg == "--exclude" .or. arg == "-e") then
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

        integer :: i
        character(len=:), allocatable :: arg

        has_output_args = .false.

        do i = 1, size(args)
            arg = trim(adjustl(args(i)))

            ! Check for output-related flags
            if (arg == "--output" .or. arg == "-o" .or. &
                arg == "--format" .or. arg == "-f" .or. &
                arg == "--tui" .or. &
                arg == "--diff" .or. &
                arg == "--diff-threshold" .or. &
                arg == "--include-unchanged") then
                has_output_args = .true.
                return
            end if
        end do

    end function has_output_related_arguments

end module config_command_parser