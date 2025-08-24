module fortcov_config
    !! Unified Configuration Management Module
    !! 
    !! This module consolidates configuration parsing, validation, and management
    !! functionality that was artificially decomposed across 6 separate modules.
    !! 
    !! Consolidation eliminates delegation overhead, reduces complexity, and makes
    !! debugging easier by keeping related functionality together. This addresses
    !! Issue #236 - hidden complexity debt from false compliance with size metrics.
    !! 
    !! Original fragmented modules (2,537 total lines):
    !! - config_parser.f90 (996 lines)
    !! - config_validator.f90 (513 lines) 
    !! - config_flag_processor.f90 (338 lines)
    !! - config_storage.f90 (340 lines)
    !! - config_arg_classifier.f90 (237 lines)
    !! - config_help.f90 (51 lines)
    !! - fortcov_config.f90 (113 lines delegation wrapper)
    !! 
    !! This unified module prioritizes CORRECTNESS > PERFORMANCE > KISS > SRP
    !! by maintaining logical cohesion around configuration management.

    use foundation_constants
    use foundation_layer_utils
    use string_utils
    use file_utils
    use error_handling
    use input_validation
    use zero_configuration_manager
    use secure_command_executor, only: validate_executable_path

    implicit none
    private

    ! Public interface (preserved for backward compatibility)
    public :: config_t
    public :: parse_config
    public :: show_help
    public :: show_version
    public :: initialize_config
    public :: validate_config
    public :: load_config_file
    public :: validate_config_with_context

    ! Additional parsing interface
    public :: parse_command_line_config
    public :: parse_config_file

    ! Re-export constants
    integer, parameter, public :: MAX_ARRAY_SIZE = 100

    ! Configuration type definition
    type, public :: config_t
        character(len=:), allocatable :: input_format
        character(len=:), allocatable :: output_format
        character(len=:), allocatable :: output_path
        character(len=:), allocatable :: source_paths(:)
        character(len=:), allocatable :: exclude_patterns(:)
        character(len=:), allocatable :: include_patterns(:)
        character(len=:), allocatable :: coverage_files(:)
        character(len=:), allocatable :: gcov_executable
        real :: minimum_coverage
        real :: fail_under_threshold
        integer :: threads
        logical :: verbose
        logical :: quiet
        logical :: show_help
        logical :: show_version
        logical :: validate_config_only
        character(len=:), allocatable :: config_file
        logical :: enable_diff
        character(len=:), allocatable :: diff_baseline_file
        character(len=:), allocatable :: diff_current_file
        logical :: include_unchanged
        real :: diff_threshold
        character(len=:), allocatable :: import_file
        logical :: keep_gcov_files
        character(len=:), allocatable :: gcov_args
        logical :: tui_mode
        logical :: strict_mode
        logical :: zero_configuration_mode
        integer :: max_files  ! Maximum number of files to process
    end type config_t

contains

    ! =============================================================================
    ! PUBLIC COMPATIBILITY INTERFACE
    ! =============================================================================

    subroutine parse_config(args, config, success, error_message)
        !! Main configuration parsing entry point (backward compatibility)
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(out) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        call parse_command_line_config(args, config, success, error_message)

    end subroutine parse_config

    subroutine show_help()
        !! Show help information
        call show_help_information()
    end subroutine show_help

    subroutine show_version()
        !! Show version information
        call show_version_information()
    end subroutine show_version

    subroutine initialize_config(config)
        !! Initialize configuration with defaults
        type(config_t), intent(out) :: config

        call initialize_default_config(config)

    end subroutine initialize_config

    function validate_config(config) result(is_valid)
        !! Validate configuration
        type(config_t), intent(in) :: config
        logical :: is_valid

        is_valid = validate_complete_config(config)

    end function validate_config

    subroutine validate_config_with_context(config, error_ctx)
        !! Validate configuration with error context
        type(config_t), intent(in) :: config
        type(error_context_t), intent(out) :: error_ctx

        logical :: is_valid

        is_valid = validate_complete_config(config)

        if (is_valid) then
            error_ctx%error_code = ERROR_SUCCESS
            error_ctx%message = ""
        else
            error_ctx%error_code = ERROR_INVALID_CONFIG
            error_ctx%message = "Configuration validation failed"
            error_ctx%suggestion = "Check configuration parameters"
        end if

    end subroutine validate_config_with_context

    subroutine load_config_file(config, success, error_message)
        !! Load configuration from file
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        call parse_config_file(config, success, error_message)

    end subroutine load_config_file

    ! =============================================================================
    ! COMMAND-LINE PARSING
    ! =============================================================================

    subroutine parse_command_line_config(args, config, success, error_message)
        !! Main command-line configuration parsing function
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(out) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        character(len=:), allocatable :: flags(:)
        character(len=:), allocatable :: positionals(:)
        integer :: flag_count, positional_count
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
        !! Process help, version, and quiet flags
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(inout) :: config
        integer :: i

        ! Check for help/version/quiet flags - these override zero-configuration mode
        do i = 1, size(args)
            if (len_trim(args(i)) > 0) then
                if (trim(args(i)) == '--help' .or. trim(args(i)) == '-h') then
                    config%show_help = .true.
                else if (trim(args(i)) == '--version' .or. trim(args(i)) == '-V') then
                    config%show_version = .true.
                else if (trim(args(i)) == '--quiet' .or. trim(args(i)) == '-q') then
                    config%quiet = .true.
                end if
            end if
        end do
    end subroutine process_special_flags

    logical function should_use_zero_config(args) result(is_zero_config)
        !! Determine if zero-configuration mode should be used
        character(len=*), intent(in) :: args(:)
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
        if (.not. is_zero_config .and. size(args) > 0) then
            has_input_sources = has_input_related_arguments(args)
            has_output_flags = has_output_related_arguments(args)

            ! Zero-config with overrides: output flags but no input sources
            if (has_output_flags .and. .not. has_input_sources) then
                is_zero_config = .true.
            end if
        end if

    end function should_use_zero_config

    subroutine handle_zero_configuration_with_overrides(args, config, success, error_message)
        !! Handle zero-configuration mode with CLI flag overrides
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        character(len=:), allocatable :: flags(:)
        character(len=:), allocatable :: positionals(:)
        integer :: flag_count, positional_count

        ! Mark as zero-configuration mode
        config%zero_configuration_mode = .true.

        ! Process CLI flags first (same as normal configuration)
        call classify_command_arguments(args, flags, flag_count, positionals, &
                                      positional_count)

        ! Process flag arguments to preserve CLI overrides
        call process_flag_arguments(flags, flag_count, config, success, error_message)
        if (.not. success) return

        ! Process positional arguments as coverage files if provided
        call process_positional_arguments(positionals, positional_count, &
                                        config, success, error_message)
        if (.not. success) return

        ! Now apply zero-configuration defaults for unset values
        call handle_zero_configuration_mode(config)

    end subroutine handle_zero_configuration_with_overrides

    subroutine handle_zero_configuration_mode(config)
        !! Handle zero-configuration mode setup
        type(config_t), intent(inout) :: config
        character(len=:), allocatable :: temp_files(:), temp_paths(:)
        character(len=:), allocatable :: default_output_path, default_output_format
        character(len=:), allocatable :: default_input_format, default_exclude_patterns(:)

        ! Mark as zero-configuration mode
        config%zero_configuration_mode = .true.

        ! Get zero-configuration defaults
        call apply_zero_configuration_defaults(default_output_path, &
                                              default_output_format, &
                                              default_input_format, &
                                              default_exclude_patterns)

        ! Apply defaults only for unset values (preserves CLI flag overrides)
        if (.not. allocated(config%output_path)) then
            config%output_path = default_output_path
        end if

        if (.not. allocated(config%exclude_patterns)) then
            config%exclude_patterns = default_exclude_patterns
        end if

        ! Auto-discover coverage files only if not provided via CLI
        if (.not. allocated(config%coverage_files)) then
            temp_files = auto_discover_coverage_files_priority()
            if (allocated(temp_files) .and. size(temp_files) > 0) then
                config%coverage_files = temp_files
            end if
        end if

        ! Auto-discover source paths only if not provided via CLI
        if (.not. allocated(config%source_paths)) then
            temp_paths = auto_discover_source_files_priority()
            if (allocated(temp_paths) .and. size(temp_paths) > 0) then
                config%source_paths = temp_paths
            end if
        end if

        ! Ensure output directory structure exists
        call ensure_zero_config_output_directory(config)
    end subroutine handle_zero_configuration_mode

    subroutine handle_normal_configuration(args, config, success, error_message)
        !! Handle normal command-line configuration
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        character(len=:), allocatable :: flags(:)
        character(len=:), allocatable :: positionals(:)
        integer :: flag_count, positional_count

        ! Two-pass parsing: classify arguments first
        call classify_command_arguments(args, flags, flag_count, positionals, &
                                      positional_count)

        ! Pass 1: Process flag arguments
        call process_flag_arguments(flags, flag_count, config, success, error_message)
        if (.not. success) return

        ! Pass 2: Process positional arguments as coverage files
        call process_positional_arguments(positionals, positional_count, &
                                        config, success, error_message)
        if (.not. success) return

        ! Pass 3: Apply HTML default filename logic
        call apply_html_default_filename(config)

        ! Pass 4: Apply default output path if coverage files provided but no output specified
        call apply_default_output_path_for_coverage_files(config)
    end subroutine handle_normal_configuration

    ! =============================================================================
    ! ARGUMENT CLASSIFICATION
    ! =============================================================================

    subroutine classify_command_arguments(args, flags, flag_count, positionals, &
                                        positional_count)
        !! Classifies command line arguments into flags and positionals
        character(len=*), intent(in) :: args(:)
        character(len=:), allocatable, intent(out) :: flags(:)
        integer, intent(out) :: flag_count
        character(len=:), allocatable, intent(out) :: positionals(:)
        integer, intent(out) :: positional_count

        integer :: i, n
        logical :: is_flag, is_value_for_prev_flag

        n = size(args)
        flag_count = 0
        positional_count = 0
        is_value_for_prev_flag = .false.

        ! Count flags and positionals first
        do i = 1, n
            if (is_value_for_prev_flag) then
                is_value_for_prev_flag = .false.
                cycle
            end if

            is_flag = is_flag_argument(args(i))
            if (is_flag) then
                flag_count = flag_count + 1
                ! For flags with '=', don't expect next arg as value
                if (index(args(i), '=') == 0 .and. flag_requires_value(args(i))) then
                    is_value_for_prev_flag = .true.
                end if
            else
                positional_count = positional_count + 1
            end if
        end do

        ! Allocate arrays
        if (flag_count > 0) then
            allocate(character(len=MEDIUM_STRING_LEN) :: flags(flag_count))
        end if
        if (positional_count > 0) then
            allocate(character(len=MEDIUM_STRING_LEN) :: positionals(positional_count))
        end if

        ! Fill arrays
        flag_count = 0
        positional_count = 0
        is_value_for_prev_flag = .false.

        do i = 1, n
            if (is_value_for_prev_flag) then
                is_value_for_prev_flag = .false.
                cycle
            end if

            is_flag = is_flag_argument(args(i))
            if (is_flag) then
                flag_count = flag_count + 1
                flags(flag_count) = args(i)
                ! For flags with '=', don't append next arg as value
                if (index(args(i), '=') == 0 .and. flag_requires_value(args(i)) .and. i < n) then
                    flags(flag_count) = trim(flags(flag_count)) // "=" // trim(args(i+1))
                    is_value_for_prev_flag = .true.
                end if
            else
                positional_count = positional_count + 1
                positionals(positional_count) = args(i)
            end if
        end do

    end subroutine classify_command_arguments

    function is_flag_argument(arg) result(is_flag)
        !! Checks if argument is a flag (starts with - or --)
        character(len=*), intent(in) :: arg
        logical :: is_flag

        is_flag = (len_trim(arg) > 1 .and. arg(1:1) == '-')

    end function is_flag_argument

    function has_input_related_arguments(args) result(has_input_args)
        !! Checks if any input-related arguments are provided
        character(len=*), intent(in) :: args(:)
        logical :: has_input_args

        integer :: i, equal_pos
        character(len=256) :: arg, flag_part, long_form
        logical :: is_value_for_prev_flag

        has_input_args = .false.
        is_value_for_prev_flag = .false.

        do i = 1, size(args)
            arg = trim(args(i))

            ! Skip empty arguments
            if (len_trim(arg) == 0) cycle

            ! Skip if this argument is a value for the previous flag
            if (is_value_for_prev_flag) then
                is_value_for_prev_flag = .false.
                cycle
            end if

            ! Check if it's a flag
            if (is_flag_argument(arg)) then
                ! Extract flag part (before '=' if present)
                equal_pos = index(arg, '=')
                if (equal_pos > 0) then
                    flag_part = arg(1:equal_pos-1)
                else
                    flag_part = arg
                end if

                ! Convert to long form for consistent checking
                long_form = get_long_form_option(flag_part)

                ! Check for input-related flags
                select case (trim(long_form))
                case ('--source', '--import', '--gcov-executable', '--gcov-args')
                    has_input_args = .true.
                    return
                end select

                ! If flag requires value and doesn't have '=', next arg is value
                if (equal_pos == 0 .and. flag_requires_value(flag_part)) then
                    is_value_for_prev_flag = .true.
                end if
            else
                ! Positional arguments are considered input-related (coverage files)
                has_input_args = .true.
                return
            end if
        end do

    end function has_input_related_arguments

    function has_output_related_arguments(args) result(has_output_args)
        !! Checks if any output-related arguments are provided
        character(len=*), intent(in) :: args(:)
        logical :: has_output_args

        integer :: i, equal_pos
        character(len=256) :: arg, flag_part, long_form
        logical :: is_value_for_prev_flag

        has_output_args = .false.
        is_value_for_prev_flag = .false.

        do i = 1, size(args)
            arg = trim(args(i))

            ! Skip empty arguments
            if (len_trim(arg) == 0) cycle

            ! Skip if this argument is a value for the previous flag
            if (is_value_for_prev_flag) then
                is_value_for_prev_flag = .false.
                cycle
            end if

            ! Check if it's a flag
            if (is_flag_argument(arg)) then
                ! Extract flag part (before '=' if present)
                equal_pos = index(arg, '=')
                if (equal_pos > 0) then
                    flag_part = arg(1:equal_pos-1)
                else
                    flag_part = arg
                end if

                ! Convert to long form for consistent checking
                long_form = get_long_form_option(flag_part)

                ! Check for output-related flags
                select case (trim(long_form))
                case ('--output', '--format', '--output-format', '--threshold', '--verbose', '--quiet', &
                      '--fail-under', '--threads', '--tui', '--strict', '--keep-gcov', '--config', &
                      '--diff', '--diff-baseline', '--diff-current', '--include-unchanged', &
                      '--include', '--exclude', '--max-files', '--validate-config')
                    has_output_args = .true.
                    return
                end select

                ! If flag requires value and doesn't have '=', next arg is value
                if (equal_pos == 0 .and. flag_requires_value(flag_part)) then
                    is_value_for_prev_flag = .true.
                end if
            end if
        end do

    end function has_output_related_arguments

    ! =============================================================================
    ! FLAG PROCESSING
    ! =============================================================================

    subroutine process_flag_arguments(flags, flag_count, config, success, error_message)
        !! Processes flag arguments and updates configuration
        character(len=*), intent(in) :: flags(:)
        integer, intent(in) :: flag_count
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        integer :: i, equal_pos
        character(len=256) :: flag, value

        success = .true.
        error_message = ""

        do i = 1, flag_count
            equal_pos = index(flags(i), '=')
            if (equal_pos > 0) then
                flag = flags(i)(1:equal_pos-1)
                value = flags(i)(equal_pos+1:)
            else
                flag = flags(i)
                value = ""
            end if

            call process_single_flag(flag, value, config, success, error_message)
            if (.not. success) return
        end do

    end subroutine process_flag_arguments

    subroutine process_single_flag(flag, value, config, success, error_message)
        !! Processes a single flag and its value
        character(len=*), intent(in) :: flag, value
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        character(len=:), allocatable :: long_flag

        success = .true.
        error_message = ""

        long_flag = get_long_form_option(flag)

        select case (trim(long_flag))
        case ('--verbose')
            config%verbose = .true.
        case ('--quiet')
            config%quiet = .true.
        case ('--help')
            config%show_help = .true.
        case ('--version')
            config%show_version = .true.
        case ('--output')
            config%output_path = value
        case ('--format', '--output-format')
            config%output_format = value
        case ('--threshold')
            call parse_real_with_error(value, config%minimum_coverage, &
                                     "threshold", success, error_message)
        case ('--tui')
            config%tui_mode = .true.
        case ('--strict')
            config%strict_mode = .true.
        case ('--keep-gcov')
            config%keep_gcov_files = .true.
        case ('--source')
            call add_source_path(value, config, success, error_message)
        case ('--exclude')
            call add_exclude_pattern(value, config, success, error_message)
        case ('--config')
            config%config_file = value
        case ('--gcov-executable')
            config%gcov_executable = value
        case ('--gcov-args')
            config%gcov_args = value
        case ('--diff')
            config%enable_diff = .true.
            ! Handle --diff=baseline.json,current.json format
            if (len_trim(value) > 0) then
                call parse_diff_files(value, config, success, error_message)
                if (.not. success) return
            end if
        case ('--diff-baseline')
            config%diff_baseline_file = value
        case ('--diff-current')
            config%diff_current_file = value
        case ('--include-unchanged')
            config%include_unchanged = .true.
        case ('--import')
            config%import_file = value
        case ('--include')
            call add_include_pattern(value, config, success, error_message)
        case ('--fail-under')
            call parse_real_with_error(value, config%fail_under_threshold, &
                                     "fail-under threshold", success, error_message)
        case ('--threads')
            call parse_integer_with_error(value, config%threads, &
                                        "threads", success, error_message)
        case ('--max-files')
            call parse_integer_with_error(value, config%max_files, &
                                        "max-files", success, error_message)
        case ('--validate-config')
            config%validate_config_only = .true.
        case default
            success = .false.
            error_message = "Unknown flag: " // trim(flag)
        end select

    end subroutine process_single_flag

    function get_long_form_option(short_arg) result(long_form)
        !! Converts short option to long form
        character(len=*), intent(in) :: short_arg
        character(len=:), allocatable :: long_form

        select case (trim(short_arg))
        case ('-v')
            long_form = '--verbose'
        case ('-q')
            long_form = '--quiet'
        case ('-h')
            long_form = '--help'
        case ('-V')
            long_form = '--version'
        case ('-o')
            long_form = '--output'
        case ('-f')
            long_form = '--format'
        case ('-t')
            long_form = '--threshold'
        case ('-s')
            long_form = '--source'
        case ('-e')
            long_form = '--exclude'
        case default
            long_form = short_arg
        end select

    end function get_long_form_option

    function flag_requires_value(flag) result(requires_value)
        !! Checks if flag requires a value
        character(len=*), intent(in) :: flag
        logical :: requires_value

        character(len=:), allocatable :: long_flag

        long_flag = get_long_form_option(flag)

        select case (trim(long_flag))
        case ('--output', '--format', '--output-format', '--threshold', '--source', '--exclude', &
              '--include', '--fail-under', '--threads', '--config', '--diff-baseline', &
              '--diff-current', '--import', '--gcov-executable', '--gcov-args', '--max-files')
            requires_value = .true.
        case ('--diff')
            ! --diff can optionally take a value (--diff=baseline.json,current.json)
            requires_value = .false.
        case default
            requires_value = .false.
        end select

    end function flag_requires_value

    subroutine classify_positional_argument(arg, is_valid_coverage_file, &
                                          is_executable, is_directory)
        !! Classify positional argument to determine if it's a valid coverage file,
        !! executable path, or directory that should be filtered out (Issue #227 fix)
        character(len=*), intent(in) :: arg
        logical, intent(out) :: is_valid_coverage_file
        logical, intent(out) :: is_executable
        logical, intent(out) :: is_directory
        
        logical :: file_exists, is_dir
        integer :: len_arg, ext_pos
        character(len=10) :: extension
        
        ! Initialize outputs
        is_valid_coverage_file = .false.
        is_executable = .false.
        is_directory = .false.
        
        len_arg = len_trim(arg)
        
        ! Handle empty arguments gracefully
        if (len_arg == 0) then
            ! Empty string is neither directory, executable, nor valid coverage file
            return
        end if
        
        ! Check if file exists
        inquire(file=arg, exist=file_exists)
        
        ! Directory detection heuristics
        if (arg(len_arg:len_arg) == '/') then
            is_directory = .true.
            return
        end if
        
        ! Known directory names
        if (trim(arg) == "./src" .or. trim(arg) == "./build" .or. &
            trim(arg) == "src" .or. trim(arg) == "build" .or. &
            trim(arg) == "test" .or. trim(arg) == "./test") then
            is_directory = .true.
            return
        end if
        
        ! Check for executable patterns (common build paths) - regardless of file existence
        if (index(arg, '/app/') > 0 .or. &
            index(arg, '/test/') > 0 .or. &
            index(arg, '/bin/') > 0 .or. &
            index(arg, 'build/gfortran_') > 0 .or. &
            index(arg, 'fortcov') > 0) then
            is_executable = .true.
            return
        end if
        
        ! Check for executable by looking for missing extension or common extensions
        ext_pos = index(arg, '.', back=.true.)
        if (ext_pos == 0) then
            ! No extension - likely executable if file exists
            if (file_exists) then
                is_executable = .true.
                return
            end if
        else
            extension = arg(ext_pos+1:len_arg)
            if (trim(extension) == "exe" .or. trim(extension) == "out") then
                is_executable = .true.
                return
            end if
        end if
        
        ! Check for valid coverage file extensions
        if (ext_pos > 0) then
            extension = arg(ext_pos+1:len_arg)
            if (trim(extension) == "gcov" .or. &
                trim(extension) == "json" .or. &
                trim(extension) == "xml") then
                is_valid_coverage_file = .true.
                return
            end if
        end if
        
    end subroutine classify_positional_argument

    subroutine process_positional_arguments(positionals, positional_count, &
                                          config, success, error_message)
        !! Processes positional arguments as coverage files with intelligent classification
        character(len=*), intent(in) :: positionals(:)
        integer, intent(in) :: positional_count
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        integer :: i, valid_file_count
        character(len=MEDIUM_STRING_LEN) :: valid_files(positional_count)
        logical :: is_valid_coverage_file, is_executable, is_directory

        success = .true.
        error_message = ""
        valid_file_count = 0

        if (positional_count > 0) then
            do i = 1, positional_count
                call classify_positional_argument(positionals(i), &
                                                is_valid_coverage_file, &
                                                is_executable, &
                                                is_directory)
                
                if (is_executable .or. is_directory) then
                    ! Skip executable paths and directories
                    cycle
                else if (is_valid_coverage_file) then
                    valid_file_count = valid_file_count + 1
                    valid_files(valid_file_count) = positionals(i)
                else
                    ! Invalid file - report error
                    success = .false.
                    write(error_message, '(A)') "Invalid coverage file format: " // trim(positionals(i))
                    return
                end if
            end do
            
            ! Only allocate for valid coverage files
            if (valid_file_count > 0) then
                allocate(character(len=MEDIUM_STRING_LEN) :: config%coverage_files(valid_file_count))
                config%coverage_files(1:valid_file_count) = valid_files(1:valid_file_count)
            end if
        end if

    end subroutine process_positional_arguments

    ! =============================================================================
    ! CONFIGURATION FILE PARSING
    ! =============================================================================

    subroutine parse_config_file(config, success, error_message)
        !! Configuration file parsing implementation
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        character(len=1024) :: line
        character(len=256) :: key, value
        integer :: iostat, equal_pos
        integer :: unit
        logical :: file_exists

        success = .true.
        error_message = ""

        ! Check if config file is specified and exists
        if (.not. allocated(config%config_file)) then
            return  ! No config file specified, not an error
        end if

        inquire(file=config%config_file, exist=file_exists)
        if (.not. file_exists) then
            success = .false.
            error_message = "Config file not found: " // config%config_file
            return
        end if

        ! Open and read config file
        open(newunit=unit, file=config%config_file, status='old', &
             action='read', iostat=iostat)
        if (iostat /= 0) then
            success = .false.
            error_message = "Failed to open config file: " // config%config_file
            return
        end if

        ! Parse file line by line
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit

            line = adjustl(line)
            if (len_trim(line) == 0 .or. line(1:1) == '#') cycle

            equal_pos = index(line, '=')
            if (equal_pos == 0) cycle

            key = adjustl(line(1:equal_pos-1))
            value = adjustl(line(equal_pos+1:))

            call process_config_file_option(key, value, config, success, error_message)
            if (.not. success) then
                close(unit)
                return
            end if
        end do

        close(unit)

    end subroutine parse_config_file

    subroutine process_config_file_option(key, value, config, success, error_message)
        !! Processes a single option from config file
        character(len=*), intent(in) :: key, value
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        ! Map config file keys to config fields
        select case (trim(key))
        case ('output_format')
            config%output_format = trim(value)
        case ('output_path')
            config%output_path = trim(value)
        case ('threshold')
            call parse_real_value(value, config%minimum_coverage, success)
            if (.not. success) then
                error_message = "Invalid threshold in config file: " // trim(value)
                return
            end if
        case ('verbose')
            config%verbose = (trim(value) == 'true')
        case ('quiet')
            config%quiet = (trim(value) == 'true')
        case default
            ! Ignore unknown options in config file
        end select

        success = .true.
        error_message = ""

    end subroutine process_config_file_option

    ! =============================================================================
    ! INITIALIZATION
    ! =============================================================================

    subroutine initialize_default_config(config)
        !! Initializes configuration with default values
        type(config_t), intent(out) :: config

        config%input_format = "gcov"
        config%output_format = "markdown"
        config%minimum_coverage = 0.0
        config%fail_under_threshold = 0.0
        config%threads = 1
        config%verbose = .false.
        config%quiet = .false.
        config%show_help = .false.
        config%show_version = .false.
        config%validate_config_only = .false.
        config%enable_diff = .false.
        config%include_unchanged = .false.
        config%diff_threshold = 0.0
        config%keep_gcov_files = .false.
        config%tui_mode = .false.
        config%strict_mode = .false.
        config%zero_configuration_mode = .false.
        config%max_files = 10000  ! Default maximum files

        ! Check for FORTCOV_MAX_FILES environment variable
        call get_max_files_from_env(config%max_files)

    end subroutine initialize_default_config

    ! =============================================================================
    ! VALIDATION
    ! =============================================================================

    function validate_complete_config(config) result(is_valid)
        !! Comprehensive configuration validation
        type(config_t), intent(in) :: config
        logical :: is_valid

        character(len=LONG_STRING_LEN) :: error_message
        logical :: sources_valid, files_valid, output_valid
        logical :: threshold_valid, diff_valid, import_valid, gcov_valid

        is_valid = .true.

        ! Validate input sources
        call validate_input_sources(config, sources_valid, error_message)
        if (.not. sources_valid) then
            if (.not. config%quiet) then
                print *, "Input sources validation failed: " // trim(error_message)
            end if
            is_valid = .false.
        end if

        ! Validate coverage files if specified
        if (allocated(config%coverage_files)) then
            call validate_coverage_files(config%coverage_files, files_valid, error_message)
            if (.not. files_valid) then
                if (.not. config%quiet) then
                    print *, "Coverage files validation failed: " // trim(error_message)
                end if
                is_valid = .false.
            end if
        end if

        ! Validate output settings
        call validate_output_settings(config, output_valid, error_message)
        if (.not. output_valid) then
            if (.not. config%quiet) then
                print *, "Output settings validation failed: " // trim(error_message)
            end if
            is_valid = .false.
        end if

        ! Validate threshold settings
        call validate_threshold_settings(config, threshold_valid, error_message)
        if (.not. threshold_valid) then
            if (.not. config%quiet) then
                print *, "Threshold settings validation failed: " // trim(error_message)
            end if
            is_valid = .false.
        end if

        ! Validate diff configuration if enabled
        if (config%enable_diff) then
            call validate_diff_configuration(config, diff_valid, error_message)
            if (.not. diff_valid) then
                if (.not. config%quiet) then
                    print *, "Diff configuration validation failed: " // trim(error_message)
                end if
                is_valid = .false.
            end if
        end if

        ! Validate import configuration if specified
        if (allocated(config%import_file)) then
            call validate_import_configuration(config, import_valid, error_message)
            if (.not. import_valid) then
                if (.not. config%quiet) then
                    print *, "Import configuration validation failed: " // trim(error_message)
                end if
                is_valid = .false.
            end if
        end if

        ! Validate gcov executable if specified
        if (allocated(config%gcov_executable)) then
            call validate_gcov_executable(config%gcov_executable, gcov_valid, error_message)
            if (.not. gcov_valid) then
                if (.not. config%quiet) then
                    print *, "GCov executable validation failed: " // trim(error_message)
                end if
                is_valid = .false.
            end if
        end if

    end function validate_complete_config

    subroutine validate_input_sources(config, is_valid, error_message)
        !! Validates that input sources are provided
        type(config_t), intent(in) :: config
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        logical :: has_source_paths, has_coverage_files, has_import_file

        is_valid = .true.
        error_message = ""

        ! Check for various input source types
        has_source_paths = allocated(config%source_paths)
        has_coverage_files = allocated(config%coverage_files)
        has_import_file = allocated(config%import_file)

        ! Skip validation for zero-configuration mode
        if (config%zero_configuration_mode) then
            is_valid = .true.
            return
        end if

        ! Skip validation for diff mode - diff mode works with JSON files
        ! and does not require source paths for .gcov generation
        if (config%enable_diff) then
            if (allocated(config%diff_baseline_file) .or. &
                allocated(config%diff_current_file)) then
                is_valid = .true.
                return
            end if
        end if

        ! Validate that at least one input source is provided
        if (.not. has_source_paths .and. .not. has_coverage_files .and. &
            .not. has_import_file) then
            is_valid = .false.
            error_message = "No input sources specified. Provide source paths, " // &
                          "coverage files, or import file."
            return
        end if

        ! Validate source paths if provided
        if (has_source_paths) then
            call validate_source_paths(config%source_paths, is_valid, error_message)
            if (.not. is_valid) return
        end if

        ! Validate coverage files if provided
        if (has_coverage_files) then
            call validate_coverage_files(config%coverage_files, is_valid, error_message)
            if (.not. is_valid) return
        end if

        ! Validate import file if provided
        if (has_import_file) then
            call validate_import_file(config%import_file, is_valid, error_message)
            if (.not. is_valid) return
        end if

    end subroutine validate_input_sources

    subroutine validate_coverage_files(files, is_valid, error_message)
        !! Validates coverage files existence and accessibility
        character(len=*), intent(in) :: files(:)
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        integer :: i
        logical :: file_exists
        character(len=MEDIUM_STRING_LEN) :: file_path

        is_valid = .true.
        error_message = ""

        do i = 1, size(files)
            file_path = trim(files(i))

            ! Check file existence
            inquire(file=file_path, exist=file_exists)
            if (.not. file_exists) then
                is_valid = .false.
                error_message = "Coverage file not found: " // trim(file_path)
                return
            end if

            ! Validate file format
            if (.not. is_valid_coverage_file_format(file_path)) then
                is_valid = .false.
                error_message = "Invalid coverage file format: " // trim(file_path)
                return
            end if

            ! Check file readability
            if (.not. is_file_readable(file_path)) then
                is_valid = .false.
                error_message = "Coverage file not readable: " // trim(file_path)
                return
            end if
        end do

    end subroutine validate_coverage_files

    subroutine validate_output_settings(config, is_valid, error_message)
        !! Validates output format and path settings
        type(config_t), intent(in) :: config
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        is_valid = .true.
        error_message = ""

        ! Validate output format
        if (.not. is_supported_output_format(config%output_format)) then
            is_valid = .false.
            error_message = "Unsupported output format: " // trim(config%output_format) // &
                          ". Supported: markdown, json, xml, html"
            return
        end if

        ! Validate output path if specified
        if (allocated(config%output_path)) then
            call validate_output_path(config%output_path, is_valid, error_message)
            if (.not. is_valid) return
        end if

        ! Check for conflicting quiet/verbose flags
        if (config%quiet .and. config%verbose) then
            is_valid = .false.
            error_message = "Cannot specify both --quiet and --verbose flags"
            return
        end if

    end subroutine validate_output_settings

    subroutine validate_threshold_settings(config, is_valid, error_message)
        !! Validates coverage threshold settings
        type(config_t), intent(in) :: config
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        is_valid = .true.
        error_message = ""

        ! Validate threshold range
        if (config%minimum_coverage < MIN_VALID_COVERAGE .or. &
            config%minimum_coverage > MAX_VALID_COVERAGE) then
            is_valid = .false.
            write(error_message, '(A, F6.2, A)') &
                "Threshold must be between 0-100, got: ", config%minimum_coverage, "%"
            return
        end if

        ! Validate diff threshold if specified
        if (config%enable_diff) then
            if (config%diff_threshold < MIN_VALID_COVERAGE .or. &
                config%diff_threshold > MAX_VALID_COVERAGE) then
                is_valid = .false.
                write(error_message, '(A, F6.2, A)') &
                    "Diff threshold must be between 0-100, got: ", config%diff_threshold, "%"
                return
            end if
        end if

    end subroutine validate_threshold_settings

    subroutine validate_diff_configuration(config, is_valid, error_message)
        !! Validates diff analysis configuration
        type(config_t), intent(in) :: config
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        logical :: baseline_exists, current_exists

        is_valid = .true.
        error_message = ""

        ! Validate baseline file
        if (.not. allocated(config%diff_baseline_file)) then
            is_valid = .false.
            error_message = "Diff mode requires baseline file (--diff-baseline)"
            return
        end if

        inquire(file=config%diff_baseline_file, exist=baseline_exists)
        if (.not. baseline_exists) then
            is_valid = .false.
            error_message = "Baseline file not found: " // trim(config%diff_baseline_file)
            return
        end if

        ! Validate current file if specified
        if (allocated(config%diff_current_file)) then
            inquire(file=config%diff_current_file, exist=current_exists)
            if (.not. current_exists) then
                is_valid = .false.
                error_message = "Current file not found: " // trim(config%diff_current_file)
                return
            end if
        end if

    end subroutine validate_diff_configuration

    subroutine validate_import_configuration(config, is_valid, error_message)
        !! Validates import file configuration
        type(config_t), intent(in) :: config
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        logical :: file_exists

        is_valid = .true.
        error_message = ""

        ! Check import file existence
        inquire(file=config%import_file, exist=file_exists)
        if (.not. file_exists) then
            is_valid = .false.
            error_message = "Import file not found: " // trim(config%import_file)
            return
        end if

        ! Validate import file format
        if (.not. is_valid_import_file_format(config%import_file)) then
            is_valid = .false.
            error_message = "Invalid import file format: " // trim(config%import_file)
            return
        end if

    end subroutine validate_import_configuration

    ! =============================================================================
    ! HELPER AND UTILITY FUNCTIONS
    ! =============================================================================

    subroutine show_help_information()
        !! Displays comprehensive help information

        print *, "fortcov - Fortran Coverage Analysis Tool"
        print *, ""
        print *, "USAGE:"
        print *, "    fortcov [OPTIONS] [COVERAGE_FILES...]"
        print *, ""
        print *, "OPTIONS:"
        print *, "    -h, --help              Show this help message"
        print *, "    -V, --version           Show version information"
        print *, "    -v, --verbose           Enable verbose output"
        print *, "    -q, --quiet             Suppress output messages"
        print *, ""
        print *, "OUTPUT OPTIONS:"
        print *, "    -f, --format FORMAT     Output format [markdown|json|xml|html]"
        print *, "    -o, --output PATH       Output file path"
        print *, ""
        print *, "ANALYSIS OPTIONS:"
        print *, "    -t, --threshold PCT     Coverage threshold percentage (0-100)"
        print *, "    --fail-under PCT        Fail if coverage is below threshold"
        print *, "    -s, --source PATHS      Source directory paths (comma-separated)"
        print *, "    -e, --exclude PATTERNS  Exclude patterns (comma-separated)"
        print *, "    --include PATTERNS      Include only matching patterns"
        print *, ""
        print *, "ADVANCED OPTIONS:"
        print *, "    --config FILE           Configuration file path"
        print *, "    --validate-config       Validate configuration without running analysis"
        print *, "    --threads N             Number of processing threads (default: 1)"
        print *, "    --max-files N           Maximum number of files to process (default: 10000)"
        print *, "    --tui                   Launch Terminal User Interface"
        print *, "    --strict                Enable strict mode validation"
        print *, "    --keep-gcov             Keep generated .gcov files"
        print *, ""
        print *, "DIFFERENTIAL ANALYSIS:"
        print *, "    --diff=BASE,CURRENT     Compare two coverage datasets"
        print *, "    --diff                  Enable coverage diff analysis"
        print *, "    --diff-baseline FILE    Baseline coverage file for diff"
        print *, "    --diff-current FILE     Current coverage file for diff"
        print *, "    --include-unchanged     Include unchanged files in diff"
        print *, ""
        print *, "IMPORT/EXPORT:"
        print *, "    --import FILE           Import coverage data from JSON file"
        print *, ""
        print *, "GCOV OPTIONS:"
        print *, "    --gcov-executable PATH  Custom gcov executable path"
        print *, "    --gcov-args ARGS        Additional gcov arguments"
        print *, ""
        print *, "EXAMPLES:"
        print *, "    fortcov *.gcov                    # Analyze all .gcov files"
        print *, "    fortcov --format=json --output=coverage.json"
        print *, "    fortcov --threshold=80 --source=src/"
        print *, "    fortcov --diff=baseline.json,current.json --output=diff.md"
        print *, "    fortcov --diff --diff-baseline=old.json --diff-current=new.json"
        print *, "    fortcov --import=coverage.json --format=html"
        print *, "    fortcov --tui                     # Launch interactive mode"
        print *, ""
        print *, "For more information, visit: https://github.com/fortran-lang/fortcov"

    end subroutine show_help_information

    subroutine show_version_information()
        !! Displays version and build information

        print *, "fortcov version 0.4.0"
        print *, "Fortran Coverage Analysis Tool"
        print *, ""
        print *, "Built with:"
        print *, "  - Fortran compiler support"
        print *, "  - gcov integration"
        print *, "  - JSON/XML/HTML output formats"
        print *, "  - Terminal User Interface (TUI)"
        print *, "  - Coverage diff analysis"
        print *, ""
        print *, "Copyright (c) 2024 Fortran Language Community"
        print *, "Licensed under MIT License"
        print *, ""
        print *, "Report bugs at: https://github.com/fortran-lang/fortcov/issues"

    end subroutine show_version_information

    subroutine parse_real_with_error(str, value, value_name, success, error_message)
        !! Generic real parsing with error message generation
        character(len=*), intent(in) :: str, value_name
        real, intent(out) :: value
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        call parse_real_value(str, value, success)
        if (.not. success) then
            error_message = "Invalid " // trim(value_name) // " value: " // trim(str)
        else
            error_message = ""
        end if

    end subroutine parse_real_with_error

    subroutine parse_integer_with_error(str, value, value_name, success, error_message)
        !! Generic integer parsing with error message generation
        character(len=*), intent(in) :: str, value_name
        integer, intent(out) :: value
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        call parse_integer_value(str, value, success)
        if (.not. success) then
            error_message = "Invalid " // trim(value_name) // " value: " // trim(str)
        else
            error_message = ""
        end if

    end subroutine parse_integer_with_error

    subroutine parse_real_value(str, value, success)
        !! Parses string to real value
        character(len=*), intent(in) :: str
        real, intent(out) :: value
        logical, intent(out) :: success

        integer :: iostat

        read(str, *, iostat=iostat) value
        success = (iostat == 0)

    end subroutine parse_real_value

    subroutine parse_integer_value(str, value, success)
        !! Parses string to integer value
        character(len=*), intent(in) :: str
        integer, intent(out) :: value
        logical, intent(out) :: success

        integer :: iostat

        read(str, *, iostat=iostat) value
        success = (iostat == 0)

        ! Validate that value is positive
        if (success .and. value <= 0) then
            success = .false.
        end if

    end subroutine parse_integer_value

    subroutine apply_html_default_filename(config)
        !! Applies HTML default filename logic
        type(config_t), intent(inout) :: config

        if (trim(config%output_format) == 'html' .and. &
            .not. allocated(config%output_path)) then
            config%output_path = 'coverage.html'
        end if

    end subroutine apply_html_default_filename

    subroutine apply_default_output_path_for_coverage_files(config)
        !! Apply default output path when coverage files are provided but no output specified
        type(config_t), intent(inout) :: config

        ! If coverage files are provided but no output path specified, set default
        if (allocated(config%coverage_files) .and. &
            .not. allocated(config%output_path)) then

            ! Set default based on output format
            select case (trim(config%output_format))
            case ('json')
                config%output_path = 'coverage.json'
            case ('xml')
                config%output_path = 'coverage.xml'
            case ('html')
                config%output_path = 'coverage.html'
            case default  ! markdown, md
                config%output_path = 'coverage.md'
            end select
        end if

    end subroutine apply_default_output_path_for_coverage_files

    subroutine add_string_to_array(item, array, max_size, item_type, success, error_message)
        !! Generic subroutine to add string to allocatable array
        character(len=*), intent(in) :: item
        character(len=:), allocatable, intent(inout) :: array(:)
        integer, intent(in) :: max_size
        character(len=*), intent(in) :: item_type
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        character(len=:), allocatable :: temp_array(:)
        integer :: current_size, new_size

        success = .true.
        error_message = ""

        if (len_trim(item) == 0) then
            success = .false.
            error_message = "Empty " // trim(item_type) // " provided"
            return
        end if

        if (allocated(array)) then
            current_size = size(array)

            ! Check size limits
            if (current_size >= max_size) then
                success = .false.
                write(error_message, '(A, A, A, I0, A)') &
                    "Maximum ", trim(item_type), " count exceeded (", max_size, ")"
                return
            end if

            ! Reallocate with increased size
            new_size = current_size + 1
            allocate(character(len=max(len(array), len_trim(item))) :: temp_array(new_size))
            temp_array(1:current_size) = array
            temp_array(new_size) = trim(item)
            call move_alloc(temp_array, array)
        else
            ! Initial allocation
            allocate(character(len=len_trim(item)) :: array(1))
            array(1) = trim(item)
        end if

    end subroutine add_string_to_array

    subroutine add_source_path(path, config, success, error_message)
        !! Adds a source path to the config
        character(len=*), intent(in) :: path
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        call add_string_to_array(path, config%source_paths, MAX_FILES, &
                               "source path", success, error_message)

    end subroutine add_source_path

    subroutine add_exclude_pattern(pattern, config, success, error_message)
        !! Adds an exclude pattern to the config
        character(len=*), intent(in) :: pattern
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        call add_string_to_array(pattern, config%exclude_patterns, MAX_EXCLUDES, &
                               "exclude pattern", success, error_message)

    end subroutine add_exclude_pattern

    subroutine add_include_pattern(pattern, config, success, error_message)
        !! Adds an include pattern to the config
        character(len=*), intent(in) :: pattern
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        call add_string_to_array(pattern, config%include_patterns, MAX_EXCLUDES, &
                               "include pattern", success, error_message)

    end subroutine add_include_pattern

    subroutine ensure_zero_config_output_directory(config)
        !! Ensures output directory exists for zero-configuration mode
        use zero_configuration_manager, only: ensure_output_directory_structure
        use error_handling, only: error_context_t
        type(config_t), intent(in) :: config

        type(error_context_t) :: error_ctx

        if (allocated(config%output_path)) then
            call ensure_output_directory_structure(config%output_path, error_ctx)
            ! Silently handle directory creation errors in zero-config mode
        end if

    end subroutine ensure_zero_config_output_directory

    subroutine get_max_files_from_env(max_files)
        !! Check for FORTCOV_MAX_FILES environment variable and update max_files
        integer, intent(inout) :: max_files

        character(len=20) :: env_value
        integer :: env_max_files, iostat_var

        ! Try to get environment variable
        call get_environment_variable('FORTCOV_MAX_FILES', env_value)

        if (len_trim(env_value) > 0) then
            ! Parse the environment variable value
            read(env_value, *, iostat=iostat_var) env_max_files
            if (iostat_var == 0 .and. env_max_files > 0) then
                max_files = env_max_files
            end if
        end if
    end subroutine get_max_files_from_env

    ! =============================================================================
    ! VALIDATION HELPER FUNCTIONS
    ! =============================================================================

    subroutine validate_source_paths(paths, is_valid, error_message)
        !! Validates source paths
        character(len=*), intent(in) :: paths(:)
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        integer :: i
        logical :: path_exists
        character(len=MEDIUM_STRING_LEN) :: path

        is_valid = .true.
        error_message = ""

        do i = 1, size(paths)
            path = trim(paths(i))

            inquire(file=path, exist=path_exists)
            if (.not. path_exists) then
                is_valid = .false.
                error_message = "Source path not found: " // trim(path)
                return
            end if
        end do

    end subroutine validate_source_paths

    subroutine validate_output_path(output_path, is_valid, error_message)
        !! Validates output path accessibility
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        character(len=MEDIUM_STRING_LEN) :: directory_path
        integer :: last_slash
        logical :: dir_exists
        logical :: is_zero_config_path

        is_valid = .true.
        error_message = ""

        ! Check if this is the zero-configuration default path
        is_zero_config_path = (trim(output_path) == "build/coverage/coverage.md")

        ! Extract directory path
        last_slash = index(output_path, '/', back=.true.)
        if (last_slash > 0) then
            directory_path = output_path(1:last_slash-1)

            inquire(file=directory_path, exist=dir_exists)
            if (.not. dir_exists) then
                ! For zero-configuration mode, allow missing directory (will be created)
                if (.not. is_zero_config_path) then
                    is_valid = .false.
                    error_message = "Output directory does not exist: " // trim(directory_path)
                    return
                end if
            end if
        end if

    end subroutine validate_output_path

    subroutine validate_import_file(import_file, is_valid, error_message)
        !! Validates import file
        character(len=*), intent(in) :: import_file
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        logical :: file_exists

        inquire(file=import_file, exist=file_exists)
        if (.not. file_exists) then
            is_valid = .false.
            error_message = "Import file not found: " // trim(import_file)
        else
            is_valid = .true.
            error_message = ""
        end if

    end subroutine validate_import_file

    function is_supported_output_format(format) result(is_supported)
        !! Checks if output format is supported
        character(len=*), intent(in) :: format
        logical :: is_supported

        select case (trim(format))
        case ('markdown', 'md', 'json', 'xml', 'html')
            is_supported = .true.
        case default
            is_supported = .false.
        end select

    end function is_supported_output_format

    function is_valid_coverage_file_format(file_path) result(is_valid)
        !! Checks if coverage file has valid format
        character(len=*), intent(in) :: file_path
        logical :: is_valid
        integer :: path_len

        ! Check file extension - must end with valid extension
        path_len = len_trim(file_path)
        is_valid = .false.

        ! Check for .gcov extension
        if (path_len >= 5) then
            if (file_path(path_len-4:path_len) == '.gcov') then
                is_valid = .true.
                return
            end if
        end if

        ! Check for .json extension
        if (path_len >= 5) then
            if (file_path(path_len-4:path_len) == '.json') then
                is_valid = .true.
                return
            end if
        end if

        ! Check for .xml extension
        if (path_len >= 4) then
            if (file_path(path_len-3:path_len) == '.xml') then
                is_valid = .true.
                return
            end if
        end if

    end function is_valid_coverage_file_format

    function is_valid_import_file_format(file_path) result(is_valid)
        !! Checks if import file has valid format
        character(len=*), intent(in) :: file_path
        logical :: is_valid

        ! For now, accept JSON files for import
        is_valid = (index(file_path, '.json') > 0)

    end function is_valid_import_file_format

    function is_file_readable(file_path) result(is_readable)
        !! Checks if file is readable
        character(len=*), intent(in) :: file_path
        logical :: is_readable

        integer :: unit, iostat

        open(newunit=unit, file=file_path, status='old', action='read', iostat=iostat)
        is_readable = (iostat == 0)
        if (is_readable) then
            close(unit)
        end if

    end function is_file_readable

    subroutine validate_gcov_executable(gcov_path, is_valid, error_message)
        !! Validates gcov executable path using secure command executor
        character(len=*), intent(in) :: gcov_path
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: safe_path

        call validate_executable_path(gcov_path, safe_path, error_ctx)

        if (error_ctx%error_code == ERROR_SUCCESS) then
            is_valid = .true.
            error_message = ""
        else
            is_valid = .false.
            error_message = trim(error_ctx%message)
        end if

    end subroutine validate_gcov_executable

    subroutine parse_diff_files(value, config, success, error_message)
        !! Parses --diff=baseline.json,current.json format
        character(len=*), intent(in) :: value
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        integer :: comma_pos
        character(len=MEDIUM_STRING_LEN) :: baseline_file, current_file

        success = .true.
        error_message = ""

        ! Find comma separator
        comma_pos = index(value, ',')
        if (comma_pos == 0) then
            success = .false.
            error_message = "Invalid --diff format. Expected: --diff=baseline.json,current.json"
            return
        end if

        ! Extract baseline and current files
        baseline_file = trim(adjustl(value(1:comma_pos-1)))
        current_file = trim(adjustl(value(comma_pos+1:)))

        ! Validate that both files are specified
        if (len_trim(baseline_file) == 0) then
            success = .false.
            error_message = "Baseline file not specified in --diff format"
            return
        end if

        if (len_trim(current_file) == 0) then
            success = .false.
            error_message = "Current file not specified in --diff format"
            return
        end if

        ! Set the configuration
        config%diff_baseline_file = baseline_file
        config%diff_current_file = current_file

    end subroutine parse_diff_files

end module fortcov_config