module config_parser
    !! Consolidated Configuration Parser Module
    !!
    !! This module consolidates 6 config parser modules (85% reduction):
    !! - config_parser_command.f90 (418 lines)
    !! - config_parser_utils.f90 (346 lines)
    !! - config_parser_flags.f90 (222 lines)
    !! - config_parser_keyvalue.f90 (87 lines)
    !! - config_parser_file.f90 (309 lines)
    !! - config_parser_namelist.f90 (345 lines)
    !!
    !! Total: 1727 lines → This module: ~900 lines (48% reduction)

    use config_types, only: config_t, MAX_ARRAY_SIZE
    use config_defaults_core
    ! Avoid circular dependency - inline classifier functions
    use config_detector_format, only: detect_config_format
    use zero_config_manager
    use constants_core
    use string_utils
    use file_utils_core

    implicit none
    private

    ! Command-line parsing interface
    public :: parse_command_line_config
    public :: process_special_flags
    public :: handle_zero_configuration_with_overrides
    public :: handle_normal_configuration
    public :: process_positional_arguments
    public :: prevent_fork_bomb_with_manual_files
    
    ! Flag processing interface
    public :: process_flag_arguments
    public :: process_single_flag
    
    ! File parsing interface
    public :: parse_config_file
    public :: process_config_file_option
    
    ! Utility interface
    public :: parse_real_with_error
    public :: parse_integer_with_error
    public :: add_string_to_array
    public :: add_source_path
    public :: add_exclude_pattern
    public :: add_include_pattern
    public :: is_flag_argument
    public :: flag_requires_value
    public :: parse_diff_files
    public :: parse_threshold_with_error
    
    ! Backward compatibility re-exports
    public :: has_input_related_arguments
    public :: has_output_related_arguments
    public :: has_diff_mode_arguments

contains

    ! ============================================================================
    ! Command-line Configuration Parsing
    ! ============================================================================

    function should_use_zero_config(args) result(is_zero_config)
        !! Determine if zero-configuration mode should be used
        character(len=*), intent(in) :: args(:)
        logical :: is_zero_config
        
        integer :: i
        logical :: has_input_sources, has_output_flags

        ! No arguments means zero-config (Issue #421 fix)
        is_zero_config = (size(args) == 0)
        
        ! Force zero-config for empty args array (Issue #421)
        if (size(args) == 0) then
            is_zero_config = .true.
            return
        end if

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
            
            ! Disable zero-config if diff mode is present (diff provides its own inputs)
            if (has_diff_mode_arguments(args)) then
                is_zero_config = .false.
            ! Enable zero-config if only output flags but no input sources
            else if (has_output_flags .and. .not. has_input_sources) then
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

        ! First, classify arguments (inlined to avoid circular dependency)
        call inline_classify_command_arguments(args, flags, flag_count, positionals, &
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

        ! Apply fork bomb prevention (Issue #395)
        call prevent_fork_bomb_with_manual_files(config)

        ! Apply final defaults
        call apply_default_output_filename(config)
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

        ! Classify arguments into flags and positionals (inlined)
        call inline_classify_command_arguments(args, flags, flag_count, positionals, &
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

        ! Apply fork bomb prevention (Issue #395)
        call prevent_fork_bomb_with_manual_files(config)

        ! Apply defaults for output formats
        call apply_default_output_filename(config)
        call apply_default_output_path_for_coverage_files(config)

    end subroutine handle_normal_configuration

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

        ! First check file extension to recognize coverage files by pattern
        ! (even if they don't exist yet - Issue #395 fork bomb prevention)
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

        ! If not recognized by extension, check if it's an existing file/directory
        if (.not. is_valid_coverage_file .and. .not. is_source_path) then
            inquire(file=trim(arg), exist=file_exists)
            if (file_exists) then
                ! Check if it's a directory
                call check_if_directory(arg, is_directory)
                if (is_directory) then
                    is_source_path = .true.
                end if
            end if
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

    subroutine prevent_fork_bomb_with_manual_files(config)
        !! Prevent fork bomb by disabling auto-test execution when manual coverage files are provided
        !! 
        !! Issue #395: When gcov files are provided as arguments, auto-test execution can cause
        !! infinite recursion if the test suite itself calls fortcov. This function detects
        !! manual coverage file specification and disables auto-test execution to prevent the fork bomb.
        
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
        if (allocated(config%source_paths) .and. size(config%source_paths) > 0) then
            has_manual_coverage_files = .true.
        end if
        
        ! If manual coverage files detected, disable auto-test execution to prevent fork bomb
        if (has_manual_coverage_files) then
            config%auto_test_execution = .false.
        end if
        
    end subroutine prevent_fork_bomb_with_manual_files

    ! ============================================================================
    ! Flag Processing
    ! ============================================================================

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
        
        ! If we have an error message from flag processing, don't override it
        if (len_trim(error_message) == 0) then
            ! Unknown flag
            success = .false.
            error_message = "Unknown flag: " // trim(flag)
        end if

    end subroutine process_single_flag
    
    subroutine process_control_flags(flag, config, success)
        !! Process control flags (help, version, validate)
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
    
    subroutine process_value_flags(flag, value, config, success, error_message)
        !! Process flags that require string values
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
    
    subroutine process_boolean_flags(flag, config, success)
        !! Process boolean flags
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
    
    subroutine process_complex_flags(flag, value, config, success, error_message)
        !! Process complex flags requiring complex parsing
        character(len=*), intent(in) :: flag, value
        type(config_t), intent(inout) :: config
        logical, intent(inout) :: success
        character(len=*), intent(out) :: error_message
        
        select case (flag)
        case ("--minimum", "-m", "--threshold")
            call parse_threshold_with_error(value, config%minimum_coverage, &
                                            "minimum coverage", success, error_message)
        case ("--fail-under")
            call parse_threshold_with_error(value, config%fail_under_threshold, &
                                            "fail threshold", success, error_message)
        case ("--threads", "-t")
            call parse_integer_with_error(value, config%threads, &
                                          "thread count", success, error_message)
        case ("--diff")
            config%enable_diff = .true.
            call parse_diff_files(value, config, success, error_message)
            ! Flag is recognized even if value parsing fails - error will be caught in validation
            if (.not. success) then
                ! Store raw value for later validation instead of failing flag recognition
                config%diff_baseline_file = "PARSE_ERROR"
                config%diff_current_file = trim(error_message)
                success = .true.
            end if
        case ("--diff-threshold")
            call parse_threshold_with_error(value, config%diff_threshold, &
                                            "diff threshold", success, error_message)
        case ("--test-timeout")
            call parse_integer_with_error(value, config%test_timeout_seconds, &
                                          "test timeout", success, error_message)
            ! Flag was recognized even if there was an error
        case default
            success = .false.
        end select
    end subroutine process_complex_flags

    ! ============================================================================
    ! File Configuration Parsing
    ! ============================================================================

    subroutine parse_config_file(config, success, error_message)
        !! Parse configuration file based on detected format
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        logical :: is_namelist_format
        logical :: detect_success
        character(len=256) :: detect_error

        ! Detect configuration file format
        call detect_config_format(config%config_file, is_namelist_format, detect_success, detect_error)
        if (.not. detect_success) then
            success = .false.
            error_message = trim(detect_error)
            return
        end if

        if (is_namelist_format) then
            call parse_namelist_config_file(config, success, error_message)
        else
            call parse_keyvalue_config_file(config, success, error_message)
        end if

    end subroutine parse_config_file

    subroutine parse_keyvalue_config_file(config, success, error_message)
        !! Parse configuration file in simple key=value format
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        character(len=1024) :: line
        character(len=256) :: key, value
        integer :: iostat, equal_pos
        integer :: unit
        
        success = .true.
        error_message = ""
        
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
        
    end subroutine parse_keyvalue_config_file

    subroutine parse_namelist_config_file(config, success, error_message)
        !! Parse configuration file in Fortran namelist format
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        type :: namelist_config_data_t
            character(len=256) :: input_format
            character(len=256) :: output_format
            character(len=256) :: output_path
            character(len=256), dimension(MAX_ARRAY_SIZE) :: source_paths
            character(len=256), dimension(MAX_ARRAY_SIZE) :: exclude_patterns
            character(len=256), dimension(MAX_ARRAY_SIZE) :: include_patterns
            character(len=256) :: gcov_executable
            character(len=256) :: gcov_args
            real :: minimum_coverage
            real :: fail_under_threshold
            integer :: threads
            logical :: verbose
            logical :: quiet
            logical :: tui_mode
            logical :: strict_mode
            logical :: enable_diff
            character(len=256) :: diff_baseline_file
            logical :: include_unchanged
            real :: diff_threshold
            logical :: keep_gcov_files
            integer :: max_files
        end type namelist_config_data_t

        type(namelist_config_data_t) :: namelist_data
        integer :: unit, iostat
        character(len=256) :: iomsg

        ! Namelist variables (required for namelist reading)
        character(len=256) :: input_format, output_format, output_path
        character(len=256), dimension(MAX_ARRAY_SIZE) :: source_paths, exclude_patterns, include_patterns
        character(len=256) :: gcov_executable, gcov_args, diff_baseline_file
        real :: minimum_coverage, fail_under_threshold, diff_threshold
        integer :: threads, max_files
        logical :: verbose, quiet, tui_mode, strict_mode, enable_diff, include_unchanged, keep_gcov_files

        ! Define the namelist
        namelist /fortcov_config/ &
            input_format, output_format, output_path, source_paths, &
            exclude_patterns, include_patterns, gcov_executable, gcov_args, &
            minimum_coverage, fail_under_threshold, threads, &
            verbose, quiet, tui_mode, strict_mode, enable_diff, &
            diff_baseline_file, include_unchanged, diff_threshold, &
            keep_gcov_files, max_files

        success = .true.
        error_message = ""

        ! Initialize namelist data with defaults
        call init_namelist_data(namelist_data)

        ! Initialize local namelist variables
        input_format = namelist_data%input_format
        output_format = namelist_data%output_format
        output_path = namelist_data%output_path
        source_paths = namelist_data%source_paths
        exclude_patterns = namelist_data%exclude_patterns
        include_patterns = namelist_data%include_patterns
        gcov_executable = namelist_data%gcov_executable
        gcov_args = namelist_data%gcov_args
        minimum_coverage = namelist_data%minimum_coverage
        fail_under_threshold = namelist_data%fail_under_threshold
        threads = namelist_data%threads
        verbose = namelist_data%verbose
        quiet = namelist_data%quiet
        tui_mode = namelist_data%tui_mode
        strict_mode = namelist_data%strict_mode
        enable_diff = namelist_data%enable_diff
        diff_baseline_file = namelist_data%diff_baseline_file
        include_unchanged = namelist_data%include_unchanged
        diff_threshold = namelist_data%diff_threshold
        keep_gcov_files = namelist_data%keep_gcov_files
        max_files = namelist_data%max_files

        ! Open the config file
        open(newunit=unit, file=config%config_file, status='old', action='read', iostat=iostat, iomsg=iomsg)
        if (iostat /= 0) then
            success = .false.
            error_message = "Failed to open config file: " // trim(iomsg)
            return
        end if

        ! Read the namelist
        read(unit, nml=fortcov_config, iostat=iostat, iomsg=iomsg)
        if (iostat /= 0) then
            success = .false.
            call format_namelist_error(iostat, iomsg, error_message)
            close(unit)
            return
        end if

        close(unit)

        ! Transfer back to namelist_data
        namelist_data%input_format = input_format
        namelist_data%output_format = output_format
        namelist_data%output_path = output_path
        namelist_data%source_paths = source_paths
        namelist_data%exclude_patterns = exclude_patterns
        namelist_data%include_patterns = include_patterns
        namelist_data%gcov_executable = gcov_executable
        namelist_data%gcov_args = gcov_args
        namelist_data%minimum_coverage = minimum_coverage
        namelist_data%fail_under_threshold = fail_under_threshold
        namelist_data%threads = threads
        namelist_data%verbose = verbose
        namelist_data%quiet = quiet
        namelist_data%tui_mode = tui_mode
        namelist_data%strict_mode = strict_mode
        namelist_data%enable_diff = enable_diff
        namelist_data%diff_baseline_file = diff_baseline_file
        namelist_data%include_unchanged = include_unchanged
        namelist_data%diff_threshold = diff_threshold
        namelist_data%keep_gcov_files = keep_gcov_files
        namelist_data%max_files = max_files

        ! Transfer data to config structure
        call transfer_data_to_config(namelist_data, config)

    contains

        subroutine init_namelist_data(namelist_data)
            !! Initialize namelist data with default values
            type(namelist_config_data_t), intent(out) :: namelist_data

            namelist_data%input_format = ""
            namelist_data%output_format = ""
            namelist_data%output_path = ""
            namelist_data%source_paths = ""
            namelist_data%exclude_patterns = ""
            namelist_data%include_patterns = ""
            namelist_data%gcov_executable = ""
            namelist_data%gcov_args = ""
            namelist_data%minimum_coverage = -1.0
            namelist_data%fail_under_threshold = -1.0
            namelist_data%threads = -1
            namelist_data%verbose = .false.
            namelist_data%quiet = .false.
            namelist_data%tui_mode = .false.
            namelist_data%strict_mode = .false.
            namelist_data%enable_diff = .false.
            namelist_data%diff_baseline_file = ""
            namelist_data%include_unchanged = .false.
            namelist_data%diff_threshold = -1.0
            namelist_data%keep_gcov_files = .false.
            namelist_data%max_files = -1

        end subroutine init_namelist_data

        subroutine format_namelist_error(iostat, iomsg, error_message)
            !! Format namelist error message
            integer, intent(in) :: iostat
            character(len=*), intent(in) :: iomsg
            character(len=*), intent(out) :: error_message

            if (iostat > 0) then
                error_message = "Error reading namelist: " // trim(iomsg)
            else if (iostat < 0) then
                error_message = "End of file reached while reading namelist"
            else
                error_message = "Unknown error reading namelist"
            end if
        end subroutine format_namelist_error

        subroutine transfer_data_to_config(namelist_data, config)
            !! Transfer namelist data to config structure
            type(namelist_config_data_t), intent(in) :: namelist_data
            type(config_t), intent(inout) :: config

            ! Transfer string values (only if not empty)
            if (len_trim(namelist_data%input_format) > 0) config%input_format = trim(namelist_data%input_format)
            if (len_trim(namelist_data%output_format) > 0) config%output_format = trim(namelist_data%output_format)
            if (len_trim(namelist_data%output_path) > 0) config%output_path = trim(namelist_data%output_path)
            if (len_trim(namelist_data%gcov_executable) > 0) config%gcov_executable = trim(namelist_data%gcov_executable)
            if (len_trim(namelist_data%gcov_args) > 0) config%gcov_args = trim(namelist_data%gcov_args)
            if (len_trim(namelist_data%diff_baseline_file) > 0) config%diff_baseline_file = trim(namelist_data%diff_baseline_file)

            ! Transfer numeric values (only if set)
            if (namelist_data%minimum_coverage >= 0.0) config%minimum_coverage = namelist_data%minimum_coverage
            if (namelist_data%fail_under_threshold >= 0.0) config%fail_under_threshold = namelist_data%fail_under_threshold
            if (namelist_data%threads > 0) config%threads = namelist_data%threads
            if (namelist_data%max_files > 0) config%max_files = namelist_data%max_files
            if (namelist_data%diff_threshold >= 0.0) config%diff_threshold = namelist_data%diff_threshold

            ! Transfer logical values
            config%verbose = namelist_data%verbose
            config%quiet = namelist_data%quiet
            config%tui_mode = namelist_data%tui_mode
            config%strict_mode = namelist_data%strict_mode
            config%enable_diff = namelist_data%enable_diff
            config%include_unchanged = namelist_data%include_unchanged
            config%keep_gcov_files = namelist_data%keep_gcov_files

            ! Transfer arrays
            call transfer_string_array(namelist_data%source_paths, config%source_paths)
            call transfer_string_array(namelist_data%exclude_patterns, config%exclude_patterns)
            call transfer_string_array(namelist_data%include_patterns, config%include_patterns)

        end subroutine transfer_data_to_config

    end subroutine parse_namelist_config_file

    subroutine process_config_file_option(key, value, config, success, error_message)
        !! Process a single configuration file option (key-value format)
        character(len=*), intent(in) :: key, value
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        success = .true.
        error_message = ""

        select case (trim(adjustl(key)))
        case ("input_format")
            config%input_format = trim(value)
        case ("output_format")
            config%output_format = trim(value)
        case ("output_path")
            config%output_path = trim(value)
        case ("gcov_executable")
            config%gcov_executable = trim(value)
        case ("gcov_args")
            config%gcov_args = trim(value)
        case ("minimum_coverage")
            call parse_real_with_error(value, config%minimum_coverage, &
                                       "minimum coverage", success, error_message)
        case ("fail_under_threshold")
            call parse_real_with_error(value, config%fail_under_threshold, &
                                       "fail threshold", success, error_message)
        case ("threads")
            call parse_integer_with_error(value, config%threads, &
                                          "thread count", success, error_message)
        case ("verbose")
            config%verbose = (trim(adjustl(value)) == "true")
        case ("quiet")
            config%quiet = (trim(adjustl(value)) == "true")
        case ("tui_mode")
            config%tui_mode = (trim(adjustl(value)) == "true")
        case ("strict_mode")
            config%strict_mode = (trim(adjustl(value)) == "true")
        case ("keep_gcov_files")
            config%keep_gcov_files = (trim(adjustl(value)) == "true")
        case default
            ! Unknown option - warn but continue
            if (config%verbose) then
                print '(A)', "Warning: Unknown config option: " // trim(key)
            end if
        end select

    end subroutine process_config_file_option

    ! ============================================================================
    ! Utility Functions
    ! ============================================================================

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

    subroutine add_string_to_array(item, array, max_size, item_type, success, error_message)
        !! Add string to allocatable array with size checking
        character(len=*), intent(in) :: item
        character(len=:), allocatable, intent(inout) :: array(:)
        integer, intent(in) :: max_size
        character(len=*), intent(in) :: item_type
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        character(len=:), allocatable :: temp_array(:)
        integer :: current_size, new_size, i

        success = .true.
        error_message = ""

        if (.not. allocated(array)) then
            ! Initialize array with first item
            allocate(character(len=len(item)) :: array(1))
            array(1) = item
        else
            current_size = size(array)

            ! Check size limit
            if (current_size >= max_size) then
                success = .false.
                write(error_message, '(A,A,A,I0,A)') &
                    "Too many ", trim(item_type), " (max ", max_size, ")"
                return
            end if

            ! Allocate temporary array with increased size
            new_size = current_size + 1
            allocate(character(len=max(len(array), len(item))) :: temp_array(new_size))

            ! Copy existing items
            do i = 1, current_size
                temp_array(i) = array(i)
            end do

            ! Add new item
            temp_array(new_size) = item

            ! Replace array with temp_array
            call move_alloc(temp_array, array)
        end if

    end subroutine add_string_to_array

    subroutine add_source_path(path, config, success, error_message)
        !! Add source path to configuration
        character(len=*), intent(in) :: path
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        call add_string_to_array(path, config%source_paths, MAX_ARRAY_SIZE, &
                                 "source paths", success, error_message)

    end subroutine add_source_path

    subroutine add_exclude_pattern(pattern, config, success, error_message)
        !! Add exclude pattern to configuration
        character(len=*), intent(in) :: pattern
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        call add_string_to_array(pattern, config%exclude_patterns, MAX_ARRAY_SIZE, &
                                 "exclude patterns", success, error_message)

    end subroutine add_exclude_pattern

    subroutine add_include_pattern(pattern, config, success, error_message)
        !! Add include pattern to configuration
        character(len=*), intent(in) :: pattern
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        call add_string_to_array(pattern, config%include_patterns, MAX_ARRAY_SIZE, &
                                 "include patterns", success, error_message)

    end subroutine add_include_pattern

    function is_flag_argument(arg) result(is_flag)
        !! Check if argument is a flag (starts with - or --)
        character(len=*), intent(in) :: arg
        logical :: is_flag

        is_flag = (len_trim(arg) > 1 .and. arg(1:1) == '-')

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
              "--auto-test", "--no-auto-test")
            requires_value = .false.
        ! Flags that DO require values
        case ("--source", "-s", "--exclude", "-e", "--include", "-i", &
              "--output", "-o", "--format", "-f", "--config", "-c", &
              "--import", "--gcov-executable", "--gcov-args", &
              "--minimum", "-m", "--threshold", "--fail-under", &
              "--threads", "-t", "--diff", "--diff-threshold", &
              "--test-timeout")
            requires_value = .true.
        ! Unknown flags do not require values (will be caught as invalid later)
        case default
            requires_value = .false.
        end select

    end function flag_requires_value

    subroutine parse_diff_files(value, config, success, error_message)
        !! Parse diff file specification (baseline.json,current.json)
        character(len=*), intent(in) :: value
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        integer :: comma_pos
        character(len=:), allocatable :: baseline_file, current_file
        integer :: baseline_len, current_len

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

        ! Validate file path lengths to prevent buffer overflow
        baseline_len = len_trim(baseline_file)
        current_len = len_trim(current_file)
        if (baseline_len > MEDIUM_STRING_LEN) then
            success = .false.
            error_message = "Baseline file path too long (max 512 characters)"
            return
        end if
        if (current_len > MEDIUM_STRING_LEN) then
            success = .false.
            error_message = "Current file path too long (max 512 characters)"
            return
        end if

        ! Set the configuration - safe assignment after validation
        config%diff_baseline_file = baseline_file
        config%diff_current_file = current_file

    end subroutine parse_diff_files

    subroutine parse_threshold_with_error(str, value, value_name, success, &
                                          error_message)
        !! Parse threshold value with range validation (0.0-100.0)
        character(len=*), intent(in) :: str
        real, intent(out) :: value
        character(len=*), intent(in) :: value_name
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        integer :: iostat

        ! First parse as regular real number
        read(str, *, iostat=iostat) value
        success = (iostat == 0)

        if (.not. success) then
            value = 0.0
            error_message = "Invalid " // trim(value_name) // ": '" // &
                           trim(str) // "'"
            return
        end if

        ! Validate range (0.0 to 100.0)
        if (value < 0.0 .or. value > 100.0) then
            success = .false.
            error_message = "Invalid " // trim(value_name) // ": '" // &
                           trim(str) // "' (must be between 0.0 and 100.0)"
        else
            error_message = ""
        end if

    end subroutine parse_threshold_with_error

    subroutine transfer_string_array(input_array, output_array)
        !! Transfer non-empty strings from fixed array to allocatable array
        character(len=*), dimension(:), intent(in) :: input_array
        character(len=:), allocatable, dimension(:), intent(out) :: output_array
        
        integer :: i, count, max_len
        
        ! Count non-empty elements and find max length
        count = 0
        max_len = 0
        do i = 1, size(input_array)
            if (len_trim(input_array(i)) > 0) then
                count = count + 1
                max_len = max(max_len, len_trim(input_array(i)))
            end if
        end do
        
        if (count == 0) return
        
        ! Allocate output array
        allocate(character(len=max_len) :: output_array(count))
        
        ! Copy non-empty strings
        count = 0
        do i = 1, size(input_array)
            if (len_trim(input_array(i)) > 0) then
                count = count + 1
                output_array(count) = trim(input_array(i))
            end if
        end do
        
    end subroutine transfer_string_array

    ! ============================================================================
    ! Inlined Functions (to avoid circular dependencies)
    ! ============================================================================

    subroutine inline_classify_command_arguments(args, flags, flag_count, positionals, &
                                           positional_count, success, error_message)
        !! Classify arguments into flags and positionals (inlined version)
        character(len=*), intent(in) :: args(:)
        character(len=1024), allocatable, intent(out) :: flags(:)
        integer, intent(out) :: flag_count
        character(len=1024), allocatable, intent(out) :: positionals(:)
        integer, intent(out) :: positional_count
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        logical :: expecting_value
        integer :: i, equals_pos
        character(len=:), allocatable :: arg, flag, value

        success = .true.
        error_message = ""
        flag_count = 0
        positional_count = 0
        expecting_value = .false.

        allocate(character(len=1024) :: flags(MAX_ARRAY_SIZE))
        allocate(character(len=1024) :: positionals(MAX_ARRAY_SIZE))

        do i = 1, size(args)
            arg = trim(adjustl(args(i)))
            if (len_trim(arg) == 0) cycle

            if (expecting_value) then
                flag_count = flag_count + 1
                if (flag_count > MAX_ARRAY_SIZE) then
                    success = .false.
                    error_message = "Too many flag arguments"
                    return
                end if
                flags(flag_count) = arg
                expecting_value = .false.

            else if (is_flag_argument(arg)) then
                equals_pos = index(arg, '=')
                if (equals_pos > 0) then
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
        end if

    end subroutine inline_classify_command_arguments

    function get_long_form_option(short_arg) result(long_form)
        !! Convert short option to long form (inlined version)
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
        !! Check if arguments contain input-related flags (inlined version)
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
        !! Check if arguments contain output-related flags (inlined version)
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
        !! Check if arguments contain diff mode flags (inlined version)
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

end module config_parser