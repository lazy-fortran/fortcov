module config_parser_command
    !! Command-line argument parsing
    !! 
    !! This module handles parsing of command-line arguments, including
    !! flag processing, positional arguments, and zero-configuration mode detection.

    use config_types, only: config_t, MAX_ARRAY_SIZE
    use config_parser_utils
    use config_defaults_core
    use config_parser_file
    use config_classifier_args
    use config_parser_flags
    use config_detector_args
    use zero_config_manager

    implicit none
    private

    public :: parse_command_line_config
    public :: process_special_flags
    public :: handle_zero_configuration_with_overrides
    public :: handle_normal_configuration
    public :: process_positional_arguments
    public :: prevent_fork_bomb_with_manual_files
    ! Re-export functions from config_argument_detector for backward compatibility
    public :: has_input_related_arguments
    public :: has_output_related_arguments
    public :: has_diff_mode_arguments

contains

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

        ! Apply fork bomb prevention (Issue #395)
        call prevent_fork_bomb_with_manual_files(config)

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

        ! Apply fork bomb prevention (Issue #395)
        call prevent_fork_bomb_with_manual_files(config)

        ! Apply defaults for HTML output
        call apply_html_default_filename(config)
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
        !!
        !! Args:
        !!   config: Configuration object to modify
        
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

end module config_parser_command