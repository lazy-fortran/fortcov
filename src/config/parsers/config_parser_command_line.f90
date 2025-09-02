module config_parser_command_line
    !! Command Line Parser - Backward Compatibility Layer
    !!
    !! Decomposed for SRP compliance (Issue #718 proactive size management).
    !! Re-exports all functionality from specialized modules to maintain
    !! backward compatibility with existing code.
    !!
    !! Original size: 416 lines -> Now: ~80 lines
    !! Implementation moved to specialized modules:
    !! - config_zero_config_detector.f90
    !! - config_special_flags.f90
    !! - config_positional_args.f90
    !! - config_fork_bomb_prevention.f90
    use config_types, only: config_t
    use config_defaults_core, only: initialize_default_config, apply_default_output_filename, &
                                   apply_default_output_path_for_coverage_files, &
                                   ensure_zero_config_output_directory, &
                                   handle_zero_configuration_mode
    use config_classifier_args, only: classify_command_arguments
    use config_parser_flags, only: process_flag_arguments
    ! Config files removed: CLI-only configuration (Issue #1165)
    use config_parser_consolidated, only: detect_zero_config_mode
    use config_parser_consolidated, only: process_special_flags
    use config_positional_args
    use config_parser_consolidated, only: prevent_fork_bomb_with_manual_files
    implicit none
    
    ! Re-export public procedures for backward compatibility
    public :: parse_command_line_config
    public :: process_special_flags
    public :: handle_zero_configuration_with_overrides
    public :: handle_normal_configuration
    public :: process_positional_arguments
    public :: prevent_fork_bomb_with_manual_files

contains

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
        is_zero_config = detect_zero_config_mode(args)

        if (is_zero_config) then
            ! Process CLI flags FIRST, then apply zero-config defaults for unset values
            call handle_zero_configuration_with_overrides(args, config, success, error_message)
            return
        end if

        ! Normal argument parsing for non-zero-configuration mode
        call handle_normal_configuration(args, config, success, error_message)

    end subroutine parse_command_line_config

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

        ! Config files are no longer supported (CLI-only). Ignore if set.

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

        ! Config files are no longer supported (CLI-only). Ignore if set.

        ! Apply fork bomb prevention (Issue #395)
        call prevent_fork_bomb_with_manual_files(config)

        ! Apply defaults for output formats
        call apply_default_output_filename(config)
        call apply_default_output_path_for_coverage_files(config)

    end subroutine handle_normal_configuration

end module config_parser_command_line
