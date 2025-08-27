module config_defaults
    !! Configuration default values and initialization
    !! 
    !! This module provides initialization routines and default values for
    !! configuration settings, extracted from fortcov_config for maintainability.

    use config_types, only: config_t, MAX_ARRAY_SIZE
    use foundation_constants
    use zero_configuration_manager
    use shell_utils, only: escape_shell_argument
    
    implicit none
    private

    public :: initialize_default_config
    public :: apply_html_default_filename
    public :: apply_default_output_path_for_coverage_files
    public :: ensure_zero_config_output_directory
    public :: get_max_files_from_env
    public :: handle_zero_configuration_mode
    public :: apply_legacy_zero_config_defaults

contains

    subroutine initialize_default_config(config)
        !! Initialize configuration with default values
        type(config_t), intent(out) :: config

        integer :: max_files_env

        ! Set default values for all configuration fields
        config%input_format = "auto"
        config%output_format = "markdown"
        config%output_path = ""
        config%gcov_executable = "gcov"
        config%minimum_coverage = 0.0
        config%fail_under_threshold = 0.0
        config%threads = 1
        config%verbose = .false.
        config%quiet = .false.
        config%show_help = .false.
        config%show_version = .false.
        config%validate_config_only = .false.
        config%config_file = ""
        config%enable_diff = .false.
        config%diff_baseline_file = ""
        config%diff_current_file = ""
        config%include_unchanged = .false.
        config%diff_threshold = 0.0
        config%import_file = ""
        config%keep_gcov_files = .false.
        config%gcov_args = ""
        config%tui_mode = .false.
        config%strict_mode = .false.
        config%zero_configuration_mode = .false.
        config%auto_discovery = .true.
        config%auto_test_execution = .true.
        config%test_timeout_seconds = 300

        ! Get max files from environment, default to 1000
        call get_max_files_from_env(max_files_env)
        config%max_files = max_files_env

    end subroutine initialize_default_config

    subroutine apply_html_default_filename(config)
        !! Apply default filename when HTML output is selected without path
        type(config_t), intent(inout) :: config

        if (config%output_format == "html" .and. &
            len_trim(config%output_path) == 0) then
            config%output_path = "coverage.html"
        end if

    end subroutine apply_html_default_filename

    subroutine apply_default_output_path_for_coverage_files(config)
        !! Apply default output path when processing coverage files directly
        type(config_t), intent(inout) :: config

        logical :: has_coverage_files

        has_coverage_files = allocated(config%coverage_files) .and. &
                              size(config%coverage_files) > 0

        if (has_coverage_files .and. &
            config%output_format == "html" .and. &
            len_trim(config%output_path) == 0) then
            config%output_path = "coverage.html"
        end if

    end subroutine apply_default_output_path_for_coverage_files

    subroutine ensure_zero_config_output_directory(config)
        !! Ensure output directory exists for zero-configuration mode
        type(config_t), intent(inout) :: config

        logical :: dir_exists
        character(len=512) :: command, directory_path
        integer :: exit_status, last_slash_pos

        if (config%zero_configuration_mode .and. &
            len_trim(config%output_path) > 0) then
            
            ! Extract directory path from output file path
            last_slash_pos = index(config%output_path, '/', back=.true.)
            if (last_slash_pos > 0) then
                directory_path = config%output_path(1:last_slash_pos-1)
                
                ! Check if directory exists
                inquire(file=trim(directory_path), exist=dir_exists)
                if (.not. dir_exists) then
                    ! Use secure shell argument escaping to prevent command injection
                    command = "mkdir -p " // escape_shell_argument(directory_path)
                    call execute_command_line(command, exitstat=exit_status)
                    
                    ! Handle directory creation failure
                    if (exit_status /= 0) then
                        write(*,'(A)') "Warning: Failed to create output directory: " // &
                                       trim(directory_path)
                        write(*,'(A)') "Please ensure the directory is writable or " // &
                                       "create it manually"
                    end if
                end if
            end if
            ! If no directory in path, current directory will be used (no mkdir needed)
        end if

    end subroutine ensure_zero_config_output_directory

    subroutine get_max_files_from_env(max_files)
        !! Get max files limit from environment variable
        integer, intent(out) :: max_files

        character(len=256) :: env_value
        integer :: status, io_stat

        ! Default value
        max_files = 1000

        ! Try to get from environment
        call get_environment_variable("FORTCOV_MAX_FILES", env_value, status=status)
        
        if (status == 0 .and. len_trim(env_value) > 0) then
            read(env_value, *, iostat=io_stat) max_files
            if (io_stat /= 0) then
                max_files = 1000  ! Fall back to default on parse error
            else if (max_files <= 0) then
                max_files = 1000  ! Ensure positive value
            end if
        end if

    end subroutine get_max_files_from_env

    subroutine handle_zero_configuration_mode(config)
        !! Apply zero-configuration defaults (basic mode)
        !! Enhanced auto-discovery integration is applied separately to avoid 
        !! circular dependencies (Issue #281)
        type(config_t), intent(inout) :: config

        ! Mark as zero-configuration mode
        config%zero_configuration_mode = .true.

        ! Apply zero-config defaults
        call apply_legacy_zero_config_defaults(config)

        ! NOTE: Enhanced auto-discovery integration (Issue #281) is now 
        ! applied separately from the main application entry point to 
        ! avoid circular module dependencies

    end subroutine handle_zero_configuration_mode

    subroutine apply_legacy_zero_config_defaults(config)
        !! Apply legacy zero-configuration defaults (preserved for compatibility)
        type(config_t), intent(inout) :: config
        
        logical :: has_source_paths
        integer :: stat
        character(len=512) :: errmsg

        ! Check if source paths are already set
        has_source_paths = allocated(config%source_paths) .and. &
                           size(config%source_paths) > 0

        ! Only set defaults for values not already set by CLI flags
        if (.not. has_source_paths) then
            ! Use current directory if no source paths specified
            if (allocated(config%source_paths)) deallocate(config%source_paths)
            allocate(character(len=1) :: config%source_paths(1), &
                stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                write(*, '(A)') "Error: Memory allocation failed for source_paths: " // &
                    trim(errmsg)
                return
            end if
            config%source_paths(1) = "."
        end if

        ! Text format is valid and supported - no conversion needed

        ! Set default output path if not set
        if (len_trim(config%output_path) == 0) then
            config%output_path = "build/coverage/coverage.md"
        end if

        ! Use auto-discovery for input format
        if (config%input_format == "auto") then
            config%input_format = "auto"
        end if

        ! Auto-discover gcov executable if needed
        if (config%gcov_executable == "gcov") then
            ! Let secure_command_executor handle discovery
            config%gcov_executable = "gcov"
        end if

    end subroutine apply_legacy_zero_config_defaults

end module config_defaults