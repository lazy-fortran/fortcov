module config_parser
    !! Configuration Parser (Refactored for size limits)
    !! 
    !! Main coordination module for configuration parsing.
    !! Uses specialized modules for different aspects of parsing.
    use foundation_constants
    use foundation_layer_utils
    use string_utils
    use file_utils
    use error_handling
    use input_validation
    use zero_configuration_manager
    use fortcov_config, only: config_t
    use config_flag_processor
    use config_arg_classifier
    implicit none
    private
    
    public :: parse_command_line_config
    public :: initialize_default_config
    
    
contains
    
    subroutine parse_command_line_config(args, config, success, error_message)
        !! Main command-line configuration parsing function
        !! Extracted from original parse_config function
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
        !! Zero-config mode ONLY triggers when NO arguments are provided
        !! Any arguments provided means normal configuration processing
        character(len=*), intent(in) :: args(:)
        integer :: i
        
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
        
        ! Zero-config mode should ONLY trigger with NO arguments
        ! Any provided arguments should use normal configuration
        
    end function should_use_zero_config
    
    subroutine handle_zero_configuration_with_overrides(args, config, success, error_message)
        !! Handle zero-configuration mode with CLI flag overrides
        !! First process CLI flags, then apply zero-config defaults for unset values
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
        
        if (.not. allocated(config%output_format) .or. &
            trim(config%output_format) == "markdown") then
            ! Keep default format from initialization unless explicitly overridden
            ! markdown is the default, so only override if it was explicitly set to something else
        end if
        
        if (.not. allocated(config%input_format) .or. &
            trim(config%input_format) == "gcov") then
            ! Keep default input format from initialization
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
        character(len=:), allocatable :: temp_files(:), temp_paths(:)
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
        
        ! Pass 2.5: Load config file if specified (after CLI parsing to allow override)
        ! TODO: Implement config file loading
        if (allocated(config%config_file)) then
            ! Placeholder for config file loading
            success = .true.
        end if
        
        ! Pass 3: Auto-discover files if no input sources provided
        ! This allows flags like --tui, --verbose, etc. to work without explicit files
        if (.not. allocated(config%coverage_files)) then
            temp_files = auto_discover_coverage_files_priority()
            if (allocated(temp_files) .and. size(temp_files) > 0) then
                config%coverage_files = temp_files
            end if
        end if
        
        if (.not. allocated(config%source_paths)) then
            temp_paths = auto_discover_source_files_priority()
            if (allocated(temp_paths) .and. size(temp_paths) > 0) then
                config%source_paths = temp_paths
            end if
        end if
        
        ! Pass 4: Apply HTML default filename logic
        call apply_html_default_filename(config)
        
        ! Pass 5: Apply default output path if coverage files provided but no output specified
        call apply_default_output_path_for_coverage_files(config)
    end subroutine handle_normal_configuration
    
    
    
    
    
    
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
    
    
    subroutine ensure_zero_config_output_directory(config)
        !! Ensures output directory exists for zero-configuration mode
        use zero_configuration_manager, only: ensure_output_directory_structure
        use error_handling, only: error_context_t
        type(config_t), intent(in) :: config
        
        type(error_context_t) :: error_ctx
        
        if (allocated(config%output_path)) then
            call ensure_output_directory_structure(config%output_path, error_ctx)
            ! Silently handle directory creation errors in zero-config mode
            ! The error will be reported later during file writing if needed
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
    
end module config_parser