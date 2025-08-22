module config_file_handler
    !! Configuration File Handling Module
    !! 
    !! Focused on reading and processing configuration files.
    !! Extracted from config_parser.f90 to maintain SRP and size limits.
    use foundation_constants
    use foundation_layer_utils
    use fortcov_config, only: config_t
    implicit none
    private
    
    public :: parse_config_file
    public :: load_config_file_with_merge
    public :: merge_config_with_cli_priority
    public :: process_config_file_option
    
contains
    
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
    
    subroutine load_config_file_with_merge(config, success, error_message)
        !! Load config file and merge with existing config (CLI takes precedence)
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        type(config_t) :: file_config
        
        ! Initialize file config with defaults
        call initialize_file_config_defaults(file_config)
        
        ! Set the config file path
        file_config%config_file = config%config_file
        
        ! Load settings from file
        call parse_config_file(file_config, success, error_message)
        if (.not. success) return
        
        ! Merge settings - CLI overrides file settings
        call merge_config_with_cli_priority(config, file_config)
        
    end subroutine load_config_file_with_merge
    
    subroutine merge_config_with_cli_priority(cli_config, file_config)
        !! Merge file config into CLI config, with CLI taking precedence
        type(config_t), intent(inout) :: cli_config
        type(config_t), intent(in) :: file_config
        
        ! Only apply file settings if CLI didn't set them
        if (.not. allocated(cli_config%output_path) .and. &
            allocated(file_config%output_path)) then
            cli_config%output_path = file_config%output_path
        end if
        
        if (.not. allocated(cli_config%output_format) .and. &
            allocated(file_config%output_format)) then
            cli_config%output_format = file_config%output_format
        end if
        
        ! For numeric values, check if they're still at default
        if (cli_config%minimum_coverage == 0.0 .and. &
            file_config%minimum_coverage > 0.0) then
            cli_config%minimum_coverage = file_config%minimum_coverage
        end if
        
        ! For boolean flags, only apply if not already set by CLI
        ! Note: This is tricky since we can't distinguish "not set" from "false"
        ! For now, we'll assume CLI always takes precedence for booleans
        
    end subroutine merge_config_with_cli_priority
    
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
            call parse_config_real_value(value, config%minimum_coverage, success)
            if (.not. success) then
                error_message = "Invalid threshold in config file: " // trim(value)
                return
            end if
        case ('verbose')
            config%verbose = (trim(value) == 'true')
        case ('quiet')
            config%quiet = (trim(value) == 'true')
        case ('tui_mode')
            config%tui_mode = (trim(value) == 'true')
        case ('strict_mode')
            config%strict_mode = (trim(value) == 'true')
        case ('gcov_executable')
            config%gcov_executable = trim(value)
        case ('gcov_args')
            config%gcov_args = trim(value)
        case ('threads')
            call parse_config_integer_value(value, config%threads, success)
            if (.not. success) then
                error_message = "Invalid threads value in config file: " // trim(value)
                return
            end if
        case ('max_files')
            call parse_config_integer_value(value, config%max_files, success)
            if (.not. success) then
                error_message = "Invalid max_files value in config file: " // trim(value)
                return
            end if
        case ('diff_baseline_file')
            config%diff_baseline_file = trim(value)
        case ('enable_diff')
            config%enable_diff = (trim(value) == 'true')
        case ('exclude')
            ! Placeholder - add_exclude_pattern would need to be implemented here
            success = .true.
        case ('source')
            ! Placeholder - add_source_path would need to be implemented here
            success = .true.
        case default
            ! Ignore unknown options in config file
        end select
        
        success = .true.
        error_message = ""
        
    end subroutine process_config_file_option
    
    subroutine parse_config_real_value(str, value, success)
        !! Parses string to real value for config file
        character(len=*), intent(in) :: str
        real, intent(out) :: value
        logical, intent(out) :: success
        
        integer :: iostat
        
        read(str, *, iostat=iostat) value
        success = (iostat == 0)
        
    end subroutine parse_config_real_value
    
    subroutine parse_config_integer_value(str, value, success)
        !! Parses string to integer value for config file
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
        
    end subroutine parse_config_integer_value
    
    subroutine initialize_file_config_defaults(config)
        !! Initializes configuration with default values for file loading
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
        
    end subroutine initialize_file_config_defaults
    
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
    
end module config_file_handler