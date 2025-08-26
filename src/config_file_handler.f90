module config_file_handler
    !! Configuration File Handling Module
    !! 
    !! Coordinates configuration file parsing using specialized modules.
    !! Extracted functionality for SRP and size limits.
    use iso_fortran_env, only: error_unit
    use fortcov_config, only: config_t
    use config_format_detector, only: detect_config_format
    use namelist_config_parser, only: parse_namelist_config_file
    use keyvalue_config_parser, only: parse_keyvalue_config_file
    use config_merger, only: merge_config_with_cli_priority, &
        initialize_file_config_defaults
    implicit none
    private
    
    public :: parse_config_file
    public :: load_config_file_with_merge
    public :: merge_config_with_cli_priority
    
contains
    
    subroutine parse_config_file(config, success, error_message)
        !! Configuration file parsing implementation with format detection
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        logical :: is_namelist
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
        
        ! Detect config file format
        call detect_config_format(config%config_file, is_namelist, success, error_message)
        if (.not. success) return
        
        ! Parse based on detected format
        if (is_namelist) then
            call parse_namelist_config_file(config, success, error_message)
        else
            call parse_keyvalue_config_file(config, success, error_message)
        end if
        
    end subroutine parse_config_file

    
    subroutine load_config_file_with_merge(config, success, error_message)
        !! Load config file and merge with existing config (CLI takes priority)
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        type(config_t) :: file_config
        
        success = .true.
        error_message = ""
        
        ! Initialize file config with defaults
        call initialize_file_config_defaults(file_config)
        
        ! Copy config file path to file_config
        if (allocated(config%config_file)) then
            file_config%config_file = config%config_file
        end if
        
        ! Parse config file into file_config
        call parse_config_file(file_config, success, error_message)
        if (.not. success) return
        
        ! Merge with CLI priority
        call merge_config_with_cli_priority(config, file_config)
        
    end subroutine load_config_file_with_merge

end module config_file_handler