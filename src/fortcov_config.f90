module fortcov_config
    !! Configuration Module (Refactored for Architectural Decomposition)
    !! 
    !! This module now serves as a compatibility layer that delegates to 
    !! the new decomposed architecture while preserving the original public interface.
    !! 
    !! Original module size: 1,128 lines → Now: <100 lines
    !! Decomposed into: config_parser + config_validator + config_storage
    use config_parser
    use config_validator
    use config_storage
    implicit none
    private
    
    ! Re-export public interface for backward compatibility
    public :: config_t
    public :: parse_config
    public :: show_help
    public :: show_version
    public :: initialize_config
    public :: validate_config
    public :: load_config_file
    
    ! Re-export constants for interface compatibility
    integer, parameter, public :: MAX_ARRAY_SIZE = 100
    
contains
    
    ! Compatibility wrappers for the original public interface
    subroutine parse_config(args, config, success, error_message)
        !! Delegates to config_parser module
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(out) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        call parse_command_line_config(args, config, success, error_message)
        
    end subroutine parse_config
    
    subroutine show_help()
        !! Delegates to config_storage module
        call show_help_information()
    end subroutine show_help
    
    subroutine show_version()
        !! Delegates to config_storage module
        call show_version_information()
    end subroutine show_version
    
    subroutine initialize_config(config)
        !! Delegates to config_parser module
        type(config_t), intent(out) :: config
        
        call initialize_default_config(config)
        
    end subroutine initialize_config
    
    function validate_config(config) result(is_valid)
        !! Delegates to config_validator module
        type(config_t), intent(in) :: config
        logical :: is_valid
        
        is_valid = validate_complete_config(config)
        
    end function validate_config
    
    subroutine load_config_file(config, success, error_message)
        !! Delegates to config_parser module
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        call parse_config_file(config, success, error_message)
        
    end subroutine load_config_file
    
    ! Note: All implementation has been moved to specialized modules:
    ! - config_parser.f90: Command-line and file parsing logic
    ! - config_validator.f90: Configuration validation and checks
    ! - config_storage.f90: Help display, storage, and utility functions
    !
    ! This architecture enables:
    ! - Better separation of concerns
    ! - Improved testability of individual components
    ! - Easier maintenance and modification
    ! - Compliance with 400-line module size targets
    ! - Preserved backward compatibility
    
end module fortcov_config