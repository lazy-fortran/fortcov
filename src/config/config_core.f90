module config_core
    !! Unified Configuration Management Module (Refactored)
    !! 
    !! This module serves as the main interface for configuration management,
    !! delegating to specialized modules for specific functionality while
    !! maintaining backward compatibility with existing code.
    !!
    !! The original monolithic module (1858 lines) has been decomposed into:
    !! - config_types.f90: Type definitions (~50 lines)
    !! - config_defaults.f90: Default values and initialization (~170 lines)
    !! - config_validation.f90: Validation logic (~400 lines)
    !! - config_help.f90: Help/version display (~90 lines)
    !! - config_parser.f90: Consolidated parser (~900 lines)
    !! - fortcov_config.f90: Orchestration layer (~150 lines)
    !!
    !! This addresses Issue #298 - module size violation while maintaining
    !! all functionality and backward compatibility.

    use config_types
    use config_defaults_core
    use config_validation
    use config_help_core
    use config_parser
    use error_handling_core

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
    public :: validate_config_with_context
    ! Command-line and file parsing now in config_parser module
    public :: parse_command_line_config
    public :: parse_config_file
    public :: MAX_ARRAY_SIZE

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

        ! Delegate to command parser module
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
        character(len=512) :: error_message

        call validate_complete_config_with_message(config, is_valid, error_message)

        if (is_valid) then
            error_ctx%error_code = ERROR_SUCCESS
            error_ctx%message = ""
            error_ctx%suggestion = ""
            error_ctx%context = ""
        else
            error_ctx%error_code = ERROR_INVALID_CONFIG
            error_ctx%message = trim(error_message)
            error_ctx%suggestion = "Check configuration parameters and ensure paths exist"
            error_ctx%context = "Configuration validation"
        end if

    end subroutine validate_config_with_context

    subroutine load_config_file(config, success, error_message)
        !! Load configuration from file
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        call parse_config_file(config, success, error_message)

    end subroutine load_config_file

end module config_core