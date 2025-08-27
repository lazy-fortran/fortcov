module zero_configuration_manager
    !! Zero-Configuration Manager (Issue #204)
    !! 
    !! High-level coordination module for zero-configuration functionality
    !! 
    !! This module provides the main entry points for zero-configuration mode.
    !! It coordinates between specialized modules for auto-discovery, command utilities,
    !! and file processing while maintaining full API compatibility.
    !! 
    !! Responsibilities:
    !! - Detect zero-argument command execution
    !! - Coordinate auto-discovery operations
    !! - Delegate to specialized modules for implementation
    !! - Maintain API compatibility with existing code
    !! 
    !! Integration with new modules:
    !! - auto_discovery: Auto-discover coverage files and source paths
    !! - command_utils: Command execution and system interaction
    !! - file_processor: File processing and directory operations
    use auto_discovery, only: auto_discover_coverage_files_priority => &
                                   auto_discover_coverage_files_priority, &
                              auto_discover_source_files_priority => &
                                   auto_discover_source_files_priority
    use file_processor, only: ensure_output_directory_structure => &
                                   ensure_output_directory_structure, &
                              show_zero_configuration_error_guidance => &
                                   show_zero_configuration_error_guidance, &
                              apply_zero_configuration_defaults => &
                                   apply_zero_configuration_defaults
    implicit none
    private
    
    ! Public interfaces for zero-configuration functionality (API compatibility)
    public :: is_zero_configuration_mode
    public :: apply_zero_configuration_defaults
    public :: auto_discover_coverage_files_priority
    public :: auto_discover_source_files_priority
    public :: ensure_output_directory_structure
    public :: show_zero_configuration_error_guidance
    
contains
    
    function is_zero_configuration_mode() result(is_zero_config)
        !! Detect if user invoked fortcov with no arguments (zero-configuration mode)
        logical :: is_zero_config
        integer :: argc
        
        ! Check if fortcov was invoked with no arguments
        argc = command_argument_count()
        is_zero_config = (argc == 0)
        
    end function is_zero_configuration_mode
    
end module zero_configuration_manager