module config_parser
    !! Consolidated Configuration Parser Module (DECOMPOSED)
    !!
    !! This module provides the main entry point for configuration parsing,
    !! delegating specific tasks to specialized submodules:
    !! - config_parser_command_line: Command-line parsing and zero-config logic
    !! - config_parser_flags: Flag processing and validation
    !! - config_parser_files: Configuration file parsing (key-value and namelist)
    !! - config_parser_utils: Common utilities and helper functions
    !!
    !! ARCHITECTURAL NOTE: This decomposition resolves Issue #665 by breaking
    !! the original 1534-line monstrosity into focused, maintainable modules.

    use config_types, only: config_t
    use config_parser_command_line, only: parse_command_line_config, &
                                                 process_special_flags, &
                                                 handle_zero_configuration_with_overrides, &
                                                 handle_normal_configuration, &
                                                 process_positional_arguments, &
                                                 prevent_fork_bomb_with_manual_files
    use config_parser_flags, only: process_flag_arguments, process_single_flag
    use config_parser_files, only: parse_config_file, process_config_file_option
    use config_parser_utils, only: parse_real_with_error, parse_integer_with_error, &
                                          add_string_to_array, add_source_path, &
                                          add_exclude_pattern, add_include_pattern, &
                                          is_flag_argument, flag_requires_value, &
                                          parse_diff_files, parse_threshold_with_error, &
                                          has_input_related_arguments, &
                                          has_output_related_arguments, &
                                          has_diff_mode_arguments

    implicit none
    private

    ! Primary interface (main entry point)
    public :: parse_command_line_config

    ! Command-line parsing interface (delegated)
    public :: process_special_flags
    public :: handle_zero_configuration_with_overrides
    public :: handle_normal_configuration
    public :: process_positional_arguments
    public :: prevent_fork_bomb_with_manual_files
    
    ! Flag processing interface (delegated)
    public :: process_flag_arguments
    public :: process_single_flag
    
    ! File parsing interface (delegated)
    public :: parse_config_file
    public :: process_config_file_option
    
    ! Utility interface (delegated)
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
    
    ! Backward compatibility re-exports (delegated)
    public :: has_input_related_arguments
    public :: has_output_related_arguments
    public :: has_diff_mode_arguments

    ! NOTE: This module now serves purely as a facade/interface aggregator.
    ! All actual implementation logic has been moved to specialized modules:
    !
    ! config_parser_command_line.f90 (~390 lines) - Command-line parsing logic
    ! config_parser_flags.f90       (~200 lines) - Flag processing logic  
    ! config_parser_files.f90       (~330 lines) - File parsing logic
    ! config_parser_utils.f90       (~350 lines) - Utility functions
    !
    ! Total: ~1270 lines decomposed from 1534 lines (17% reduction)
    ! This module: ~70 lines (95% reduction from original)
    !
    ! ARCHITECTURAL BENEFITS:
    ! - Single Responsibility Principle: Each module has one clear purpose
    ! - Dependency management: Clear separation prevents circular dependencies
    ! - Maintainability: Small, focused modules are easier to understand and modify
    ! - Testability: Individual components can be tested in isolation
    ! - Extensibility: New functionality can be added to appropriate modules

    ! NOTE: All implementation has been decomposed into specialized modules.
    ! This module now serves as a pure facade/aggregator for backward compatibility.

end module config_parser