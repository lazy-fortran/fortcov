module config_parser_utils
    !! Main coordination module for configuration parsing utilities
    !!
    !! This module re-exports essential parsing utilities from specialized modules
    !! to maintain interface compatibility while enforcing architectural boundaries.

    ! Import all specialized modules
    use config_parser_string_utils, only: parse_real_with_error, &
                                          parse_integer_with_error, &
                                          parse_threshold_with_error
    
    use config_parser_array_utils, only: add_string_to_array, &
                                         add_source_path, &
                                         add_exclude_pattern, &
                                         add_include_pattern, &
                                         transfer_string_array
    
    use config_parser_flag_utils, only: is_flag_argument, &
                                        flag_requires_value, &
                                        get_long_form_option, &
                                        has_input_related_arguments, &
                                        has_output_related_arguments, &
                                        has_diff_mode_arguments
    
    use config_parser_complex_utils, only: parse_diff_files

    implicit none
    private

    ! Re-export string parsing utilities
    public :: parse_real_with_error
    public :: parse_integer_with_error
    public :: parse_threshold_with_error

    ! Re-export array management utilities
    public :: add_string_to_array
    public :: add_source_path
    public :: add_exclude_pattern
    public :: add_include_pattern
    public :: transfer_string_array

    ! Re-export flag processing utilities
    public :: is_flag_argument
    public :: flag_requires_value
    public :: get_long_form_option
    public :: has_input_related_arguments
    public :: has_output_related_arguments
    public :: has_diff_mode_arguments

    ! Re-export complex parsing utilities
    public :: parse_diff_files

contains

    ! This module serves as a coordination layer for specialized utilities
    ! All functionality has been decomposed into specialized modules:
    ! - config_parser_string_utils: Type conversion and parsing
    ! - config_parser_array_utils: Array management operations  
    ! - config_parser_flag_utils: Command-line flag processing
    ! - config_parser_complex_utils: Complex parsing operations

end module config_parser_utils