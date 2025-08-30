module config_parser_consolidated
    !! Consolidated configuration parser module interface
    !!
    !! Provides unified interface to all configuration parsing functionality
    !! by re-exporting functions from focused modules.
    
    use config_parser_string, only: parse_real_with_error, parse_integer_with_error, &
                                    parse_threshold_with_error
    use config_parser_arrays, only: add_string_to_array, add_source_path, &
                                    add_exclude_pattern, add_include_pattern, &
                                    transfer_string_array
    use config_parser_flags, only: is_flag_argument, flag_requires_value, &
                                  get_long_form_option, has_input_related_arguments, &
                                  has_output_related_arguments, has_diff_mode_arguments
    use config_parser_special, only: process_special_flags, &
                                     prevent_fork_bomb_with_manual_files, &
                                     detect_zero_config_mode
    use config_parser_complex, only: parse_diff_files
    implicit none
    private
    
    ! Re-export all public interfaces
    public :: parse_real_with_error
    public :: parse_integer_with_error
    public :: parse_threshold_with_error
    public :: add_string_to_array
    public :: add_source_path
    public :: add_exclude_pattern
    public :: add_include_pattern
    public :: transfer_string_array
    public :: is_flag_argument
    public :: flag_requires_value
    public :: get_long_form_option
    public :: has_input_related_arguments
    public :: has_output_related_arguments
    public :: has_diff_mode_arguments
    public :: process_special_flags
    public :: prevent_fork_bomb_with_manual_files
    public :: detect_zero_config_mode
    public :: parse_diff_files

end module config_parser_consolidated