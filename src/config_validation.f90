module config_validation
    use config_validation_core
    use config_validators_impl
    use config_format_validators
    use input_source_validator, only: validate_input_sources
    implicit none
    private

    ! Re-export all procedures for backward compatibility
    public :: validate_complete_config
    public :: validate_input_sources
    public :: validate_coverage_files
    public :: validate_output_settings
    public :: validate_threshold_settings
    public :: validate_diff_configuration
    public :: validate_import_configuration
    public :: validate_source_paths
    public :: validate_output_path
    public :: validate_import_file
    public :: validate_gcov_executable
    public :: is_supported_output_format
    public :: is_valid_coverage_file_format
    public :: is_valid_import_file_format
    public :: is_file_readable

end module config_validation