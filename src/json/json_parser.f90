module json_parser
    !! JSON Parser - Backward Compatibility Layer
    !!
    !! Decomposed for SRP compliance (Issue #718 proactive size management).
    !! Re-exports all functionality from specialized modules to maintain
    !! backward compatibility with existing code.
    !!
    !! Original size: 445 lines -> Now: ~40 lines
    !! Implementation moved to specialized modules:
    !! - json_object_parser.f90
    !! - json_value_parser.f90
    !! - json_array_utils.f90
    use json_object_parser
    use json_value_parser
    use json_array_utils
    implicit none
    
    ! Re-export all public procedures for backward compatibility
    public :: parse_coverage_object_from_tokens
    public :: parse_files_array_from_tokens
    public :: parse_file_object_from_tokens
    public :: parse_lines_array_from_tokens
    public :: expect_token_type
    public :: parse_string_value
    public :: parse_number_value
    public :: skip_value
    public :: grow_files_array
    public :: grow_lines_array

end module json_parser