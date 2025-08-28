module json_core
    !! JSON Core Module - Re-architected for optimal maintainability
    !!
    !! Previously consolidated from multiple over-modularized components,
    !! but grew to 966 lines approaching the 1000-line hard limit.
    !! Now properly decomposed into focused, cohesive modules:
    !! - json_tokenizer: Token definitions and tokenization logic
    !! - json_parser: Token-to-data-structure parsing
    !! - json_converter: JSON-to-XML conversion and utilities
    !!
    !! This architecture prevents hard limit violations while maintaining
    !! all functionality and public interfaces.
    
    ! Import all functionality from specialized modules
    use json_tokenizer, only: json_token_t, JSON_NULL, JSON_STRING, JSON_NUMBER, &
                              JSON_OBJECT, JSON_ARRAY, JSON_BOOLEAN, tokenize_json_content
    use json_parser, only: parse_coverage_object_from_tokens, parse_files_array_from_tokens, &
                           parse_file_object_from_tokens, parse_lines_array_from_tokens, &
                           expect_token_type, parse_string_value, parse_number_value, &
                           skip_value, grow_files_array, grow_lines_array
    use json_converter, only: extract_coverage_rates_from_json, generate_packages_from_json, &
                              generate_classes_from_json_files, generate_lines_from_json_file, &
                              extract_json_real_value, extract_json_int_value, &
                              extract_json_string_value
    implicit none
    private
    
    ! Re-export all public interfaces to maintain backwards compatibility
    public :: json_token_t
    public :: JSON_NULL, JSON_STRING, JSON_NUMBER, JSON_OBJECT, JSON_ARRAY, JSON_BOOLEAN
    public :: tokenize_json_content
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
    public :: extract_coverage_rates_from_json
    public :: generate_packages_from_json
    public :: generate_classes_from_json_files
    public :: generate_lines_from_json_file
    public :: extract_json_real_value
    public :: extract_json_int_value
    public :: extract_json_string_value

    ! All implementation is now provided by the specialized modules
    ! This module serves as a facade to maintain backwards compatibility

end module json_core