module json_parser
    !! JSON Parser Module - Coordinator for JSON parsing
    !! 
    !! Delegates to specialized modules for tokenization and object parsing.
    use json_token_types
    use json_tokenizer
    use json_object_parser
    implicit none
    private
    
    ! Re-export types and procedures from sub-modules
    public :: json_token_t
    public :: tokenize_json_content
    public :: parse_coverage_object_from_tokens
    public :: expect_token_type
    public :: parse_files_array_from_tokens
    public :: parse_file_object_from_tokens
    public :: parse_lines_array_from_tokens
    
    ! Re-export JSON token type constants
    public :: JSON_NULL, JSON_STRING, JSON_NUMBER
    public :: JSON_OBJECT, JSON_ARRAY, JSON_BOOLEAN

end module json_parser