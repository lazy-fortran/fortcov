module coverage_json_io
    !! JSON Coverage I/O (Refactored for Architectural Decomposition)
    !! 
    !! This module now serves as a compatibility layer that delegates to 
    !! the new decomposed architecture while preserving the original public interface.
    !! 
    !! Original module size: 1,053 lines → Now: <100 lines
    !! Decomposed into: json_io_core + json_parser + json_validator
    use coverage_model_core
    use json_io_core
    use json_parser_core
    use json_validator_impl
    implicit none
    private
    
    ! Re-export public interface for backward compatibility
    public :: import_json_coverage
    public :: export_json_coverage
    public :: import_json_coverage_safe
    
    ! Re-export JSON token types for compatibility
    public :: json_token_t
    public :: JSON_NULL, JSON_STRING, JSON_NUMBER, JSON_OBJECT, JSON_ARRAY, JSON_BOOLEAN
    
contains
    
    ! Compatibility wrappers for the original public interface
    subroutine import_json_coverage(json_content, coverage_data)
        !! Delegates to json_io_core module
        character(len=*), intent(in) :: json_content
        type(coverage_data_t), intent(out) :: coverage_data
        
        call import_coverage_from_json(json_content, coverage_data)
        
    end subroutine import_json_coverage
    
    subroutine export_json_coverage(coverage_data, json_output)
        !! Delegates to json_io_core module
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=:), allocatable, intent(out) :: json_output
        
        call export_coverage_to_json(coverage_data, json_output)
        
    end subroutine export_json_coverage
    
    subroutine import_json_coverage_safe(json_content, coverage_data, error_caught)
        !! Delegates to json_io_core module
        character(len=*), intent(in) :: json_content
        type(coverage_data_t), intent(out) :: coverage_data
        logical, intent(out) :: error_caught
        
        call import_coverage_from_json_safe(json_content, coverage_data, error_caught)
        
    end subroutine import_json_coverage_safe
    
    ! Note: All implementation has been moved to specialized modules:
    ! - json_io_core.f90: Core JSON import/export operations
    ! - json_parser.f90: JSON parsing logic and tokenization
    ! - json_validator.f90: JSON validation and format checking
    !
    ! This architecture enables:
    ! - Better separation of concerns
    ! - Improved testability of JSON operations
    ! - Easier maintenance and debugging
    ! - Compliance with 400-line module size targets
    ! - Preserved backward compatibility
    
end module coverage_json_io