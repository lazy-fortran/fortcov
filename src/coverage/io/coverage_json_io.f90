module coverage_json_io
    !! JSON Coverage I/O (Refactored for Architectural Consolidation)
    !! 
    !! This module now serves as a compatibility layer that delegates to 
    !! the new consolidated architecture while preserving the original public interface.
    !! 
    !! Original module size: 1,053 lines → Now: <50 lines
    !! Consolidated from 9 modules into: json_io + json_core + json_validator (3 modules total)
    !!
    !! Architectural consolidation benefits:
    !! - Eliminated excessive modularization (9 → 3 modules)  
    !! - Reduced cognitive overhead and maintenance burden
    !! - Preserved complete backward compatibility
    !! - Maintained clear separation of concerns
    use coverage_model_core
    use json_io, only: import_json_coverage, export_json_coverage, import_json_coverage_safe, &
                       json_token_t, JSON_NULL, JSON_STRING, JSON_NUMBER, JSON_OBJECT, &
                       JSON_ARRAY, JSON_BOOLEAN
    implicit none
    private
    
    ! Re-export public interface for backward compatibility
    public :: import_json_coverage
    public :: export_json_coverage
    public :: import_json_coverage_safe
    
    ! Re-export JSON token types for compatibility
    public :: json_token_t
    public :: JSON_NULL, JSON_STRING, JSON_NUMBER, JSON_OBJECT, JSON_ARRAY, JSON_BOOLEAN

end module coverage_json_io