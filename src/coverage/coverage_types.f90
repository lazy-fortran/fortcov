module coverage_types
    !! Coverage Types Module - Re-architected for optimal maintainability
    !!
    !! Previously consolidated as a single 960-line module approaching the 
    !! 1000-line hard limit (92% capacity). Now properly decomposed into 
    !! focused, cohesive modules following the same pattern used in json_core:
    !! - coverage_basic_types: Basic types and constants
    !! - coverage_complex_types: Complex aggregation types  
    !! - coverage_diff_types: Coverage comparison types
    !!
    !! This architecture prevents hard limit violations while maintaining
    !! all functionality and public interfaces.
    
    ! Import all functionality from specialized modules
    use coverage_basic_types, only: &
        COV_MAX_FILENAME_LENGTH, COV_MAX_NAME_LENGTH, &
        source_location_t, coverage_line_t, coverage_branch_t, &
        line_coverage_t, file_coverage_t, &
        line_constructor, branch_constructor, line_is_covered
    use coverage_complex_types, only: &
        coverage_function_t, coverage_file_t, coverage_data_t, &
        file_init_simple, file_init_with_lines, file_calculate_coverage, &
        data_init_simple, data_init_with_files, data_calculate_overall_coverage
    use coverage_diff_types, only: &
        DIFF_UNCHANGED, DIFF_IMPROVED, DIFF_DEGRADED, DIFF_NEW_LINE, DIFF_REMOVED_LINE, &
        line_diff_t, file_diff_t, coverage_diff_t, &
        line_diff_constructor
    implicit none
    private
    
    ! Re-export all public interfaces to maintain backwards compatibility
    
    ! Constants
    public :: COV_MAX_FILENAME_LENGTH, COV_MAX_NAME_LENGTH
    public :: DIFF_UNCHANGED, DIFF_IMPROVED, DIFF_DEGRADED, DIFF_NEW_LINE, DIFF_REMOVED_LINE
    
    ! Basic types
    public :: source_location_t
    public :: coverage_line_t
    public :: coverage_branch_t
    public :: line_coverage_t
    public :: file_coverage_t
    
    ! Complex types
    public :: coverage_function_t
    public :: coverage_file_t
    public :: coverage_data_t
    
    ! Diff types
    public :: line_diff_t
    public :: file_diff_t
    public :: coverage_diff_t
    
    ! Constructors
    public :: line_constructor
    public :: branch_constructor
    public :: line_diff_constructor
    
    ! Procedures
    public :: file_init_simple, file_init_with_lines, file_calculate_coverage
    public :: data_init_simple, data_init_with_files, data_calculate_overall_coverage
    public :: line_is_covered
    
    ! All implementation is now provided by the specialized modules
    ! This module serves as a facade to maintain backwards compatibility

end module coverage_types