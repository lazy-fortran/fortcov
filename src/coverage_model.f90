module coverage_model
    !! Coverage Model (Refactored for Architectural Decomposition)
    !! 
    !! This module now serves as a compatibility layer that delegates to 
    !! the new decomposed architecture while preserving the original public interface.
    !! 
    !! Original module size: 900 lines → Now: <100 lines
    !! Decomposed into: coverage_data_model + coverage_operations
    use coverage_data_model
    use coverage_operations
    use coverage_statistics, only: coverage_stats_t, extended_coverage_stats_t
    implicit none
    private
    
    ! Re-export all public types for backward compatibility
    public :: source_location_t
    public :: coverage_line_t
    public :: coverage_branch_t
    public :: coverage_function_t
    public :: coverage_file_t
    public :: coverage_data_t
    public :: coverage_stats_t
    public :: extended_coverage_stats_t
    public :: coverage_diff_t
    public :: line_diff_t
    public :: file_diff_t
    public :: line_coverage_t
    public :: file_coverage_t
    
    ! Re-export public procedures for backward compatibility
    public :: calculate_statistics
    public :: merge_coverage
    public :: compare_coverage
    
contains
    
    ! Compatibility wrappers for the original public interface
    subroutine calculate_statistics(coverage_data, stats)
        !! Delegates to coverage_operations module
        type(coverage_data_t), intent(in) :: coverage_data
        type(extended_coverage_stats_t), intent(out) :: stats
        
        call calculate_coverage_statistics(coverage_data, stats)
        
    end subroutine calculate_statistics
    
    subroutine merge_coverage(data1, data2, merged_data)
        !! Delegates to coverage_operations module
        type(coverage_data_t), intent(in) :: data1, data2
        type(coverage_data_t), intent(out) :: merged_data
        
        call merge_coverage_data(data1, data2, merged_data)
        
    end subroutine merge_coverage
    
    subroutine compare_coverage(baseline, current, diff_result)
        !! Delegates to coverage_operations module
        type(coverage_data_t), intent(in) :: baseline, current
        type(coverage_diff_t), intent(out) :: diff_result
        
        call compare_coverage_data(baseline, current, diff_result)
        
    end subroutine compare_coverage
    
    ! Note: All implementation has been moved to specialized modules:
    ! - coverage_data_model.f90: Type definitions and basic initialization
    ! - coverage_operations.f90: Coverage calculations, merging, and operations
    !
    ! This architecture enables:
    ! - Better separation of concerns between data and operations
    ! - Improved testability of individual components
    ! - Easier maintenance and modification
    ! - Compliance with 400-line module size targets
    ! - Preserved backward compatibility
    
end module coverage_model