module coverage_operations_core
    !! Coverage Operations - Backward Compatibility Layer
    !! 
    !! Decomposed for SRP compliance (Issue #718 proactive size management).
    !! Re-exports all functionality from specialized modules to maintain
    !! backward compatibility with existing code.
    !!
    !! Original size: 449 lines -> Now: ~60 lines
    !! Implementation moved to specialized modules:
    !! - coverage_calculations.f90
    !! - coverage_merging.f90
    !! - coverage_filtering.f90
    !! - coverage_comparison.f90
    use coverage_calculations
    use coverage_merging
    use coverage_filtering
    use coverage_comparison
    implicit none
    
    ! Re-export all public procedures for backward compatibility
    public :: calculate_coverage_statistics
    public :: merge_coverage_data
    public :: merge_coverage_files
    public :: merge_coverage_lines
    public :: calculate_file_coverage
    public :: calculate_overall_coverage
    public :: filter_coverage_by_threshold
    public :: find_uncovered_lines
    public :: compare_coverage_data

end module coverage_operations_core