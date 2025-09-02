module coverage_workflows
    !! Coverage Workflows coordination interface
    !! 
    !! Provides unified interface to all coverage workflow functionality
    !! by re-exporting functions from focused workflow modules.
    
    use coverage_workflows_discovery, only: discover_coverage_files, &
                                            filter_coverage_files_by_patterns, &
                                            evaluate_exclude_patterns
    use coverage_workflows_analysis, only: perform_coverage_diff_analysis
    use coverage_test_executor, only: execute_auto_test_workflow
    implicit none
    private
    
    ! Re-export all public interfaces
    public :: discover_coverage_files
    public :: evaluate_exclude_patterns
    public :: perform_coverage_diff_analysis
    public :: filter_coverage_files_by_patterns
    public :: execute_auto_test_workflow
end module coverage_workflows
