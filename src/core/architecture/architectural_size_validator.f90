module architectural_size_validator
    !! Architectural Size Validation Orchestrator (QADS Compliant)
    !! 
    !! Orchestrates comprehensive size validation using focused modules.
    !! Now QADS-compliant at 180 lines vs previous 580 lines.
    !! 
    !! ARCHITECTURAL IMPROVEMENT:
    !! - Decomposed into focused modules using SRP
    !! - size_scanner_core: Data collection
    !! - size_violation_analyzer: Violation analysis  
    !! - size_report_generator: Report formatting
    !! - This module: Orchestration only
    use error_handling_core, only: error_context_t, ERROR_SUCCESS, clear_error_context
    use size_scanner_core, only: scan_file_sizes_in_directory, scan_directory_item_counts, &
                                 scan_result_t
    use size_violation_analyzer, only: analyze_file_violations, analyze_directory_violations, &
                                       check_imminent_violations_in_results, &
                                       file_size_violation_t, directory_size_violation_t
    use size_report_generator, only: generate_comprehensive_size_report, &
                                     generate_report_in_format, &
                                     architectural_size_report_t
    implicit none
    private
    
    ! Public interface for size validation and monitoring
    public :: validate_codebase_architecture
    public :: scan_file_sizes
    public :: scan_directory_sizes  
    public :: generate_size_report
    public :: check_imminent_violations
    public :: architectural_size_report_t
    public :: file_size_violation_t
    public :: directory_size_violation_t

contains

    subroutine validate_codebase_architecture(base_directory, report, error_ctx)
        !! Main entry point for comprehensive architectural size validation
        !! Orchestrates scanning, analysis, and reporting
        character(len=*), intent(in) :: base_directory
        type(architectural_size_report_t), intent(out) :: report
        type(error_context_t), intent(out) :: error_ctx
        
        type(scan_result_t), allocatable :: file_scan_results(:)
        type(scan_result_t), allocatable :: dir_scan_results(:)
        type(file_size_violation_t), allocatable :: file_violations(:)
        type(directory_size_violation_t), allocatable :: dir_violations(:)
        integer :: files_scanned, dirs_scanned
        
        call clear_error_context(error_ctx)
        
        ! Scan files and directories
        call scan_file_sizes_in_directory(base_directory, file_scan_results, &
                                         files_scanned, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        call scan_directory_item_counts(base_directory, dir_scan_results, &
                                       dirs_scanned, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        ! Analyze violations
        call analyze_file_violations(file_scan_results, file_violations)
        call analyze_directory_violations(dir_scan_results, dir_violations)
        
        ! Generate comprehensive report
        call generate_comprehensive_size_report(file_violations, dir_violations, &
                                               files_scanned, dirs_scanned, report)
        
    end subroutine validate_codebase_architecture
    
    subroutine scan_file_sizes(base_directory, violations, files_scanned, error_ctx)
        !! Wrapper for file size scanning (maintains API compatibility)
        character(len=*), intent(in) :: base_directory
        type(file_size_violation_t), allocatable, intent(out) :: violations(:)
        integer, intent(out) :: files_scanned
        type(error_context_t), intent(out) :: error_ctx
        
        type(scan_result_t), allocatable :: scan_results(:)
        
        call scan_file_sizes_in_directory(base_directory, scan_results, &
                                         files_scanned, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        call analyze_file_violations(scan_results, violations)
        
    end subroutine scan_file_sizes
    
    subroutine scan_directory_sizes(base_directory, violations, dirs_scanned, error_ctx)
        !! Wrapper for directory size scanning (maintains API compatibility)
        character(len=*), intent(in) :: base_directory
        type(directory_size_violation_t), allocatable, intent(out) :: violations(:)
        integer, intent(out) :: dirs_scanned  
        type(error_context_t), intent(out) :: error_ctx
        
        type(scan_result_t), allocatable :: scan_results(:)
        
        call scan_directory_item_counts(base_directory, scan_results, &
                                       dirs_scanned, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        call analyze_directory_violations(scan_results, violations)
        
    end subroutine scan_directory_sizes
    
    subroutine generate_size_report(report, output_format, report_text)
        !! Wrapper for report generation (maintains API compatibility)
        type(architectural_size_report_t), intent(in) :: report
        character(len=*), intent(in) :: output_format  ! "human", "ci", "json"
        character(len=:), allocatable, intent(out) :: report_text
        
        call generate_report_in_format(report, output_format, report_text)
        
    end subroutine generate_size_report
    
    subroutine check_imminent_violations(base_directory, imminent_files, error_ctx)
        !! Wrapper for imminent violation checking (maintains API compatibility)
        character(len=*), intent(in) :: base_directory
        character(len=:), allocatable, intent(out) :: imminent_files(:)
        type(error_context_t), intent(out) :: error_ctx
        
        type(scan_result_t), allocatable :: scan_results(:)
        integer :: files_scanned
        
        call clear_error_context(error_ctx)
        
        call scan_file_sizes_in_directory(base_directory, scan_results, &
                                         files_scanned, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        call check_imminent_violations_in_results(scan_results, imminent_files)
        
    end subroutine check_imminent_violations


end module architectural_size_validator