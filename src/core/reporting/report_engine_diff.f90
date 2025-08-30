module report_engine_diff
    !! Report Engine Diff Generation
    !! 
    !! Handles diff report generation between coverage datasets.
    !! Extracted from report_engine.f90 for SRP compliance (Issue #718).
    use coverage_model_core
    use coverage_metrics_core
    implicit none
    private
    
    public :: generate_diff_report_content

contains

    ! Generate diff report content
    subroutine generate_diff_report_content(baseline_data, current_data, &
                                           diff_output, success, error_msg)
        type(coverage_data_t), intent(in) :: baseline_data, current_data
        character(len=:), allocatable, intent(out) :: diff_output
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        type(coverage_metrics_t) :: baseline_metrics, current_metrics
        
        success = .false.
        error_msg = ""
        
        ! Calculate metrics for both datasets
        call calculate_metrics_for_data(baseline_data, baseline_metrics)
        call calculate_metrics_for_data(current_data, current_metrics)
        
        ! Generate diff report
        call generate_diff_output(baseline_metrics, current_metrics, diff_output)
        
        success = .true.
    end subroutine generate_diff_report_content

end module report_engine_diff