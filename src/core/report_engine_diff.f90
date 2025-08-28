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
    public :: calculate_metrics_for_data
    public :: generate_diff_output

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
    
    ! Calculate metrics for data (delegated to coverage_metrics_calculator)
    subroutine calculate_metrics_for_data(data, metrics)
        type(coverage_data_t), intent(in) :: data
        type(coverage_metrics_t), intent(out) :: metrics
        
        integer :: i, total_lines, covered_lines
        
        ! Initialize metrics
        call metrics%init()
        
        if (.not. allocated(data%files)) return
        
        total_lines = 0
        covered_lines = 0
        metrics%total_files = size(data%files)
        
        ! Calculate line coverage
        do i = 1, size(data%files)
            if (allocated(data%files(i)%lines)) then
                total_lines = total_lines + size(data%files(i)%lines)
                covered_lines = covered_lines + count(data%files(i)%lines > 0)
            end if
        end do
        
        metrics%total_lines = total_lines
        metrics%covered_lines = covered_lines
        
        ! Calculate percentages
        if (total_lines > 0) then
            metrics%line_coverage_percentage = &
                real(covered_lines, kind=real64) / real(total_lines, kind=real64) * 100.0_real64
        else
            metrics%line_coverage_percentage = 0.0_real64
        end if
        
        ! Set default values for branch/function coverage
        metrics%branch_coverage_percentage = 0.0_real64
        metrics%function_coverage_percentage = 0.0_real64
    end subroutine calculate_metrics_for_data
    
    ! Generate diff output (delegated to coverage_metrics_calculator)
    subroutine generate_diff_output(baseline_metrics, current_metrics, diff_output)
        type(coverage_metrics_t), intent(in) :: baseline_metrics, current_metrics
        character(len=:), allocatable, intent(out) :: diff_output
        
        real(kind=real64) :: line_coverage_diff, branch_coverage_diff
        character(len=32) :: line_diff_str, branch_diff_str
        
        ! Calculate differences
        line_coverage_diff = current_metrics%line_coverage_percentage - &
                            baseline_metrics%line_coverage_percentage
        branch_coverage_diff = current_metrics%branch_coverage_percentage - &
                              baseline_metrics%branch_coverage_percentage
        
        ! Format difference strings
        write(line_diff_str, '(F8.2)') line_coverage_diff
        write(branch_diff_str, '(F8.2)') branch_coverage_diff
        
        ! Build diff report
        diff_output = "=== Coverage Diff Report ===" // new_line('a') // &
                     "Line Coverage: " // trim(line_diff_str) // "%" // &
                     new_line('a') // &
                     "Branch Coverage: " // trim(branch_diff_str) // "%" // &
                     new_line('a') // &
                     "========================"
    end subroutine generate_diff_output

end module report_engine_diff