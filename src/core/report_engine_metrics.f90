module report_engine_metrics
    !! Report Engine Metrics Calculation
    !! 
    !! Handles coverage metrics calculation for reports.
    !! Extracted from report_engine.f90 for SRP compliance (Issue #718).
    use coverage_model_core
    use coverage_metrics_core
    implicit none
    private
    
    public :: calculate_coverage_metrics

contains

    ! Calculate coverage metrics for data
    subroutine calculate_coverage_metrics(source_data, metrics, success, error_msg)
        type(coverage_data_t), intent(in) :: source_data
        type(coverage_metrics_t), intent(out) :: metrics
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        integer :: i, total_lines, covered_lines, total_functions, covered_functions
        integer :: total_branches, covered_branches
        
        success = .false.
        error_msg = ""
        
        ! Initialize metrics
        call metrics%init()
        
        if (.not. allocated(source_data%files)) then
            error_msg = "No source data available for metrics calculation"
            return
        end if
        
        ! Initialize counters
        total_lines = 0
        covered_lines = 0
        total_functions = 0
        covered_functions = 0
        total_branches = 0
        covered_branches = 0
        
        metrics%total_files = size(source_data%files)
        
        ! Process each file
        do i = 1, size(source_data%files)
            ! Count lines
            if (allocated(source_data%files(i)%lines)) then
                total_lines = total_lines + size(source_data%files(i)%lines)
                covered_lines = covered_lines + &
                    count(source_data%files(i)%lines > 0)
            end if
            
            ! Count functions (if function coverage data available)
            if (allocated(source_data%files(i)%function_coverage)) then
                total_functions = total_functions + &
                    size(source_data%files(i)%function_coverage)
                covered_functions = covered_functions + &
                    count(source_data%files(i)%function_coverage > 0)
            end if
            
            ! Count branches (if branch coverage data available)
            if (allocated(source_data%files(i)%branch_coverage)) then
                total_branches = total_branches + &
                    size(source_data%files(i)%branch_coverage)
                covered_branches = covered_branches + &
                    count(source_data%files(i)%branch_coverage > 0)
            end if
        end do
        
        ! Store totals
        metrics%total_lines = total_lines
        metrics%covered_lines = covered_lines
        
        ! Calculate line coverage percentage
        if (total_lines > 0) then
            metrics%line_coverage_percentage = &
                real(covered_lines, kind=real64) / &
                real(total_lines, kind=real64) * 100.0_real64
        else
            metrics%line_coverage_percentage = 0.0_real64
        end if
        
        ! Calculate function coverage percentage
        if (total_functions > 0) then
            metrics%function_coverage_percentage = &
                real(covered_functions, kind=real64) / &
                real(total_functions, kind=real64) * 100.0_real64
        else
            metrics%function_coverage_percentage = 0.0_real64
        end if
        
        ! Calculate branch coverage percentage
        if (total_branches > 0) then
            metrics%branch_coverage_percentage = &
                real(covered_branches, kind=real64) / &
                real(total_branches, kind=real64) * 100.0_real64
        else
            metrics%branch_coverage_percentage = 0.0_real64
        end if
        
        success = .true.
    end subroutine calculate_coverage_metrics

end module report_engine_metrics