module coverage_diff_analysis
    !! Coverage Diff Analysis Workflow
    !! 
    !! Handles coverage difference analysis between baseline and current data.
    !! Extracted from coverage_workflows.f90 for SRP compliance (Issue #718).
    use constants_core
    use config_core
    use coverage_diff
    use coverage_types, only: coverage_diff_t
    implicit none
    private
    
    public :: perform_coverage_diff_analysis, output_coverage_diff_summary

contains

    function perform_coverage_diff_analysis(config) result(exit_code)
        !! Coverage diff analysis workflow implementation
        !! Extracted from original analyze_coverage_diff function
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        type(coverage_diff_t) :: diff_result
        logical :: diff_success
        
        exit_code = EXIT_SUCCESS
        
        if (.not. config%quiet) then
            print *, "📊 Analyzing coverage differences..."
            if (allocated(config%diff_baseline_file)) then
                print *, "   Baseline: " // trim(config%diff_baseline_file)
            end if
            if (allocated(config%diff_current_file)) then
                print *, "   Compare:  " // trim(config%diff_current_file)
            end if
        end if
        
        ! Perform coverage diff analysis
        ! Note: This would need actual coverage data loaded from baseline and current files
        ! For now, just set a success flag and empty result
        diff_success = .true.
        
        if (.not. diff_success) then
            if (.not. config%quiet) then
                print *, "❌ Coverage diff analysis failed"
            end if
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Output diff summary
        call output_coverage_diff_summary(diff_result, config)
        
        ! Apply threshold validation for diff
        if (config%minimum_coverage > 0.0) then
            if (diff_result%current_coverage < config%minimum_coverage) then
                if (.not. config%quiet) then
                    print *, "❌ Coverage threshold not met in comparison"
                    write(*, '(A, F5.1, A, F5.1, A)') &
                        "   Required: ", config%minimum_coverage, "%, Current: ", &
                        diff_result%current_coverage, "%"
                end if
                exit_code = EXIT_THRESHOLD_NOT_MET
            end if
        end if
        
    end function perform_coverage_diff_analysis
    
    subroutine output_coverage_diff_summary(diff_result, config)
        !! Outputs coverage diff analysis summary
        type(coverage_diff_t), intent(in) :: diff_result
        type(config_t), intent(in) :: config
        
        if (.not. config%quiet) then
            print *, "📊 Coverage Diff Analysis Results:"
            write(*, '(A, F5.1, A)') "   Baseline Coverage: ", &
                diff_result%baseline_coverage, "%"
            write(*, '(A, F5.1, A)') "   Current Coverage:  ", &
                diff_result%current_coverage, "%"
            write(*, '(A, F5.1, A)') "   Coverage Change:   ", &
                diff_result%coverage_change, "%"
            
            if (diff_result%coverage_change > 0.0) then
                print *, "   ✅ Coverage improved"
            else if (diff_result%coverage_change < 0.0) then
                print *, "   ⚠️  Coverage decreased"
            else
                print *, "   ➡️  Coverage unchanged"
            end if
        end if
        
    end subroutine output_coverage_diff_summary

end module coverage_diff_analysis