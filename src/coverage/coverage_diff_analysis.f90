module coverage_diff_analysis
    !! Coverage Diff Analysis Workflow
    !! 
    !! Handles coverage difference analysis between baseline and current data.
    !! Extracted from coverage_workflows.f90 for SRP compliance (Issue #718).
    use constants_core
    use config_core
    use coverage_diff
    use coverage_types, only: coverage_diff_t
    use json_io, only: import_coverage_from_json_file
    use coverage_model_core, only: coverage_data_t
    use file_utilities, only: file_exists
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
        call perform_actual_coverage_diff(config, diff_result, diff_success)
        
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
    
    subroutine perform_actual_coverage_diff(config, diff_result, success)
        !! Performs actual coverage diff analysis using existing infrastructure
        type(config_t), intent(in) :: config
        type(coverage_diff_t), intent(out) :: diff_result
        logical, intent(out) :: success
        
        type(coverage_data_t) :: baseline_data, current_data
        logical :: baseline_error, current_error
        character(len=:), allocatable :: baseline_file, current_file
        
        success = .false.
        
        ! Initialize diff_result with empty data
        diff_result%baseline_coverage = 0.0
        diff_result%current_coverage = 0.0
        diff_result%coverage_change = 0.0
        
        ! Check if diff file paths are configured
        if (.not. allocated(config%diff_baseline_file)) then
            if (.not. config%quiet) then
                print *, "❌ Baseline file not configured for diff analysis"
            end if
            return
        end if
        
        if (.not. allocated(config%diff_current_file)) then
            if (.not. config%quiet) then
                print *, "❌ Current file not configured for diff analysis"
            end if
            return
        end if
        
        baseline_file = config%diff_baseline_file
        current_file = config%diff_current_file
        
        ! Check if files exist
        if (.not. file_exists(baseline_file)) then
            if (.not. config%quiet) then
                print *, "❌ Baseline file does not exist: " // baseline_file
            end if
            return
        end if
        
        if (.not. file_exists(current_file)) then
            if (.not. config%quiet) then
                print *, "❌ Current file does not exist: " // current_file
            end if
            return
        end if
        
        ! Load baseline coverage data
        call import_coverage_from_json_file(baseline_file, baseline_data, baseline_error)
        if (baseline_error) then
            if (.not. config%quiet) then
                print *, "❌ Failed to load baseline coverage data from: " // baseline_file
            end if
            return
        end if
        
        ! Load current coverage data
        call import_coverage_from_json_file(current_file, current_data, current_error)
        if (current_error) then
            if (.not. config%quiet) then
                print *, "❌ Failed to load current coverage data from: " // current_file
            end if
            return
        end if
        
        ! Compute coverage differences using existing infrastructure
        if (allocated(baseline_data%files) .and. allocated(current_data%files)) then
            diff_result = compute_coverage_diff(baseline_data, current_data)
            
            ! Calculate overall coverage percentages for summary
            diff_result%baseline_coverage = calculate_overall_coverage(baseline_data)
            diff_result%current_coverage = calculate_overall_coverage(current_data)
            diff_result%coverage_change = diff_result%current_coverage - &
                                        diff_result%baseline_coverage
            
            success = .true.
        else
            if (.not. config%quiet) then
                print *, "❌ Invalid coverage data structure in loaded files"
            end if
        end if
        
    end subroutine perform_actual_coverage_diff
    
    function calculate_overall_coverage(coverage_data) result(coverage_percent)
        !! Calculates overall coverage percentage from coverage data
        type(coverage_data_t), intent(in) :: coverage_data
        real :: coverage_percent
        
        integer :: total_lines, covered_lines, i, j
        
        total_lines = 0
        covered_lines = 0
        
        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
                if (allocated(coverage_data%files(i)%lines)) then
                    do j = 1, size(coverage_data%files(i)%lines)
                        total_lines = total_lines + 1
                        if (coverage_data%files(i)%lines(j)%execution_count > 0) then
                            covered_lines = covered_lines + 1
                        end if
                    end do
                end if
            end do
        end if
        
        if (total_lines > 0) then
            coverage_percent = (real(covered_lines) / real(total_lines)) * 100.0
        else
            coverage_percent = 0.0
        end if
        
    end function calculate_overall_coverage

end module coverage_diff_analysis