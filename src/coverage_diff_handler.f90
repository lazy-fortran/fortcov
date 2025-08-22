module coverage_diff_handler
    !! Coverage Diff Handler Module
    !! 
    !! Focused on diff mode analysis functionality.
    !! Extracted from coverage_analysis.f90 to maintain SRP and size limits.
    use foundation_constants
    use foundation_layer_utils
    use fortcov_config, only: config_t
    use coverage_model
    use coverage_diff
    use coverage_statistics, only: calculate_line_coverage
    use coverage_reporter
    use file_utils, only: file_exists
    use json_coverage_io
    implicit none
    private
    
    public :: perform_diff_analysis
    public :: load_baseline_coverage
    public :: generate_current_coverage
    public :: generate_diff_report
    
contains
    
    function perform_diff_analysis(config) result(exit_code)
        !! Diff mode analysis implementation
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        type(coverage_data_t) :: baseline_coverage, current_coverage
        type(coverage_stats_t) :: baseline_stats, current_stats
        type(coverage_diff_t) :: diff_result
        logical :: baseline_error, current_error
        real :: coverage_delta
        
        if (.not. config%quiet) then
            print *, "🔍 Starting diff analysis mode"
            if (allocated(config%diff_baseline_file)) then
                print *, "   Baseline: " // trim(config%diff_baseline_file)
            end if
        end if
        
        ! Validate baseline file exists
        if (.not. validate_baseline_file(config)) then
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Load baseline coverage data
        call load_baseline_coverage(config%diff_baseline_file, baseline_coverage, baseline_error)
        if (baseline_error) then
            if (.not. config%quiet) then
                print *, "❌ Failed to load baseline coverage data"
            end if
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Generate current coverage data by running normal analysis
        call generate_current_coverage(config, current_coverage, current_error)
        if (current_error) then
            if (.not. config%quiet) then
                print *, "❌ Failed to generate current coverage data"
            end if
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Calculate statistics and display results
        call calculate_and_display_diff_stats(baseline_coverage, current_coverage, &
                                            config, coverage_delta)
        
        ! Generate diff report
        call generate_diff_report(baseline_coverage, current_coverage, config)
        
        ! Determine exit code based on results
        exit_code = determine_diff_exit_code(config, coverage_delta)
        
    end function perform_diff_analysis
    
    function validate_baseline_file(config) result(is_valid)
        !! Validates that baseline file exists and is accessible
        type(config_t), intent(in) :: config
        logical :: is_valid
        
        is_valid = .true.
        
        if (.not. allocated(config%diff_baseline_file)) then
            if (.not. config%quiet) then
                print *, "❌ Diff mode requires baseline file (--diff-baseline)"
            end if
            is_valid = .false.
            return
        end if
        
        if (.not. file_exists(config%diff_baseline_file)) then
            if (.not. config%quiet) then
                print *, "❌ Baseline file not found: " // trim(config%diff_baseline_file)
            end if
            is_valid = .false.
            return
        end if
        
    end function validate_baseline_file
    
    subroutine calculate_and_display_diff_stats(baseline_coverage, current_coverage, &
                                               config, coverage_delta)
        !! Calculates and displays diff statistics
        type(coverage_data_t), intent(in) :: baseline_coverage, current_coverage
        type(config_t), intent(in) :: config
        real, intent(out) :: coverage_delta
        
        type(coverage_stats_t) :: baseline_stats, current_stats
        integer :: lines_added
        
        ! Calculate statistics for comparison
        baseline_stats = calculate_line_coverage(baseline_coverage)
        current_stats = calculate_line_coverage(current_coverage)
        
        ! Display diff summary
        if (.not. config%quiet) then
            call display_diff_summary_header()
            call display_coverage_comparison(baseline_stats, current_stats, coverage_delta)
            call display_line_statistics(baseline_stats, current_stats, lines_added)
            call display_diff_summary_footer()
        end if
        
    end subroutine calculate_and_display_diff_stats
    
    subroutine display_diff_summary_header()
        !! Displays diff summary header
        print *, "=============================================="
        print *, "Coverage Diff Summary"
        print *, "=============================================="
        print *, ""
    end subroutine display_diff_summary_header
    
    subroutine display_coverage_comparison(baseline_stats, current_stats, coverage_delta)
        !! Displays coverage comparison metrics
        type(coverage_stats_t), intent(in) :: baseline_stats, current_stats
        real, intent(out) :: coverage_delta
        
        write(*, '(A,F6.2,A)') "Baseline coverage: ", baseline_stats%percentage, "%"
        write(*, '(A,F6.2,A)') "Current coverage:  ", current_stats%percentage, "%"
        
        coverage_delta = current_stats%percentage - baseline_stats%percentage
        if (coverage_delta > 0.0) then
            write(*, '(A,F6.2,A)') "Coverage change:   +", coverage_delta, "% 🚀"
        else if (coverage_delta < 0.0) then
            write(*, '(A,F6.2,A)') "Coverage change:   ", coverage_delta, "% ⚠️"
        else
            print *, "Coverage change:   No change"
        end if
        
        print *, ""
        
    end subroutine display_coverage_comparison
    
    subroutine display_line_statistics(baseline_stats, current_stats, lines_added)
        !! Displays line-by-line statistics
        type(coverage_stats_t), intent(in) :: baseline_stats, current_stats
        integer, intent(out) :: lines_added
        
        print *, "Line Statistics:"
        print *, "----------------"
        write(*, '(A,I0,A,I0)') "Baseline: ", baseline_stats%covered_count, &
                                 " / ", baseline_stats%total_count
        write(*, '(A,I0,A,I0)') "Current:  ", current_stats%covered_count, &
                                 " / ", current_stats%total_count
        
        lines_added = current_stats%covered_count - baseline_stats%covered_count
        if (lines_added > 0) then
            write(*, '(A,I0,A)') "New lines covered: +", lines_added, " ✓"
        else if (lines_added < 0) then
            write(*, '(A,I0,A)') "Lines lost: ", lines_added, " ❌"
        end if
        
    end subroutine display_line_statistics
    
    subroutine display_diff_summary_footer()
        !! Displays diff summary footer
        print *, "=============================================="
    end subroutine display_diff_summary_footer
    
    function determine_diff_exit_code(config, coverage_delta) result(exit_code)
        !! Determines appropriate exit code based on diff results
        type(config_t), intent(in) :: config
        real, intent(in) :: coverage_delta
        integer :: exit_code
        
        if (.not. config%quiet) then
            print *, "✅ Diff analysis completed"
            print *, "   Report saved to: ", trim(config%output_path)
        end if
        
        ! Check if coverage decreased (fail in strict mode)
        if (config%strict_mode .and. coverage_delta < 0.0) then
            if (.not. config%quiet) then
                print *, "❌ Coverage decreased in strict mode - failing"
            end if
            exit_code = EXIT_THRESHOLD_NOT_MET
        else
            exit_code = EXIT_SUCCESS
        end if
        
    end function determine_diff_exit_code
    
    subroutine load_baseline_coverage(baseline_file, coverage_data, load_error)
        !! Loads baseline coverage data from JSON file
        use file_utils, only: read_file_content
        character(len=*), intent(in) :: baseline_file
        type(coverage_data_t), intent(out) :: coverage_data
        logical, intent(out) :: load_error
        
        character(len=:), allocatable :: json_content
        logical :: file_error
        
        ! Read JSON file
        call read_file_content(baseline_file, json_content, file_error)
        if (file_error) then
            load_error = .true.
            return
        end if
        
        ! Parse JSON to coverage data
        call import_json_coverage_safe(json_content, coverage_data, load_error)
        
    end subroutine load_baseline_coverage
    
    subroutine generate_current_coverage(config, coverage_data, generation_error)
        !! Generates current coverage data by running normal analysis
        type(config_t), intent(in) :: config
        type(coverage_data_t), intent(out) :: coverage_data
        logical, intent(out) :: generation_error
        
        ! Placeholder implementation - would use coverage_analysis functions
        ! This creates a circular dependency that needs architectural redesign
        generation_error = .true.
        print *, "Current coverage generation not implemented in diff handler"
        
    end subroutine generate_current_coverage
    
    subroutine generate_diff_report(baseline_coverage, current_coverage, config)
        !! Generates diff report comparing baseline and current coverage
        type(coverage_data_t), intent(in) :: baseline_coverage, current_coverage
        type(config_t), intent(in) :: config
        
        type(coverage_diff_t) :: diff_result
        class(coverage_reporter_t), allocatable :: reporter
        logical :: creation_error, generation_success
        character(len=:), allocatable :: error_message
        
        ! Calculate diff
        diff_result = compute_coverage_diff(baseline_coverage, current_coverage)
        
        ! Create reporter for diff output
        call create_reporter(config%output_format, reporter, creation_error)
        if (creation_error) then
            return
        end if
        
        ! Generate diff report with diff data
        call reporter%generate_report(current_coverage, config%output_path, &
                                    generation_success, error_message, diff_result)
        
    end subroutine generate_diff_report
    
end module coverage_diff_handler