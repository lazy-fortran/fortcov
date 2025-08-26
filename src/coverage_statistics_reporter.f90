module coverage_statistics_reporter
    !! Coverage Statistics and Reporting Functions
    !! 
    !! Statistics calculation and report generation functions extracted from 
    !! coverage_analysis.f90 for maintainability and adherence to size limits 
    !! (Issue #333 - Patrick's review feedback).
    use foundation_constants
    use coverage_model
    use fortcov_config
    use coverage_statistics, only: coverage_stats_t, extended_coverage_stats_t
    use coverage_reporter
    use report_engine
    use error_handling
    use string_utils, only: int_to_string
    
    implicit none
    private
    
    public :: calculate_coverage_statistics
    public :: generate_coverage_reports
    public :: calculate_and_display_statistics
    public :: apply_threshold_validation
    public :: report_auto_test_failure
    public :: line_coverage_stats_t
    
    ! Type for combined line/branch stats
    type :: line_coverage_stats_t
        real :: percentage
        integer :: covered_lines
        integer :: total_lines
        real :: branch_percentage
        integer :: covered_branches
        integer :: total_branches
    end type line_coverage_stats_t
    
contains

    subroutine calculate_coverage_statistics(coverage_data, stats)
        !! Calculate coverage statistics from coverage data
        use coverage_statistics, only: calculate_line_coverage
        type(coverage_data_t), intent(in) :: coverage_data
        type(line_coverage_stats_t), intent(out) :: stats
        
        type(coverage_stats_t) :: line_stats, branch_stats
        
        ! Calculate line coverage
        line_stats = calculate_line_coverage(coverage_data)
        
        ! Calculate branch coverage (using line coverage function for now)
        branch_stats = calculate_line_coverage(coverage_data)
        
        ! Fill combined stats
        stats%percentage = line_stats%percentage
        stats%covered_lines = line_stats%covered_count
        stats%total_lines = line_stats%total_count
        stats%branch_percentage = 0.0  ! Placeholder
        stats%covered_branches = 0
        stats%total_branches = 0
        
    end subroutine calculate_coverage_statistics

    subroutine generate_coverage_reports(coverage_data, stats, config, report_error)
        !! Generate coverage reports in specified formats
        type(coverage_data_t), intent(in) :: coverage_data
        type(line_coverage_stats_t), intent(in) :: stats
        type(config_t), intent(in) :: config
        logical, intent(out) :: report_error
        
        report_error = .false.
        
        ! For now, just provide a simple success message
        ! Full report generation will be implemented separately
        if (.not. config%quiet) then
            select case (config%output_format)
            case ("html")
                write(*,'(A)') "HTML coverage report would be generated: " // trim(config%output_path)
            case ("markdown", "md")
                write(*,'(A)') "Markdown coverage report would be generated: " // trim(config%output_path)
            case ("json")
                write(*,'(A)') "JSON coverage report would be generated: " // trim(config%output_path)
            case ("terminal")
                write(*,'(A)') "Coverage report displayed in terminal"
            case default
                write(*,'(A)') "Coverage report would be generated"
            end select
        end if
        
    end subroutine generate_coverage_reports

    subroutine calculate_and_display_statistics(merged_coverage, config, line_stats)
        !! Calculate and display coverage statistics
        type(coverage_data_t), intent(in) :: merged_coverage
        type(config_t), intent(in) :: config
        type(line_coverage_stats_t), intent(out) :: line_stats
        
        call calculate_coverage_statistics(merged_coverage, line_stats)
        
        if (.not. config%quiet) then
            write(*,'(A)') ""
            write(*,'(A)') "Coverage Statistics:"
            write(*,'(A,F6.2,A)') "  Line Coverage: ", line_stats%percentage, "%"
            write(*,'(A,I0,A,I0,A)') "  Lines Covered: ", line_stats%covered_lines, &
                                    " of ", line_stats%total_lines, " lines"
            
            if (line_stats%total_branches > 0) then
                write(*,'(A,F6.2,A)') "  Branch Coverage: ", line_stats%branch_percentage, "%"
                write(*,'(A,I0,A,I0,A)') "  Branches Covered: ", line_stats%covered_branches, &
                                        " of ", line_stats%total_branches, " branches"
            end if
        end if
        
    end subroutine calculate_and_display_statistics

    function apply_threshold_validation(line_stats, config) result(exit_code)
        !! Apply threshold validation and return appropriate exit code
        type(line_coverage_stats_t), intent(in) :: line_stats
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        exit_code = EXIT_SUCCESS
        
        ! Check minimum coverage threshold
        if (config%minimum_coverage > 0.0) then
            if (line_stats%percentage < config%minimum_coverage) then
                if (.not. config%quiet) then
                    write(*,'(A)') ""
                    write(*,'(A,F6.2,A,F6.2,A)') "Warning: Coverage ", line_stats%percentage, &
                                                 "% is below minimum threshold of ", &
                                                 config%minimum_coverage, "%"
                end if
            end if
        end if
        
        ! Check fail-under threshold (this causes failure)
        if (config%fail_under_threshold > 0.0) then
            if (line_stats%percentage < config%fail_under_threshold) then
                if (.not. config%quiet) then
                    write(*,'(A)') ""
                    write(*,'(A,F6.2,A,F6.2,A)') "Error: Coverage ", line_stats%percentage, &
                                                 "% is below fail-under threshold of ", &
                                                 config%fail_under_threshold, "%"
                end if
                exit_code = EXIT_FAILURE
            end if
        end if
        
    end function apply_threshold_validation

    subroutine report_auto_test_failure(auto_test_exit_code)
        !! Report auto-test execution failure with helpful message
        integer, intent(in) :: auto_test_exit_code
        
        write(*,'(A)') ""
        write(*,'(A)') "Auto-test execution failed with exit code " // &
                      int_to_string(auto_test_exit_code)
        write(*,'(A)') ""
        write(*,'(A)') "Possible solutions:"
        write(*,'(A)') "  1. Check that test files exist and are executable"
        write(*,'(A)') "  2. Verify build system is properly configured"
        write(*,'(A)') "  3. Run tests manually first to check for errors"
        write(*,'(A)') "  4. Use --no-auto-test to skip automated testing"
        write(*,'(A)') ""
        
    end subroutine report_auto_test_failure

end module coverage_statistics_reporter