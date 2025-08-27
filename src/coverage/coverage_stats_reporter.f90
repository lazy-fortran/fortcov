module coverage_stats_reporter
    !! Coverage Statistics and Reporting Functions
    !! 
    !! Statistics calculation and report generation functions extracted from 
    !! coverage_analysis.f90 for maintainability and adherence to size limits 
    !! (Issue #333 - Patrick's review feedback).
    use constants_core
    use coverage_model_core
    use config_core
    use coverage_stats_core, only: coverage_stats_t, extended_coverage_stats_t
    use coverage_reporter
    use report_engine_core
    use error_handling_core
    use string_utils, only: int_to_string
    
    implicit none
    private
    
    public :: calculate_coverage_statistics
    public :: generate_coverage_reports
    public :: calculate_and_display_statistics
    public :: apply_threshold_validation
    public :: report_auto_test_failure
    public :: line_coverage_stats_t
    public :: suppress_unused_warning_stats
    
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
        use coverage_stats_core, only: calculate_line_coverage
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
        use coverage_reporter, only: coverage_reporter_t
        use coverage_reporter_factory, only: create_reporter
        use report_engine_core, only: report_engine_t
        type(coverage_data_t), intent(in) :: coverage_data
        type(line_coverage_stats_t), intent(in) :: stats
        type(config_t), intent(in) :: config
        logical, intent(out) :: report_error
        
        class(coverage_reporter_t), allocatable :: reporter
        type(report_engine_t) :: engine
        logical :: success, factory_error, engine_success
        character(len=:), allocatable :: error_message
        
        report_error = .false.
        
        ! Handle terminal output specially
        if (trim(config%output_format) == "terminal") then
            if (.not. config%quiet) then
                write(*,'(A)') "Coverage report displayed in terminal"
            end if
            return
        end if
        
        ! Handle HTML format using report engine
        if (trim(config%output_format) == "html") then
            call engine%init(engine_success, error_message)
            if (.not. engine_success) then
                if (.not. config%quiet) then
                    write(*,'(A)') "Error initializing HTML report engine: " // error_message
                end if
                report_error = .true.
                return
            end if
            
            ! Set the source data
            engine%source_data = coverage_data
            
            call engine%generate_html_report(config%output_path, engine_success, error_message)
            if (.not. engine_success) then
                if (.not. config%quiet) then
                    write(*,'(A)') "Error generating HTML report: " // error_message
                end if
                report_error = .true.
            else
                if (.not. config%quiet) then
                    write(*,'(A)') "HTML coverage report generated: " // trim(config%output_path)
                end if
            end if
            return
        end if
        
        ! Handle other formats using factory
        call create_reporter(config%output_format, reporter, factory_error)
        if (factory_error) then
            if (.not. config%quiet) then
                write(*,'(A)') "Error: Unsupported output format '" // &
                              trim(config%output_format) // "'"
            end if
            report_error = .true.
            return
        end if
        
        ! Generate the report
        call reporter%generate_report(coverage_data, config%output_path, success, error_message)
        if (.not. success) then
            if (.not. config%quiet) then
                write(*,'(A)') "Error generating " // trim(config%output_format) // &
                              " report: " // error_message
            end if
            report_error = .true.
        else
            if (.not. config%quiet) then
                select case (trim(config%output_format))
                case ("markdown", "md")
                    write(*,'(A)') "Markdown coverage report generated: " // trim(config%output_path)
                case ("json")
                    write(*,'(A)') "JSON coverage report generated: " // trim(config%output_path)
                case ("xml")
                    write(*,'(A)') "XML coverage report generated: " // trim(config%output_path)
                case default
                    write(*,'(A)') trim(config%output_format) // " coverage report generated: " // trim(config%output_path)
                end select
            end if
        end if
        
        ! Suppress unused parameter warning
        call suppress_unused_warning_stats(stats)
        
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

    subroutine suppress_unused_warning_stats(stats)
        !! Suppress unused parameter warning for stats
        type(line_coverage_stats_t), intent(in) :: stats
        ! This subroutine exists only to suppress compiler warnings
        ! The stats parameter is intentionally unused
        continue  ! Explicit no-op
    end subroutine suppress_unused_warning_stats

end module coverage_stats_reporter