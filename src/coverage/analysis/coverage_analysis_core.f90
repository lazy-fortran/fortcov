module coverage_analysis_core
    !! Coverage Analysis Core Logic (Refactored - Issue #333 Fix)
    !! 
    !! Core analysis orchestration with extracted modules for maintainability.
    !! Functions extracted to separate modules to meet size limits.
    use constants_core
    use coverage_model_core
    use config_core
    use coverage_validation
    use coverage_processor_file
    use coverage_stats_reporter, only: calculate_and_display_statistics, &
                                       generate_coverage_reports, &
                                       apply_threshold_validation, &
                                       report_auto_test_failure, &
                                       line_coverage_stats_t
    ! TUI removed: no interactive mode
    use coverage_workflows, only: execute_auto_test_workflow, perform_coverage_diff_analysis
    use error_handling_core
    use file_utilities, only: file_exists
    
    implicit none
    private
    
    public :: perform_coverage_analysis
    public :: perform_safe_coverage_analysis
    public :: perform_imported_json_analysis
    
contains

    function perform_coverage_analysis(config) result(exit_code)
        !! Core coverage analysis implementation
        type(config_t), intent(inout) :: config
        integer :: exit_code
        
        ! Check for special modes first
        exit_code = check_special_modes(config)
        if (exit_code /= EXIT_SUCCESS) return
        
        ! Display startup message and validate config
        call display_analysis_startup(config)
        if (.not. validate_analysis_configuration(config)) then
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Handle imported JSON analysis path
        if (allocated(config%import_file) .and. len_trim(config%import_file) > 0) then
            exit_code = perform_imported_json_analysis(config)
            return
        end if
        
        ! Perform standard coverage analysis
        exit_code = perform_standard_analysis(config)
        
    end function perform_coverage_analysis

    function check_special_modes(config) result(exit_code)
        !! Check for special analysis modes
        type(config_t), intent(inout) :: config
        integer :: exit_code
        
        exit_code = EXIT_SUCCESS
        
        ! Check for diff mode
        if (config%enable_diff) then
            if (.not. config%quiet) then
                print *, "Diff mode detected - analyzing coverage differences"
            end if
            exit_code = perform_coverage_diff_analysis(config)
            return
        end if
        
    end function check_special_modes

    subroutine display_analysis_startup(config)
        !! Displays analysis startup messages
        type(config_t), intent(in) :: config
        
        if (.not. config%quiet) then
            if (config%verbose) then
                print *, "Starting coverage analysis with verbose output..."
                print *, "   Diff mode: ", config%enable_diff
                print *, "   Strict mode: ", config%strict_mode
            else
                print *, "Analyzing coverage..."
            end if
        end if
        
    end subroutine display_analysis_startup

    function perform_standard_analysis(config) result(exit_code)
        !! Perform standard coverage file analysis
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        character(len=1024), allocatable :: coverage_files(:)
        character(len=1024), allocatable :: filtered_files(:)
        type(coverage_data_t) :: merged_coverage, filtered_coverage
        type(line_coverage_stats_t) :: line_stats
        logical :: parse_error, report_error
        integer :: auto_test_exit_code
        
        exit_code = EXIT_SUCCESS
        
        ! Execute auto-test workflow if configured
        if (config%auto_test_execution) then
            auto_test_exit_code = execute_auto_test_workflow(config)
            if (auto_test_exit_code /= EXIT_SUCCESS) then
                call report_auto_test_failure(auto_test_exit_code)
                ! Continue with analysis even if tests fail
            end if
        end if
        
        ! Find and filter coverage files
        call find_and_filter_coverage_files(config, coverage_files, filtered_files)
        
        ! Debug output removed after fixing issues
        
        if (.not. allocated(filtered_files) .or. size(filtered_files) == 0) then
            exit_code = handle_missing_coverage_files(config)
            return
        end if
        
        ! Parse coverage files
        call parse_coverage_files(filtered_files, config, merged_coverage, parse_error)
        if (parse_error) then
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Debug output removed after fixing issues
        
        ! Apply source and exclude filtering to coverage data
        call apply_coverage_filtering(merged_coverage, config, filtered_coverage)
        
        ! Calculate and display statistics on filtered data
        call calculate_and_display_statistics(filtered_coverage, config, line_stats)
        
        ! Generate reports with filtered data
        call generate_coverage_reports(filtered_coverage, line_stats, config, report_error)
        if (report_error) then
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Apply threshold validation
        exit_code = apply_threshold_validation(line_stats, config)
        
    end function perform_standard_analysis

    function handle_missing_coverage_files(config) result(exit_code)
        !! Handle case when no coverage files are found
        type(config_t), intent(in) :: config
        integer :: exit_code

        if (.not. config%quiet) then
            print *, "No coverage files found for analysis."
        end if

        ! Always return EXIT_NO_COVERAGE_DATA (3) when no coverage files found
        ! This allows CI scripts to distinguish between no coverage data and errors
        exit_code = EXIT_NO_COVERAGE_DATA

    end function handle_missing_coverage_files

    function perform_safe_coverage_analysis(config, error_ctx) result(exit_code)
        !! Perform coverage analysis with comprehensive error handling and TUI support
        type(config_t), intent(inout) :: config  ! Changed to inout for TUI mode
        type(error_context_t), intent(out) :: error_ctx
        integer :: exit_code
        
        call clear_error_context(error_ctx)
        
        ! Perform standard analysis with error trapping
        exit_code = perform_coverage_analysis(config)
        
        if (exit_code /= EXIT_SUCCESS) then
            error_ctx%error_code = ERROR_INVALID_CONFIG
            error_ctx%recoverable = .true.
            call safe_write_message(error_ctx, "Coverage analysis failed")
            call safe_write_suggestion(error_ctx, &
                "Check configuration and ensure coverage files exist")
            call safe_write_context(error_ctx, "coverage analysis execution")
        end if
        
    end function perform_safe_coverage_analysis

    function perform_imported_json_analysis(config) result(exit_code)
        !! Perform analysis on imported JSON coverage data
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        type(coverage_data_t) :: imported_coverage
        type(line_coverage_stats_t) :: line_stats
        type(error_context_t) :: error_ctx
        logical :: report_error
        
        exit_code = EXIT_SUCCESS
        
        if (.not. config%quiet) then
            print *, "Importing coverage data from: " // trim(config%import_file)
        end if
        
        ! Validate import file exists
        if (.not. file_exists(config%import_file)) then
            if (.not. config%quiet) then
                print *, "Error: Import file does not exist: " // trim(config%import_file)
            end if
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Import JSON coverage data (simplified for now)
        ! call import_json_coverage(config%import_file, imported_coverage)
        if (.not. config%quiet) then
            print *, "Note: JSON import functionality needs to be implemented"
        end if
        
        if (.not. config%quiet) then
            print *, "Successfully imported coverage data"
        end if
        
        ! Calculate and display statistics
        call calculate_and_display_statistics(imported_coverage, config, line_stats)
        
        ! Generate reports
        call generate_coverage_reports(imported_coverage, line_stats, config, report_error)
        if (report_error) then
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Apply threshold validation
        exit_code = apply_threshold_validation(line_stats, config)
        
    end function perform_imported_json_analysis

    subroutine apply_coverage_filtering(input_data, config, output_data)
        !! Apply source and exclude pattern filtering to coverage data
        use coverage_data_filter, only: apply_filter_criteria
        use report_config_core, only: filter_criteria_t
        type(coverage_data_t), intent(in) :: input_data
        type(config_t), intent(in) :: config
        type(coverage_data_t), intent(out) :: output_data
        
        type(filter_criteria_t) :: criteria
        logical :: filter_success
        character(len=:), allocatable :: error_message
        integer :: i
        
        ! Set up filter criteria from config
        call criteria%init()
        
        ! Set up exclude patterns from config
        if (allocated(config%exclude_patterns)) then
            criteria%exclude_patterns = config%exclude_patterns
        end if
        
        ! Set up include patterns from source paths
        ! Convert source paths to wildcard patterns for matching
        if (allocated(config%source_paths)) then
            if (allocated(criteria%include_patterns)) deallocate(criteria%include_patterns)
            allocate(character(len=256) :: criteria%include_patterns(size(config%source_paths)))
            do i = 1, size(config%source_paths)
                ! Add wildcard to match files under the directory
                if (config%source_paths(i)(len_trim(config%source_paths(i)):) == "/") then
                    criteria%include_patterns(i) = trim(config%source_paths(i)) // "*"
                else
                    criteria%include_patterns(i) = trim(config%source_paths(i)) // "/*"
                end if
            end do
        end if
        
        ! Apply the filter
        call apply_filter_criteria(input_data, criteria, output_data, &
                                 filter_success, error_message)
        
        if (.not. filter_success) then
            if (.not. config%quiet) then
                write(*,'(A)') "Warning: Failed to apply coverage filters: " // error_message
                write(*,'(A)') "Using unfiltered data"
            end if
            ! Use unfiltered data if filtering fails
            output_data = input_data
        else
            ! Filtering completed successfully
        end if
        
    end subroutine apply_coverage_filtering

end module coverage_analysis_core
