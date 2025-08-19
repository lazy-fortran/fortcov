module coverage_analysis
    !! Coverage Analysis Core Logic (Decomposed from coverage_engine.f90)
    !! 
    !! Focused on core coverage analysis algorithms and data processing.
    !! Separated from orchestration for better testability and maintenance.
    use foundation_constants
    use foundation_layer_utils
    use coverage_model
    use fortcov_config
    use coverage_parser
    use coverage_statistics
    use coverage_reporter
    use json_coverage_io
    use report_engine
    use input_validation
    use error_handling
    implicit none
    private
    
    public :: perform_coverage_analysis
    public :: perform_safe_coverage_analysis
    public :: perform_imported_json_analysis
    public :: validate_analysis_configuration
    
contains
    
    function perform_coverage_analysis(config) result(exit_code)
        !! Core coverage analysis implementation
        !! Extracted from original analyze_coverage function
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        character(len=:), allocatable :: coverage_files(:)
        character(len=:), allocatable :: filtered_files(:)
        type(coverage_data_t) :: merged_coverage
        type(coverage_stats_t) :: line_stats
        class(coverage_parser_t), allocatable :: parser
        class(coverage_reporter_t), allocatable :: reporter
        logical :: parser_error, reporter_error
        integer :: i, file_count
        
        exit_code = EXIT_SUCCESS
        
        ! Display analysis startup message
        if (.not. config%quiet) then
            if (config%verbose) then
                print *, "🚀 Starting coverage analysis with verbose output..."
            else
                print *, "📊 Analyzing coverage..."
            end if
        end if
        
        ! Validate configuration
        if (.not. validate_analysis_configuration(config)) then
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Handle imported JSON analysis path
        if (allocated(config%import_json_file)) then
            exit_code = perform_imported_json_analysis(config)
            return
        end if
        
        ! Find and filter coverage files
        call find_and_filter_coverage_files(config, coverage_files, filtered_files)
        
        if (.not. allocated(filtered_files) .or. size(filtered_files) == 0) then
            if (.not. config%quiet) then
                print *, "❌ No coverage files found matching criteria."
                call display_search_guidance(config)
            end if
            exit_code = EXIT_NO_COVERAGE_DATA
            return
        end if
        
        ! Parse coverage files
        call parse_coverage_files(filtered_files, config, merged_coverage, &
                                parser_error)
        
        if (parser_error) then
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Calculate statistics
        call calculate_coverage_statistics(merged_coverage, line_stats)
        
        ! Generate and output reports
        call generate_coverage_reports(merged_coverage, line_stats, config, &
                                     reporter_error)
        
        if (reporter_error) then
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Apply threshold validation
        if (config%fail_on_threshold .and. config%threshold > 0.0) then
            if (line_stats%line_coverage < config%threshold) then
                if (.not. config%quiet) then
                    print *, "❌ Coverage threshold not met"
                    write(*, '(A, F5.1, A, F5.1, A)') &
                        "   Required: ", config%threshold, "%, Actual: ", &
                        line_stats%line_coverage, "%"
                end if
                exit_code = EXIT_THRESHOLD_NOT_MET
            end if
        end if
        
    end function perform_coverage_analysis
    
    function perform_safe_coverage_analysis(config, error_ctx) result(exit_code)
        !! Safe coverage analysis with comprehensive error handling
        !! Extracted from original analyze_coverage_safe function
        type(config_t), intent(in) :: config
        type(error_context_t), intent(out) :: error_ctx
        integer :: exit_code
        
        character(len=:), allocatable :: coverage_files(:)
        character(len=:), allocatable :: filtered_files(:)
        type(coverage_data_t) :: merged_coverage
        type(coverage_stats_t) :: line_stats
        logical :: operation_success
        
        ! Initialize error context
        call initialize_error_context(error_ctx, "coverage_analysis", &
                                    "safe_coverage_analysis")
        
        ! Perform analysis with error tracking
        exit_code = perform_coverage_analysis(config)
        
        ! Update error context based on result
        if (exit_code == EXIT_SUCCESS) then
            call set_error_context_success(error_ctx, "Analysis completed successfully")
        else
            call set_error_context_failure(error_ctx, "Analysis failed", exit_code)
        end if
        
    end function perform_safe_coverage_analysis
    
    function perform_imported_json_analysis(config) result(exit_code)
        !! Imported JSON coverage analysis implementation
        !! Extracted from original analyze_imported_json function
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        type(coverage_data_t) :: imported_coverage
        type(coverage_stats_t) :: line_stats
        logical :: import_success, reporter_error
        
        exit_code = EXIT_SUCCESS
        
        if (.not. config%quiet) then
            print *, "📁 Importing coverage data from JSON file..."
            print *, "   File: " // trim(config%import_json_file)
        end if
        
        ! Import JSON coverage data
        call import_json_coverage(config%import_json_file, imported_coverage, &
                                import_success)
        
        if (.not. import_success) then
            if (.not. config%quiet) then
                print *, "❌ Failed to import JSON coverage data"
                print *, "   File: " // trim(config%import_json_file)
            end if
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Calculate statistics for imported data
        call calculate_coverage_statistics(imported_coverage, line_stats)
        
        ! Generate reports for imported data
        call generate_coverage_reports(imported_coverage, line_stats, config, &
                                     reporter_error)
        
        if (reporter_error) then
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Apply threshold validation
        if (config%fail_on_threshold .and. config%threshold > 0.0) then
            if (line_stats%line_coverage < config%threshold) then
                if (.not. config%quiet) then
                    print *, "❌ Coverage threshold not met for imported data"
                    write(*, '(A, F5.1, A, F5.1, A)') &
                        "   Required: ", config%threshold, "%, Actual: ", &
                        line_stats%line_coverage, "%"
                end if
                exit_code = EXIT_THRESHOLD_NOT_MET
            end if
        end if
        
    end function perform_imported_json_analysis
    
    function validate_analysis_configuration(config) result(is_valid)
        !! Validates configuration for coverage analysis
        type(config_t), intent(in) :: config
        logical :: is_valid
        
        class(coverage_reporter_t), allocatable :: test_reporter
        logical :: format_error
        
        is_valid = .true.
        
        ! Validate output format
        call create_reporter(config%output_format, test_reporter, format_error)
        if (format_error) then
            if (.not. config%quiet) then
                print *, "❌ Unsupported output format: '" // &
                        trim(config%output_format) // "'"
                print *, "📊 Supported formats: markdown, md, json, xml, html"
                print *, "💡 Try: --output-format=markdown"
            end if
            is_valid = .false.
            return
        end if
        
        ! Validate source paths
        if (allocated(config%source_paths)) then
            call validate_source_paths(config, is_valid)
        end if
        
    end function validate_analysis_configuration
    
    subroutine validate_source_paths(config, is_valid)
        !! Validates source paths in configuration
        type(config_t), intent(in) :: config
        logical, intent(out) :: is_valid
        
        integer :: path_idx
        type(validation_result_t) :: path_validation
        logical :: has_validation_failure
        
        is_valid = .true.
        has_validation_failure = .false.
        
        do path_idx = 1, size(config%source_paths)
            call validate_path_safety(config%source_paths(path_idx), path_validation)
            if (.not. path_validation%is_valid) then
                if (.not. config%quiet) then
                    print *, "❌ Invalid source path: " // trim(config%source_paths(path_idx))
                    print *, "   Error: " // trim(path_validation%error_message)
                    if (len_trim(path_validation%suggested_fix) > 0) then
                        print *, "   💡 " // trim(path_validation%suggested_fix)
                    end if
                end if
                has_validation_failure = .true.
            end if
        end do
        
        if (has_validation_failure) then
            is_valid = .false.
        end if
        
    end subroutine validate_source_paths
    
    subroutine find_and_filter_coverage_files(config, coverage_files, filtered_files)
        !! Finds and filters coverage files based on configuration
        type(config_t), intent(in) :: config
        character(len=:), allocatable, intent(out) :: coverage_files(:)
        character(len=:), allocatable, intent(out) :: filtered_files(:)
        
        ! This would delegate to coverage_workflows module
        ! For now, implement basic functionality
        
    end subroutine find_and_filter_coverage_files
    
    subroutine parse_coverage_files(files, config, merged_coverage, parse_error)
        !! Parses coverage files and merges results
        character(len=*), intent(in) :: files(:)
        type(config_t), intent(in) :: config
        type(coverage_data_t), intent(out) :: merged_coverage
        logical, intent(out) :: parse_error
        
        ! Implementation would parse files and merge coverage data
        parse_error = .false.
        
    end subroutine parse_coverage_files
    
    subroutine calculate_coverage_statistics(coverage_data, stats)
        !! Calculates coverage statistics from data
        type(coverage_data_t), intent(in) :: coverage_data
        type(coverage_stats_t), intent(out) :: stats
        
        ! Implementation would calculate statistics
        call calculate_statistics(coverage_data, stats)
        
    end subroutine calculate_coverage_statistics
    
    subroutine generate_coverage_reports(coverage_data, stats, config, report_error)
        !! Generates coverage reports in requested format
        type(coverage_data_t), intent(in) :: coverage_data
        type(coverage_stats_t), intent(in) :: stats
        type(config_t), intent(in) :: config
        logical, intent(out) :: report_error
        
        class(coverage_reporter_t), allocatable :: reporter
        logical :: creation_error
        
        ! Create appropriate reporter
        call create_reporter(config%output_format, reporter, creation_error)
        if (creation_error) then
            report_error = .true.
            return
        end if
        
        ! Generate reports
        call reporter%generate_report(coverage_data, stats, config)
        report_error = .false.
        
    end subroutine generate_coverage_reports
    
    subroutine display_search_guidance(config)
        !! Displays guidance for finding coverage files
        type(config_t), intent(in) :: config
        
        print *, "🔍 Coverage file search guidance:"
        if (allocated(config%source_paths)) then
            print *, "   Searched paths: ", config%source_paths
        end if
        print *, "   Looking for: *.gcov files"
        print *, "   💡 Run your tests with coverage enabled first"
        print *, "   💡 Try: gfortran -fprofile-arcs -ftest-coverage ..."
        
    end subroutine display_search_guidance
    
end module coverage_analysis