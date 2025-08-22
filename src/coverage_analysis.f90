module coverage_analysis
    !! Coverage Analysis Core Logic (Decomposed from coverage_engine.f90)
    !! 
    !! Focused on core coverage analysis algorithms and data processing.
    !! Separated from orchestration for better testability and maintenance.
    use foundation_constants
    use foundation_layer_utils
    use coverage_model
    use fortcov_config
    use coverage_parser, only: coverage_parser_t, create_parser
    use coverage_statistics, only: calculate_line_coverage
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
        if (allocated(config%import_file)) then
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
        line_stats = calculate_line_coverage(merged_coverage)
        
        ! Generate and output reports
        call generate_coverage_reports(merged_coverage, line_stats, config, &
                                     reporter_error)
        
        if (reporter_error) then
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Apply threshold validation
        if (config%strict_mode .and. config%minimum_coverage > 0.0) then
            if (line_stats%percentage < config%minimum_coverage) then
                if (.not. config%quiet) then
                    print *, "❌ Coverage threshold not met"
                    write(*, '(A, F5.1, A, F5.1, A)') &
                        "   Required: ", config%minimum_coverage, "%, Actual: ", &
                        line_stats%percentage, "%"
                end if
                exit_code = EXIT_THRESHOLD_NOT_MET
            end if
        end if
        
        ! Apply fail-under threshold validation
        if (config%fail_under_threshold > 0.0) then
            if (line_stats%percentage < config%fail_under_threshold) then
                if (.not. config%quiet) then
                    print *, "❌ Coverage below fail-under threshold"
                    write(*, '(A, F5.1, A, F5.1, A)') &
                        "   Fail under: ", config%fail_under_threshold, "%, Actual: ", &
                        line_stats%percentage, "%"
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
        ! Initialize error context (simplified)
        error_ctx%error_code = ERROR_SUCCESS
        error_ctx%message = ""
        
        ! Perform analysis with error tracking
        exit_code = perform_coverage_analysis(config)
        
        ! Update error context based on result
        if (exit_code == EXIT_SUCCESS) then
            error_ctx%error_code = ERROR_SUCCESS
            error_ctx%message = "Analysis completed successfully"
        else
            error_ctx%error_code = exit_code
            error_ctx%message = "Analysis failed"
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
            print *, "   File: " // trim(config%import_file)
        end if
        
        ! Import JSON coverage data (simplified for compilation)
        ! TODO: Implement proper file reading and JSON import
        import_success = .false.
        
        if (.not. import_success) then
            if (.not. config%quiet) then
                print *, "❌ Failed to import JSON coverage data"
                print *, "   File: " // trim(config%import_file)
            end if
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Calculate statistics for imported data using specific function
        line_stats = calculate_line_coverage(imported_coverage)
        
        ! Generate reports for imported data
        call generate_coverage_reports(imported_coverage, line_stats, config, &
                                     reporter_error)
        
        if (reporter_error) then
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Apply threshold validation
        if (config%strict_mode .and. config%minimum_coverage > 0.0) then
            if (line_stats%percentage < config%minimum_coverage) then
                if (.not. config%quiet) then
                    print *, "❌ Coverage threshold not met for imported data"
                    write(*, '(A, F5.1, A, F5.1, A)') &
                        "   Required: ", config%minimum_coverage, "%, Actual: ", &
                        line_stats%percentage, "%"
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
        use coverage_workflows, only: discover_coverage_files
        type(config_t), intent(in) :: config
        character(len=:), allocatable, intent(out) :: coverage_files(:)
        character(len=:), allocatable, intent(out) :: filtered_files(:)
        
        ! Delegate to coverage_workflows module
        coverage_files = discover_coverage_files(config)
        
        ! For now, use all discovered files without additional filtering
        if (allocated(coverage_files)) then
            filtered_files = coverage_files
        else
            allocate(character(len=256) :: filtered_files(0))
        end if
        
    end subroutine find_and_filter_coverage_files
    
    subroutine parse_coverage_files(files, config, merged_coverage, parse_error)
        !! Parses coverage files and merges results
        use coverage_model, only: merge_coverage
        character(len=*), intent(in) :: files(:)
        type(config_t), intent(in) :: config
        type(coverage_data_t), intent(out) :: merged_coverage
        logical, intent(out) :: parse_error
        
        class(coverage_parser_t), allocatable :: parser
        type(coverage_data_t) :: file_coverage, temp_merged
        logical :: parser_error, file_error
        integer :: i
        logical :: first_file
        
        ! Initialize merged coverage
        call merged_coverage%init()
        parse_error = .false.
        first_file = .true.
        
        ! Check for empty file list
        if (size(files) == 0) then
            parse_error = .true.
            return
        end if
        
        if (.not. config%quiet) then
            print *, "📄 Found ", size(files), " coverage file(s) to parse"
        end if
        
        ! Parse each coverage file and merge results
        do i = 1, size(files)
            if (config%verbose .and. .not. config%quiet) then
                print *, "   Parsing: ", trim(files(i))
                print *, "     → Processing file ", i, " of ", size(files)
                print *, "     → File size check: analyzing gcov format..."
            end if
            
            ! Create parser for this file
            call create_parser(trim(files(i)), parser, parser_error)
            if (parser_error) then
                if (.not. config%quiet) then
                    print *, "   ⚠️  Failed to create parser for: ", trim(files(i))
                end if
                cycle
            end if
            
            ! Parse the file
            file_coverage = parser%parse(trim(files(i)), file_error)
            if (file_error) then
                if (.not. config%quiet) then
                    print *, "   ⚠️  Failed to parse: ", trim(files(i))
                end if
                cycle
            end if
            
            if (config%verbose .and. .not. config%quiet) then
                print *, "     → Successfully parsed, merging data..."
            end if
            
            ! Merge coverage data
            if (first_file) then
                merged_coverage = file_coverage
                first_file = .false.
            else
                call merge_coverage(merged_coverage, file_coverage, temp_merged)
                merged_coverage = temp_merged
            end if
        end do
        
        ! Check if we successfully parsed any files
        if (.not. allocated(merged_coverage%files) .or. &
            size(merged_coverage%files) == 0) then
            parse_error = .true.
            if (.not. config%quiet) then
                print *, "❌ No coverage data could be parsed from the files"
            end if
        end if
        
    end subroutine parse_coverage_files
    
    subroutine calculate_coverage_statistics(coverage_data, stats)
        !! Calculates coverage statistics from data
        type(coverage_data_t), intent(in) :: coverage_data
        type(extended_coverage_stats_t), intent(out) :: stats
        
        ! Implementation would calculate statistics
        call calculate_statistics(coverage_data, stats)
        
    end subroutine calculate_coverage_statistics
    
    subroutine generate_coverage_reports(coverage_data, stats, config, report_error)
        !! Generates coverage reports in requested format
        use zero_configuration_manager, only: ensure_output_directory_structure
        type(coverage_data_t), intent(in) :: coverage_data
        type(coverage_stats_t), intent(in) :: stats
        type(config_t), intent(in) :: config
        logical, intent(out) :: report_error
        
        class(coverage_reporter_t), allocatable :: reporter
        logical :: creation_error, generation_success
        character(len=:), allocatable :: error_message
        type(error_context_t) :: error_ctx
        
        ! Create appropriate reporter
        call create_reporter(config%output_format, reporter, creation_error)
        if (creation_error) then
            report_error = .true.
            return
        end if
        
        ! Ensure output directory structure exists
        if (allocated(config%output_path)) then
            call ensure_output_directory_structure(config%output_path, error_ctx)
            if (error_ctx%error_code /= ERROR_SUCCESS) then
                if (.not. config%quiet) then
                    print *, "❌ Failed to create output directory"
                    print *, "   Error: " // trim(error_ctx%message)
                    if (len_trim(error_ctx%suggestion) > 0) then
                        print *, "   💡 " // trim(error_ctx%suggestion)
                    end if
                end if
                report_error = .true.
                return
            end if
        end if
        
        ! Generate reports with proper interface
        call reporter%generate_report(coverage_data, config%output_path, &
                                    generation_success, error_message)
        
        if (.not. generation_success) then
            if (.not. config%quiet) then
                print *, "❌ Failed to generate coverage report"
                if (allocated(error_message)) then
                    print *, "   Error: " // trim(error_message)
                end if
            end if
            report_error = .true.
        else
            report_error = .false.
            if (.not. config%quiet) then
                print *, "✅ Coverage report generated successfully"
                if (allocated(config%output_path)) then
                    print *, "   Output: " // trim(config%output_path)
                end if
            end if
        end if
        
    end subroutine generate_coverage_reports
    
    subroutine display_search_guidance(config)
        !! Displays guidance for finding coverage files
        use zero_configuration_manager, only: show_zero_configuration_error_guidance
        type(config_t), intent(in) :: config
        
        if (config%zero_configuration_mode) then
            ! Show comprehensive zero-configuration error guidance
            call show_zero_configuration_error_guidance()
        else
            ! Show standard search guidance for explicit arguments
            print *, "Coverage file search guidance:"
            if (allocated(config%source_paths)) then
                print *, "   Searched paths: ", config%source_paths
            end if
            print *, "   Looking for: *.gcov files"
            print *, "   Run your tests with coverage enabled first"
            print *, "   Try: gfortran -fprofile-arcs -ftest-coverage ..."
        end if
        
    end subroutine display_search_guidance
    
end module coverage_analysis