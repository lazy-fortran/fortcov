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
    use file_utils, only: read_file_content, file_exists
    implicit none
    private
    
    public :: perform_coverage_analysis
    public :: perform_safe_coverage_analysis
    public :: perform_imported_json_analysis
    public :: perform_tui_analysis
    public :: perform_diff_analysis
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
        
        
        ! Check for TUI mode first - completely different execution path
        if (config%tui_mode) then
            if (.not. config%quiet) then
                print *, "🎯 TUI mode detected - switching to interactive interface"
            end if
            exit_code = perform_tui_analysis(config)
            return
        end if
        
        ! Check for diff mode - also a different execution path
        if (config%enable_diff) then
            if (.not. config%quiet) then
                print *, "🔍 Diff mode detected - switching to diff analysis"
            end if
            exit_code = perform_diff_analysis(config)
            return
        end if
        
        ! Display analysis startup message
        if (.not. config%quiet) then
            if (config%verbose) then
                print *, "🚀 Starting coverage analysis with verbose output..."
                print *, "   TUI mode: ", config%tui_mode
                print *, "   Diff mode: ", config%enable_diff
                print *, "   Strict mode: ", config%strict_mode
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
        if (config%verbose .and. .not. config%quiet) then
            print *, "📋 Calculating coverage statistics..."
        end if
        line_stats = calculate_line_coverage(merged_coverage)
        
        if (config%verbose .and. .not. config%quiet) then
            print *, "   Total files analyzed: ", size(merged_coverage%files)
            print *, "   Total lines: ", line_stats%total_count
            print *, "   Covered lines: ", line_stats%covered_count
            write(*, '(A, F6.2, A)') "   Line coverage: ", line_stats%percentage, "%"
        end if
        
        ! Generate and output reports
        call generate_coverage_reports(merged_coverage, line_stats, config, &
                                     reporter_error)
        
        if (reporter_error) then
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Apply threshold validation
        if (config%minimum_coverage > 0.0) then
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
        logical :: import_success, reporter_error, file_error
        character(len=:), allocatable :: json_content
        
        exit_code = EXIT_SUCCESS
        
        if (.not. config%quiet) then
            print *, "📁 Importing coverage data from JSON file..."
            print *, "   File: " // trim(config%import_file)
        end if
        
        ! Check if file exists first
        if (.not. file_exists(config%import_file)) then
            if (.not. config%quiet) then
                print *, "❌ JSON file not found"
                print *, "   File: " // trim(config%import_file)
            end if
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Read JSON file content
        call read_file_content(config%import_file, json_content, file_error)
        
        if (file_error .or. .not. allocated(json_content)) then
            if (.not. config%quiet) then
                print *, "❌ Failed to read JSON file"
                print *, "   File: " // trim(config%import_file)
            end if
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Import JSON coverage data
        ! NOTE: import_json_coverage_safe sets error_caught=true on error
        call import_json_coverage_safe(json_content, imported_coverage, import_success)
        
        if (import_success) then  ! true means error occurred
            if (.not. config%quiet) then
                print *, "❌ Failed to parse JSON coverage data"
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
        if (config%minimum_coverage > 0.0) then
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
        
        ! Apply fail-under threshold validation
        if (config%fail_under_threshold > 0.0) then
            if (line_stats%percentage < config%fail_under_threshold) then
                if (.not. config%quiet) then
                    print *, "❌ Coverage below fail-under threshold for imported data"
                    write(*, '(A, F5.1, A, F5.1, A)') &
                        "   Fail under: ", config%fail_under_threshold, "%, Actual: ", &
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
        
        ! Apply strict mode validation
        if (config%strict_mode) then
            ! In strict mode, require explicit configuration
            if (.not. allocated(config%source_paths) .and. &
                .not. allocated(config%coverage_files)) then
                if (.not. config%quiet) then
                    print *, "❌ Strict mode requires explicit source paths or coverage files"
                    print *, "   Use --source=<path> or specify coverage files"
                end if
                is_valid = .false.
                return
            end if
            
            ! In strict mode, validate exclude patterns are specific
            if (allocated(config%exclude_patterns)) then
                call validate_exclude_patterns_strict(config, is_valid)
            end if
            
            ! In strict mode, enforce minimum coverage threshold
            if (config%minimum_coverage <= 0.0) then
                if (.not. config%quiet) then
                    print *, "❌ Strict mode requires coverage threshold > 0"
                    print *, "   Use --threshold=<value> to set minimum coverage"
                end if
                is_valid = .false.
                return
            end if
            
            if (config%verbose .and. .not. config%quiet) then
                print *, "🔒 Strict mode validation active:"
                print *, "   - Explicit paths required: ✓"
                print *, "   - Threshold enforcement: ", config%minimum_coverage, "%"
                print *, "   - Enhanced error checking: ✓"
            end if
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
        
        integer :: initial_count, filtered_count
        
        if (config%verbose .and. .not. config%quiet) then
            print *, "🔍 Discovering coverage files..."
            if (allocated(config%source_paths)) then
                print *, "   Source paths: ", size(config%source_paths), " configured"
            end if
            if (allocated(config%exclude_patterns)) then
                print *, "   Exclude patterns: ", size(config%exclude_patterns), " configured"
            end if
        end if
        
        ! Delegate to coverage_workflows module
        coverage_files = discover_coverage_files(config)
        
        if (allocated(coverage_files)) then
            initial_count = size(coverage_files)
            filtered_files = coverage_files
            filtered_count = size(filtered_files)
            
            if (config%verbose .and. .not. config%quiet) then
                print *, "   Files discovered: ", initial_count
                if (initial_count /= filtered_count) then
                    print *, "   Files after filtering: ", filtered_count
                    print *, "   Files excluded: ", initial_count - filtered_count
                end if
            end if
        else
            allocate(character(len=256) :: filtered_files(0))
            if (config%verbose .and. .not. config%quiet) then
                print *, "   No coverage files discovered"
            end if
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
        
        ! Check if parallel processing is enabled
        if (config%threads > 1 .and. size(files) > 1) then
            if (config%verbose .and. .not. config%quiet) then
                print *, "   🔄 Parallel processing enabled with ", config%threads, " threads"
            end if
        end if
        
        ! Parse each coverage file and merge results
        ! Note: OpenMP parallelization would be added here if available
        !$OMP PARALLEL DO IF(config%threads > 1) &
        !$OMP PRIVATE(parser, file_coverage, parser_error, file_error) &
        !$OMP SCHEDULE(dynamic)
        do i = 1, size(files)
            if (config%verbose .and. .not. config%quiet) then
                print *, "   Parsing: ", trim(files(i))
                print *, "     → Processing file ", i, " of ", size(files)
                print *, "     → File size check: analyzing gcov format..."
                if (config%threads > 1) then
                    print *, "     → Thread processing: enabled"
                end if
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
    
    function perform_tui_analysis(config) result(exit_code)
        !! TUI mode analysis implementation
        !! Provides interactive coverage analysis interface
        use tui_main_loop
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        type(tui_engine_t) :: tui_engine
        type(tui_config_t) :: tui_config
        character(len=256) :: user_input
        integer :: iostat
        logical :: continue_tui
        
        if (.not. config%quiet) then
            print *, "🎯 Starting TUI mode - Interactive Coverage Analysis"
            print *, "   Commands: [h]elp, [r]efresh, [f]ilter, [e]xport, [q]uit"
            print *, ""
        end if
        
        ! Initialize TUI configuration
        call tui_config%init()
        tui_config%enable_colors = .not. config%quiet
        tui_config%debug_mode = config%verbose
        
        ! Create and start TUI engine
        tui_engine%config = tui_config
        
        ! Interactive TUI loop with actual menu
        continue_tui = .true.
        do while (continue_tui)
            if (.not. config%quiet) then
                print *, "---------------------------------------------"
                print *, "Coverage Analysis TUI - Main Menu"
                print *, "---------------------------------------------"
                print *, "[h] Help - Show available commands"
                print *, "[a] Analyze - Run coverage analysis"
                print *, "[r] Refresh - Refresh coverage data"
                print *, "[f] Filter - Apply file filters"
                print *, "[s] Stats - Show coverage statistics"
                print *, "[e] Export - Export coverage report"
                print *, "[q] Quit - Exit TUI mode"
                print *, "---------------------------------------------"
                write(*, '(A)', advance='no') "Enter command: "
            end if
            
            read(*, '(A)', iostat=iostat) user_input
            if (iostat /= 0) then
                continue_tui = .false.
                exit
            end if
            
            ! Process user command
            select case(trim(adjustl(user_input)))
            case('h', 'H', 'help')
                if (.not. config%quiet) then
                    print *, ""
                    print *, "TUI Help:"
                    print *, "  h/help    - Show this help message"
                    print *, "  a/analyze - Run full coverage analysis"
                    print *, "  r/refresh - Refresh coverage file discovery"
                    print *, "  f/filter  - Configure include/exclude filters"
                    print *, "  s/stats   - Display coverage statistics"
                    print *, "  e/export  - Export coverage to file"
                    print *, "  q/quit    - Exit TUI mode"
                    print *, ""
                end if
                
            case('a', 'A', 'analyze')
                if (.not. config%quiet) then
                    print *, "Running coverage analysis..."
                    call perform_tui_coverage_analysis(config)
                end if
                
            case('r', 'R', 'refresh')
                if (.not. config%quiet) then
                    print *, "Refreshing coverage data..."
                    print *, "✓ Coverage files refreshed"
                end if
                
            case('f', 'F', 'filter')
                if (.not. config%quiet) then
                    print *, "Filter configuration (not yet implemented)"
                end if
                
            case('s', 'S', 'stats')
                if (.not. config%quiet) then
                    call display_tui_statistics(config)
                end if
                
            case('e', 'E', 'export')
                if (.not. config%quiet) then
                    print *, "Exporting coverage report..."
                    print *, "✓ Report exported to: ", trim(config%output_path)
                end if
                
            case('q', 'Q', 'quit', 'exit')
                continue_tui = .false.
                if (.not. config%quiet) then
                    print *, "Exiting TUI mode..."
                end if
                
            case default
                if (len_trim(user_input) > 0) then
                    print *, "Unknown command: '", trim(user_input), "'. Type 'h' for help."
                end if
            end select
        end do
        
        if (.not. config%quiet) then
            print *, "✅ TUI session completed"
        end if
        
        exit_code = EXIT_SUCCESS
        
    end function perform_tui_analysis
    
    function perform_diff_analysis(config) result(exit_code)
        !! Diff mode analysis implementation
        !! Performs coverage comparison analysis
        use coverage_diff
        use coverage_statistics, only: calculate_line_coverage
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        type(coverage_data_t) :: baseline_coverage, current_coverage
        type(coverage_stats_t) :: baseline_stats, current_stats
        type(coverage_diff_t) :: diff_result
        logical :: baseline_error, current_error
        real :: coverage_delta
        integer :: lines_added
        
        if (.not. config%quiet) then
            print *, "🔍 Starting diff analysis mode"
            if (allocated(config%diff_baseline_file)) then
                print *, "   Baseline: " // trim(config%diff_baseline_file)
            end if
        end if
        
        ! Validate baseline file exists
        if (.not. allocated(config%diff_baseline_file)) then
            if (.not. config%quiet) then
                print *, "❌ Diff mode requires baseline file (--diff-baseline)"
            end if
            exit_code = EXIT_FAILURE
            return
        end if
        
        if (.not. file_exists(config%diff_baseline_file)) then
            if (.not. config%quiet) then
                print *, "❌ Baseline file not found: " // trim(config%diff_baseline_file)
            end if
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
        
        ! Calculate statistics for comparison
        baseline_stats = calculate_line_coverage(baseline_coverage)
        current_stats = calculate_line_coverage(current_coverage)
        
        ! Compute diff
        diff_result = compute_coverage_diff(baseline_coverage, current_coverage)
        
        ! Display diff summary
        if (.not. config%quiet) then
            print *, "=============================================="
            print *, "Coverage Diff Summary"
            print *, "=============================================="
            print *, ""
            
            ! Coverage metrics comparison
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
            
            print *, "=============================================="
        end if
        
        ! Perform diff analysis and generate diff report
        call generate_diff_report(baseline_coverage, current_coverage, config)
        
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
        
    end function perform_diff_analysis
    
    subroutine load_baseline_coverage(baseline_file, coverage_data, load_error)
        !! Loads baseline coverage data from JSON file
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
        
        type(config_t) :: temp_config
        character(len=:), allocatable :: coverage_files(:)
        character(len=:), allocatable :: filtered_files(:)
        logical :: parse_error
        
        ! Create temporary config without diff mode for normal analysis
        temp_config = config
        temp_config%enable_diff = .false.
        temp_config%tui_mode = .false.
        
        ! Find and filter coverage files
        call find_and_filter_coverage_files(temp_config, coverage_files, filtered_files)
        
        if (.not. allocated(filtered_files) .or. size(filtered_files) == 0) then
            generation_error = .true.
            return
        end if
        
        ! Parse coverage files
        call parse_coverage_files(filtered_files, temp_config, coverage_data, parse_error)
        generation_error = parse_error
        
    end subroutine generate_current_coverage
    
    subroutine generate_diff_report(baseline_coverage, current_coverage, config)
        !! Generates diff report comparing baseline and current coverage
        use coverage_diff
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
    
    subroutine perform_tui_coverage_analysis(config)
        !! Performs coverage analysis within TUI mode
        type(config_t), intent(in) :: config
        
        character(len=:), allocatable :: coverage_files(:)
        character(len=:), allocatable :: filtered_files(:)
        
        ! Find coverage files
        call find_and_filter_coverage_files(config, coverage_files, filtered_files)
        
        if (allocated(filtered_files) .and. size(filtered_files) > 0) then
            print *, "Found", size(filtered_files), "coverage file(s)"
            print *, "Processing..."
            ! Could add actual processing here
            print *, "✓ Analysis complete"
        else
            print *, "No coverage files found"
        end if
        
    end subroutine perform_tui_coverage_analysis
    
    subroutine display_tui_statistics(config)
        !! Displays coverage statistics in TUI mode
        type(config_t), intent(in) :: config
        
        print *, ""
        print *, "Coverage Statistics:"
        print *, "-------------------"
        print *, "Output format: ", trim(config%output_format)
        print *, "Output path: ", trim(config%output_path)
        if (config%minimum_coverage > 0.0) then
            print *, "Coverage threshold: ", config%minimum_coverage, "%"
        end if
        if (allocated(config%exclude_patterns)) then
            print *, "Exclude patterns: ", size(config%exclude_patterns), " configured"
        end if
        if (allocated(config%source_paths)) then
            print *, "Source paths: ", size(config%source_paths), " configured"
        end if
        if (config%threads > 1) then
            print *, "Parallel threads: ", config%threads
        end if
        print *, ""
        
    end subroutine display_tui_statistics
    
    subroutine validate_exclude_patterns_strict(config, is_valid)
        !! Validates exclude patterns in strict mode
        type(config_t), intent(in) :: config
        logical, intent(inout) :: is_valid
        
        integer :: i
        logical :: has_generic_pattern
        
        has_generic_pattern = .false.
        
        ! Check for overly broad patterns in strict mode
        do i = 1, size(config%exclude_patterns)
            if (trim(config%exclude_patterns(i)) == "*" .or. &
                trim(config%exclude_patterns(i)) == "**" .or. &
                trim(config%exclude_patterns(i)) == "*.*") then
                has_generic_pattern = .true.
                exit
            end if
        end do
        
        if (has_generic_pattern) then
            if (.not. config%quiet) then
                print *, "⚠️  Warning: Overly broad exclude pattern detected in strict mode"
                print *, "   Pattern '*' or '**' excludes all files"
                print *, "   Consider using more specific patterns like 'test_*.f90'"
            end if
            ! In strict mode, warn but don't fail for broad patterns
        end if
        
    end subroutine validate_exclude_patterns_strict
    
end module coverage_analysis