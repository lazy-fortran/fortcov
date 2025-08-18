module coverage_engine
    use coverage_model
    use fortcov_config
    use coverage_parser
    use coverage_statistics
    use coverage_reporter
    use file_utils
    use string_utils
    use error_handling
    use coverage_diff
    use json_coverage_io
    use report_engine
    implicit none
    private
    
    ! Exit codes for CI/CD integration
    integer, parameter, public :: EXIT_SUCCESS = 0
    integer, parameter, public :: EXIT_FAILURE = 1
    integer, parameter, public :: EXIT_THRESHOLD_NOT_MET = 2
    integer, parameter, public :: EXIT_NO_COVERAGE_DATA = 3
    
    ! Public procedures
    public :: analyze_coverage
    public :: analyze_coverage_diff
    public :: find_coverage_files
    public :: check_exclude_patterns
    public :: analyze_coverage_safe
    public :: validate_system_integration
    
contains

    ! Main coverage analysis orchestration function
    function analyze_coverage(config) result(exit_code)
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
        
        if (.not. config%quiet) then
            if (config%verbose) then
                print *, "🚀 Starting coverage analysis with verbose output..."
            else
                print *, "📊 Analyzing coverage..."
            end if
        end if
        
        ! Check if we're importing JSON instead of analyzing gcov files
        if (len_trim(config%import_file) > 0) then
            exit_code = analyze_imported_json(config)
            return
        end if
        
        ! Check if we're doing diff analysis
        if (config%enable_diff) then
            exit_code = analyze_coverage_diff(config)
            return
        end if
        
        ! Check if we're launching TUI mode (Issue #106)
        if (config%tui_mode) then
            exit_code = launch_tui_mode(config)
            return
        end if
        
        ! Find coverage files in source directories
        coverage_files = find_coverage_files(config)
        
        if (size(coverage_files) == 0) then
            if (.not. config%quiet) then
                print *, "⚠️  No .gcov files found in source directories"
                print *, "💡 Make sure you:"
                print *, "   1. Built with coverage: fpm build --flag ""-fprofile-arcs -ftest-coverage"""
                print *, "   2. Ran your tests: fpm test --flag ""-fprofile-arcs -ftest-coverage"""
                print *, "   3. Generated .gcov files: gcov src/*.f90"
                print *, "   4. Located .gcov files: find . -name '*.gcov' -type f"
                print *, "   5. Set --source to directory containing .gcov files"
                if (allocated(config%source_paths) .and. size(config%source_paths) > 0) then
                    print *, "📁 Searched in:", (trim(config%source_paths(i))//" ", i=1,min(3,size(config%source_paths)))
                    if (size(config%source_paths) > 3) then
                        print *, "     ... and", size(config%source_paths)-3, "more directories"
                    end if
                end if
            end if
            ! Issue #109: Differentiate between strict and default modes
            if (config%strict_mode) then
                exit_code = EXIT_NO_COVERAGE_DATA  ! Error exit (code 3) 
            else
                exit_code = EXIT_SUCCESS           ! Success exit (code 0)
            end if
            return
        end if
        
        ! Filter files based on exclude patterns
        filtered_files = filter_files_by_patterns(coverage_files, config)
        
        if (size(filtered_files) == 0) then
            if (.not. config%quiet) then
                print *, "⚠️  All coverage files were excluded by filters"
                print *, "💡 Check your --exclude patterns or remove filters to include files"
            end if
            ! Issue #109: Differentiate between strict and default modes
            if (config%strict_mode) then
                exit_code = EXIT_NO_COVERAGE_DATA  ! Error exit (code 3)
            else
                exit_code = EXIT_SUCCESS           ! Success exit (code 0)
            end if
            return
        end if
        
        if (.not. config%quiet) then
            print *, "📁 Found", size(filtered_files), "coverage file(s) to process"
            if (config%verbose) then
                print *, "🔍 Processing:"
                do i = 1, min(5, size(filtered_files))
                    print *, "   • " // trim(filtered_files(i))
                end do
                if (size(filtered_files) > 5) then
                    print *, "   ... and", size(filtered_files) - 5, "more files"
                end if
            end if
        end if
        
        ! Parse coverage data from all files using safe parsing
        block
            type(error_context_t) :: parse_error_ctx
            merged_coverage = parse_all_coverage_files_safe(filtered_files, &
                                                          config, parse_error_ctx)
            
            if (parse_error_ctx%error_code /= ERROR_SUCCESS .and. &
                .not. parse_error_ctx%recoverable) then
                if (.not. config%quiet) then
                    print *, "❌ Coverage parsing failed: " // &
                            trim(parse_error_ctx%message)
                    if (len_trim(parse_error_ctx%suggestion) > 0) then
                        print *, "🔧 Solution: " // trim(parse_error_ctx%suggestion)
                    end if
                end if
                exit_code = EXIT_FAILURE
                return
            else if (parse_error_ctx%error_code /= ERROR_SUCCESS .and. &
                     parse_error_ctx%recoverable) then
                ! Report partial processing issues as warnings
                if (.not. config%quiet) then
                    print *, "⚠️  " // trim(parse_error_ctx%message)
                    print *, "✅ Continuing with available coverage data..."
                end if
            end if
        end block
        
        ! Calculate coverage statistics
        line_stats = calculate_line_coverage(merged_coverage)
        
        if (.not. config%quiet) then
            print *, "📊 Overall coverage:", line_stats%percentage, "%"
            if (config%verbose) then
                print *, "📈 Coverage statistics:"
                print *, "   • Total lines:", line_stats%total_count
                print *, "   • Covered lines:", line_stats%covered_count
                print *, "   • Percentage:", line_stats%percentage, "%"
            end if
        end if
        
        ! Generate report
        call create_reporter(config%output_format, reporter, reporter_error)
        if (reporter_error) then
            if (.not. config%quiet) then
                print *, "❌ Unsupported output format: '" // &
                        trim(config%output_format) // "'"
                print *, "📊 Supported formats: markdown, md, json, xml, html"
                print *, "💡 Try: --output-format=markdown"
            end if
            exit_code = EXIT_FAILURE
            return
        end if
        
        if (.not. config%quiet) then
            print *, "📝 Generating " // trim(config%output_format) // " report..."
        end if
        
        call reporter%generate_report(merged_coverage, config%output_path, &
                                     reporter_error, config%quiet)
        if (reporter_error) then
            if (.not. config%quiet) then
                print *, "❌ Failed to generate report at: " // &
                        trim(config%output_path)
                print *, "💡 Check directory permissions and disk space"
            end if
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Provide user feedback for report creation
        if (.not. config%quiet .and. config%output_path /= "-") then
            select case(trim(config%output_format))
            case("html")
                print *, "✨ HTML report saved to: " // trim(config%output_path)
            case("json")
                print *, "📊 JSON report saved to: " // trim(config%output_path)
            case("markdown", "md")
                print *, "📝 Markdown report saved to: " // trim(config%output_path)
            case default
                print *, "✅ Report saved to: " // trim(config%output_path)
            end select
        end if
        
        ! Check coverage threshold
        if (line_stats%percentage < config%minimum_coverage) then
            if (.not. config%quiet) then
                print *, "⚠️  Coverage threshold not met:"
                print *, "   📉 Current: ", line_stats%percentage, "%"
                print *, "   🎯 Required: ", config%minimum_coverage, "%"
                print *, "   🚀 Shortfall: ", config%minimum_coverage - line_stats%percentage, "%"
            end if
            exit_code = EXIT_THRESHOLD_NOT_MET
            return
        end if
        
        if (.not. config%quiet) then
            if (config%minimum_coverage > 0.0 .and. &
                line_stats%percentage >= config%minimum_coverage) then
                print *, "✅ Coverage analysis completed successfully! Threshold met."
            else
                print *, "✅ Coverage analysis completed successfully!"
            end if
            if (config%verbose) then
                print *, "🎉 Analysis summary:"
                print *, "   • Files processed: ", size(filtered_files)
                print *, "   • Final coverage: ", line_stats%percentage, "%"
                print *, "   • Output format: ", trim(config%output_format)
            end if
        end if
    end function analyze_coverage

    ! Find all coverage files in configured source directories
    function find_coverage_files(config) result(files)
        type(config_t), intent(in) :: config
        character(len=:), allocatable :: files(:)
        
        character(len=:), allocatable :: all_files(:)
        character(len=:), allocatable :: temp_files(:)
        character(len=:), allocatable :: search_pattern
        integer :: i, total_count, current_count
        
        ! Priority 1: Use user-specified coverage files if provided
        if (allocated(config%coverage_files) .and. size(config%coverage_files) > 0) then
            ! User provided specific files - use them directly
            allocate(character(len=256) :: files(size(config%coverage_files)))
            do i = 1, size(config%coverage_files)
                files(i) = config%coverage_files(i)
            end do
            return
        end if
        
        ! Priority 2: Fall back to directory search
        total_count = 0
        
        ! If no source paths specified, search current directory
        if (size(config%source_paths) == 0) then
            ! Auto-detect parser format and search for appropriate files
            if (trim(config%input_format) == "gcov") then
                temp_files = find_files("*.gcov")
            else
                ! Default to gcov format
                temp_files = find_files("*.gcov")
            end if
            total_count = size(temp_files)
            if (total_count > 0) then
                allocate(character(len=256) :: all_files(total_count))
                all_files(1:total_count) = temp_files
            end if
        else
            ! OPTIMIZED: Pre-calculate total file count to avoid O(n²) array extensions
            ! First pass: count total files needed
            total_count = 0
            do i = 1, size(config%source_paths)
                if (trim(config%input_format) == "gcov") then
                    search_pattern = trim(config%source_paths(i)) // "/*.gcov"
                else
                    search_pattern = trim(config%source_paths(i)) // "/*.gcov"
                end if
                
                temp_files = find_files(search_pattern)
                total_count = total_count + size(temp_files)
            end do
            
            ! Allocate result array once with correct size
            if (total_count > 0) then
                allocate(character(len=256) :: all_files(total_count))
                current_count = 0
                
                ! Second pass: populate the pre-allocated array
                do i = 1, size(config%source_paths)
                    if (trim(config%input_format) == "gcov") then
                        search_pattern = trim(config%source_paths(i)) // "/*.gcov"
                    else
                        search_pattern = trim(config%source_paths(i)) // "/*.gcov"
                    end if
                    
                    temp_files = find_files(search_pattern)
                    if (size(temp_files) > 0) then
                        all_files(current_count+1:current_count+size(temp_files)) = &
                            temp_files
                        current_count = current_count + size(temp_files)
                    end if
                end do
            end if
        end if
        
        ! Return result
        if (total_count > 0) then
            allocate(character(len=256) :: files(total_count))
            files = all_files(1:total_count)
        else
            allocate(character(len=256) :: files(0))
        end if
    end function find_coverage_files

    ! Filter files based on exclude patterns
    function filter_files_by_patterns(files, config) result(filtered)
        character(len=*), intent(in) :: files(:)
        type(config_t), intent(in) :: config
        character(len=:), allocatable :: filtered(:)
        
        character(len=:), allocatable :: temp_filtered(:)
        integer :: i, count
        logical :: exclude_file
        integer :: max_filename_len
        
        ! Security: Find maximum filename length to prevent buffer issues
        max_filename_len = 0
        do i = 1, size(files)
            max_filename_len = max(max_filename_len, len_trim(files(i)))
        end do
        
        ! Security: Enforce reasonable filename length limits
        if (max_filename_len > 4096) then
            ! Extremely long filenames could indicate an attack
            allocate(character(len=256) :: filtered(0))
            return
        end if
        
        ! Memory safety: Use dynamic allocation with proper sizing
        allocate(character(len=max(max_filename_len, 256)) :: &
                 & temp_filtered(size(files)))
        count = 0
        
        do i = 1, size(files)
            exclude_file = check_exclude_patterns(files(i), config)
            if (.not. exclude_file) then
                count = count + 1
                temp_filtered(count) = files(i)
            end if
        end do
        
        if (count > 0) then
            ! Memory safety: Allocate result with exact size needed
            allocate(character(len=max(max_filename_len, 256)) :: filtered(count))
            filtered = temp_filtered(1:count)
        else
            allocate(character(len=256) :: filtered(0))
        end if
        
        ! Memory safety: Clean up temporary array
        deallocate(temp_filtered)
    end function filter_files_by_patterns

    ! Check if file matches any exclude pattern
    function check_exclude_patterns(filepath, config) result(should_exclude)
        character(len=*), intent(in) :: filepath
        type(config_t), intent(in) :: config
        logical :: should_exclude
        
        should_exclude = .false.
        
        ! Check if exclude patterns are allocated and not empty
        if (.not. allocated(config%exclude_patterns) .or. &
            size(config%exclude_patterns) == 0) then
            return
        end if
        
        ! Use foundation layer utility
        should_exclude = check_exclude_patterns_list(filepath, config%exclude_patterns)
    end function check_exclude_patterns


    ! Parse coverage data from all files and merge
    function parse_all_coverage_files(files, config, error_flag, error_context) &
            result(merged_coverage)
        character(len=*), intent(in) :: files(:)
        type(config_t), intent(in) :: config
        logical, intent(out) :: error_flag
        character(len=:), allocatable, intent(out) :: error_context
        type(coverage_data_t) :: merged_coverage
        
        class(coverage_parser_t), allocatable :: parser
        type(coverage_data_t) :: file_coverage
        type(coverage_file_t), allocatable :: all_files(:)
        logical :: parser_error, file_error
        integer :: i, total_files, current_files
        
        error_flag = .false.
        error_context = ""
        total_files = 0
        
        ! Create parser based on first file (auto-detection)
        if (size(files) > 0) then
            call create_parser(files(1), parser, parser_error)
            if (parser_error) then
                error_flag = .true.
                error_context = "Unsupported coverage file format: " // trim(files(1))
                return
            end if
        else
            error_flag = .true.
            error_context = "No coverage files provided to parser"
            return
        end if
        
        ! Parse each file and collect coverage data
        do i = 1, size(files)
            if (config%verbose .and. .not. config%quiet) then
                print *, "Processing file:", trim(files(i))
            end if
            
            file_coverage = parser%parse(files(i), file_error)
            
            if (file_error) then
                if (.not. config%quiet) then
                    print *, "Warning: Failed to parse file:", trim(files(i))
                end if
                ! Continue with other files rather than fail completely
                cycle
            end if
            
            current_files = size(file_coverage%files)
            if (current_files > 0) then
                if (total_files == 0) then
                    allocate(all_files(current_files))
                    all_files = file_coverage%files
                else
                    ! Merge with existing files
                    all_files = [all_files, file_coverage%files]
                end if
                total_files = total_files + current_files
            end if
        end do
        
        ! Create merged coverage data
        if (total_files > 0) then
            call merged_coverage%init(all_files)
        else
            ! No coverage data found
            allocate(all_files(0))
            call merged_coverage%init(all_files)
            error_flag = .true.
            error_context = "No valid coverage data found in any of the " // &
                           trim(format_integer(size(files))) // " coverage files"
        end if
    end function parse_all_coverage_files



    ! Enhanced coverage analysis with comprehensive error handling
    function analyze_coverage_safe(config, error_ctx) result(exit_code)
        type(config_t), intent(in) :: config
        type(error_context_t), intent(out) :: error_ctx
        integer :: exit_code
        
        character(len=:), allocatable :: coverage_files(:)
        character(len=:), allocatable :: filtered_files(:)
        type(coverage_data_t) :: merged_coverage
        type(coverage_stats_t) :: line_stats
        class(coverage_parser_t), allocatable :: parser
        class(coverage_reporter_t), allocatable :: reporter
        logical :: parser_error, reporter_error
        integer :: i, file_count
        
        call clear_error_context(error_ctx)
        exit_code = EXIT_SUCCESS
        
        if (config%verbose .and. .not. config%quiet) then
            print *, "Starting coverage analysis..."
        end if
        
        ! Find coverage files with enhanced error handling
        coverage_files = find_coverage_files(config)
        
        if (size(coverage_files) == 0) then
            error_ctx%error_code = ERROR_INCOMPLETE_COVERAGE
            write(error_ctx%message, '(A)') &
                "No coverage files found in specified directories."
            write(error_ctx%suggestion, '(A)') &
                "Ensure tests are run with coverage flags " // &
                "(-fprofile-arcs -ftest-coverage)."
            ! Issue #109: Differentiate between strict and default modes
            if (config%strict_mode) then
                error_ctx%recoverable = .false.
                exit_code = EXIT_NO_COVERAGE_DATA
            else
                error_ctx%recoverable = .true.
                exit_code = EXIT_SUCCESS
            end if
            return
        end if
        
        ! Enhanced file filtering with error context
        filtered_files = filter_files_by_patterns(coverage_files, config)
        
        if (size(filtered_files) == 0) then
            error_ctx%error_code = ERROR_INVALID_CONFIG
            write(error_ctx%message, '(A)') &
                "All coverage files excluded by patterns."
            write(error_ctx%suggestion, '(A)') &
                "Review exclude patterns in configuration."
            ! Issue #109: Differentiate between strict and default modes
            if (config%strict_mode) then
                error_ctx%recoverable = .false.
                exit_code = EXIT_NO_COVERAGE_DATA
            else
                error_ctx%recoverable = .true.
                exit_code = EXIT_SUCCESS
            end if
            return
        end if
        
        if (config%verbose .and. .not. config%quiet) then
            print *, "Found", size(filtered_files), "coverage files to process"
        end if
        
        ! Enhanced parsing with comprehensive error handling
        merged_coverage = parse_all_coverage_files_safe(filtered_files, &
                                                      config, error_ctx)
        
        if (error_ctx%error_code /= ERROR_SUCCESS .and. &
            .not. error_ctx%recoverable) then
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Calculate coverage statistics
        line_stats = calculate_line_coverage(merged_coverage)
        
        if (config%verbose .and. .not. config%quiet) then
            print *, "Line coverage:", line_stats%percentage, "%"
        end if
        
        ! Enhanced report generation
        call create_reporter(config%output_format, reporter, reporter_error)
        if (reporter_error) then
            error_ctx%error_code = ERROR_INVALID_CONFIG
            write(error_ctx%message, '(A,A)') &
                "Unsupported output format: ", config%output_format
            write(error_ctx%suggestion, '(A)') &
                "Use supported formats: markdown, md, json, xml, html."
            error_ctx%recoverable = .false.
            exit_code = EXIT_FAILURE
            return
        end if
        
        call reporter%generate_report(merged_coverage, config%output_path, &
                                     reporter_error, config%quiet)
        if (reporter_error) then
            call handle_permission_denied(config%output_path, error_ctx)
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Provide user feedback for HTML report creation (Issue #104)
        if (.not. config%quiet .and. trim(config%output_format) == "html" .and. &
            config%output_path /= "-") then
            print *, "HTML report saved to: ", trim(config%output_path)
        end if
        
        ! Check coverage threshold with enhanced messaging
        if (line_stats%percentage < config%minimum_coverage) then
            error_ctx%error_code = ERROR_THRESHOLD_NOT_MET
            write(error_ctx%message, '(A,F6.2,A,F6.2,A)') &
                "Coverage threshold not met: ", &
                line_stats%percentage, "% < ", &
                config%minimum_coverage, "%"
            write(error_ctx%suggestion, '(A)') &
                "Add more tests or adjust threshold setting."
            error_ctx%recoverable = .true.
            exit_code = EXIT_THRESHOLD_NOT_MET
            return
        end if
        
        if (config%verbose .and. .not. config%quiet) then
            print *, "Coverage analysis completed successfully"
        end if
    end function analyze_coverage_safe

    ! Enhanced file parsing with comprehensive error handling  
    function parse_all_coverage_files_safe(files, config, error_ctx) &
            result(merged_coverage)
        character(len=*), intent(in) :: files(:)
        type(config_t), intent(in) :: config
        type(error_context_t), intent(inout) :: error_ctx
        type(coverage_data_t) :: merged_coverage
        
        class(coverage_parser_t), allocatable :: parser
        type(coverage_data_t) :: file_coverage
        type(coverage_file_t), allocatable :: all_files(:)
        logical :: parser_error, file_error
        integer :: i, total_files, current_files, failed_files
        
        total_files = 0
        failed_files = 0
        
        ! Create parser based on first file (auto-detection)
        if (size(files) > 0) then
            call create_parser(files(1), parser, parser_error)
            if (parser_error) then
                error_ctx%error_code = ERROR_INVALID_CONFIG
                write(error_ctx%message, '(A,A,A)') &
                    "Unsupported coverage file format: ", trim(files(1)), &
                    ". Parser could not be created."
                write(error_ctx%suggestion, '(A)') &
                    "Ensure coverage files are in .gcov text format."
                error_ctx%recoverable = .false.
                return
            end if
        else
            error_ctx%error_code = ERROR_INCOMPLETE_COVERAGE
            return
        end if
        
        ! Parse each file with enhanced error handling
        do i = 1, size(files)
            if (config%verbose .and. .not. config%quiet) then
                print *, "Processing file:", trim(files(i))
            end if
            
            file_coverage = parser%parse(files(i), file_error)
            
            if (file_error) then
                failed_files = failed_files + 1
                if (.not. config%quiet) then
                    print *, "Warning: Failed to parse file:", trim(files(i))
                end if
                ! Continue with other files for partial recovery
                cycle
            end if
            
            current_files = size(file_coverage%files)
            if (current_files > 0) then
                if (total_files == 0) then
                    allocate(all_files(current_files))
                    all_files = file_coverage%files
                else
                    ! OPTIMIZED: Use move_alloc for efficient array merging
                    block
                        type(coverage_file_t), allocatable :: temp_files(:)
                        allocate(temp_files(total_files + current_files))
                        temp_files(1:total_files) = all_files
                        temp_files(total_files+1:total_files+current_files) = &
                            file_coverage%files
                        call move_alloc(temp_files, all_files)
                    end block
                end if
                total_files = total_files + current_files
            end if
        end do
        
        ! Create merged coverage data with error context
        if (total_files > 0) then
            call merged_coverage%init(all_files)
            
            if (failed_files > 0) then
                error_ctx%error_code = ERROR_PARTIAL_PROCESSING
                write(error_ctx%message, '(A,I0,A,I0,A)') &
                    "Processed ", total_files, " files successfully, ", &
                    failed_files, " files failed."
                error_ctx%recoverable = .true.
            end if
        else
            allocate(all_files(0))
            call merged_coverage%init(all_files)
            call handle_incomplete_coverage("all files", error_ctx)
        end if
    end function parse_all_coverage_files_safe

    ! Analyze coverage data imported from JSON file
    function analyze_imported_json(config) result(exit_code)
        use json_coverage_io, only: import_json_coverage_safe
        use file_utils, only: file_exists, read_file_content
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        type(coverage_data_t) :: coverage_data
        type(coverage_stats_t) :: line_stats
        character(len=:), allocatable :: json_content
        logical :: error_occurred, file_error
        class(coverage_reporter_t), allocatable :: reporter
        logical :: reporter_error
        
        exit_code = EXIT_SUCCESS
        
        if (config%verbose .and. .not. config%quiet) then
            print *, "Importing coverage data from JSON file: ", config%import_file
        end if
        
        ! Check if import file exists
        if (.not. file_exists(config%import_file)) then
            if (.not. config%quiet) then
                print *, "Error: Import file not found: ", config%import_file
            end if
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Read JSON file content
        call read_file_content(config%import_file, json_content, file_error)
        if (file_error) then
            if (.not. config%quiet) then
                print *, "Error: Failed to read import file: ", config%import_file
            end if
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Parse JSON content
        call import_json_coverage_safe(json_content, coverage_data, error_occurred)
        if (error_occurred) then
            if (.not. config%quiet) then
                print *, "Error: Failed to parse JSON coverage data from: ", &
                        config%import_file
            end if
            exit_code = EXIT_FAILURE
            return
        end if
        
        if (config%verbose .and. .not. config%quiet) then
            print *, "Successfully imported coverage data for", &
                    size(coverage_data%files), "files"
        end if
        
        ! Calculate coverage statistics
        line_stats = calculate_line_coverage(coverage_data)
        
        if (config%verbose .and. .not. config%quiet) then
            print *, "Line coverage:", line_stats%percentage, "%"
        end if
        
        ! Generate report
        call create_reporter(config%output_format, reporter, reporter_error)
        if (reporter_error) then
            if (.not. config%quiet) then
                print *, "Error: Unsupported output format '" // &
                        trim(config%output_format) // &
                        "'. Supported formats: markdown, md, json, xml, html"
            end if
            exit_code = EXIT_FAILURE
            return
        end if
        
        call reporter%generate_report(coverage_data, config%output_path, &
                                     reporter_error, config%quiet)
        if (reporter_error) then
            if (.not. config%quiet) then
                print *, "Error: Failed to generate report at: ", &
                        config%output_path
            end if
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Provide user feedback for HTML report creation (Issue #104)
        if (.not. config%quiet .and. trim(config%output_format) == "html" .and. &
            config%output_path /= "-") then
            print *, "HTML report saved to: ", trim(config%output_path)
        end if
        
        ! Check coverage threshold
        if (line_stats%percentage < config%minimum_coverage) then
            if (.not. config%quiet) then
                print *, "Coverage threshold not met:", line_stats%percentage, &
                        "% <", config%minimum_coverage, "%"
            end if
            exit_code = EXIT_THRESHOLD_NOT_MET
        end if
        
        if (config%verbose .and. .not. config%quiet) then
            print *, "JSON import completed successfully"
        end if
    end function analyze_imported_json

    ! Analyze coverage diff between baseline and current files
    function analyze_coverage_diff(config) result(exit_code)
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        type(coverage_data_t) :: baseline_coverage, current_coverage
        type(coverage_diff_t) :: diff_result
        logical :: baseline_error, current_error
        class(coverage_reporter_t), allocatable :: reporter
        logical :: reporter_error
        
        exit_code = EXIT_SUCCESS
        
        if (config%verbose .and. .not. config%quiet) then
            print *, "Starting coverage diff analysis..."
            print *, "Baseline file:", trim(config%diff_baseline_file)
            print *, "Current file:", trim(config%diff_current_file)
        end if
        
        ! Import baseline coverage data
        block
            character(len=:), allocatable :: baseline_json_content
            logical :: file_error
            
            call read_file_content(config%diff_baseline_file, &
                                  baseline_json_content, file_error)
            if (file_error) then
                print *, "Error: Failed to read baseline coverage file: ", &
                        trim(config%diff_baseline_file)
                exit_code = EXIT_FAILURE
                return
            end if
            
            call import_json_coverage_safe(baseline_json_content, &
                                          baseline_coverage, baseline_error)
            if (baseline_error) then
                print *, "Error: Failed to parse baseline coverage JSON from: ", &
                        trim(config%diff_baseline_file)
                exit_code = EXIT_FAILURE
                return
            end if
        end block
        
        ! Import current coverage data
        block
            character(len=:), allocatable :: current_json_content
            logical :: file_error
            
            call read_file_content(config%diff_current_file, &
                                  current_json_content, file_error)
            if (file_error) then
                print *, "Error: Failed to read current coverage file: ", &
                        trim(config%diff_current_file)
                exit_code = EXIT_FAILURE
                return
            end if
            
            call import_json_coverage_safe(current_json_content, &
                                          current_coverage, current_error)
            if (current_error) then
                print *, "Error: Failed to parse current coverage JSON from: ", &
                        trim(config%diff_current_file)
                exit_code = EXIT_FAILURE
                return
            end if
        end block
        
        if (config%verbose .and. .not. config%quiet) then
            print *, "Computing coverage diff..."
        end if
        
        ! Compute coverage diff
        diff_result = compute_coverage_diff(baseline_coverage, current_coverage, &
                                           config%include_unchanged, &
                                           config%diff_threshold)
        
        ! Generate output report
        call create_reporter(config%output_format, reporter, reporter_error)
        if (reporter_error) then
            print *, "Error: Failed to create reporter"
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Note: This would require extending the reporter interface to support &
        ! diff output
        ! For now, just output a summary
        call output_diff_summary(diff_result, config)
        
        if (config%verbose .and. .not. config%quiet) then
            print *, "Coverage diff analysis completed successfully"
        end if
    end function analyze_coverage_diff
    
    ! Output diff summary to console/file
    subroutine output_diff_summary(diff_result, config)
        type(coverage_diff_t), intent(in) :: diff_result
        type(config_t), intent(in) :: config
        integer :: i, j
        
        if (.not. config%quiet) then
            print *, "=== Coverage Diff Summary ==="
            print *, "Total files with changes:", size(diff_result%file_diffs)
            print *, "Total lines added:", diff_result%total_added_lines
            print *, "Total lines removed:", diff_result%total_removed_lines
            print *, "Total lines changed:", diff_result%total_changed_lines
            print *, "Total newly covered lines:", diff_result%total_newly_covered_lines
            print *, "Total newly uncovered lines:", &
                     & diff_result%total_newly_uncovered_lines
            
            if (size(diff_result%file_diffs) > 0) then
                print *, ""
                print *, "File details:"
                do i = 1, size(diff_result%file_diffs)
                    print *, "File:", trim(diff_result%file_diffs(i)%filename)
                    print *, "  Coverage change:", &
                             & diff_result%file_diffs(i)%coverage_percentage_delta, "%"
                    print *, "  Added lines:", diff_result%file_diffs(i)%added_lines
                    print *, "  Removed lines:", diff_result%file_diffs(i)%removed_lines
                    print *, "  Changed lines:", diff_result%file_diffs(i)%changed_lines
                    print *, "  Newly covered:", &
                             & diff_result%file_diffs(i)%newly_covered_lines
                    print *, "  Newly uncovered:", &
                             & diff_result%file_diffs(i)%newly_uncovered_lines
                    print *, ""
                end do
            end if
        end if
    end subroutine output_diff_summary

    ! Launch TUI mode with coverage analysis (Issue #106)
    function launch_tui_mode(config) result(exit_code)
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        character(len=:), allocatable :: coverage_files(:)
        character(len=:), allocatable :: filtered_files(:)
        type(coverage_data_t) :: merged_coverage
        type(coverage_stats_t) :: line_stats
        type(report_engine_t) :: engine
        type(terminal_session_t) :: session
        logical :: success, engine_error
        character(len=:), allocatable :: error_msg
        
        exit_code = EXIT_SUCCESS
        
        if (config%verbose .and. .not. config%quiet) then
            print *, "Launching TUI mode..."
        end if
        
        ! Find and parse coverage data (same as regular analysis)
        coverage_files = find_coverage_files(config)
        
        if (size(coverage_files) == 0) then
            if (.not. config%quiet) then
                print *, "Warning: No coverage files found for TUI"
            end if
            ! Issue #109: Differentiate between strict and default modes
            if (config%strict_mode) then
                exit_code = EXIT_NO_COVERAGE_DATA  ! Error exit (code 3)
            else
                exit_code = EXIT_SUCCESS           ! Success exit (code 0)
            end if
            return
        end if
        
        filtered_files = filter_files_by_patterns(coverage_files, config)
        
        if (size(filtered_files) == 0) then
            if (.not. config%quiet) then
                print *, "Warning: All coverage files were excluded"
            end if
            ! Issue #109: Differentiate between strict and default modes
            if (config%strict_mode) then
                exit_code = EXIT_NO_COVERAGE_DATA  ! Error exit (code 3)
            else
                exit_code = EXIT_SUCCESS           ! Success exit (code 0)
            end if
            return
        end if
        
        ! Parse coverage data
        block
            type(error_context_t) :: parse_error_ctx
            merged_coverage = parse_all_coverage_files_safe(filtered_files, &
                                                          config, parse_error_ctx)
            
            if (parse_error_ctx%error_code /= ERROR_SUCCESS .and. &
                .not. parse_error_ctx%recoverable) then
                if (.not. config%quiet) then
                    print *, "Error: Coverage parsing failed for TUI - " // &
                            trim(parse_error_ctx%message)
                end if
                exit_code = EXIT_FAILURE
                return
            end if
        end block
        
        ! Initialize report engine
        call engine%init(success, error_msg)
        if (.not. success) then
            if (.not. config%quiet) then
                print *, "Error: Failed to initialize TUI engine - " // error_msg
            end if
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Load coverage data into engine
        engine%source_data = merged_coverage
        
        ! Launch terminal browser
        call engine%launch_terminal_browser(session, .true., success, error_msg)
        if (.not. success) then
            if (.not. config%quiet) then
                print *, "Error: Failed to launch TUI - " // error_msg
            end if
            exit_code = EXIT_FAILURE
            return
        end if
        
        if (config%verbose .and. .not. config%quiet) then
            print *, "TUI session completed"
        end if
    end function launch_tui_mode

    ! System integration validation for comprehensive system validation
    ! Validates that all module interfaces are working correctly after refactoring
    function validate_system_integration() result(validation_passed)
        logical :: validation_passed
        type(config_t) :: test_config
        type(coverage_data_t) :: test_data
        type(coverage_file_t), allocatable :: test_files(:)
        type(coverage_line_t), allocatable :: test_lines(:)
        character(len=:), allocatable :: test_output
        logical :: component_working
        
        validation_passed = .true.
        
        ! Test 1: Configuration system integration
        call initialize_config(test_config)
        test_config%output_format = "markdown"
        test_config%verbose = .false.
        test_config%quiet = .true.
        if (.not. allocated(test_config%output_format)) then
            validation_passed = .false.
            return
        end if
        
        ! Test 2: Coverage model integration
        allocate(test_files(1))
        allocate(test_lines(2))
        
        call test_lines(1)%init(5, 1, 'test_integration.f90', .true.)
        call test_lines(2)%init(0, 2, 'test_integration.f90', .true.)
        call test_files(1)%init('test_integration.f90', test_lines)
        call test_data%init(test_files)
        
        ! Validate coverage data structure
        if (.not. allocated(test_data%files)) then
            validation_passed = .false.
            return
        end if
        
        if (size(test_data%files) /= 1) then
            validation_passed = .false.
            return
        end if
        
        ! Test 3: String utilities integration
        test_output = to_lower("TEST_INTEGRATION")
        if (test_output /= "test_integration") then
            validation_passed = .false.
            return
        end if
        
        test_output = format_integer(42)
        if (test_output /= "42") then
            validation_passed = .false.
            return  
        end if
        
        ! Test 4: Pattern matching integration
        component_working = matches_pattern("test_file.gcov", "*.gcov")
        if (.not. component_working) then
            validation_passed = .false.
            return
        end if
        
        component_working = matches_pattern("regular_file.txt", "*.gcov")
        if (component_working) then ! Should NOT match
            validation_passed = .false.
            return
        end if
        
        ! Test 5: Coverage statistics integration
        block
            type(coverage_stats_t) :: stats
            stats = calculate_line_coverage(test_data)
            
            ! Should have 1 covered line out of 2 executable = 50%
            if (abs(stats%percentage - 50.0) > 0.1) then
                validation_passed = .false.
                return
            end if
        end block
        
        ! All integration tests passed
        validation_passed = .true.
        
    end function validate_system_integration

end module coverage_engine
