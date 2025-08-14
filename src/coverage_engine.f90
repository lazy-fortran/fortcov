module coverage_engine
    use coverage_model
    use fortcov_config
    use coverage_parser
    use coverage_statistics
    use coverage_reporter
    use file_utils
    use string_utils
    use error_handling
    implicit none
    private
    
    ! Exit codes for CI/CD integration
    integer, parameter, public :: EXIT_SUCCESS = 0
    integer, parameter, public :: EXIT_FAILURE = 1
    integer, parameter, public :: EXIT_THRESHOLD_NOT_MET = 2
    integer, parameter, public :: EXIT_NO_COVERAGE_DATA = 3
    
    ! Public procedures
    public :: analyze_coverage
    public :: find_coverage_files
    public :: check_exclude_patterns
    public :: analyze_coverage_safe
    
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
        
        if (config%verbose .and. .not. config%quiet) then
            print *, "Starting coverage analysis..."
        end if
        
        ! Find coverage files in source directories
        coverage_files = find_coverage_files(config)
        
        if (size(coverage_files) == 0) then
            if (.not. config%quiet) then
                print *, "Warning: No coverage files found"
            end if
            exit_code = EXIT_NO_COVERAGE_DATA
            return
        end if
        
        ! Filter files based on exclude patterns
        filtered_files = filter_files_by_patterns(coverage_files, config)
        
        if (size(filtered_files) == 0) then
            if (.not. config%quiet) then
                print *, "Warning: All coverage files were excluded"
            end if
            exit_code = EXIT_NO_COVERAGE_DATA
            return
        end if
        
        if (config%verbose .and. .not. config%quiet) then
            print *, "Found", size(filtered_files), "coverage files to process"
        end if
        
        ! Parse coverage data from all files using safe parsing
        block
            type(error_context_t) :: parse_error_ctx
            merged_coverage = parse_all_coverage_files_safe(filtered_files, &
                                                          config, parse_error_ctx)
            
            if (parse_error_ctx%error_code /= ERROR_SUCCESS .and. &
                .not. parse_error_ctx%recoverable) then
                if (.not. config%quiet) then
                    print *, "Error: Coverage parsing failed - " // &
                            trim(parse_error_ctx%message)
                    if (len_trim(parse_error_ctx%suggestion) > 0) then
                        print *, "Suggestion: " // trim(parse_error_ctx%suggestion)
                    end if
                end if
                exit_code = EXIT_FAILURE
                return
            else if (parse_error_ctx%error_code /= ERROR_SUCCESS .and. &
                     parse_error_ctx%recoverable) then
                ! Report partial processing issues as warnings
                if (.not. config%quiet) then
                    print *, "Warning: " // trim(parse_error_ctx%message)
                end if
            end if
        end block
        
        ! Calculate coverage statistics
        line_stats = calculate_line_coverage(merged_coverage)
        
        if (config%verbose .and. .not. config%quiet) then
            print *, "Line coverage:", line_stats%percentage, "%"
        end if
        
        ! Generate report
        call create_reporter(config%output_format, reporter, reporter_error)
        if (reporter_error) then
            if (.not. config%quiet) then
                print *, "Error: Unsupported output format '" // &
                        trim(config%output_format) // &
                        "'. Supported formats: markdown, md"
            end if
            exit_code = EXIT_FAILURE
            return
        end if
        
        call reporter%generate_report(merged_coverage, config%output_path, &
                                     reporter_error)
        if (reporter_error) then
            if (.not. config%quiet) then
                print *, "Error: Failed to generate report at: ", &
                        config%output_path
            end if
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Check coverage threshold
        if (line_stats%percentage < config%minimum_coverage) then
            if (.not. config%quiet) then
                print *, "Coverage threshold not met: ", &
                        line_stats%percentage, "% < ", &
                        config%minimum_coverage, "%"
            end if
            exit_code = EXIT_THRESHOLD_NOT_MET
            return
        end if
        
        if (config%verbose .and. .not. config%quiet) then
            print *, "Coverage analysis completed successfully"
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
            ! Search in each specified source directory
            do i = 1, size(config%source_paths)
                if (trim(config%input_format) == "gcov") then
                    search_pattern = trim(config%source_paths(i)) // "/*.gcov"
                else
                    search_pattern = trim(config%source_paths(i)) // "/*.gcov"
                end if
                
                temp_files = find_files(search_pattern)
                current_count = size(temp_files)
                
                if (current_count > 0) then
                    if (total_count == 0) then
                        allocate(character(len=256) :: all_files(current_count))
                        all_files(1:current_count) = temp_files
                    else
                        ! Extend array
                        all_files = [all_files, temp_files]
                    end if
                    total_count = total_count + current_count
                end if
            end do
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
        
        character(len=256) :: temp_filtered(size(files))
        integer :: i, count
        logical :: exclude_file
        
        count = 0
        
        do i = 1, size(files)
            exclude_file = check_exclude_patterns(files(i), config)
            if (.not. exclude_file) then
                count = count + 1
                temp_filtered(count) = files(i)
            end if
        end do
        
        if (count > 0) then
            allocate(character(len=256) :: filtered(count))
            filtered = temp_filtered(1:count)
        else
            allocate(character(len=256) :: filtered(0))
        end if
    end function filter_files_by_patterns

    ! Check if file matches any exclude pattern
    function check_exclude_patterns(filepath, config) result(should_exclude)
        character(len=*), intent(in) :: filepath
        type(config_t), intent(in) :: config
        logical :: should_exclude
        
        integer :: i
        
        should_exclude = .false.
        
        do i = 1, size(config%exclude_patterns)
            if (matches_pattern(filepath, config%exclude_patterns(i))) then
                should_exclude = .true.
                return
            end if
        end do
    end function check_exclude_patterns

    ! Simple pattern matching (supports * wildcard)
    function matches_pattern(filepath, pattern) result(matches)
        character(len=*), intent(in) :: filepath
        character(len=*), intent(in) :: pattern
        logical :: matches
        
        character(len=:), allocatable :: pattern_lower, filepath_lower
        integer :: star_pos
        
        ! Convert to lowercase for case-insensitive matching
        pattern_lower = to_lower(trim(pattern))
        filepath_lower = to_lower(trim(filepath))
        
        star_pos = index(pattern_lower, "*")
        
        if (star_pos == 0) then
            ! No wildcard, exact match
            matches = (filepath_lower == pattern_lower)
        else if (star_pos == len(pattern_lower)) then
            ! Pattern ends with *, check prefix
            matches = (filepath_lower(1:star_pos-1) == &
                      pattern_lower(1:star_pos-1))
        else if (star_pos == 1) then
            ! Pattern starts with *, check suffix
            matches = (len(filepath_lower) >= len(pattern_lower) - 1) .and. &
                     (filepath_lower(len(filepath_lower) - len(pattern_lower) + 2:) == &
                      pattern_lower(2:))
        else
            ! Wildcard in middle - check both prefix and suffix match
            matches = (index(filepath_lower, pattern_lower(1:star_pos-1)) == 1) .and. &
                     (index(filepath_lower, pattern_lower(star_pos+1:)) > 0)
        end if
    end function matches_pattern

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
                           trim(int_to_string(size(files))) // " coverage files"
        end if
    end function parse_all_coverage_files

    ! Convert string to lowercase (helper function)
    function to_lower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower_str
        integer :: i, ascii_val
        
        lower_str = str
        do i = 1, len(str)
            ascii_val = ichar(str(i:i))
            if (ascii_val >= 65 .and. ascii_val <= 90) then  ! A-Z
                lower_str(i:i) = char(ascii_val + 32)
            end if
        end do
    end function to_lower

    ! Convert integer to string (helper function)
    function int_to_string(value) result(str)
        integer, intent(in) :: value
        character(len=:), allocatable :: str
        character(len=20) :: temp
        
        write(temp, '(I0)') value
        str = trim(temp)
    end function int_to_string

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
            error_ctx%recoverable = .false.
            exit_code = EXIT_NO_COVERAGE_DATA
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
            error_ctx%recoverable = .true.
            exit_code = EXIT_NO_COVERAGE_DATA
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
                "Use supported formats: markdown, html, xml."
            error_ctx%recoverable = .false.
            exit_code = EXIT_FAILURE
            return
        end if
        
        call reporter%generate_report(merged_coverage, config%output_path, &
                                     reporter_error)
        if (reporter_error) then
            call handle_permission_denied(config%output_path, error_ctx)
            exit_code = EXIT_FAILURE
            return
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
                    ! Merge with existing files
                    all_files = [all_files, file_coverage%files]
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

end module coverage_engine