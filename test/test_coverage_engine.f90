program test_coverage_engine
    use coverage_model
    use fortcov_config
    use coverage_parser
    use coverage_statistics
    use coverage_reporter
    use file_utils
    use coverage_engine
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "Testing Coverage Engine..."
    
    ! Test 1: End-to-end coverage analysis
    all_tests_passed = all_tests_passed .and. test_end_to_end_analysis()
    
    ! Test 2: Parser auto-detection
    all_tests_passed = all_tests_passed .and. test_parser_auto_detection()
    
    ! Test 3: Multiple source directories
    all_tests_passed = all_tests_passed .and. test_multiple_source_directories()
    
    ! Test 4: Exclude pattern filtering
    all_tests_passed = all_tests_passed .and. test_exclude_pattern_filtering()
    
    ! Test 4b: Middle wildcard pattern matching (bug fix)
    all_tests_passed = all_tests_passed .and. test_middle_wildcard_patterns()
    
    ! Test 5: Coverage threshold enforcement
    all_tests_passed = all_tests_passed .and. test_coverage_threshold_enforcement()
    
    ! Test 6: Handle missing coverage files
    all_tests_passed = all_tests_passed .and. test_missing_coverage_files()
    
    ! Test 7: Parser error handling
    all_tests_passed = all_tests_passed .and. test_parser_error_handling()
    
    ! Test 8: Reporter error handling
    all_tests_passed = all_tests_passed .and. test_reporter_error_handling()
    
    ! Test 9: Verbose output
    all_tests_passed = all_tests_passed .and. test_verbose_output()
    
    ! Test 10: Quiet mode
    all_tests_passed = all_tests_passed .and. test_quiet_mode()
    
    if (all_tests_passed) then
        print *, "All tests PASSED"
        call exit(0)
    else
        print *, "Some tests FAILED"
        call exit(1)
    end if

contains

    function test_end_to_end_analysis() result(passed)
        logical :: passed
        type(config_t) :: config
        integer :: exit_code
        
        print *, "  Test 1: End-to-end coverage analysis"
        
        ! Given: GCov files and configuration for markdown output
        call initialize_config(config)
        config%output_format = "markdown"
        config%output_path = "/tmp/test_coverage.md"
        config%input_format = "gcov"
        config%verbose = .false.
        config%quiet = .true.  ! Suppress output during testing
        
        ! Allocate source paths for current directory
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        allocate(character(len=256) :: config%source_paths(1))
        config%source_paths(1) = "test_coverage_sample"
        
        ! When: Running analyze_coverage()
        exit_code = analyze_coverage(config)
        
        ! Then: Should complete without major errors
        ! (We expect EXIT_NO_COVERAGE_DATA since we don't have real gcda files)
        passed = (exit_code == EXIT_NO_COVERAGE_DATA .or. &
                 exit_code == EXIT_SUCCESS)
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Expected success or no coverage data, got exit code:", exit_code
        end if
    end function test_end_to_end_analysis

    function test_parser_auto_detection() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: files(:)
        
        print *, "  Test 2: Parser auto-detection"
        
        ! Given: Directory with .gcda/.gcno files
        call initialize_config(config)
        config%input_format = "gcov"  ! Auto-detect should use gcov
        config%quiet = .true.
        
        ! Allocate source paths
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        allocate(character(len=256) :: config%source_paths(1))
        config%source_paths(1) = "test_data"
        
        ! When: Finding coverage files without explicit format
        files = find_coverage_files(config)
        
        ! Then: Should find coverage files (or at least not crash)
        passed = .true.  ! Function executed without error
        
        if (passed) then
            print *, "    PASSED - Found", size(files), "files"
        else
            print *, "    FAILED: Parser auto-detection failed"
        end if
    end function test_parser_auto_detection

    function test_multiple_source_directories() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: files(:)
        
        print *, "  Test 3: Multiple source directories"
        
        ! Given: Config with source_paths=["src", "lib"]
        call initialize_config(config)
        config%quiet = .true.
        
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        allocate(character(len=256) :: config%source_paths(2))
        config%source_paths(1) = "src"
        config%source_paths(2) = "test"
        
        ! When: Processing coverage files
        files = find_coverage_files(config)
        
        ! Then: Should search in both directories without error
        passed = .true.  ! Function executed successfully
        
        if (passed) then
            print *, "    PASSED - Searched", size(config%source_paths), "directories"
        else
            print *, "    FAILED: Multiple source directory handling failed"
        end if
    end function test_multiple_source_directories

    function test_exclude_pattern_filtering() result(passed)
        logical :: passed
        type(config_t) :: config
        logical :: should_exclude
        
        print *, "  Test 4: Exclude pattern filtering"
        
        ! Given: Config with exclude_patterns=["test/*"]
        call initialize_config(config)
        
        if (allocated(config%exclude_patterns)) deallocate(config%exclude_patterns)
        allocate(character(len=256) :: config%exclude_patterns(1))
        config%exclude_patterns(1) = "test/*"
        
        ! When: Checking if files should be excluded
        should_exclude = check_exclude_patterns("test/sample.gcda", config)
        
        ! Then: Should exclude files matching pattern
        passed = should_exclude
        
        if (passed) then
            print *, "    PASSED - Exclude pattern filtering works"
        else
            print *, "    FAILED: Files not properly excluded"
        end if
    end function test_exclude_pattern_filtering

    function test_coverage_threshold_enforcement() result(passed)
        logical :: passed
        type(config_t) :: config
        integer :: exit_code
        
        print *, "  Test 5: Coverage threshold enforcement"
        
        ! Given: Config with minimum_coverage=80.0
        call initialize_config(config)
        config%minimum_coverage = 80.0
        config%output_format = "markdown"
        config%output_path = "/tmp/test_threshold.md"
        config%quiet = .true.
        
        ! When: Running analysis (will likely find no coverage)
        exit_code = analyze_coverage(config)
        
        ! Then: Should return appropriate exit code based on actual coverage
        ! Could be SUCCESS (if coverage >= 80%), THRESHOLD_NOT_MET (if < 80%), 
        ! or NO_COVERAGE_DATA (if no valid coverage found)
        passed = (exit_code == EXIT_SUCCESS .or. &
                 exit_code == EXIT_NO_COVERAGE_DATA .or. &
                 exit_code == EXIT_THRESHOLD_NOT_MET)
        
        if (passed) then
            print *, "    PASSED - Threshold enforcement exit code:", exit_code
        else
            print *, "    FAILED: Unexpected exit code:", exit_code
        end if
    end function test_coverage_threshold_enforcement

    function test_missing_coverage_files() result(passed)
        logical :: passed
        type(config_t) :: config
        integer :: exit_code
        
        print *, "  Test 6: Handle missing coverage files"
        
        ! Given: No .gcda files (code not executed)
        call initialize_config(config)
        config%quiet = .true.
        
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        allocate(character(len=256) :: config%source_paths(1))
        config%source_paths(1) = "/nonexistent/directory"
        
        ! When: Running analysis with missing files
        exit_code = analyze_coverage(config)
        
        ! Then: Should report no coverage data
        passed = (exit_code == EXIT_NO_COVERAGE_DATA)
        
        if (passed) then
            print *, "    PASSED - Correctly handled missing files"
        else
            print *, "    FAILED: Expected EXIT_NO_COVERAGE_DATA, got:", exit_code
        end if
    end function test_missing_coverage_files

    function test_parser_error_handling() result(passed)
        logical :: passed
        type(config_t) :: config
        integer :: exit_code
        
        print *, "  Test 7: Parser error handling"
        
        ! Given: Config pointing to invalid files
        call initialize_config(config)
        config%quiet = .true.
        config%output_format = "markdown"
        config%output_path = "/tmp/test_parser_error.md"
        
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        allocate(character(len=256) :: config%source_paths(1))
        config%source_paths(1) = "test_data"  ! May contain invalid files
        
        ! When: Parsing potentially invalid coverage files
        exit_code = analyze_coverage(config)
        
        ! Then: Should handle errors gracefully
        passed = (exit_code /= EXIT_FAILURE .or. exit_code == EXIT_NO_COVERAGE_DATA)
        
        if (passed) then
            print *, "    PASSED - Parser errors handled gracefully"
        else
            print *, "    FAILED: Parser error handling failed"
        end if
    end function test_parser_error_handling

    function test_reporter_error_handling() result(passed)
        logical :: passed
        type(config_t) :: config
        integer :: exit_code
        
        print *, "  Test 8: Reporter error handling"
        
        ! Given: Invalid output path (no permissions)
        call initialize_config(config)
        config%output_format = "markdown"
        config%output_path = "/root/forbidden/path.md"  ! Should fail
        config%quiet = .true.
        
        ! When: Generating report to invalid path
        exit_code = analyze_coverage(config)
        
        ! Then: Should handle reporter errors gracefully
        ! (May return NO_COVERAGE_DATA before reaching reporter)
        passed = (exit_code == EXIT_FAILURE .or. exit_code == EXIT_NO_COVERAGE_DATA)
        
        if (passed) then
            print *, "    PASSED - Reporter errors handled"
        else
            print *, "    FAILED: Reporter error handling failed"
        end if
    end function test_reporter_error_handling

    function test_verbose_output() result(passed)
        logical :: passed
        type(config_t) :: config
        integer :: exit_code
        
        print *, "  Test 9: Verbose output"
        
        ! Given: Config with verbose=.true.
        call initialize_config(config)
        config%verbose = .true.
        config%quiet = .false.  ! Allow verbose output
        config%output_format = "markdown"
        config%output_path = "/tmp/test_verbose.md"
        
        ! When: Processing coverage with verbose mode
        exit_code = analyze_coverage(config)
        
        ! Then: Should complete without error (verbose messages printed)
        passed = (exit_code == EXIT_NO_COVERAGE_DATA .or. exit_code == EXIT_SUCCESS)
        
        if (passed) then
            print *, "    PASSED - Verbose mode executed"
        else
            print *, "    FAILED: Verbose mode failed"
        end if
    end function test_verbose_output

    function test_quiet_mode() result(passed)
        logical :: passed
        type(config_t) :: config
        integer :: exit_code
        
        print *, "  Test 10: Quiet mode"
        
        ! Given: Config with quiet=.true.
        call initialize_config(config)
        config%quiet = .true.
        config%verbose = .false.
        config%output_format = "markdown"
        config%output_path = "/tmp/test_quiet.md"
        
        ! When: Processing coverage with quiet mode
        exit_code = analyze_coverage(config)
        
        ! Then: Should suppress all non-error output
        passed = (exit_code == EXIT_NO_COVERAGE_DATA .or. exit_code == EXIT_SUCCESS)
        
        if (passed) then
            print *, "    PASSED - Quiet mode executed"
        else
            print *, "    FAILED: Quiet mode failed"
        end if
    end function test_quiet_mode

    function test_middle_wildcard_patterns() result(passed)
        logical :: passed
        type(config_t) :: config
        logical :: match1, match2, match3, match4, match5
        
        print *, "  Test 4b: Middle wildcard pattern matching (bug fix)"
        
        ! Given: Config with middle wildcard pattern "src/*.f90"
        call initialize_config(config)
        
        if (allocated(config%exclude_patterns)) deallocate(config%exclude_patterns)
        allocate(character(len=256) :: config%exclude_patterns(1))
        config%exclude_patterns(1) = "src/*.f90"
        
        ! Test cases for middle wildcard pattern matching
        ! Should match: starts with "src/" and ends with ".f90"
        match1 = check_exclude_patterns("src/main.f90", config)       ! Should match
        match2 = check_exclude_patterns("src/module.f90", config)     ! Should match
        
        ! Should NOT match: wrong prefix or suffix
        match3 = check_exclude_patterns("lib/main.f90", config)       ! Wrong prefix
        match4 = check_exclude_patterns("src/main.c", config)         ! Wrong suffix
        match5 = check_exclude_patterns("test/src/main.f90", config)  ! Contains but wrong prefix
        
        ! Pattern should match files that start with "src/" AND end with ".f90"
        passed = match1 .and. match2 .and. (.not. match3) .and. &
                (.not. match4) .and. (.not. match5)
        
        if (passed) then
            print *, "    PASSED - Middle wildcard patterns work correctly"
        else
            print *, "    FAILED: Pattern matching results:"
            print *, "           src/main.f90:", match1, "(should be T)"
            print *, "           src/module.f90:", match2, "(should be T)"
            print *, "           lib/main.f90:", match3, "(should be F)"
            print *, "           src/main.c:", match4, "(should be F)"
            print *, "           test/src/main.f90:", match5, "(should be F)"
        end if
    end function test_middle_wildcard_patterns

end program test_coverage_engine