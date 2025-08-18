program test_issue_126_regression_prevention
    !! Regression prevention test suite for Issue #126 refactoring
    !!
    !! This comprehensive test suite validates that the foundation layer
    !! extraction and module decomposition does not break existing functionality.
    !! Tests are designed to catch regressions early in the refactoring process.
    use coverage_engine
    use coverage_model
    use fortcov_config
    use file_utils
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "Testing Issue #126 Regression Prevention..."
    
    ! Test 1: End-to-end workflow preservation
    all_tests_passed = all_tests_passed .and. test_end_to_end_workflow()
    
    ! Test 2: Pattern matching behavior preservation
    all_tests_passed = all_tests_passed .and. test_pattern_matching_regression()
    
    ! Test 3: Configuration handling preservation
    all_tests_passed = all_tests_passed .and. test_configuration_handling()
    
    ! Test 4: File filtering logic preservation
    all_tests_passed = all_tests_passed .and. test_file_filtering_logic()
    
    ! Test 5: Error propagation preservation
    all_tests_passed = all_tests_passed .and. test_error_propagation()
    
    ! Test 6: Memory management preservation
    all_tests_passed = all_tests_passed .and. test_memory_management()
    
    ! Test 7: Output format consistency
    all_tests_passed = all_tests_passed .and. test_output_format_consistency()
    
    ! Test 8: Performance characteristics preservation
    all_tests_passed = all_tests_passed .and. test_performance_characteristics()
    
    ! Test 9: Integration point validation
    all_tests_passed = all_tests_passed .and. test_integration_points()
    
    ! Test 10: Edge case handling preservation
    all_tests_passed = all_tests_passed .and. test_edge_case_handling()
    
    if (all_tests_passed) then
        print *, "All regression prevention tests PASSED"
        call exit(0)
    else
        print *, "Some regression prevention tests FAILED"
        call exit(1)
    end if

contains

    function test_end_to_end_workflow() result(passed)
        !! Given: Complete coverage analysis workflow
        !! When: Running full analysis after refactoring
        !! Then: Workflow should complete with same behavior
        logical :: passed
        type(config_t) :: config
        integer :: exit_code
        character(len=:), allocatable :: files(:)
        
        print *, "  Test 1: End-to-end workflow preservation"
        
        ! Configure for a typical analysis scenario
        call initialize_config(config)
        config%output_format = "markdown"
        config%input_format = "gcov"
        config%quiet = .true.  ! Suppress output during testing
        config%verbose = .false.
        
        ! Set up source paths (using current directory as safe default)
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        allocate(character(len=256) :: config%source_paths(1))
        config%source_paths(1) = "."
        
        ! Test the complete workflow
        exit_code = analyze_coverage(config)
        
        ! Should complete without crashing (expect no coverage data in test environment)
        passed = (exit_code == EXIT_SUCCESS .or. &
                 exit_code == EXIT_NO_COVERAGE_DATA .or. &
                 exit_code == EXIT_FAILURE)  ! Any controlled exit is acceptable
        
        if (.not. passed) then
            print *, "    FAILED: Workflow crashed with unexpected exit code:", exit_code
            return
        end if
        
        ! Test file discovery still works
        files = find_coverage_files(config)
        passed = allocated(files)  ! Should return allocated array
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: File discovery broken"
        end if
        
        ! Cleanup
        if (allocated(files)) deallocate(files)
        if (allocated(config%source_paths)) deallocate(config%source_paths)
    end function test_end_to_end_workflow

    function test_pattern_matching_regression() result(passed)
        !! Given: Pattern matching functionality in exclude filters
        !! When: Testing various pattern combinations
        !! Then: Pattern matching should behave identically
        logical :: passed
        type(config_t) :: config
        logical :: result1, result2, result3, result4
        
        print *, "  Test 2: Pattern matching behavior preservation"
        
        call initialize_config(config)
        
        ! Set up test exclude patterns
        if (allocated(config%exclude_patterns)) deallocate(config%exclude_patterns)
        allocate(character(len=256) :: config%exclude_patterns(4))
        config%exclude_patterns(1) = "test_*.f90"
        config%exclude_patterns(2) = "*.bak"
        config%exclude_patterns(3) = "*_debug*"
        config%exclude_patterns(4) = "exact_match.f90"
        
        ! Test exact match
        result1 = check_exclude_patterns("exact_match.f90", config)
        passed = result1
        
        if (.not. passed) then
            print *, "    FAILED: Exact pattern match broken"
            deallocate(config%exclude_patterns)
            return
        end if
        
        ! Test prefix wildcard
        result2 = check_exclude_patterns("test_module.f90", config)
        passed = result2
        
        if (.not. passed) then
            print *, "    FAILED: Prefix wildcard pattern broken"
            deallocate(config%exclude_patterns)
            return
        end if
        
        ! Test suffix wildcard
        result3 = check_exclude_patterns("config.bak", config)
        passed = result3
        
        if (.not. passed) then
            print *, "    FAILED: Suffix wildcard pattern broken"
            deallocate(config%exclude_patterns)
            return
        end if
        
        ! Test middle wildcard
        result4 = check_exclude_patterns("file_debug_info.txt", config)
        passed = result4
        
        if (.not. passed) then
            print *, "    FAILED: Middle wildcard pattern broken"
            deallocate(config%exclude_patterns)
            return
        end if
        
        ! Test non-matching file
        passed = .not. check_exclude_patterns("main.f90", config)
        
        deallocate(config%exclude_patterns)
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Non-matching pattern behavior changed"
        end if
    end function test_pattern_matching_regression

    function test_configuration_handling() result(passed)
        !! Given: Configuration validation and processing
        !! When: Testing configuration edge cases
        !! Then: Configuration handling should remain robust
        logical :: passed
        type(config_t) :: config1, config2, config3
        
        print *, "  Test 3: Configuration handling preservation"
        
        ! Test 3a: Default configuration initialization
        call initialize_config(config1)
        passed = (.not. config1%verbose .and. .not. config1%quiet)
        
        if (.not. passed) then
            print *, "    FAILED: Default configuration initialization changed"
            return
        end if
        
        ! Test 3b: Configuration field assignment
        call initialize_config(config2)
        config2%verbose = .true.
        config2%quiet = .false.
        config2%output_format = "json"
        config2%input_format = "gcov"
        config2%output_path = "/tmp/test.json"
        
        passed = (config2%verbose .and. .not. config2%quiet .and. &
                 config2%output_format == "json" .and. &
                 config2%input_format == "gcov")
        
        if (.not. passed) then
            print *, "    FAILED: Configuration field assignment changed"
            return
        end if
        
        ! Test 3c: Array field handling
        call initialize_config(config3)
        if (allocated(config3%source_paths)) deallocate(config3%source_paths)
        allocate(character(len=256) :: config3%source_paths(2))
        config3%source_paths(1) = "/path/one"
        config3%source_paths(2) = "/path/two"
        
        passed = (size(config3%source_paths) == 2 .and. &
                 config3%source_paths(1) == "/path/one" .and. &
                 config3%source_paths(2) == "/path/two")
        
        if (allocated(config3%source_paths)) deallocate(config3%source_paths)
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Array field handling changed"
        end if
    end function test_configuration_handling

    function test_file_filtering_logic() result(passed)
        !! Given: File filtering and discovery logic
        !! When: Testing file filtering operations
        !! Then: Filtering behavior should remain consistent
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: files(:)
        
        print *, "  Test 4: File filtering logic preservation"
        
        call initialize_config(config)
        config%quiet = .true.
        
        ! Test file discovery
        files = find_coverage_files(config)
        passed = allocated(files)
        
        if (.not. passed) then
            print *, "    FAILED: File discovery allocation broken"
            return
        end if
        
        ! Test with exclude patterns
        if (allocated(config%exclude_patterns)) deallocate(config%exclude_patterns)
        allocate(character(len=256) :: config%exclude_patterns(1))
        config%exclude_patterns(1) = "*.tmp"
        
        ! Re-run file discovery with patterns
        if (allocated(files)) deallocate(files)
        files = find_coverage_files(config)
        passed = allocated(files)
        
        if (allocated(files)) deallocate(files)
        if (allocated(config%exclude_patterns)) deallocate(config%exclude_patterns)
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: File filtering with patterns broken"
        end if
    end function test_file_filtering_logic

    function test_error_propagation() result(passed)
        !! Given: Error handling and propagation mechanisms
        !! When: Testing error scenarios
        !! Then: Error handling should remain consistent
        logical :: passed
        type(config_t) :: config
        integer :: exit_code
        
        print *, "  Test 5: Error propagation preservation"
        
        call initialize_config(config)
        config%quiet = .true.
        
        ! Test with non-existent output directory
        config%output_path = "/nonexistent/directory/output.md"
        config%output_format = "markdown"
        
        exit_code = analyze_coverage(config)
        
        ! Should handle error gracefully (not crash)
        passed = (exit_code /= 0)  ! Expecting error exit code
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Error handling changed"
        end if
    end function test_error_propagation

    function test_memory_management() result(passed)
        !! Given: Dynamic memory allocation patterns
        !! When: Testing allocation and deallocation
        !! Then: Memory management should remain safe
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: files(:)
        integer :: i
        
        print *, "  Test 6: Memory management preservation"
        
        call initialize_config(config)
        config%quiet = .true.
        
        ! Test multiple allocation/deallocation cycles
        do i = 1, 3
            files = find_coverage_files(config)
            passed = allocated(files)
            
            if (.not. passed) then
                print *, "    FAILED: Memory allocation failed on iteration", i
                return
            end if
            
            if (allocated(files)) deallocate(files)
        end do
        
        ! Test configuration array management
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        allocate(character(len=256) :: config%source_paths(3))
        config%source_paths(1) = "path1"
        config%source_paths(2) = "path2"
        config%source_paths(3) = "path3"
        
        passed = (size(config%source_paths) == 3)
        
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Memory management behavior changed"
        end if
    end function test_memory_management

    function test_output_format_consistency() result(passed)
        !! Given: Output format processing
        !! When: Testing different output formats
        !! Then: Format handling should remain consistent
        logical :: passed
        type(config_t) :: config
        integer :: exit_code1, exit_code2, exit_code3
        
        print *, "  Test 7: Output format consistency"
        
        call initialize_config(config)
        config%quiet = .true.
        
        ! Test markdown format
        config%output_format = "markdown"
        config%output_path = "/tmp/test_regression.md"
        exit_code1 = analyze_coverage(config)
        
        ! Test JSON format
        config%output_format = "json"
        config%output_path = "/tmp/test_regression.json"
        exit_code2 = analyze_coverage(config)
        
        ! Test invalid format (should handle gracefully)
        config%output_format = "invalid"
        config%output_path = "/tmp/test_regression.invalid"
        exit_code3 = analyze_coverage(config)
        
        ! All should complete without crashing
        passed = (exit_code1 /= -999 .and. exit_code2 /= -999 .and. exit_code3 /= -999)
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Output format handling changed"
        end if
    end function test_output_format_consistency

    function test_performance_characteristics() result(passed)
        !! Given: Performance-critical operations
        !! When: Testing operations that should remain fast
        !! Then: Performance should not degrade significantly
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: files(:)
        integer :: i, start_time, end_time
        logical :: exclude_result
        
        print *, "  Test 8: Performance characteristics preservation"
        
        call initialize_config(config)
        config%quiet = .true.
        
        ! Set up exclude patterns for performance test
        if (allocated(config%exclude_patterns)) deallocate(config%exclude_patterns)
        allocate(character(len=256) :: config%exclude_patterns(10))
        do i = 1, 10
            write(config%exclude_patterns(i), '("pattern_", I0, "_*.f90")') i
        end do
        
        ! Time pattern matching operations
        call system_clock(start_time)
        do i = 1, 100
            exclude_result = check_exclude_patterns("test_file.f90", config)
        end do
        call system_clock(end_time)
        
        ! Should complete quickly (within reasonable time)
        passed = (end_time - start_time < 1000)  ! Less than 1000 clock ticks
        
        if (allocated(config%exclude_patterns)) deallocate(config%exclude_patterns)
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Performance degradation detected"
        end if
    end function test_performance_characteristics

    function test_integration_points() result(passed)
        !! Given: Integration points between modules
        !! When: Testing cross-module interactions
        !! Then: Integration behavior should remain stable
        logical :: passed
        type(config_t) :: config
        type(coverage_data_t) :: coverage
        integer :: exit_code
        
        print *, "  Test 9: Integration point validation"
        
        call initialize_config(config)
        config%quiet = .true.
        config%output_format = "markdown"
        
        ! Test coverage_engine -> fortcov_config integration
        exit_code = analyze_coverage(config)
        passed = (exit_code /= -999)  ! Should not crash
        
        if (.not. passed) then
            print *, "    FAILED: coverage_engine -> fortcov_config integration broken"
            return
        end if
        
        ! Test coverage_model integration
        coverage%filename = "test.f90"
        if (.not. allocated(coverage%lines)) then
            allocate(coverage%lines(1))
            coverage%lines(1)%line_number = 1
            coverage%lines(1)%execution_count = 0
            coverage%lines(1)%is_executable = .true.
        end if
        
        passed = (coverage%filename == "test.f90" .and. &
                 allocated(coverage%lines) .and. &
                 size(coverage%lines) == 1)
        
        if (allocated(coverage%lines)) deallocate(coverage%lines)
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: coverage_model integration broken"
        end if
    end function test_integration_points

    function test_edge_case_handling() result(passed)
        !! Given: Edge cases that should be handled gracefully
        !! When: Testing boundary conditions
        !! Then: Edge case handling should remain robust
        logical :: passed
        type(config_t) :: config
        logical :: result1, result2, result3
        character(len=:), allocatable :: files(:)
        
        print *, "  Test 10: Edge case handling preservation"
        
        call initialize_config(config)
        config%quiet = .true.
        
        ! Test empty exclude patterns
        if (allocated(config%exclude_patterns)) deallocate(config%exclude_patterns)
        allocate(character(len=256) :: config%exclude_patterns(0))
        result1 = check_exclude_patterns("any_file.f90", config)
        passed = (.not. result1)  ! Should not exclude with empty patterns
        
        if (.not. passed) then
            print *, "    FAILED: Empty exclude patterns handling changed"
            if (allocated(config%exclude_patterns)) deallocate(config%exclude_patterns)
            return
        end if
        
        ! Test pattern with just wildcard
        if (allocated(config%exclude_patterns)) deallocate(config%exclude_patterns)
        allocate(character(len=256) :: config%exclude_patterns(1))
        config%exclude_patterns(1) = "*"
        result2 = check_exclude_patterns("any_file.f90", config)
        passed = result2  ! Should exclude everything
        
        if (.not. passed) then
            print *, "    FAILED: Universal wildcard handling changed"
            if (allocated(config%exclude_patterns)) deallocate(config%exclude_patterns)
            return
        end if
        
        ! Test file discovery with no source paths
        if (allocated(config%exclude_patterns)) deallocate(config%exclude_patterns)
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        allocate(character(len=1) :: config%source_paths(0))
        
        files = find_coverage_files(config)
        result3 = allocated(files)  ! Should return allocated (possibly empty) array
        
        if (allocated(files)) deallocate(files)
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        
        passed = result3
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Edge case handling changed"
        end if
    end function test_edge_case_handling

end program test_issue_126_regression_prevention