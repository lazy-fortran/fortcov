program test_refactoring_boundary_validation
    !! Module boundary validation tests for Issue #126 refactoring
    !! 
    !! This test suite validates that module interfaces remain stable
    !! during the foundation layer extraction and module decomposition.
    !! Tests ensure that all public interfaces continue to work correctly
    !! after refactoring operations.
    use coverage_engine
    use coverage_model
    use coverage_statistics
    use fortcov_config
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "Testing Module Boundary Validation for Refactoring..."
    
    ! Test 1: Coverage engine public interface stability
    all_tests_passed = all_tests_passed .and. test_coverage_engine_interface()
    
    ! Test 2: Configuration module interface stability
    all_tests_passed = all_tests_passed .and. test_config_module_interface()
    
    ! Test 3: Coverage model interface stability
    all_tests_passed = all_tests_passed .and. test_coverage_model_interface()
    
    ! Test 4: Cross-module dependency validation
    all_tests_passed = all_tests_passed .and. test_cross_module_dependencies()
    
    ! Test 5: Public constant accessibility
    all_tests_passed = all_tests_passed .and. test_public_constants_access()
    
    ! Test 6: Function signature compatibility
    all_tests_passed = all_tests_passed .and. test_function_signatures()
    
    ! Test 7: Type definition stability
    all_tests_passed = all_tests_passed .and. test_type_definitions()
    
    ! Test 8: Error handling interface consistency
    all_tests_passed = all_tests_passed .and. test_error_handling_interfaces()
    
    if (all_tests_passed) then
        print *, "All module boundary validation tests PASSED"
        call exit(0)
    else
        print *, "Some module boundary validation tests FAILED"
        call exit(1)
    end if

contains

    function test_coverage_engine_interface() result(passed)
        !! Given: Coverage engine module with public procedures
        !! When: Accessing public interface after refactoring
        !! Then: All public procedures should remain accessible
        logical :: passed
        type(config_t) :: test_config
        integer :: exit_code
        character(len=:), allocatable :: files(:)
        logical :: exclude_result
        
        print *, "  Test 1: Coverage engine public interface stability"
        
        ! Initialize test configuration
        call initialize_config(test_config)
        test_config%quiet = .true.
        
        ! Test 1a: analyze_coverage function is accessible
        exit_code = analyze_coverage(test_config)
        passed = (exit_code == EXIT_SUCCESS .or. exit_code == EXIT_NO_COVERAGE_DATA)
        
        if (.not. passed) then
            print *, "    FAILED: analyze_coverage function not accessible"
            return
        end if
        
        ! Test 1b: find_coverage_files function is accessible
        files = find_coverage_files(test_config)
        passed = allocated(files)  ! Should return allocated array (even if empty)
        
        if (.not. passed) then
            print *, "    FAILED: find_coverage_files function not accessible"
            return
        end if
        
        ! Test 1c: check_exclude_patterns function is accessible
        if (allocated(test_config%exclude_patterns)) deallocate(test_config%exclude_patterns)
        allocate(character(len=10) :: test_config%exclude_patterns(1))
        test_config%exclude_patterns(1) = "test_*"
        exclude_result = check_exclude_patterns("test_file.f90", test_config)
        passed = .true.  ! Function should execute without error
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: check_exclude_patterns function not accessible"
        end if
        
        ! Cleanup
        if (allocated(files)) deallocate(files)
        if (allocated(test_config%exclude_patterns)) deallocate(test_config%exclude_patterns)
    end function test_coverage_engine_interface

    function test_config_module_interface() result(passed)
        !! Given: Configuration module with public types and procedures
        !! When: Accessing configuration interfaces after refactoring
        !! Then: All configuration operations should work correctly
        logical :: passed
        type(config_t) :: config
        logical :: init_success
        
        print *, "  Test 2: Configuration module interface stability"
        
        ! Test 2a: config_t type is accessible
        call initialize_config(config)
        init_success = .true.  ! If we get here, type is accessible
        
        ! Test 2b: Configuration fields are accessible
        config%verbose = .true.
        config%quiet = .false.
        config%output_format = "markdown"
        config%input_format = "gcov"
        
        ! Test 2c: Configuration validation (if available)
        passed = (config%output_format == "markdown" .and. &
                 config%input_format == "gcov" .and. &
                 config%verbose .and. .not. config%quiet)
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Configuration interface not stable"
        end if
    end function test_config_module_interface

    function test_coverage_model_interface() result(passed)
        !! Given: Coverage model with public types
        !! When: Accessing coverage data types after refactoring
        !! Then: All type operations should work correctly
        logical :: passed
        type(coverage_data_t) :: coverage
        type(coverage_line_t) :: line
        type(coverage_stats_t) :: stats
        
        print *, "  Test 3: Coverage model interface stability"
        
        ! Test 3a: coverage_data_t type is accessible
        allocate(coverage%files(1))
        coverage%files(1)%filename = "test.f90"
        
        ! Test 3b: coverage_line_t type is accessible
        line%line_number = 1
        line%execution_count = 0
        line%is_executable = .true.
        
        ! Test 3c: coverage_stats_t type is accessible
        stats%total_count = 100
        stats%covered_count = 60
        stats%percentage = 60.0
        
        ! Verify basic field access works
        passed = (coverage%files(1)%filename == "test.f90" .and. &
                 line%line_number == 1 .and. &
                 stats%total_count == 100)
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Coverage model interface not stable"
        end if
    end function test_coverage_model_interface

    function test_cross_module_dependencies() result(passed)
        !! Given: Modules with cross-dependencies
        !! When: Refactoring changes module structure
        !! Then: Dependencies should remain resolved
        logical :: passed
        type(config_t) :: config
        type(coverage_data_t) :: coverage
        integer :: result
        
        print *, "  Test 4: Cross-module dependency validation"
        
        ! Test 4a: coverage_engine can use config types
        call initialize_config(config)
        config%quiet = .true.
        result = analyze_coverage(config)
        
        ! Test 4b: Operations that cross module boundaries work
        allocate(coverage%files(1))
        coverage%files(1)%filename = "test.f90"
        
        ! If we reach here without compilation errors, dependencies are valid
        passed = .true.
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Cross-module dependencies broken"
        end if
    end function test_cross_module_dependencies

    function test_public_constants_access() result(passed)
        !! Given: Public constants defined in modules
        !! When: Accessing constants after refactoring
        !! Then: All constants should remain accessible
        logical :: passed
        integer :: success_code, failure_code, threshold_code, no_data_code
        
        print *, "  Test 5: Public constant accessibility"
        
        ! Test access to exit code constants from coverage_engine
        success_code = EXIT_SUCCESS
        failure_code = EXIT_FAILURE
        threshold_code = EXIT_THRESHOLD_NOT_MET
        no_data_code = EXIT_NO_COVERAGE_DATA
        
        ! Verify constants have expected values
        passed = (success_code == 0 .and. &
                 failure_code == 1 .and. &
                 threshold_code == 2 .and. &
                 no_data_code == 3)
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Public constants not accessible or incorrect values"
        end if
    end function test_public_constants_access

    function test_function_signatures() result(passed)
        !! Given: Public functions with specific signatures
        !! When: Calling functions after refactoring
        !! Then: Function signatures should remain unchanged
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: files(:)
        logical :: exclude_result
        integer :: exit_code
        
        print *, "  Test 6: Function signature compatibility"
        
        call initialize_config(config)
        config%quiet = .true.
        
        ! Test function signatures haven't changed
        ! These calls verify the compiler accepts the expected signatures
        
        ! analyze_coverage(config_t) -> integer
        exit_code = analyze_coverage(config)
        
        ! find_coverage_files(config_t) -> character(:), allocatable
        files = find_coverage_files(config)
        
        ! check_exclude_patterns(character(*), config_t) -> logical
        if (allocated(config%exclude_patterns)) deallocate(config%exclude_patterns)
        allocate(character(len=256) :: config%exclude_patterns(0))  ! Empty array
        exclude_result = check_exclude_patterns("test.f90", config)
        
        ! If compilation succeeds and we reach here, signatures are compatible
        passed = .true.
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Function signatures changed"
        end if
        
        ! Cleanup
        if (allocated(files)) deallocate(files)
        if (allocated(config%exclude_patterns)) deallocate(config%exclude_patterns)
    end function test_function_signatures

    function test_type_definitions() result(passed)
        !! Given: Public type definitions
        !! When: Using types after refactoring
        !! Then: Type definitions should remain stable
        logical :: passed
        type(config_t) :: config
        type(coverage_data_t) :: coverage
        type(coverage_line_t) :: line
        type(coverage_stats_t) :: stats
        
        print *, "  Test 7: Type definition stability"
        
        ! Test that all expected fields are accessible
        
        ! Config type fields
        config%verbose = .false.
        config%quiet = .true.
        config%output_format = "json"
        config%input_format = "gcov"
        config%output_path = "/tmp/test.json"
        
        ! Coverage data type fields
        allocate(coverage%files(1))
        coverage%files(1)%filename = "test.f90"
        if (.not. allocated(coverage%files(1)%lines)) then
            allocate(coverage%files(1)%lines(1))
        end if
        
        ! Coverage line type fields
        line%line_number = 1
        line%execution_count = 5
        line%is_executable = .true.
        
        ! Coverage statistics type fields
        stats%total_count = 100
        stats%covered_count = 60
        stats%percentage = 60.0
        
        ! If we can access all these fields, type definitions are stable
        passed = .true.
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Type definitions changed"
        end if
        
        ! Cleanup
        if (allocated(coverage%files(1)%lines)) deallocate(coverage%files(1)%lines)
    end function test_type_definitions

    function test_error_handling_interfaces() result(passed)
        !! Given: Error handling interfaces
        !! When: Using error handling after refactoring
        !! Then: Error handling should work consistently
        logical :: passed
        type(config_t) :: config
        integer :: exit_code
        
        print *, "  Test 8: Error handling interface consistency"
        
        ! Test error handling by providing invalid configuration
        call initialize_config(config)
        config%quiet = .true.
        config%output_format = "invalid_format"  ! Should cause graceful error
        
        exit_code = analyze_coverage(config)
        
        ! Should return failure code, not crash
        passed = (exit_code /= EXIT_SUCCESS)  ! Expecting error due to invalid format
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Error handling not consistent"
        end if
    end function test_error_handling_interfaces

end program test_refactoring_boundary_validation