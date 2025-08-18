program test_interface_contract_validation
    !! Interface contract validation for Issue #126 refactoring
    !!
    !! This test suite validates that all public interfaces maintain their
    !! contracts during the foundation layer extraction and module decomposition.
    !! Each test verifies that function signatures, return types, and behavioral
    !! contracts remain unchanged.
    use coverage_engine
    use coverage_model  
    use fortcov_config
    use coverage_parser
    use coverage_statistics
    use coverage_reporter
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "Testing Interface Contract Validation..."
    
    ! Test 1: analyze_coverage function contract
    all_tests_passed = all_tests_passed .and. test_analyze_coverage_contract()
    
    ! Test 2: find_coverage_files function contract
    all_tests_passed = all_tests_passed .and. test_find_coverage_files_contract()
    
    ! Test 3: check_exclude_patterns function contract
    all_tests_passed = all_tests_passed .and. test_check_exclude_patterns_contract()
    
    ! Test 4: Type definition contracts
    all_tests_passed = all_tests_passed .and. test_type_definition_contracts()
    
    ! Test 5: Configuration interface contracts
    all_tests_passed = all_tests_passed .and. test_configuration_contracts()
    
    ! Test 6: Coverage model contracts
    all_tests_passed = all_tests_passed .and. test_coverage_model_contracts()
    
    ! Test 7: Error handling contracts
    all_tests_passed = all_tests_passed .and. test_error_handling_contracts()
    
    ! Test 8: Memory management contracts
    all_tests_passed = all_tests_passed .and. test_memory_management_contracts()
    
    ! Test 9: Parser interface contracts
    all_tests_passed = all_tests_passed .and. test_parser_interface_contracts()
    
    ! Test 10: Reporter interface contracts
    all_tests_passed = all_tests_passed .and. test_reporter_interface_contracts()
    
    if (all_tests_passed) then
        print *, "All interface contract validation tests PASSED"
        call exit(0)
    else
        print *, "Some interface contract validation tests FAILED"
        call exit(1)
    end if

contains

    function test_analyze_coverage_contract() result(passed)
        !! Given: analyze_coverage function with defined contract
        !! When: Calling function with valid and invalid inputs
        !! Then: Function should maintain its behavioral contract
        logical :: passed
        type(config_t) :: valid_config, invalid_config
        integer :: valid_result, invalid_result
        
        print *, "  Test 1: analyze_coverage function contract"
        
        ! Contract: analyze_coverage(config_t) -> integer
        ! Pre-conditions: config must be initialized
        ! Post-conditions: returns valid exit code (0-3)
        
        ! Test 1a: Valid configuration
        call initialize_config(valid_config)
        valid_config%output_format = "markdown"
        valid_config%input_format = "gcov"
        valid_config%quiet = .true.
        
        valid_result = analyze_coverage(valid_config)
        passed = (valid_result >= 0 .and. valid_result <= 3)
        
        if (.not. passed) then
            print *, "    FAILED: Valid config returned invalid exit code:", valid_result
            return
        end if
        
        ! Test 1b: Configuration with invalid output format
        call initialize_config(invalid_config)
        invalid_config%output_format = "invalid_format"
        invalid_config%input_format = "gcov"
        invalid_config%quiet = .true.
        
        invalid_result = analyze_coverage(invalid_config)
        passed = (invalid_result >= 0 .and. invalid_result <= 3)
        
        if (.not. passed) then
            print *, "    FAILED: Invalid config returned out-of-range exit code:", invalid_result
            return
        end if
        
        ! Contract verification: function should not crash and return valid codes
        passed = .true.
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: analyze_coverage contract violated"
        end if
    end function test_analyze_coverage_contract

    function test_find_coverage_files_contract() result(passed)
        !! Given: find_coverage_files function with defined contract
        !! When: Calling function with various configurations
        !! Then: Function should maintain its behavioral contract
        logical :: passed
        type(config_t) :: config1, config2
        character(len=:), allocatable :: files1(:), files2(:)
        
        print *, "  Test 2: find_coverage_files function contract"
        
        ! Contract: find_coverage_files(config_t) -> character(:), allocatable
        ! Pre-conditions: config must be initialized
        ! Post-conditions: returns allocated array (may be empty)
        
        ! Test 2a: Default configuration
        call initialize_config(config1)
        files1 = find_coverage_files(config1)
        
        passed = allocated(files1)
        
        if (.not. passed) then
            print *, "    FAILED: find_coverage_files did not return allocated array"
            return
        end if
        
        ! Test 2b: Configuration with source paths
        call initialize_config(config2)
        if (allocated(config2%source_paths)) deallocate(config2%source_paths)
        allocate(character(len=256) :: config2%source_paths(2))
        config2%source_paths(1) = "."
        config2%source_paths(2) = "./test"
        
        files2 = find_coverage_files(config2)
        passed = allocated(files2)
        
        if (.not. passed) then
            print *, "    FAILED: find_coverage_files with source paths did not return allocated array"
            if (allocated(files1)) deallocate(files1)
            if (allocated(config2%source_paths)) deallocate(config2%source_paths)
            return
        end if
        
        ! Contract verification: always returns allocated array
        passed = .true.
        
        ! Cleanup
        if (allocated(files1)) deallocate(files1)
        if (allocated(files2)) deallocate(files2)
        if (allocated(config2%source_paths)) deallocate(config2%source_paths)
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: find_coverage_files contract violated"
        end if
    end function test_find_coverage_files_contract

    function test_check_exclude_patterns_contract() result(passed)
        !! Given: check_exclude_patterns function with defined contract
        !! When: Testing with various pattern configurations
        !! Then: Function should maintain its behavioral contract
        logical :: passed
        type(config_t) :: config1, config2, config3
        logical :: result1, result2, result3, result4
        
        print *, "  Test 3: check_exclude_patterns function contract"
        
        ! Contract: check_exclude_patterns(character(*), config_t) -> logical
        ! Pre-conditions: filepath is valid string, config is initialized
        ! Post-conditions: returns boolean indicating exclusion
        
        ! Test 3a: No exclude patterns
        call initialize_config(config1)
        result1 = check_exclude_patterns("test.f90", config1)
        passed = (.not. result1)  ! Should not exclude with no patterns
        
        if (.not. passed) then
            print *, "    FAILED: No patterns should not exclude files"
            return
        end if
        
        ! Test 3b: Empty exclude patterns array
        call initialize_config(config2)
        if (allocated(config2%exclude_patterns)) deallocate(config2%exclude_patterns)
        allocate(character(len=256) :: config2%exclude_patterns(0))
        result2 = check_exclude_patterns("test.f90", config2)
        passed = (.not. result2)  ! Should not exclude with empty array
        
        if (.not. passed) then
            print *, "    FAILED: Empty patterns array should not exclude files"
            if (allocated(config2%exclude_patterns)) deallocate(config2%exclude_patterns)
            return
        end if
        
        ! Test 3c: Matching pattern
        call initialize_config(config3)
        if (allocated(config3%exclude_patterns)) deallocate(config3%exclude_patterns)
        allocate(character(len=256) :: config3%exclude_patterns(1))
        config3%exclude_patterns(1) = "test.f90"
        result3 = check_exclude_patterns("test.f90", config3)
        passed = result3  ! Should exclude exact match
        
        if (.not. passed) then
            print *, "    FAILED: Exact match should exclude file"
            if (allocated(config3%exclude_patterns)) deallocate(config3%exclude_patterns)
            return
        end if
        
        ! Test 3d: Non-matching pattern
        result4 = check_exclude_patterns("other.f90", config3)
        passed = (.not. result4)  ! Should not exclude non-match
        
        if (allocated(config2%exclude_patterns)) deallocate(config2%exclude_patterns)
        if (allocated(config3%exclude_patterns)) deallocate(config3%exclude_patterns)
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: check_exclude_patterns contract violated"
        end if
    end function test_check_exclude_patterns_contract

    function test_type_definition_contracts() result(passed)
        !! Given: Public type definitions with field contracts
        !! When: Accessing and modifying type fields
        !! Then: Type definitions should maintain their contracts
        logical :: passed
        type(config_t) :: config
        type(coverage_data_t) :: coverage
        type(coverage_line_t) :: line
        type(coverage_stats_t) :: stats
        
        print *, "  Test 4: Type definition contracts"
        
        ! Test 4a: config_t type contract
        call initialize_config(config)
        config%verbose = .true.
        config%quiet = .false.
        config%output_format = "json"
        config%input_format = "gcov"
        config%output_path = "/tmp/test.json"
        
        passed = (config%verbose .and. .not. config%quiet .and. &
                 config%output_format == "json" .and. &
                 config%input_format == "gcov" .and. &
                 config%output_path == "/tmp/test.json")
        
        if (.not. passed) then
            print *, "    FAILED: config_t type contract violated"
            return
        end if
        
        ! Test 4b: coverage_data_t type contract
        allocate(coverage%files(1))
        coverage%files(1)%filename = "test.f90"
        if (.not. allocated(coverage%files(1)%lines)) then
            allocate(coverage%files(1)%lines(2))
        end if
        
        passed = (allocated(coverage%files) .and. &
                 size(coverage%files) == 1 .and. &
                 coverage%files(1)%filename == "test.f90" .and. &
                 allocated(coverage%files(1)%lines) .and. &
                 size(coverage%files(1)%lines) == 2)
        
        if (.not. passed) then
            print *, "    FAILED: coverage_data_t type contract violated"
            if (allocated(coverage%files)) deallocate(coverage%files)
            return
        end if
        
        ! Test 4c: coverage_line_t type contract
        line%line_number = 42
        line%execution_count = 5
        line%is_executable = .true.
        
        passed = (line%line_number == 42 .and. &
                 line%execution_count == 5 .and. &
                 line%is_executable)
        
        if (.not. passed) then
            print *, "    FAILED: coverage_line_t type contract violated"
            if (allocated(coverage%files)) deallocate(coverage%files)
            return
        end if
        
        ! Test 4d: coverage_stats_t type contract
        stats%total_count = 100
        stats%covered_count = 60
        stats%percentage = 60.0
        
        passed = (stats%total_count == 100 .and. &
                 stats%covered_count == 60 .and. &
                 abs(stats%percentage - 60.0) < 1.0e-6)
        
        if (allocated(coverage%files)) deallocate(coverage%files)
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: coverage_stats_t type contract violated"
        end if
    end function test_type_definition_contracts

    function test_configuration_contracts() result(passed)
        !! Given: Configuration initialization and validation contracts
        !! When: Using configuration operations
        !! Then: Configuration contracts should be maintained
        logical :: passed
        type(config_t) :: config
        
        print *, "  Test 5: Configuration interface contracts"
        
        ! Contract: initialize_config sets reasonable defaults
        call initialize_config(config)
        
        ! Verify default state
        passed = (.not. config%verbose .and. .not. config%quiet)
        
        if (.not. passed) then
            print *, "    FAILED: initialize_config default contract violated"
            return
        end if
        
        ! Contract: fields can be safely modified
        config%verbose = .true.
        config%quiet = .true.  ! This combination should be allowed
        config%output_format = "markdown"
        config%input_format = "gcov"
        
        passed = (config%verbose .and. config%quiet .and. &
                 config%output_format == "markdown" .and. &
                 config%input_format == "gcov")
        
        if (.not. passed) then
            print *, "    FAILED: Configuration modification contract violated"
            return
        end if
        
        ! Contract: arrays can be safely allocated/deallocated
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        allocate(character(len=256) :: config%source_paths(3))
        config%source_paths(1) = "path1"
        config%source_paths(2) = "path2"
        config%source_paths(3) = "path3"
        
        passed = (size(config%source_paths) == 3 .and. &
                 config%source_paths(2) == "path2")
        
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Configuration array contract violated"
        end if
    end function test_configuration_contracts

    function test_coverage_model_contracts() result(passed)
        !! Given: Coverage model type and operation contracts
        !! When: Using coverage model operations
        !! Then: Model contracts should be maintained
        logical :: passed
        type(coverage_data_t) :: coverage1, coverage2
        type(coverage_line_t) :: line1, line2
        
        print *, "  Test 6: Coverage model contracts"
        
        ! Contract: coverage_data_t can be independently created
        call coverage1%init()
        call coverage2%init()
        
        passed = (allocated(coverage1%files) .and. &
                 allocated(coverage2%files))
        
        if (.not. passed) then
            print *, "    FAILED: Independent coverage_data_t creation failed"
            return
        end if
        
        ! Contract: files array can be allocated independently
        allocate(coverage1%files(2))
        allocate(coverage2%files(3))
        
        passed = (allocated(coverage1%files) .and. allocated(coverage2%files) .and. &
                 size(coverage1%files) == 2 .and. size(coverage2%files) == 3)
        
        if (.not. passed) then
            print *, "    FAILED: Independent files allocation failed"
            if (allocated(coverage1%files)) deallocate(coverage1%files)
            if (allocated(coverage2%files)) deallocate(coverage2%files)
            return
        end if
        
        ! Contract: coverage_line_t fields are independent
        line1%line_number = 10
        line1%execution_count = 3
        line1%is_executable = .true.
        
        line2%line_number = 20
        line2%execution_count = 0
        line2%is_executable = .false.
        
        passed = (line1%line_number == 10 .and. line2%line_number == 20 .and. &
                 line1%execution_count == 3 .and. line2%execution_count == 0 .and. &
                 line1%is_executable .and. .not. line2%is_executable)
        
        if (allocated(coverage1%files)) deallocate(coverage1%files)
        if (allocated(coverage2%files)) deallocate(coverage2%files)
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Coverage model field independence failed"
        end if
    end function test_coverage_model_contracts

    function test_error_handling_contracts() result(passed)
        !! Given: Error handling behavioral contracts
        !! When: Testing error scenarios
        !! Then: Error handling should follow contracts
        logical :: passed
        type(config_t) :: config
        integer :: exit_code
        
        print *, "  Test 7: Error handling contracts"
        
        ! Contract: Functions should not crash on invalid input
        call initialize_config(config)
        config%quiet = .true.
        
        ! Test invalid output format
        config%output_format = "invalid_format_xyz"
        exit_code = analyze_coverage(config)
        
        ! Should return error code, not crash
        passed = (exit_code >= 0 .and. exit_code <= 10)  ! Reasonable error code range
        
        if (.not. passed) then
            print *, "    FAILED: Error handling contract violated, exit code:", exit_code
            return
        end if
        
        # Contract: Invalid paths should be handled gracefully
        config%output_format = "markdown"
        config%output_path = "/invalid/nonexistent/path/output.md"
        exit_code = analyze_coverage(config)
        
        # Should handle gracefully
        passed = (exit_code >= 0 .and. exit_code <= 10)
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Invalid path handling contract violated"
        end if
    end function test_error_handling_contracts

    function test_memory_management_contracts() result(passed)
        !! Given: Memory management contracts for allocatable arrays
        !! When: Allocating and deallocating memory
        !! Then: Memory operations should follow contracts
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: files(:)
        integer :: i
        
        print *, "  Test 8: Memory management contracts"
        
        call initialize_config(config)
        config%quiet = .true.
        
        # Contract: Repeated allocation/deallocation should work safely
        do i = 1, 5
            files = find_coverage_files(config)
            passed = allocated(files)
            
            if (.not. passed) then
                print *, "    FAILED: Memory allocation failed on iteration", i
                return
            end if
            
            if (allocated(files)) deallocate(files)
        end do
        
        # Contract: Configuration arrays should be manageable
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        allocate(character(len=256) :: config%source_paths(2))
        config%source_paths(1) = "test1"
        config%source_paths(2) = "test2"
        
        passed = (size(config%source_paths) == 2)
        
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Memory management contract violated"
        end if
    end function test_memory_management_contracts

    function test_parser_interface_contracts() result(passed)
        !! Given: Parser interface contracts (if accessible)
        !! When: Testing parser operations
        !! Then: Parser contracts should be maintained
        logical :: passed
        
        print *, "  Test 9: Parser interface contracts"
        
        # Note: Parser interfaces might not be directly testable if they're
        # internal to coverage_engine. This test verifies that parser
        # functionality through public interfaces remains consistent.
        
        # Contract: Parser behavior should be stable through public interface
        # We test this indirectly through analyze_coverage
        type(config_t) :: config
        integer :: exit_code
        
        call initialize_config(config)
        config%quiet = .true.
        config%input_format = "gcov"
        config%output_format = "markdown"
        
        exit_code = analyze_coverage(config)
        
        # Should complete without crashing (even if no data found)
        passed = (exit_code >= 0 .and. exit_code <= 3)
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Parser interface contract violated"
        end if
    end function test_parser_interface_contracts

    function test_reporter_interface_contracts() result(passed)
        !! Given: Reporter interface contracts (if accessible)
        !! When: Testing reporter operations
        !! Then: Reporter contracts should be maintained
        logical :: passed
        type(config_t) :: config
        integer :: exit_code1, exit_code2
        
        print *, "  Test 10: Reporter interface contracts"
        
        # Contract: Different output formats should be supported
        call initialize_config(config)
        config%quiet = .true.
        config%input_format = "gcov"
        
        # Test markdown format
        config%output_format = "markdown"
        config%output_path = "/tmp/test_contract.md"
        exit_code1 = analyze_coverage(config)
        
        # Test JSON format
        config%output_format = "json"
        config%output_path = "/tmp/test_contract.json"
        exit_code2 = analyze_coverage(config)
        
        # Both should complete without crashing
        passed = (exit_code1 >= 0 .and. exit_code1 <= 3 .and. &
                 exit_code2 >= 0 .and. exit_code2 <= 3)
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Reporter interface contract violated"
        end if
    end function test_reporter_interface_contracts

end program test_interface_contract_validation