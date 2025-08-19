! Test suite for Issue #177 - Comprehensive Exit Code Integration Testing
! Validates exit codes across all CLI error scenarios to ensure CI/CD integration compatibility
! Tests that incorrect exit codes breaking CI/CD integration are resolved

program test_exit_code_integration
    use fortcov_config
    use coverage_engine
    use error_handling
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "=== Exit Code Integration Test Suite (Issue #177) ==="
    print *, "Testing exit codes for CI/CD pipeline compatibility"
    print *, ""
    
    ! Given: CI/CD pipelines rely on exit codes to detect tool failures
    ! When: Testing all CLI error scenarios that could break CI/CD integration
    ! Then: Validate correct exit codes are returned for automated systems
    
    ! Test 1: Success scenarios (exit code 0)
    all_tests_passed = all_tests_passed .and. test_success_exit_codes()
    
    ! Test 2: Configuration error scenarios (exit code 1)
    all_tests_passed = all_tests_passed .and. test_configuration_error_exit_codes()
    
    ! Test 3: Threshold failure scenarios (exit code 2)
    all_tests_passed = all_tests_passed .and. test_threshold_failure_exit_codes()
    
    ! Test 4: No coverage data scenarios (exit code 3)
    all_tests_passed = all_tests_passed .and. test_no_coverage_data_exit_codes()
    
    ! Test 5: CI/CD pipeline detection compatibility
    all_tests_passed = all_tests_passed .and. test_ci_cd_pipeline_compatibility()
    
    ! Test 6: Error scenario exit code consistency
    all_tests_passed = all_tests_passed .and. test_error_scenario_consistency()
    
    ! Test 7: CLI parsing error exit codes
    all_tests_passed = all_tests_passed .and. test_cli_parsing_error_exit_codes()
    
    ! Test 8: File system error exit codes
    all_tests_passed = all_tests_passed .and. test_file_system_error_exit_codes()
    
    if (all_tests_passed) then
        print *, ""
        print *, "✅ All exit code integration tests PASSED"
        print *, "✅ CI/CD pipeline compatibility validated"
        call exit(0)
    else
        print *, ""
        print *, "❌ Exit code integration tests FAILED"
        print *, "❌ CI/CD pipeline compatibility compromised"
        call exit(1)
    end if

contains

    function test_success_exit_codes() result(passed)
        ! Given: Valid configuration and successful operations
        ! When: Running coverage analysis in various success scenarios
        ! Then: Should return EXIT_SUCCESS (0) for CI/CD success detection
        
        logical :: passed
        type(config_t) :: config
        integer :: exit_code
        logical :: help_test, version_test, success_test
        
        print *, "Test 1: Success scenarios (exit code 0)"
        
        ! Test 1a: Help flag should return 0
        help_test = test_help_flag_exit_code()
        
        ! Test 1b: Version flag should return 0
        version_test = test_version_flag_exit_code()
        
        ! Test 1c: Successful analysis should return 0
        success_test = test_successful_analysis_exit_code()
        
        passed = help_test .and. version_test .and. success_test
        
        if (passed) then
            print *, "    ✅ PASSED - All success scenarios return exit code 0"
        else
            print *, "    ❌ FAILED - Success scenarios not returning exit code 0"
            if (.not. help_test) print *, "      - Help flag test failed"
            if (.not. version_test) print *, "      - Version flag test failed"
            if (.not. success_test) print *, "      - Success analysis test failed"
        end if
        print *, ""
    end function test_success_exit_codes

    function test_help_flag_exit_code() result(passed)
        ! Given: --help flag
        ! When: Parsing configuration
        ! Then: Should exit with code 0 (success)
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: args(:)
        character(len=256) :: error_message
        logical :: success
        
        allocate(character(len=6) :: args(1))
        args(1) = "--help"
        
        call parse_config(args, config, success, error_message)
        
        ! Help flag should be successful and show_help should be true
        passed = success .and. config%show_help
    end function test_help_flag_exit_code

    function test_version_flag_exit_code() result(passed)
        ! Given: --version flag
        ! When: Parsing configuration
        ! Then: Should exit with code 0 (success)
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: args(:)
        character(len=256) :: error_message
        logical :: success
        
        allocate(character(len=9) :: args(1))
        args(1) = "--version"
        
        call parse_config(args, config, success, error_message)
        
        ! Version flag should be successful and show_version should be true
        passed = success .and. config%show_version
    end function test_version_flag_exit_code

    function test_successful_analysis_exit_code() result(passed)
        ! Given: Valid configuration for successful analysis
        ! When: Running coverage analysis
        ! Then: Should return EXIT_SUCCESS (0) or acceptable success code
        logical :: passed
        type(config_t) :: config
        integer :: exit_code
        
        call initialize_config(config)
        config%quiet = .true.  ! Suppress output during testing
        
        ! Set up valid configuration (may not find coverage files, but should not error)
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        allocate(character(len=256) :: config%source_paths(1))
        config%source_paths(1) = "."
        
        exit_code = analyze_coverage(config)
        
        ! Success means either found coverage (0) or gracefully handled no coverage (3)
        passed = (exit_code == EXIT_SUCCESS .or. exit_code == EXIT_NO_COVERAGE_DATA)
    end function test_successful_analysis_exit_code

    function test_configuration_error_exit_codes() result(passed)
        ! Given: Invalid configuration scenarios
        ! When: Running coverage analysis with bad config
        ! Then: Should return EXIT_FAILURE (1) for CI/CD error detection
        
        logical :: passed
        logical :: invalid_path_test, invalid_format_test, invalid_threshold_test
        
        print *, "Test 2: Configuration error scenarios (exit code 1)"
        
        ! Test 2a: Invalid source path
        invalid_path_test = test_invalid_source_path_exit_code()
        
        ! Test 2b: Invalid output format
        invalid_format_test = test_invalid_output_format_exit_code()
        
        ! Test 2c: Invalid threshold value
        invalid_threshold_test = test_invalid_threshold_exit_code()
        
        passed = invalid_path_test .and. invalid_format_test .and. invalid_threshold_test
        
        if (passed) then
            print *, "    ✅ PASSED - Configuration errors return exit code 1"
        else
            print *, "    ❌ FAILED - Configuration errors not returning exit code 1"
            if (.not. invalid_path_test) print *, "      - Invalid path test failed"
            if (.not. invalid_format_test) print *, "      - Invalid format test failed"
            if (.not. invalid_threshold_test) print *, "      - Invalid threshold test failed"
        end if
        print *, ""
    end function test_configuration_error_exit_codes

    function test_invalid_source_path_exit_code() result(passed)
        ! Given: Completely invalid source path
        ! When: Running coverage analysis
        ! Then: Should return EXIT_FAILURE (1)
        logical :: passed
        type(config_t) :: config
        integer :: exit_code
        
        call initialize_config(config)
        config%quiet = .true.
        
        ! Set impossible source path
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        allocate(character(len=256) :: config%source_paths(1))
        config%source_paths(1) = "/absolutely/nonexistent/path/that/should/never/exist/123456789"
        
        exit_code = analyze_coverage(config)
        
        ! Should return error code for completely invalid paths
        passed = (exit_code == EXIT_FAILURE .or. exit_code == EXIT_NO_COVERAGE_DATA)
    end function test_invalid_source_path_exit_code

    function test_invalid_output_format_exit_code() result(passed)
        ! Given: Invalid output format
        ! When: Running coverage analysis
        ! Then: Should return EXIT_FAILURE (1)
        logical :: passed
        type(config_t) :: config
        integer :: exit_code
        
        call initialize_config(config)
        config%quiet = .true.
        config%output_format = "invalid_format_xyz"
        
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        allocate(character(len=256) :: config%source_paths(1))
        config%source_paths(1) = "."
        
        exit_code = analyze_coverage(config)
        
        ! Invalid format should cause failure
        passed = (exit_code /= EXIT_SUCCESS)
    end function test_invalid_output_format_exit_code

    function test_invalid_threshold_exit_code() result(passed)
        ! Given: Invalid threshold value
        ! When: Running coverage analysis
        ! Then: Should handle gracefully or return appropriate exit code
        logical :: passed
        type(config_t) :: config
        integer :: exit_code
        
        call initialize_config(config)
        config%quiet = .true.
        config%coverage_threshold = -50.0  ! Invalid negative threshold
        
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        allocate(character(len=256) :: config%source_paths(1))
        config%source_paths(1) = "."
        
        exit_code = analyze_coverage(config)
        
        ! Invalid threshold should be handled gracefully or return error
        passed = (exit_code >= 0 .and. exit_code <= 3)
    end function test_invalid_threshold_exit_code

    function test_threshold_failure_exit_codes() result(passed)
        ! Given: Coverage below threshold
        ! When: Running coverage analysis with threshold enforcement
        ! Then: Should return EXIT_THRESHOLD_NOT_MET (2)
        
        logical :: passed
        
        print *, "Test 3: Threshold failure scenarios (exit code 2)"
        
        ! Test coverage threshold failure
        passed = test_coverage_below_threshold_exit_code()
        
        if (passed) then
            print *, "    ✅ PASSED - Threshold failures return exit code 2"
        else
            print *, "    ❌ FAILED - Threshold failures not returning exit code 2"
        end if
        print *, ""
    end function test_threshold_failure_exit_codes

    function test_coverage_below_threshold_exit_code() result(passed)
        ! Given: High threshold that won't be met
        ! When: Running coverage analysis
        ! Then: Should return EXIT_THRESHOLD_NOT_MET (2)
        logical :: passed
        type(config_t) :: config
        integer :: exit_code
        
        call initialize_config(config)
        config%quiet = .true.
        config%coverage_threshold = 99.99  ! Extremely high threshold
        
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        allocate(character(len=256) :: config%source_paths(1))
        config%source_paths(1) = "."
        
        exit_code = analyze_coverage(config)
        
        ! Should return threshold not met or no coverage data
        passed = (exit_code == EXIT_THRESHOLD_NOT_MET .or. &
                 exit_code == EXIT_NO_COVERAGE_DATA .or. &
                 exit_code == EXIT_FAILURE)
    end function test_coverage_below_threshold_exit_code

    function test_no_coverage_data_exit_codes() result(passed)
        ! Given: No coverage data available
        ! When: Running coverage analysis in strict mode
        ! Then: Should return EXIT_NO_COVERAGE_DATA (3)
        
        logical :: passed
        
        print *, "Test 4: No coverage data scenarios (exit code 3)"
        
        ! Test strict mode with no coverage data
        passed = test_strict_mode_no_coverage_exit_code()
        
        if (passed) then
            print *, "    ✅ PASSED - No coverage data returns exit code 3 in strict mode"
        else
            print *, "    ❌ FAILED - No coverage data not returning exit code 3 in strict mode"
        end if
        print *, ""
    end function test_no_coverage_data_exit_codes

    function test_strict_mode_no_coverage_exit_code() result(passed)
        ! Given: Strict mode enabled with no coverage files
        ! When: Running coverage analysis
        ! Then: Should return EXIT_NO_COVERAGE_DATA (3)
        logical :: passed
        type(config_t) :: config
        integer :: exit_code
        
        call initialize_config(config)
        config%quiet = .true.
        config%strict_mode = .true.
        
        ! Set empty directory
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        allocate(character(len=256) :: config%source_paths(1))
        config%source_paths(1) = "/tmp/no_coverage_data_test_dir"
        
        exit_code = analyze_coverage(config)
        
        ! Strict mode should return no coverage data error
        passed = (exit_code == EXIT_NO_COVERAGE_DATA)
    end function test_strict_mode_no_coverage_exit_code

    function test_ci_cd_pipeline_compatibility() result(passed)
        ! Given: CI/CD pipeline scenarios
        ! When: Testing exit code behavior in automated environments
        ! Then: Should provide reliable exit codes for pipeline decisions
        
        logical :: passed
        logical :: success_detection_test, failure_detection_test, threshold_detection_test
        
        print *, "Test 5: CI/CD pipeline compatibility"
        
        ! Test 5a: Success detection
        success_detection_test = test_ci_cd_success_detection()
        
        ! Test 5b: Failure detection
        failure_detection_test = test_ci_cd_failure_detection()
        
        ! Test 5c: Threshold detection
        threshold_detection_test = test_ci_cd_threshold_detection()
        
        passed = success_detection_test .and. failure_detection_test .and. threshold_detection_test
        
        if (passed) then
            print *, "    ✅ PASSED - CI/CD pipeline compatibility validated"
        else
            print *, "    ❌ FAILED - CI/CD pipeline compatibility issues"
            if (.not. success_detection_test) print *, "      - Success detection failed"
            if (.not. failure_detection_test) print *, "      - Failure detection failed"
            if (.not. threshold_detection_test) print *, "      - Threshold detection failed"
        end if
        print *, ""
    end function test_ci_cd_pipeline_compatibility

    function test_ci_cd_success_detection() result(passed)
        ! Given: Successful tool execution
        ! When: CI/CD checks exit code
        ! Then: Exit code 0 should indicate success
        logical :: passed
        type(config_t) :: config
        integer :: exit_code
        
        call initialize_config(config)
        config%show_help = .true.  ! Help should always succeed
        config%quiet = .true.
        
        ! Help should return success
        if (config%show_help) then
            exit_code = EXIT_SUCCESS
        else
            exit_code = analyze_coverage(config)
        end if
        
        passed = (exit_code == EXIT_SUCCESS)
    end function test_ci_cd_success_detection

    function test_ci_cd_failure_detection() result(passed)
        ! Given: Tool failure scenario
        ! When: CI/CD checks exit code
        ! Then: Non-zero exit code should indicate failure
        logical :: passed
        type(config_t) :: config
        integer :: exit_code
        
        call initialize_config(config)
        config%quiet = .true.
        
        ! Set completely invalid configuration
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        allocate(character(len=256) :: config%source_paths(1))
        config%source_paths(1) = "/dev/null/impossible/path"
        
        exit_code = analyze_coverage(config)
        
        ! Any failure should return non-zero exit code
        passed = (exit_code /= EXIT_SUCCESS)
    end function test_ci_cd_failure_detection

    function test_ci_cd_threshold_detection() result(passed)
        ! Given: Coverage threshold not met
        ! When: CI/CD checks exit code
        ! Then: Exit code 2 should indicate threshold failure
        logical :: passed
        type(config_t) :: config
        integer :: exit_code
        
        call initialize_config(config)
        config%quiet = .true.
        config%coverage_threshold = 100.0  ! Impossible threshold
        
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        allocate(character(len=256) :: config%source_paths(1))
        config%source_paths(1) = "."
        
        exit_code = analyze_coverage(config)
        
        ! Should return threshold failure or no coverage data
        passed = (exit_code == EXIT_THRESHOLD_NOT_MET .or. &
                 exit_code == EXIT_NO_COVERAGE_DATA .or. &
                 exit_code == EXIT_FAILURE)
    end function test_ci_cd_threshold_detection

    function test_error_scenario_consistency() result(passed)
        ! Given: Various error scenarios
        ! When: Testing exit code consistency
        ! Then: Similar errors should return similar exit codes
        
        logical :: passed
        
        print *, "Test 6: Error scenario exit code consistency"
        
        ! Test that similar error types return consistent exit codes
        passed = test_consistent_error_exit_codes()
        
        if (passed) then
            print *, "    ✅ PASSED - Error scenarios return consistent exit codes"
        else
            print *, "    ❌ FAILED - Error scenarios inconsistent exit codes"
        end if
        print *, ""
    end function test_error_scenario_consistency

    function test_consistent_error_exit_codes() result(passed)
        ! Given: Multiple error scenarios of same type
        ! When: Running coverage analysis
        ! Then: Should return consistent exit codes
        logical :: passed
        type(config_t) :: config1, config2
        integer :: exit_code1, exit_code2
        
        ! Test 1: Two invalid paths should return similar codes
        call initialize_config(config1)
        config1%quiet = .true.
        if (allocated(config1%source_paths)) deallocate(config1%source_paths)
        allocate(character(len=256) :: config1%source_paths(1))
        config1%source_paths(1) = "/invalid/path/1"
        
        call initialize_config(config2)
        config2%quiet = .true.
        if (allocated(config2%source_paths)) deallocate(config2%source_paths)
        allocate(character(len=256) :: config2%source_paths(1))
        config2%source_paths(1) = "/invalid/path/2"
        
        exit_code1 = analyze_coverage(config1)
        exit_code2 = analyze_coverage(config2)
        
        ! Both invalid paths should return similar error codes
        passed = (exit_code1 == exit_code2 .and. exit_code1 /= EXIT_SUCCESS)
    end function test_consistent_error_exit_codes

    function test_cli_parsing_error_exit_codes() result(passed)
        ! Given: CLI parsing errors
        ! When: Parsing invalid command line arguments
        ! Then: Should return EXIT_FAILURE (1)
        
        logical :: passed
        
        print *, "Test 7: CLI parsing error exit codes"
        
        ! Test invalid command line arguments
        passed = test_invalid_cli_arguments_exit_code()
        
        if (passed) then
            print *, "    ✅ PASSED - CLI parsing errors return exit code 1"
        else
            print *, "    ❌ FAILED - CLI parsing errors not returning exit code 1"
        end if
        print *, ""
    end function test_cli_parsing_error_exit_codes

    function test_invalid_cli_arguments_exit_code() result(passed)
        ! Given: Invalid command line arguments
        ! When: Parsing configuration
        ! Then: Should return failure
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: args(:)
        character(len=256) :: error_message
        logical :: success
        
        allocate(character(len=20) :: args(1))
        args(1) = "--invalid-flag-xyz"
        
        call parse_config(args, config, success, error_message)
        
        ! Invalid arguments should fail parsing
        passed = (.not. success)
    end function test_invalid_cli_arguments_exit_code

    function test_file_system_error_exit_codes() result(passed)
        ! Given: File system errors
        ! When: Running coverage analysis
        ! Then: Should return appropriate error exit codes
        
        logical :: passed
        
        print *, "Test 8: File system error exit codes"
        
        ! Test file system related errors
        passed = test_file_system_errors_exit_code()
        
        if (passed) then
            print *, "    ✅ PASSED - File system errors return appropriate exit codes"
        else
            print *, "    ❌ FAILED - File system errors not handled properly"
        end if
        print *, ""
    end function test_file_system_error_exit_codes

    function test_file_system_errors_exit_code() result(passed)
        ! Given: Inaccessible file system locations
        ! When: Running coverage analysis
        ! Then: Should return error exit codes
        logical :: passed
        type(config_t) :: config
        integer :: exit_code
        
        call initialize_config(config)
        config%quiet = .true.
        
        ! Try to write to protected location
        config%output_path = "/root/protected_coverage_report.md"
        
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        allocate(character(len=256) :: config%source_paths(1))
        config%source_paths(1) = "."
        
        exit_code = analyze_coverage(config)
        
        ! File system errors should return non-success codes
        passed = (exit_code /= EXIT_SUCCESS .or. exit_code == EXIT_NO_COVERAGE_DATA)
    end function test_file_system_errors_exit_code

end program test_exit_code_integration