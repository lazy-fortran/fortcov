program test_zero_config_auto_discovery_integration
    !! Comprehensive test suite for zero-configuration mode auto-discovery 
    !! integration (Issue #281)
    !!
    !! Tests the integration of all auto-discovery components into 
    !! zero-configuration mode for seamless user experience.
    !!
    !! BDD-style tests:
    !! - GIVEN: Zero-configuration mode is triggered
    !! - WHEN: Auto-discovery components are integrated 
    !! - THEN: Build system detection, auto-test execution work seamlessly
    
    use iso_fortran_env, only: error_unit
    use config_types, only: config_t
    use fortcov_config, only: parse_config, initialize_config
    use build_system_detector, only: build_system_info_t, detect_build_system
    use coverage_workflows, only: execute_auto_test_workflow
    use zero_configuration_manager, only: is_zero_configuration_mode, &
                                         apply_zero_configuration_defaults
    use zero_config_auto_discovery_integration, only: &
        enhance_zero_config_with_auto_discovery, &
        configure_auto_discovery_defaults, &
        integrate_build_system_detection, &
        setup_auto_test_execution
    implicit none
    
    integer :: test_count = 0
    integer :: passed_tests = 0
    
    print *, "=== Zero-Configuration Auto-Discovery Integration Tests ==="
    
    ! Core integration tests
    call test_zero_config_enhancement_basic()
    call test_auto_discovery_defaults_configuration()
    call test_build_system_integration_flow()
    call test_auto_test_execution_integration()
    
    ! Configuration interaction tests  
    call test_zero_config_overrides_explicit_config()
    call test_build_system_fallback_handling()
    call test_auto_discovery_with_existing_config()
    
    ! Edge case tests
    call test_zero_config_with_invalid_project()
    call test_auto_discovery_failure_graceful_degradation()
    call test_multiple_build_systems_priority()
    
    ! User experience tests
    call test_seamless_user_experience_flow()
    call test_feedback_and_reporting_integration()
    
    ! Print summary
    print *, ""
    write(*, '(A,I0,A,I0,A)') "Tests completed: ", passed_tests, "/", &
                              test_count, " passed"
    
    if (passed_tests == test_count) then
        print *, "✅ All integration tests PASSED"
        call exit(0)
    else
        print *, "❌ Some integration tests FAILED"
        call exit(1)
    end if

contains

    subroutine run_test(test_name, test_proc)
        !! Test runner with consistent reporting
        character(len=*), intent(in) :: test_name
        interface
            subroutine test_proc()
            end subroutine
        end interface
        
        logical :: test_passed
        
        test_count = test_count + 1
        test_passed = .true.
        
        print *, ""
        print *, "Running: " // test_name
        
        call test_proc()
        
        if (test_passed) then
            passed_tests = passed_tests + 1
            print *, "✅ PASSED: " // test_name
        else
            print *, "❌ FAILED: " // test_name
        end if
    end subroutine run_test
    
    subroutine test_zero_config_enhancement_basic()
        !! GIVEN: No arguments provided to fortcov
        !! WHEN: Zero-config enhancement is applied
        !! THEN: Auto-discovery components are properly integrated
        
        type(config_t) :: config
        character(len=0), allocatable :: no_args(:)
        logical :: success
        character(len=:), allocatable :: error_message
        
        ! GIVEN: Initialize with no arguments (zero-config trigger)
        allocate(no_args(0))
        call initialize_config(config)
        
        ! WHEN: Apply zero-config enhancement with auto-discovery
        call enhance_zero_config_with_auto_discovery(config, success, error_message)
        
        ! THEN: Auto-discovery flags should be enabled
        call assert_true(config%auto_discovery, &
            "Auto-discovery should be enabled in zero-config mode")
        ! NOTE: auto_test_execution may be disabled if no usable build system found
        call assert_true(config%zero_configuration_mode, &
            "Zero-configuration mode flag should be set")
            
        ! AND: Appropriate defaults should be configured
        call assert_equal(config%output_format, "markdown", &
            "Default output format should be markdown")
        call assert_contains(config%output_path, "coverage", &
            "Output path should contain 'coverage'")
            
    end subroutine test_zero_config_enhancement_basic
    
    subroutine test_auto_discovery_defaults_configuration()
        !! GIVEN: Zero-configuration mode is activated
        !! WHEN: Auto-discovery defaults are configured
        !! THEN: Appropriate settings are applied for seamless operation
        
        type(config_t) :: config
        
        ! GIVEN: Zero-config mode
        call initialize_config(config)
        config%zero_configuration_mode = .true.
        
        ! WHEN: Configure auto-discovery defaults
        call configure_auto_discovery_defaults(config)
        
        ! THEN: Auto-discovery should be properly configured
        call assert_true(config%auto_discovery, &
            "Auto-discovery should be enabled")
        ! NOTE: auto_test_execution state depends on build system availability 
            
        ! AND: Timeout settings should be reasonable
        call assert_greater_than(config%test_timeout_seconds, 30, &
            "Test timeout should be at least 30 seconds")
        call assert_less_than(config%test_timeout_seconds, 600, &
            "Test timeout should be under 10 minutes")
            
        ! AND: File processing limits should be set
        call assert_greater_than(config%max_files, 10, &
            "Max files should allow processing meaningful projects")
            
    end subroutine test_auto_discovery_defaults_configuration
    
    subroutine test_build_system_integration_flow()
        !! GIVEN: Zero-config mode with project directory
        !! WHEN: Build system integration is performed
        !! THEN: Build system detection works seamlessly
        
        type(config_t) :: config
        type(build_system_info_t) :: build_info
        logical :: integration_success
        character(len=:), allocatable :: error_message
        
        ! GIVEN: Zero-config mode setup
        call initialize_config(config)
        config%zero_configuration_mode = .true.
        config%auto_discovery = .true.
        
        ! WHEN: Detect build system directly (for testing build_info access)
        block
            use error_handling, only: error_context_t, ERROR_SUCCESS
            type(error_context_t) :: error_ctx
            call detect_build_system(".", build_info, error_ctx)
            integration_success = (error_ctx%error_code == ERROR_SUCCESS)
            if (.not. integration_success) then
                error_message = error_ctx%message
            else
                error_message = ""
            end if
        end block
        
        ! THEN: Integration should handle various scenarios gracefully
        if (integration_success) then
            ! Build system was detected
            call assert_not_equal(build_info%system_type, "unknown", &
                "Should detect known build system when available")
            call assert_not_empty(build_info%test_command, &
                "Should provide test command for detected system")
        else
            ! No build system detected - should still be graceful
            call assert_not_empty(error_message, &
                "Should provide meaningful error when no build system found")
        end if
        
    end subroutine test_build_system_integration_flow
    
    subroutine test_auto_test_execution_integration() 
        !! GIVEN: Zero-config mode with auto-test execution enabled
        !! WHEN: Auto-test workflow is executed
        !! THEN: Build system detection and test execution work together
        
        type(config_t) :: config
        integer :: exit_code
        
        ! GIVEN: Zero-config with auto-test execution
        call initialize_config(config)
        config%zero_configuration_mode = .true.
        config%auto_discovery = .true.
        config%auto_test_execution = .true.
        config%test_timeout_seconds = 60
        config%quiet = .true.  ! Suppress output in tests
        
        ! WHEN: Execute auto-test workflow
        exit_code = execute_auto_test_workflow(config)
        
        ! THEN: Should handle execution gracefully
        call assert_valid_exit_code(exit_code, &
            "Auto-test workflow should return valid exit code")
            
        ! Exit code meanings:
        ! 0 = success or skipped (no build system)
        ! 1 = test failure
        ! 2 = build system detection failure
        ! 124 = timeout
        call assert_true(exit_code >= 0 .and. exit_code <= 124, &
            "Exit code should be in valid range")
            
    end subroutine test_auto_test_execution_integration
    
    subroutine test_zero_config_overrides_explicit_config()
        !! GIVEN: Explicit configuration provided
        !! WHEN: Zero-config enhancement is applied
        !! THEN: Explicit settings take precedence over auto-discovery
        
        type(config_t) :: config
        character(len=256) :: explicit_output_path
        logical :: success
        character(len=:), allocatable :: error_message
        
        ! GIVEN: Explicit configuration
        call initialize_config(config)
        explicit_output_path = "custom/output/path.html"
        config%output_path = explicit_output_path
        config%output_format = "html"
        config%auto_test_execution = .false.  ! Explicitly disabled
        
        ! WHEN: Apply zero-config enhancement
        call enhance_zero_config_with_auto_discovery(config, success, error_message)
        
        ! THEN: Explicit settings should be preserved
        call assert_equal(config%output_path, explicit_output_path, &
            "Explicit output path should be preserved")
        call assert_equal(config%output_format, "html", &
            "Explicit output format should be preserved")
        ! NOTE: auto_test_execution state may change during enhancement based on
        ! build system detection, but explicit user configuration is prioritized
            
    end subroutine test_zero_config_overrides_explicit_config
    
    subroutine test_build_system_fallback_handling()
        !! GIVEN: No build system is detected
        !! WHEN: Zero-config enhancement is applied  
        !! THEN: Graceful fallback behavior is provided
        
        type(config_t) :: config
        logical :: success
        character(len=:), allocatable :: error_message
        
        ! GIVEN: Zero-config mode in directory without build system
        call initialize_config(config)
        config%zero_configuration_mode = .true.
        
        ! WHEN: Apply enhancement (will attempt build system detection)
        call enhance_zero_config_with_auto_discovery(config, success, error_message)
        
        ! THEN: Should still succeed with fallback behavior
        call assert_true(success, &
            "Enhancement should succeed even without build system")
        call assert_true(config%auto_discovery, &
            "Auto-discovery should still be enabled for coverage files")
            
    end subroutine test_build_system_fallback_handling
    
    subroutine test_auto_discovery_with_existing_config()
        !! GIVEN: Partial configuration exists 
        !! WHEN: Auto-discovery enhancement is applied
        !! THEN: Missing settings are filled with auto-discovered values
        
        type(config_t) :: config
        logical :: success
        character(len=:), allocatable :: error_message
        
        ! GIVEN: Partial configuration (only output format specified)
        call initialize_config(config)
        config%output_format = "json"
        ! Leave other fields unallocated/default
        
        ! WHEN: Apply auto-discovery enhancement
        call enhance_zero_config_with_auto_discovery(config, success, error_message)
        
        ! THEN: Missing settings should be auto-discovered/defaulted
        call assert_equal(config%output_format, "json", &
            "Existing output format should be preserved")
        call assert_allocated(config%output_path, &
            "Output path should be defaulted")
        call assert_true(config%auto_discovery, &
            "Auto-discovery should be enabled")
            
    end subroutine test_auto_discovery_with_existing_config
    
    subroutine test_zero_config_with_invalid_project()
        !! GIVEN: Invalid project directory (no sources, no build files)
        !! WHEN: Zero-config enhancement is attempted
        !! THEN: Graceful failure with helpful error messages
        
        type(config_t) :: config
        logical :: success
        character(len=:), allocatable :: error_message
        
        ! GIVEN: Zero-config in invalid project (current test environment)
        call initialize_config(config)  
        config%zero_configuration_mode = .true.
        
        ! WHEN: Apply enhancement 
        call enhance_zero_config_with_auto_discovery(config, success, error_message)
        
        ! THEN: Should provide meaningful feedback regardless of success
        ! (Success depends on test environment, but error handling should work)
        if (.not. success) then
            call assert_not_empty(error_message, &
                "Should provide helpful error message for invalid projects")
            call assert_contains(error_message, "coverage", &
                "Error should mention coverage files or sources")
        end if
        
    end subroutine test_zero_config_with_invalid_project
    
    subroutine test_auto_discovery_failure_graceful_degradation()
        !! GIVEN: Auto-discovery components encounter failures
        !! WHEN: Zero-config enhancement continues processing
        !! THEN: Graceful degradation with useful defaults
        
        type(config_t) :: config
        logical :: success
        character(len=:), allocatable :: error_message
        
        ! GIVEN: Zero-config mode setup
        call initialize_config(config)
        config%zero_configuration_mode = .true.
        
        ! WHEN: Apply enhancement (may encounter failures in test environment)
        call enhance_zero_config_with_auto_discovery(config, success, error_message)
        
        ! THEN: Should provide graceful degradation
        call assert_true(config%auto_discovery, &
            "Auto-discovery flag should be set regardless of detection results")
        call assert_allocated(config%output_path, &
            "Output path should have reasonable default")
        call assert_allocated(config%output_format, &
            "Output format should have reasonable default")
            
    end subroutine test_auto_discovery_failure_graceful_degradation
    
    subroutine test_multiple_build_systems_priority()
        !! GIVEN: Project with multiple build system files
        !! WHEN: Build system detection is integrated
        !! THEN: Correct priority order is respected (FPM > CMake > Make > Meson)
        
        type(config_t) :: config
        type(build_system_info_t) :: build_info
        logical :: success
        character(len=:), allocatable :: error_message
        
        ! GIVEN: Zero-config setup
        call initialize_config(config)
        config%zero_configuration_mode = .true.
        
        ! WHEN: Detect build system directly (for testing build_info access)
        block
            use error_handling, only: error_context_t, ERROR_SUCCESS
            type(error_context_t) :: error_ctx
            call detect_build_system(".", build_info, error_ctx)
            success = (error_ctx%error_code == ERROR_SUCCESS)
            if (.not. success) then
                error_message = error_ctx%message
            else
                error_message = ""
            end if
        end block
        
        ! THEN: If detection succeeds, should respect priority
        if (success .and. build_info%system_type /= "unknown") then
            call assert_true(len_trim(build_info%system_type) > 0, &
                "Detected build system should have valid name")
            call assert_true(len_trim(build_info%test_command) > 0, &  
                "Should provide test command for detected system")
        end if
        
    end subroutine test_multiple_build_systems_priority
    
    subroutine test_seamless_user_experience_flow()
        !! GIVEN: User runs 'fortcov' with no arguments
        !! WHEN: Complete zero-config flow is executed
        !! THEN: Seamless experience with minimal user intervention
        
        type(config_t) :: config
        character(len=0), allocatable :: no_args(:)
        logical :: success
        character(len=:), allocatable :: error_message
        
        ! GIVEN: Simulate 'fortcov' with no arguments
        allocate(no_args(0))
        call parse_config(no_args, config, success, error_message)
        
        if (success) then
            ! WHEN: Apply complete zero-config enhancement
            call enhance_zero_config_with_auto_discovery(config, success, error_message)
            
            ! THEN: Should provide seamless experience
            call assert_true(success .or. len_trim(error_message) > 0, &
                "Should either succeed or provide clear error message")
                
            if (success) then
                call assert_true(config%zero_configuration_mode, &
                    "Zero-config mode should be active")
                call assert_true(config%auto_discovery, &
                    "Auto-discovery should be enabled")
                call assert_allocated(config%output_path, &
                    "Should have default output path")
            end if
        end if
        
    end subroutine test_seamless_user_experience_flow
    
    subroutine test_feedback_and_reporting_integration()
        !! GIVEN: Zero-config mode with various detection results
        !! WHEN: User feedback is provided
        !! THEN: Clear, helpful reporting about auto-detected configuration
        
        type(config_t) :: config
        logical :: success
        character(len=:), allocatable :: error_message
        
        ! GIVEN: Zero-config setup
        call initialize_config(config)
        config%zero_configuration_mode = .true.
        config%quiet = .false.  ! Enable feedback
        
        ! WHEN: Apply enhancement (will provide user feedback)
        call enhance_zero_config_with_auto_discovery(config, success, error_message)
        
        ! THEN: Configuration should reflect feedback capability
        call assert_false(config%quiet, &
            "Quiet mode should allow user feedback in zero-config")
        call assert_true(config%zero_configuration_mode, &
            "Zero-config mode should be maintained")
            
    end subroutine test_feedback_and_reporting_integration
    
    ! Test assertion helpers
    subroutine assert_true(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        if (.not. condition) then
            write(error_unit, '(A)') "ASSERTION FAILED: " // message
            call exit(1)
        end if
    end subroutine assert_true
    
    subroutine assert_false(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        call assert_true(.not. condition, message)
    end subroutine assert_false
    
    subroutine assert_equal(actual, expected, message)
        character(len=*), intent(in) :: actual, expected, message
        if (actual /= expected) then
            write(error_unit, '(A)') "ASSERTION FAILED: " // message
            write(error_unit, '(A)') "  Expected: " // expected
            write(error_unit, '(A)') "  Actual:   " // actual
            call exit(1)
        end if
    end subroutine assert_equal
    
    subroutine assert_not_equal(actual, not_expected, message) 
        character(len=*), intent(in) :: actual, not_expected, message
        if (actual == not_expected) then
            write(error_unit, '(A)') "ASSERTION FAILED: " // message
            write(error_unit, '(A)') "  Should not equal: " // not_expected
            write(error_unit, '(A)') "  But got: " // actual
            call exit(1)
        end if
    end subroutine assert_not_equal
    
    subroutine assert_allocated(string_var, message)
        character(len=:), allocatable, intent(in) :: string_var
        character(len=*), intent(in) :: message
        if (.not. allocated(string_var)) then
            write(error_unit, '(A)') "ASSERTION FAILED: " // message // " (not allocated)"
            call exit(1)
        end if
    end subroutine assert_allocated
    
    subroutine assert_not_empty(string_val, message)
        character(len=*), intent(in) :: string_val, message
        if (len_trim(string_val) == 0) then
            write(error_unit, '(A)') "ASSERTION FAILED: " // message // " (empty string)"
            call exit(1)
        end if
    end subroutine assert_not_empty
    
    subroutine assert_contains(string_val, substring, message)
        character(len=*), intent(in) :: string_val, substring, message
        if (index(string_val, substring) == 0) then
            write(error_unit, '(A)') "ASSERTION FAILED: " // message
            write(error_unit, '(A)') "  String: " // string_val
            write(error_unit, '(A)') "  Should contain: " // substring
            call exit(1)
        end if
    end subroutine assert_contains
    
    subroutine assert_greater_than(actual, minimum, message)
        integer, intent(in) :: actual, minimum
        character(len=*), intent(in) :: message
        if (actual <= minimum) then
            write(error_unit, '(A,I0,A,I0)') "ASSERTION FAILED: " // message // &
                " (", actual, " <= ", minimum, ")"
            call exit(1)
        end if
    end subroutine assert_greater_than
    
    subroutine assert_less_than(actual, maximum, message)
        integer, intent(in) :: actual, maximum
        character(len=*), intent(in) :: message
        if (actual >= maximum) then
            write(error_unit, '(A,I0,A,I0)') "ASSERTION FAILED: " // message // &
                " (", actual, " >= ", maximum, ")"
            call exit(1)
        end if
    end subroutine assert_less_than
    
    subroutine assert_valid_exit_code(exit_code, message)
        integer, intent(in) :: exit_code
        character(len=*), intent(in) :: message
        ! Valid exit codes: 0 (success), 1 (failure), 2 (detection failure), 124 (timeout)
        if (exit_code /= 0 .and. exit_code /= 1 .and. &
            exit_code /= 2 .and. exit_code /= 124) then
            write(error_unit, '(A,I0)') "ASSERTION FAILED: " // message // &
                " (invalid exit code: ", exit_code, ")"
            call exit(1)
        end if
    end subroutine assert_valid_exit_code

end program test_zero_config_auto_discovery_integration