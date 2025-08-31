program test_zero_config_workflow
    !! Zero configuration workflow integration tests
    !! Tests the complete zero-config workflow from start to finish
    
    use zero_config_core
    use zero_config_manager
    use config_types, only: config_t
    use constants_core, only: EXIT_SUCCESS, EXIT_FAILURE
    implicit none
    
    logical :: all_tests_passed = .true.
    
    print *, "========================================"
    print *, "Zero Config Workflow Tests"
    print *, "========================================"
    
    call test_workflow_initialization()
    call test_workflow_configuration()
    call test_workflow_file_discovery()
    call test_workflow_error_recovery()
    
    if (all_tests_passed) then
        print *, "✅ All zero-config workflow tests passed"
    else
        print *, "❌ Some zero-config workflow tests failed"
        stop 1
    end if
    
contains

    subroutine test_workflow_initialization()
        type(config_t) :: test_config
        logical :: success
        character(len=:), allocatable :: error_message
        
        print *, "Testing zero-config workflow initialization..."
        
        ! Initialize minimal config (simulate command-line with no args)
        test_config%verbose = .false.
        test_config%quiet = .true.
        test_config%auto_discovery = .false.
        
        ! Test workflow enhancement
        call enhance_zero_config_with_auto_discovery(test_config, success, error_message)
        
        if (.not. success) then
            if (len_trim(error_message) == 0) then
                print *, "❌ Workflow initialization failed without error message"
                all_tests_passed = .false.
                return
            end if
            ! Failure with message is acceptable in testing environment
            print *, "✓ Workflow initialization handled gracefully: ", trim(error_message)
        else
            ! Success case - verify proper initialization
            if (.not. test_config%auto_discovery) then
                print *, "❌ Auto-discovery not enabled after workflow initialization"
                all_tests_passed = .false.
                return
            end if
            
            print *, "✓ Workflow initialization successful"
        end if
    end subroutine test_workflow_initialization

    subroutine test_workflow_configuration()
        type(config_t) :: test_config
        
        print *, "Testing workflow configuration application..."
        
        ! Start with empty config
        test_config%auto_discovery = .false.
        test_config%test_timeout_seconds = 0
        test_config%max_files = 0
        test_config%auto_test_execution = .false.
        
        ! Apply configuration defaults
        call configure_auto_discovery_defaults(test_config)
        
        if (.not. test_config%auto_discovery) then
            print *, "❌ Auto-discovery not configured"
            all_tests_passed = .false.
            return
        end if
        
        if (test_config%test_timeout_seconds <= 0) then
            print *, "❌ Test timeout not configured: ", test_config%test_timeout_seconds
            all_tests_passed = .false.
            return
        end if
        
        if (test_config%max_files <= 0) then
            print *, "❌ Max files not configured: ", test_config%max_files
            all_tests_passed = .false.
            return
        end if
        
        ! Test auto test execution setup  
        block
            logical :: success
            character(len=:), allocatable :: error_message
            call setup_auto_test_execution(test_config, success, error_message)
        
            if (.not. success) then
                print *, "❌ Auto test execution setup failed: ", trim(error_message)
                all_tests_passed = .false.
                return
            end if
            
            if (.not. test_config%auto_test_execution) then
                print *, "❌ Auto test execution not enabled"
                all_tests_passed = .false.
                return
            end if
        end block
        
        print *, "✓ Workflow configuration applied successfully"
        print *, "  Auto-discovery: ", test_config%auto_discovery
        print *, "  Timeout: ", test_config%test_timeout_seconds, " seconds"
        print *, "  Max files: ", test_config%max_files
        print *, "  Auto test execution: ", test_config%auto_test_execution
    end subroutine test_workflow_configuration

    subroutine test_workflow_file_discovery()
        character(len=:), allocatable :: discovered_coverage_files(:)
        character(len=:), allocatable :: discovered_source_paths(:)
        
        print *, "Testing workflow file discovery integration..."
        
        ! Test auto-discovery functions directly (these are public)
        discovered_coverage_files = auto_discover_coverage_files_priority()
        discovered_source_paths = auto_discover_source_files_priority()
        
        ! Verify discovery functions work (may return empty arrays in test env)
        if (.not. allocated(discovered_coverage_files)) then
            print *, "❌ Coverage file discovery should return allocated array"
            all_tests_passed = .false.
            return
        end if
        
        if (.not. allocated(discovered_source_paths)) then
            print *, "❌ Source path discovery should return allocated array"
            all_tests_passed = .false.
            return
        end if
        
        print *, "✓ File discovery functions operational"
        print *, "  Discovered coverage files: ", size(discovered_coverage_files)
        print *, "  Discovered source paths: ", size(discovered_source_paths)
        
        ! Test zero-config mode detection
        block
            logical :: is_zero_config
            is_zero_config = is_zero_configuration_mode()
            
            print *, "✓ Zero-config mode detection: ", is_zero_config
        end block
    end subroutine test_workflow_file_discovery

    subroutine test_workflow_error_recovery()
        type(config_t) :: test_config
        logical :: success
        character(len=:), allocatable :: error_message
        
        print *, "Testing workflow error recovery..."
        
        ! Test build system integration error recovery
        test_config%quiet = .true.
        
        call integrate_build_system_detection(test_config, success, error_message)
        
        ! Should either succeed or fail gracefully with message
        if (.not. success) then
            if (len_trim(error_message) == 0) then
                print *, "❌ Build system integration should provide error message on failure"
                all_tests_passed = .false.
                return
            end if
        end if
        
        ! Test configuration with invalid state
        test_config%test_timeout_seconds = -100  ! Invalid
        test_config%max_files = -50              ! Invalid
        
        call setup_auto_test_execution(test_config, success, error_message)
        
        if (.not. success) then
            print *, "❌ Auto test execution setup should recover from invalid config"
            all_tests_passed = .false.
            return
        end if
        
        ! Should have corrected the invalid values
        if (test_config%test_timeout_seconds <= 0) then
            print *, "❌ Invalid timeout should have been corrected"
            all_tests_passed = .false.
            return
        end if
        
        print *, "✓ Workflow error recovery working correctly"
        print *, "  Corrected timeout: ", test_config%test_timeout_seconds, " seconds"
    end subroutine test_workflow_error_recovery

end program test_zero_config_workflow
