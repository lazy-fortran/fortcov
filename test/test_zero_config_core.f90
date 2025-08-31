program test_zero_config_core
    !! Zero configuration core functionality tests
    !! Tests zero_config_core and zero_config_manager functionality
    
    use zero_config_core
    use zero_config_manager  
    use config_types, only: config_t
    implicit none
    
    logical :: all_tests_passed = .true.
    
    print *, "================================="
    print *, "Zero Config Core Tests"
    print *, "================================="
    
    call test_zero_config_mode_detection()
    call test_auto_discovery_defaults()
    call test_build_system_integration()
    call test_zero_config_enhancement()
    call test_error_handling()
    
    if (all_tests_passed) then
        print *, "✅ All zero-config tests passed"
    else
        print *, "❌ Some zero-config tests failed"
        stop 1
    end if
    
contains

    subroutine test_zero_config_mode_detection()
        logical :: is_zero_config
        
        print *, "Testing zero-configuration mode detection..."
        
        ! Test the zero-configuration mode detection
        is_zero_config = is_zero_configuration_mode()
        
        ! Note: This will be true/false depending on how the test is run
        ! The important thing is that it doesn't crash and returns a logical value
        
        print *, "✓ Zero-config mode detection functional"
        print *, "  Current mode: ", is_zero_config
    end subroutine test_zero_config_mode_detection

    subroutine test_auto_discovery_defaults()
        type(config_t) :: test_config
        
        print *, "Testing auto-discovery defaults configuration..."
        
        ! Initialize a basic config
        test_config%auto_discovery = .false.
        test_config%test_timeout_seconds = 0
        test_config%max_files = 0
        
        ! Configure auto-discovery defaults
        call configure_auto_discovery_defaults(test_config)
        
        ! Verify defaults were applied
        if (.not. test_config%auto_discovery) then
            print *, "❌ Auto-discovery not enabled"
            all_tests_passed = .false.
            return
        end if
        
        if (test_config%test_timeout_seconds <= 0) then
            print *, "❌ Test timeout not set properly: ", test_config%test_timeout_seconds
            all_tests_passed = .false.
            return
        end if
        
        if (test_config%max_files <= 0) then
            print *, "❌ Max files not set properly: ", test_config%max_files
            all_tests_passed = .false.
            return
        end if
        
        print *, "✓ Auto-discovery defaults configured successfully"
        print *, "  Timeout: ", test_config%test_timeout_seconds, " seconds"
        print *, "  Max files: ", test_config%max_files
    end subroutine test_auto_discovery_defaults

    subroutine test_build_system_integration()
        type(config_t) :: test_config
        logical :: success
        character(len=:), allocatable :: error_message
        
        print *, "Testing build system integration..."
        
        ! Initialize config
        test_config%auto_discovery = .false.
        
        ! Test build system detection integration
        call integrate_build_system_detection(test_config, success, error_message)
        
        ! Build system detection may succeed or fail depending on environment
        ! but it should not crash and should provide meaningful feedback
        if (.not. success .and. len_trim(error_message) == 0) then
            print *, "❌ Build system integration failed without error message"
            all_tests_passed = .false.
            return
        end if
        
        if (success) then
            print *, "✓ Build system integration successful"
        else
            print *, "✓ Build system integration handled gracefully"
            print *, "  Message: ", trim(error_message)
        end if
    end subroutine test_build_system_integration

    subroutine test_zero_config_enhancement()
        type(config_t) :: test_config
        logical :: success
        character(len=:), allocatable :: error_message
        
        print *, "Testing zero-config enhancement..."
        
        ! Initialize minimal config (simulating zero-config mode)
        ! Leave output_path unallocated to trigger zero-config defaults
        test_config%auto_discovery = .false.
        test_config%verbose = .false.
        test_config%quiet = .true.  ! Suppress output during test
        
        ! Test zero-config enhancement
        call enhance_zero_config_with_auto_discovery(test_config, success, error_message)
        
        if (.not. success .and. len_trim(error_message) == 0) then
            print *, "❌ Zero-config enhancement failed without error message"
            all_tests_passed = .false.
            return
        end if
        
        if (success) then
            ! Verify enhancement applied defaults
            if (.not. test_config%auto_discovery) then
                print *, "❌ Auto-discovery not enabled by enhancement"
                all_tests_passed = .false.
                return
            end if
            
            if (.not. allocated(test_config%output_path)) then
                print *, "❌ Output path not set by enhancement"
                all_tests_passed = .false.
                return
            end if
            
            if (len_trim(test_config%output_path) == 0) then
                print *, "❌ Output path is empty"
                all_tests_passed = .false.
                return
            end if
            
            print *, "✓ Zero-config enhancement successful"
            print *, "  Output path: ", trim(test_config%output_path)
        else
            print *, "✓ Zero-config enhancement handled gracefully"
            print *, "  Message: ", trim(error_message)
        end if
    end subroutine test_zero_config_enhancement

    subroutine test_error_handling()
        type(config_t) :: test_config
        logical :: success
        character(len=:), allocatable :: error_message
        
        print *, "Testing zero-config error handling..."
        
        ! Initialize config and test auto test execution setup
        test_config%auto_test_execution = .false.
        test_config%test_timeout_seconds = -1  ! Invalid value
        
        call setup_auto_test_execution(test_config, success, error_message)
        
        ! This should succeed and fix the invalid timeout
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
        
        if (test_config%test_timeout_seconds <= 0) then
            print *, "❌ Test timeout not corrected: ", test_config%test_timeout_seconds
            all_tests_passed = .false.
            return
        end if
        
        print *, "✓ Zero-config error handling successful"
        print *, "  Corrected timeout: ", test_config%test_timeout_seconds, " seconds"
    end subroutine test_error_handling

end program test_zero_config_core
