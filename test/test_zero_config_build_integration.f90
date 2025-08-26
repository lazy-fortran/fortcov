program test_zero_config_build_integration
    !! Zero-configuration build system integration tests
    !! Tests integration with build system detection
    
    use test_utilities, only: test_runner_t, assert_true, assert_false, &
                              assert_equals_str, assert_present
    use config_types, only: config_t
    use fortcov_config, only: initialize_config
    use build_system_detector, only: build_system_info_t, detect_build_system
    use zero_config_auto_discovery_integration, only: &
        integrate_build_system_detection, &
        setup_auto_test_execution
    implicit none
    
    type(test_runner_t) :: runner
    
    call runner%init("Zero-Config Build Integration Tests")
    
    ! Build system integration tests
    call test_build_system_detection()
    call test_build_fallback_handling()
    call test_multiple_build_systems()
    call test_auto_test_setup()
    
    ! Project validation tests
    call test_invalid_project_handling()
    call test_missing_build_system()
    
    call runner%print_summary()
    
    if (runner%get_pass_rate() == 100.0) then
        call exit(0)
    else
        call exit(1)
    end if
    
contains
    
    subroutine test_build_system_detection()
        !! Test basic build system detection
        type(config_t) :: config
        type(build_system_info_t) :: build_info
        logical :: passed, success
        integer :: unit
        
        passed = .true.
        
        ! Create test Makefile
        open(newunit=unit, file="Makefile", status="replace")
        write(unit, '(A)') "all:"
        write(unit, '(A)') "	@echo test"
        close(unit)
        
        call initialize_config(config)
        success = integrate_build_system_detection(config, build_info)
        
        call assert_true(success, &
            "Build system detection should succeed", passed)
        call assert_equals_str(build_info%system_type, "make", &
            "Should detect make build system", passed)
        
        ! Clean up
        open(newunit=unit, file="Makefile")
        close(unit, status="delete")
        
        call runner%run_test("build_system_detection", passed)
    end subroutine test_build_system_detection
    
    subroutine test_build_fallback_handling()
        !! Test fallback when no build system is found
        type(config_t) :: config
        type(build_system_info_t) :: build_info
        logical :: passed, success
        
        passed = .true.
        
        call initialize_config(config)
        config%build_fallback_enabled = .true.
        
        ! No build files present
        success = integrate_build_system_detection(config, build_info)
        
        call assert_true(success, &
            "Should handle missing build system gracefully", passed)
        call assert_equals_str(build_info%system_type, "none", &
            "Should indicate no build system", passed)
        
        call runner%run_test("build_fallback_handling", passed)
    end subroutine test_build_fallback_handling
    
    subroutine test_multiple_build_systems()
        !! Test priority handling with multiple build systems
        type(config_t) :: config
        type(build_system_info_t) :: build_info
        logical :: passed, success
        integer :: unit
        
        passed = .true.
        
        ! Create multiple build files
        open(newunit=unit, file="Makefile", status="replace")
        write(unit, '(A)') "all:"
        close(unit)
        
        open(newunit=unit, file="fpm.toml", status="replace")
        write(unit, '(A)') '[package]'
        write(unit, '(A)') 'name = "test"'
        close(unit)
        
        call initialize_config(config)
        success = integrate_build_system_detection(config, build_info)
        
        call assert_true(success, &
            "Should detect with multiple build systems", passed)
        call assert_equals_str(build_info%system_type, "fpm", &
            "Should prefer fpm over make", passed)
        
        ! Clean up
        open(newunit=unit, file="Makefile")
        close(unit, status="delete")
        open(newunit=unit, file="fpm.toml")
        close(unit, status="delete")
        
        call runner%run_test("multiple_build_systems", passed)
    end subroutine test_multiple_build_systems
    
    subroutine test_auto_test_setup()
        !! Test auto-test execution setup
        type(config_t) :: config
        type(build_system_info_t) :: build_info
        logical :: passed, success
        integer :: unit
        
        passed = .true.
        
        ! Setup fpm project
        open(newunit=unit, file="fpm.toml", status="replace")
        write(unit, '(A)') '[package]'
        write(unit, '(A)') 'name = "test"'
        close(unit)
        
        call initialize_config(config)
        build_info%system_type = "fpm"
        build_info%has_tests = .true.
        
        success = setup_auto_test_execution(config, build_info)
        
        call assert_true(success, &
            "Auto-test setup should succeed", passed)
        call assert_true(config%auto_test_enabled, &
            "Auto-test should be enabled", passed)
        call assert_equals_str(config%test_command, "fpm test", &
            "Should set correct test command", passed)
        
        ! Clean up
        open(newunit=unit, file="fpm.toml")
        close(unit, status="delete")
        
        call runner%run_test("auto_test_setup", passed)
    end subroutine test_auto_test_setup
    
    subroutine test_invalid_project_handling()
        !! Test handling of invalid project structure
        type(config_t) :: config
        type(build_system_info_t) :: build_info
        logical :: passed, success
        integer :: unit
        
        passed = .true.
        
        ! Create invalid build file
        open(newunit=unit, file="Makefile", status="replace")
        write(unit, '(A)') "invalid syntax @#$%"
        close(unit)
        
        call initialize_config(config)
        config%strict_mode = .false.
        
        success = integrate_build_system_detection(config, build_info)
        
        call assert_true(success, &
            "Should handle invalid project gracefully", passed)
        
        ! Clean up
        open(newunit=unit, file="Makefile")
        close(unit, status="delete")
        
        call runner%run_test("invalid_project_handling", passed)
    end subroutine test_invalid_project_handling
    
    subroutine test_missing_build_system()
        !! Test behavior when no build system exists
        type(config_t) :: config
        type(build_system_info_t) :: build_info
        logical :: passed, success
        
        passed = .true.
        
        call initialize_config(config)
        config%require_build_system = .false.
        
        success = integrate_build_system_detection(config, build_info)
        
        call assert_true(success, &
            "Should succeed without build system when not required", passed)
        call assert_false(build_info%detected, &
            "Should indicate no detection", passed)
        
        call runner%run_test("missing_build_system", passed)
    end subroutine test_missing_build_system
    
end program test_zero_config_build_integration