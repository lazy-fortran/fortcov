program test_zero_config_workflow
    !! Zero-configuration workflow and user experience tests
    !! Tests end-to-end workflows and failure handling
    
    use test_utilities, only: test_runner_t, assert_true, assert_false, &
                              assert_equals_str, assert_present
    use config_types, only: config_t
    use fortcov_config, only: initialize_config
    use coverage_workflows, only: execute_auto_test_workflow
    use zero_configuration_manager, only: apply_zero_configuration_defaults
    use zero_config_auto_discovery_integration, only: &
        enhance_zero_config_with_auto_discovery
    implicit none
    
    type(test_runner_t) :: runner
    
    call runner%init("Zero-Config Workflow Tests")
    
    ! Workflow tests
    call test_seamless_user_flow()
    call test_feedback_integration()
    call test_graceful_degradation()
    call test_error_recovery()
    
    call runner%print_summary()
    
    if (runner%get_pass_rate() == 100.0) then
        call exit(0)
    else
        call exit(1)
    end if
    
contains
    
    subroutine test_seamless_user_flow()
        !! Test complete user workflow from start to finish
        type(config_t) :: config
        logical :: passed, success
        integer :: unit
        
        passed = .true.
        
        ! Setup minimal project
        open(newunit=unit, file="fpm.toml", status="replace")
        write(unit, '(A)') '[package]'
        write(unit, '(A)') 'name = "test_project"'
        close(unit)
        
        ! Simulate zero-config workflow
        call initialize_config(config)
        success = apply_zero_configuration_defaults(config)
        call assert_true(success, &
            "Zero-config defaults should apply", passed)
        
        success = enhance_zero_config_with_auto_discovery(config)
        call assert_true(success, &
            "Auto-discovery enhancement should work", passed)
        
        ! Verify expected state
        call assert_true(config%auto_discovery_enabled, &
            "Auto-discovery should be enabled", passed)
        call assert_equals_str(config%coverage_output, "coverage", &
            "Coverage output should use default", passed)
        
        ! Clean up
        open(newunit=unit, file="fpm.toml")
        close(unit, status="delete")
        
        call runner%run_test("seamless_user_flow", passed)
    end subroutine test_seamless_user_flow
    
    subroutine test_feedback_integration()
        !! Test user feedback and reporting integration
        type(config_t) :: config
        logical :: passed
        character(len=256) :: message
        
        passed = .true.
        
        call initialize_config(config)
        config%verbose = .true.
        config%show_progress = .true.
        
        call apply_zero_configuration_defaults(config)
        
        ! Verify feedback settings
        call assert_true(config%verbose, &
            "Verbose mode should remain enabled", passed)
        call assert_true(config%show_progress, &
            "Progress display should remain enabled", passed)
        
        call runner%run_test("feedback_integration", passed)
    end subroutine test_feedback_integration
    
    subroutine test_graceful_degradation()
        !! Test graceful degradation when components fail
        type(config_t) :: config
        logical :: passed, success
        
        passed = .true.
        
        call initialize_config(config)
        config%auto_discovery_enabled = .true.
        config%fail_on_error = .false.
        
        ! Simulate partial failure scenario
        success = apply_zero_configuration_defaults(config)
        call assert_true(success, &
            "Should apply defaults despite missing components", passed)
        
        ! Should still have basic functionality
        call assert_equals_str(config%coverage_output, "coverage", &
            "Should maintain basic defaults", passed)
        
        call runner%run_test("graceful_degradation", passed)
    end subroutine test_graceful_degradation
    
    subroutine test_error_recovery()
        !! Test error recovery mechanisms
        type(config_t) :: config
        logical :: passed
        integer :: unit
        
        passed = .true.
        
        ! Create corrupted config file
        open(newunit=unit, file="fortcov.toml", status="replace")
        write(unit, '(A)') "invalid toml syntax {"
        close(unit)
        
        call initialize_config(config)
        config%ignore_errors = .true.
        
        ! Should recover and use defaults
        call apply_zero_configuration_defaults(config)
        
        call assert_equals_str(config%coverage_output, "coverage", &
            "Should use defaults after error recovery", passed)
        
        ! Clean up
        open(newunit=unit, file="fortcov.toml")
        close(unit, status="delete")
        
        call runner%run_test("error_recovery", passed)
    end subroutine test_error_recovery
    
end program test_zero_config_workflow