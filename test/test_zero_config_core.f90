program test_zero_config_core
    !! Core zero-configuration mode tests
    !! Tests basic zero-config functionality and defaults
    
    use test_utilities, only: test_runner_t, assert_true, assert_false, &
                              assert_equals_str, assert_equals_int
    use config_types, only: config_t
    use fortcov_config, only: parse_config, initialize_config
    use zero_configuration_manager, only: is_zero_configuration_mode, &
                                         apply_zero_configuration_defaults
    use zero_config_auto_discovery_integration, only: &
        enhance_zero_config_with_auto_discovery, &
        configure_auto_discovery_defaults
    implicit none
    
    type(test_runner_t) :: runner
    logical :: all_passed = .true.
    
    call runner%init("Zero-Configuration Core Tests")
    
    ! Core functionality tests
    call test_zero_config_detection()
    call test_zero_config_defaults()
    call test_zero_config_enhancement()
    call test_auto_discovery_defaults()
    
    ! Configuration interaction tests
    call test_config_override_behavior()
    call test_existing_config_interaction()
    
    call runner%print_summary()
    
    if (runner%get_pass_rate() == 100.0) then
        call exit(0)
    else
        call exit(1)
    end if
    
contains
    
    subroutine test_zero_config_detection()
        !! Test detection of zero-configuration mode
        type(config_t) :: config
        character(len=0), allocatable :: no_args(:)
        logical :: passed, is_zero_config
        
        passed = .true.
        
        ! Test with no arguments
        allocate(no_args(0))
        call initialize_config(config)
        call parse_config(no_args, config)
        is_zero_config = is_zero_configuration_mode(config)
        
        call assert_true(is_zero_config, &
            "Zero-config mode should be detected with no args", passed)
        
        call runner%run_test("zero_config_detection", passed)
    end subroutine test_zero_config_detection
    
    subroutine test_zero_config_defaults()
        !! Test application of zero-config defaults
        type(config_t) :: config
        logical :: passed, success
        
        passed = .true.
        
        call initialize_config(config)
        success = apply_zero_configuration_defaults(config)
        
        call assert_true(success, &
            "Zero-config defaults should apply successfully", passed)
        call assert_true(config%auto_discovery_enabled, &
            "Auto-discovery should be enabled by default", passed)
        call assert_equals_str(config%coverage_output, "coverage", &
            "Default output directory should be 'coverage'", passed)
        
        call runner%run_test("zero_config_defaults", passed)
    end subroutine test_zero_config_defaults
    
    subroutine test_zero_config_enhancement()
        !! Test enhancement with auto-discovery
        type(config_t) :: config
        logical :: passed, success
        
        passed = .true.
        
        call initialize_config(config)
        call apply_zero_configuration_defaults(config)
        success = enhance_zero_config_with_auto_discovery(config)
        
        call assert_true(success, &
            "Zero-config enhancement should succeed", passed)
        call assert_true(config%auto_test_enabled, &
            "Auto-test should be enabled after enhancement", passed)
        
        call runner%run_test("zero_config_enhancement", passed)
    end subroutine test_zero_config_enhancement
    
    subroutine test_auto_discovery_defaults()
        !! Test configuration of auto-discovery defaults
        type(config_t) :: config
        logical :: passed
        
        passed = .true.
        
        call initialize_config(config)
        call configure_auto_discovery_defaults(config)
        
        call assert_true(config%auto_discovery_enabled, &
            "Auto-discovery should be configured", passed)
        call assert_true(config%build_detection_enabled, &
            "Build detection should be enabled", passed)
        call assert_equals_int(config%max_discovery_depth, 3, &
            "Default discovery depth should be 3", passed)
        
        call runner%run_test("auto_discovery_defaults", passed)
    end subroutine test_auto_discovery_defaults
    
    subroutine test_config_override_behavior()
        !! Test that explicit config overrides zero-config
        type(config_t) :: config
        character(len=32), allocatable :: args(:)
        logical :: passed, is_zero_config
        
        passed = .true.
        
        allocate(args(2))
        args(1) = "--coverage"
        args(2) = "custom_dir"
        
        call initialize_config(config)
        call parse_config(args, config)
        is_zero_config = is_zero_configuration_mode(config)
        
        call assert_false(is_zero_config, &
            "Zero-config should be disabled with explicit args", passed)
        call assert_equals_str(config%coverage_output, "custom_dir", &
            "Custom output dir should be preserved", passed)
        
        call runner%run_test("config_override_behavior", passed)
    end subroutine test_config_override_behavior
    
    subroutine test_existing_config_interaction()
        !! Test interaction with existing config file
        type(config_t) :: config
        logical :: passed, has_config_file
        integer :: unit
        
        passed = .true.
        
        ! Create temporary config file
        open(newunit=unit, file="fortcov.toml", status="replace")
        write(unit, '(A)') "[fortcov]"
        write(unit, '(A)') 'coverage_output = "test_output"'
        close(unit)
        
        call initialize_config(config)
        call parse_config([character(len=0)::], config)
        
        call assert_equals_str(config%coverage_output, "test_output", &
            "Config file settings should be respected", passed)
        
        ! Clean up
        open(newunit=unit, file="fortcov.toml")
        close(unit, status="delete")
        
        call runner%run_test("existing_config_interaction", passed)
    end subroutine test_existing_config_interaction
    
end program test_zero_config_core