program test_zero_config_no_help_issue_249
    !! Test that zero-configuration mode doesn't show help when no args
    !! Issue #249: fortcov without arguments should NOT show help
    
    use fortcov_config
    use error_handling
    implicit none
    
    type(config_t) :: config
    character(len=:), allocatable :: args(:)
    character(len=256) :: error_message
    logical :: success
    integer :: exit_code
    
    exit_code = 0
    
    print *, "============================================"
    print *, "Testing Issue #249: Zero-config vs Help"
    print *, "============================================"
    
    ! Test 1: Empty args should NOT trigger help
    print *, ""
    print *, "Test 1: Empty arguments array"
    allocate(character(len=256) :: args(0))
    
    call parse_config(args, config, success, error_message)
    
    if (config%show_help) then
        print *, "FAIL: show_help is TRUE with no arguments"
        print *, "      Zero-config mode should NOT show help"
        exit_code = 1
    else
        print *, "PASS: show_help is FALSE (correct)"
    end if
    
    if (.not. config%zero_configuration_mode) then
        print *, "FAIL: zero_configuration_mode is FALSE"
        print *, "      Should be TRUE with no arguments"
        exit_code = 1
    else
        print *, "PASS: zero_configuration_mode is TRUE (correct)"
    end if
    
    ! Test 2: Verify --help flag works correctly
    print *, ""
    print *, "Test 2: Explicit --help flag"
    deallocate(args)
    allocate(character(len=256) :: args(1))
    args(1) = "--help"
    
    call parse_config(args, config, success, error_message)
    
    if (.not. config%show_help) then
        print *, "FAIL: show_help is FALSE with --help flag"
        exit_code = 1
    else
        print *, "PASS: --help flag correctly sets show_help"
    end if
    
    ! Test 3: Single positional arg shouldn't trigger help
    print *, ""
    print *, "Test 3: Single coverage file argument"
    deallocate(args)
    allocate(character(len=256) :: args(1))
    args(1) = "test.gcov"
    
    call parse_config(args, config, success, error_message)
    
    if (config%show_help) then
        print *, "FAIL: show_help is TRUE with coverage file"
        exit_code = 1
    else
        print *, "PASS: Coverage file doesn't trigger help"
    end if
    
    ! Summary
    print *, ""
    print *, "============================================"
    if (exit_code == 0) then
        print *, "✓ All tests PASSED - Zero-config works!"
        print *, "  Issue #249 appears to be already fixed"
    else
        print *, "✗ Tests FAILED - Zero-config shows help"
        print *, "  Issue #249 needs to be fixed"
    end if
    print *, "============================================"
    
    call exit(exit_code)
    
end program test_zero_config_no_help_issue_249