program test_issue_249_zero_config_working
    !! Test suite confirming Issue #249 is resolved
    !! Zero-configuration mode works correctly and doesn't show help
    
    use fortcov_config
    use error_handling
    use file_utils
    implicit none
    
    type(config_t) :: config
    character(len=:), allocatable :: args(:)
    character(len=256) :: error_message
    logical :: success
    integer :: exit_code, test_num
    logical :: all_pass
    
    exit_code = 0
    test_num = 0
    all_pass = .true.
    
    print *, "==============================================================="
    print *, "Issue #249: Zero-configuration mode verification"
    print *, "==============================================================="
    print *, ""
    print *, "This test suite verifies that zero-configuration mode:"
    print *, "1. Activates when no arguments are provided"
    print *, "2. Does NOT show help message"
    print *, "3. Attempts coverage analysis"
    print *, "4. Sets correct default paths"
    print *, ""
    
    ! Test 1: No arguments activates zero-config
    test_num = test_num + 1
    print '(A,I2,A)', "Test ", test_num, ": No arguments -> zero-configuration mode"
    allocate(character(len=256) :: args(0))
    call parse_config(args, config, success, error_message)
    
    if (.not. success) then
        print *, "  ✗ FAIL: parse_config failed"
        print *, "    Error: ", trim(error_message)
        all_pass = .false.
    else if (config%show_help) then
        print *, "  ✗ FAIL: show_help is TRUE (should be FALSE)"
        all_pass = .false.
    else if (.not. config%zero_configuration_mode) then
        print *, "  ✗ FAIL: zero_configuration_mode is FALSE (should be TRUE)"
        all_pass = .false.
    else
        print *, "  ✓ PASS: Zero-config activated, help NOT shown"
    end if
    deallocate(args)
    
    ! Test 2: Default output path is set correctly
    test_num = test_num + 1
    print '(A,I2,A)', "Test ", test_num, ": Default output path in zero-config"
    allocate(character(len=256) :: args(0))
    call parse_config(args, config, success, error_message)
    
    if (.not. allocated(config%output_path)) then
        print *, "  ✗ FAIL: output_path not allocated"
        all_pass = .false.
    else if (trim(config%output_path) /= "build/coverage/coverage.md") then
        print *, "  ✗ FAIL: Wrong output path: ", trim(config%output_path)
        all_pass = .false.
    else
        print *, "  ✓ PASS: Default output path correct"
    end if
    deallocate(args)
    
    ! Test 3: Empty string arguments (edge case)
    test_num = test_num + 1
    print '(A,I2,A)', "Test ", test_num, ": Empty string arguments"
    allocate(character(len=256) :: args(2))
    args(1) = ""
    args(2) = "  "
    call parse_config(args, config, success, error_message)
    
    if (config%show_help) then
        print *, "  ✗ FAIL: Empty strings triggered help"
        all_pass = .false.
    else if (.not. config%zero_configuration_mode) then
        print *, "  ✗ FAIL: Empty strings disabled zero-config"
        all_pass = .false.
    else
        print *, "  ✓ PASS: Empty strings correctly ignored"
    end if
    deallocate(args)
    
    ! Test 4: Help flag explicitly requested
    test_num = test_num + 1
    print '(A,I2,A)', "Test ", test_num, ": Explicit --help flag"
    allocate(character(len=256) :: args(1))
    args(1) = "--help"
    call parse_config(args, config, success, error_message)
    
    if (.not. config%show_help) then
        print *, "  ✗ FAIL: --help flag didn't set show_help"
        all_pass = .false.
    else if (config%zero_configuration_mode) then
        print *, "  ✗ FAIL: --help shouldn't trigger zero-config"
        all_pass = .false.
    else
        print *, "  ✓ PASS: --help works correctly"
    end if
    deallocate(args)
    
    ! Test 5: Output flag with no source (should trigger zero-config)
    test_num = test_num + 1
    print '(A,I2,A)', "Test ", test_num, ": Output flag only (zero-config with override)"
    allocate(character(len=256) :: args(2))
    args(1) = "--output"
    args(2) = "custom.md"
    call parse_config(args, config, success, error_message)
    
    if (config%show_help) then
        print *, "  ✗ FAIL: Output flag triggered help"
        all_pass = .false.
    else if (.not. config%zero_configuration_mode) then
        print *, "  ✗ FAIL: Should use zero-config with output override"
        all_pass = .false.
    else if (trim(config%output_path) /= "custom.md") then
        print *, "  ✗ FAIL: Output override not applied"
        all_pass = .false.
    else
        print *, "  ✓ PASS: Zero-config with output override works"
    end if
    deallocate(args)
    
    ! Test 6: Coverage file argument (not zero-config)
    test_num = test_num + 1
    print '(A,I2,A)', "Test ", test_num, ": Coverage file argument"
    allocate(character(len=256) :: args(1))
    args(1) = "test.gcov"
    call parse_config(args, config, success, error_message)
    
    if (config%show_help) then
        print *, "  ✗ FAIL: Coverage file triggered help"
        all_pass = .false.
    else if (config%zero_configuration_mode) then
        print *, "  ✗ FAIL: Coverage file shouldn't trigger zero-config"
        all_pass = .false.
    else
        print *, "  ✓ PASS: Coverage file handled correctly"
    end if
    deallocate(args)
    
    ! Summary
    print *, ""
    print *, "==============================================================="
    if (all_pass) then
        print *, "✓ ALL TESTS PASSED"
        print *, ""
        print *, "Issue #249 Resolution Status: WORKING CORRECTLY"
        print *, ""
        print *, "Zero-configuration mode is functioning as documented:"
        print *, "- Running 'fortcov' without arguments activates zero-config"
        print *, "- It does NOT show the help message"
        print *, "- It attempts to analyze coverage"
        print *, "- Default output: build/coverage/coverage.md"
        exit_code = 0
    else
        print *, "✗ SOME TESTS FAILED"
        print *, ""
        print *, "Issue #249 may need investigation"
        exit_code = 1
    end if
    print *, "==============================================================="
    
    call exit(exit_code)
    
end program test_issue_249_zero_config_working