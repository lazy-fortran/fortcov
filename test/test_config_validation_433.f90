program test_config_validation_433
    !! Test for issue #433 - Configuration validation always fails for valid CLI
    !! arguments
    use fortcov_config
    use config_types
    implicit none
    
    type(config_t) :: config
    logical :: success
    character(len=512) :: error_message
    character(len=256) :: args(3)
    logical :: valid
    integer :: test_failures
    
    test_failures = 0
    
    ! Test 1: Valid source path with existing directory
    print *, "Test 1: Valid source path with existing directory"
    args(1) = "--source=/home/ert/code/fortcov/src"
    args(2) = "--output=coverage.md"
    args(3) = ""
    call parse_config(args(1:2), config, success, error_message)
    if (.not. success) then
        print *, "  FAIL: Parsing failed: ", trim(error_message)
        test_failures = test_failures + 1
    else
        print *, "  Parsing succeeded"
        valid = validate_config(config)
        if (.not. valid) then
            print *, "  FAIL: Validation failed for valid path"
            test_failures = test_failures + 1
        else
            print *, "  PASS: Validation succeeded"
        end if
    end if
    
    ! Test 2: Non-existent source path
    print *, ""
    print *, "Test 2: Non-existent source path"
    args(1) = "--source=/nonexistent/path"
    args(2) = "--output=coverage.md"
    call parse_config(args(1:2), config, success, error_message)
    if (.not. success) then
        print *, "  Parsing failed (expected): ", trim(error_message)
    else
        print *, "  Parsing succeeded"
        valid = validate_config(config)
        if (.not. valid) then
            print *, "  PASS: Validation correctly failed for non-existent path"
        else
            print *, "  FAIL: Validation should have failed for non-existent path"
            test_failures = test_failures + 1
        end if
    end if
    
    ! Test 3: Config file argument
    print *, ""
    print *, "Test 3: Config file argument"
    args(1) = "--config=fortcov.nml.example"
    call parse_config(args(1:1), config, success, error_message)
    if (.not. success) then
        print *, "  FAIL: Parsing failed: ", trim(error_message)
        test_failures = test_failures + 1
    else
        print *, "  PASS: Config argument parsed successfully"
    end if
    
    ! Test 4: Validate flag alone
    print *, ""
    print *, "Test 4: Validate flag alone"
    args(1) = "--validate"
    call parse_config(args(1:1), config, success, error_message)
    if (.not. success) then
        print *, "  FAIL: Parsing failed: ", trim(error_message)
        test_failures = test_failures + 1
    else
        print *, "  PASS: Validate flag parsed successfully"
    end if
    
    ! Summary
    print *, ""
    print *, "================================"
    if (test_failures == 0) then
        print *, "All tests passed!"
        call exit(0)
    else
        print *, "Test failures: ", test_failures
        call exit(1)
    end if
    
end program test_config_validation_433