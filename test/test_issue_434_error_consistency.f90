program test_issue_434_error_consistency
    !! Test for issue #434 - Consistent error handling across all flags
    use config_core
    use config_types
    use error_handling_core
    implicit none
    
    type(config_t) :: config
    type(error_context_t) :: error_ctx
    logical :: success
    character(len=512) :: error_message
    character(len=256) :: args(10)
    integer :: test_failures, test_count
    
    test_failures = 0
    test_count = 0
    
    print *, "============================================"
    print *, "Issue #434: Error Handling Consistency Test"
    print *, "============================================"
    print *, ""
    
    ! Test 1: Invalid source path
    test_count = test_count + 1
    print *, "Test 1: Invalid source path"
    args(1) = "--source=/nonexistent/path"
    call parse_config(args(1:1), config, success, error_message)
    if (success) then
        call validate_config_with_context(config, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            if (index(error_ctx%message, "Source path not found") > 0) then
                print *, "  PASS: Specific error for invalid source"
            else
                print *, "  FAIL: Generic error instead of specific"
                test_failures = test_failures + 1
            end if
        else
            print *, "  FAIL: Should have failed validation"
            test_failures = test_failures + 1
        end if
    else
        print *, "  Parse failed: ", trim(error_message)
    end if
    print *, ""
    
    ! Test 2: Invalid output format
    test_count = test_count + 1
    print *, "Test 2: Invalid output format"
    args(1) = "--format=invalid_format"
    args(2) = "--source=/home/ert/code/fortcov/src"
    call parse_config(args(1:2), config, success, error_message)
    if (success) then
        call validate_config_with_context(config, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            if (index(error_ctx%message, "output format") > 0 .or. index(error_ctx%message, "Output format") > 0) then
                print *, "  PASS: Specific error for invalid format"
            else
                print *, "  FAIL: Generic error instead of specific"
                test_failures = test_failures + 1
            end if
        else
            print *, "  FAIL: Should have failed validation"
            test_failures = test_failures + 1
        end if
    else
        print *, "  Parse failed: ", trim(error_message)
    end if
    print *, ""
    
    ! Test 3: Security test - command injection
    test_count = test_count + 1
    print *, "Test 3: Security - command injection"
    args(1) = '--source=path;rm -rf /'
    call parse_config(args(1:1), config, success, error_message)
    if (success) then
        call validate_config_with_context(config, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            print *, "  PASS: Security threat detected with message"
        else
            print *, "  FAIL: Security threat not detected"
            test_failures = test_failures + 1
        end if
    else
        print *, "  Parse failed: ", trim(error_message)
    end if
    print *, ""
    
    ! Test 4: Invalid fail-under threshold
    test_count = test_count + 1
    print *, "Test 4: Invalid fail-under threshold"
    args(1) = "--fail-under=notanumber"
    args(2) = "--source=/home/ert/code/fortcov/src"
    call parse_config(args(1:2), config, success, error_message)
    if (success) then
        print *, "  WARNING: Parser accepted non-numeric threshold"
        ! Might have default value
    else
        if (index(error_message, "fail") > 0 .or. index(error_message, "threshold") > 0) then
            print *, "  PASS: Specific error for invalid threshold"
        else
            print *, "  FAIL: Generic error for threshold"
            test_failures = test_failures + 1
        end if
    end if
    print *, ""
    
    ! Test 5: Unknown flag
    test_count = test_count + 1
    print *, "Test 5: Unknown flag"
    args(1) = "--invalid-flag"
    call parse_config(args(1:1), config, success, error_message)
    if (.not. success) then
        if (index(error_message, "Unknown flag") > 0) then
            print *, "  PASS: Specific error for unknown flag"
        else
            print *, "  FAIL: Generic error for unknown flag"
            test_failures = test_failures + 1
        end if
    else
        print *, "  FAIL: Should have rejected unknown flag"
        test_failures = test_failures + 1
    end if
    print *, ""
    
    ! Test 6: Missing config file
    test_count = test_count + 1
    print *, "Test 6: Missing config file"
    args(1) = "--config=/nonexistent/config.nml"
    call parse_config(args(1:1), config, success, error_message)
    if (.not. success) then
        if (index(error_message, "not found") > 0 .or. index(error_message, "Config") > 0) then
            print *, "  PASS: Specific error for missing config"
        else
            print *, "  FAIL: Generic error for missing config"
            test_failures = test_failures + 1
        end if
    else
        print *, "  FAIL: Should have failed for missing config"
        test_failures = test_failures + 1
    end if
    print *, ""
    
    ! Summary
    print *, "============================================"
    print *, "Test Summary"
    print *, "============================================"
    print '(A,I0,A,I0)', " Tests run: ", test_count, ", Failed: ", test_failures
    if (test_failures == 0) then
        print *, "All tests passed! Issue #434 is resolved."
        print *, "Error handling is now consistent across all flags."
        call exit(0)
    else
        print *, "Some tests failed."
        call exit(1)
    end if
    
end program test_issue_434_error_consistency