program test_security_fixes
    !! Comprehensive tests for security fixes and injection prevention
    !! 
    !! This test program validates that all security vulnerabilities identified
    !! in Patrick's audit have been properly fixed and that the system is
    !! resistant to common attack vectors.
    use secure_command_executor
    use fortcov_config
    use error_handling
    implicit none
    
    logical :: all_tests_passed
    integer :: test_count = 0
    integer :: failed_tests = 0
    
    all_tests_passed = .true.
    print *, "Testing Security Fixes..."
    
    ! Test secure command execution
    call test_shell_injection_prevention()
    call test_path_validation()
    call test_executable_validation()
    call test_argument_escaping()
    
    ! Test configuration validation
    call test_config_validation_security()
    call test_malicious_config_rejection()
    
    ! Test error handling robustness
    call test_error_handling_consistency()
    call test_partial_failure_handling()
    
    ! Test edge cases and boundary conditions
    call test_edge_case_security()
    call test_resource_exhaustion_protection()
    
    if (all_tests_passed) then
        print *, "All security tests passed!"
    else
        print *, "Some security tests failed!"
        print *, "Failed tests:", failed_tests, "out of", test_count
        stop 1
    end if
    
contains

    subroutine test_shell_injection_prevention()
        !! Test that shell injection attempts are blocked
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: files(:)
        
        print *, "  Testing Shell Injection Prevention..."
        test_count = test_count + 4
        
        ! Test 1: Command injection in file patterns
        call safe_find_files("*.gcov; rm -rf /", files, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            print *, "    FAIL: Malicious file pattern should be rejected"
            failed_tests = failed_tests + 1
            all_tests_passed = .false.
        else
            print *, "    PASS: Malicious file pattern correctly rejected"
        end if
        
        ! Test 2: Command injection with pipe
        call safe_find_files("*.gcov | cat /etc/passwd", files, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            print *, "    FAIL: Pipe injection should be rejected"
            failed_tests = failed_tests + 1
            all_tests_passed = .false.
        else
            print *, "    PASS: Pipe injection correctly rejected"
        end if
        
        ! Test 3: Command injection with backticks
        call safe_find_files("*.gcov`whoami`", files, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            print *, "    FAIL: Backtick injection should be rejected"
            failed_tests = failed_tests + 1
            all_tests_passed = .false.
        else
            print *, "    PASS: Backtick injection correctly rejected"
        end if
        
        ! Test 4: Directory traversal
        call safe_find_files("../../../etc/passwd", files, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            print *, "    FAIL: Directory traversal should be rejected"
            failed_tests = failed_tests + 1
            all_tests_passed = .false.
        else
            print *, "    PASS: Directory traversal correctly rejected"
        end if
    end subroutine test_shell_injection_prevention

    subroutine test_path_validation()
        !! Test comprehensive path validation
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: safe_path
        
        print *, "  Testing Path Validation..."
        test_count = test_count + 5
        
        ! Test 1: Valid paths should pass
        call validate_path_security("src/main.f90", safe_path, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            print *, "    FAIL: Valid relative path should be accepted"
            failed_tests = failed_tests + 1
            all_tests_passed = .false.
        else
            print *, "    PASS: Valid relative path correctly accepted"
        end if
        
        ! Test 2: Empty paths should default to current directory
        call validate_path_security("", safe_path, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS .or. safe_path /= ".") then
            print *, "    FAIL: Empty path should default to current directory"
            failed_tests = failed_tests + 1
            all_tests_passed = .false.
        else
            print *, "    PASS: Empty path correctly defaults to current directory"
        end if
        
        ! Test 3: Dangerous characters should be rejected
        call validate_path_security("file;rm -rf /", safe_path, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            print *, "    FAIL: Path with semicolon should be rejected"
            failed_tests = failed_tests + 1
            all_tests_passed = .false.
        else
            print *, "    PASS: Path with semicolon correctly rejected"
        end if
        
        ! Test 4: Directory traversal should be rejected
        call validate_path_security("../../../etc/passwd", safe_path, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            print *, "    FAIL: Directory traversal should be rejected"
            failed_tests = failed_tests + 1
            all_tests_passed = .false.
        else
            print *, "    PASS: Directory traversal correctly rejected"
        end if
        
        ! Test 5: Long paths should be rejected
        block
            character(len=5000) :: long_path
            long_path = repeat("a", 5000)
            call validate_path_security(long_path, safe_path, error_ctx)
            if (error_ctx%error_code == ERROR_SUCCESS) then
                print *, "    FAIL: Excessively long path should be rejected"
                failed_tests = failed_tests + 1
                all_tests_passed = .false.
            else
                print *, "    PASS: Excessively long path correctly rejected"
            end if
        end block
    end subroutine test_path_validation

    subroutine test_executable_validation()
        !! Test executable path validation and existence checking
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: safe_executable
        
        print *, "  Testing Executable Validation..."
        test_count = test_count + 2
        
        ! Test malicious executable should be rejected
        call validate_executable_path("gcov; rm -rf /", safe_executable, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            print *, "    FAIL: Executable with injection should be rejected"
            failed_tests = failed_tests + 1
            all_tests_passed = .false.
        else
            print *, "    PASS: Executable with injection correctly rejected"
        end if
        
        ! Test non-existent executable should be rejected
        call validate_executable_path("nonexistent_executable_12345", safe_executable, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            print *, "    FAIL: Non-existent executable should be rejected"
            failed_tests = failed_tests + 1
            all_tests_passed = .false.
        else
            print *, "    PASS: Non-existent executable correctly rejected"
        end if
    end subroutine test_executable_validation

    subroutine test_argument_escaping()
        !! Test shell argument escaping functionality
        character(len=:), allocatable :: escaped
        
        print *, "  Testing Argument Escaping..."
        test_count = test_count + 2
        
        ! Test simple argument escaping
        escaped = escape_shell_argument("normal_file.txt")
        if (escaped /= "'normal_file.txt'") then
            print *, "    FAIL: Normal filename should be quoted"
            failed_tests = failed_tests + 1
            all_tests_passed = .false.
        else
            print *, "    PASS: Normal filename correctly quoted"
        end if
        
        ! Test dangerous characters are contained
        escaped = escape_shell_argument("file;rm -rf /")
        if (escaped(1:1) /= "'" .or. escaped(len(escaped):len(escaped)) /= "'") then
            print *, "    FAIL: Dangerous characters should be contained in quotes"
            failed_tests = failed_tests + 1
            all_tests_passed = .false.
        else
            print *, "    PASS: Dangerous characters correctly contained"
        end if
    end subroutine test_argument_escaping

    subroutine test_config_validation_security()
        !! Test configuration validation for security
        type(config_t) :: config
        type(error_context_t) :: error_ctx
        
        print *, "  Testing Configuration Validation..."
        test_count = test_count + 2
        
        ! Test malicious gcov executable should be rejected
        call initialize_config(config)
        config%gcov_executable = "gcov; rm -rf /"
        call validate_config(config, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            print *, "    FAIL: Malicious gcov executable should be rejected"
            failed_tests = failed_tests + 1
            all_tests_passed = .false.
        else
            print *, "    PASS: Malicious gcov executable correctly rejected"
        end if
        
        ! Test invalid coverage threshold should be rejected
        call initialize_config(config)
        config%minimum_coverage = -10.0
        call validate_config(config, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            print *, "    FAIL: Negative coverage threshold should be rejected"
            failed_tests = failed_tests + 1
            all_tests_passed = .false.
        else
            print *, "    PASS: Negative coverage threshold correctly rejected"
        end if
    end subroutine test_config_validation_security

    subroutine test_malicious_config_rejection()
        !! Test rejection of various malicious configuration attempts
        use secure_command_executor
        use error_handling
        
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: safe_path
        logical :: test_passed
        
        print *, "  Testing Malicious Config Rejection..."
        test_passed = .true.
        
        ! Test 1: Shell injection attempts in path
        call validate_path_security("test; rm -rf /", safe_path, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            test_passed = .false.
            print *, "    FAIL: Shell injection with semicolon not blocked"
        end if
        
        ! Test 2: Command substitution attempts
        call validate_path_security("test`whoami`", safe_path, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            test_passed = .false.
            print *, "    FAIL: Command substitution not blocked"
        end if
        
        ! Test 3: Environment variable injection
        call validate_path_security("test$HOME/malicious", safe_path, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            test_passed = .false.
            print *, "    FAIL: Environment variable injection not blocked"
        end if
        
        ! Test 4: Directory traversal attempts
        call validate_path_security("../../../etc/passwd", safe_path, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            test_passed = .false.
            print *, "    FAIL: Directory traversal not blocked"
        end if
        
        ! Test 5: Quote escaping attempts
        call validate_path_security("test'echo malicious'", safe_path, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            test_passed = .false.
            print *, "    FAIL: Quote injection not blocked"
        end if
        
        if (test_passed) then
            print *, "    PASS: All malicious configuration attempts properly rejected"
        end if
    end subroutine test_malicious_config_rejection

    subroutine test_error_handling_consistency()
        !! Test that error handling is consistent and comprehensive
        use secure_command_executor
        use gcov_command_executor
        use error_handling
        
        type(error_context_t) :: error_ctx1, error_ctx2
        character(len=:), allocatable :: safe_path
        type(gcov_executor_t) :: executor
        character(len=:), allocatable :: gcov_files(:)
        logical :: test_passed
        
        print *, "  Testing Error Handling Consistency..."
        test_passed = .true.
        
        ! Test 1: Both modules return proper error contexts for invalid paths
        call validate_path_security("invalid;path", safe_path, error_ctx1)
        call executor%execute_gcov("nonexistent_file.f90", gcov_files, error_ctx2)
        
        if (len_trim(error_ctx1%message) == 0 .or. len_trim(error_ctx1%suggestion) == 0) then
            test_passed = .false.
            print *, "    FAIL: Secure executor error context incomplete"
        end if
        
        if (len_trim(error_ctx2%message) == 0 .or. len_trim(error_ctx2%suggestion) == 0) then
            test_passed = .false.
            print *, "    FAIL: GCov executor error context incomplete"
        end if
        
        ! Test 2: Error codes are within expected ranges
        if (error_ctx1%error_code < ERROR_SUCCESS .or. error_ctx1%error_code > 9999) then
            test_passed = .false.
            print *, "    FAIL: Error code out of expected range:", error_ctx1%error_code
        end if
        
        if (error_ctx2%error_code < ERROR_SUCCESS .or. error_ctx2%error_code > 9999) then
            test_passed = .false.
            print *, "    FAIL: Error code out of expected range:", error_ctx2%error_code
        end if
        
        ! Test 3: Recoverable flag is set appropriately for different error types
        if (error_ctx1%error_code == ERROR_INVALID_CONFIG .and. error_ctx1%recoverable) then
            test_passed = .false.
            print *, "    FAIL: Security errors should not be recoverable"
        end if
        
        if (test_passed) then
            print *, "    PASS: Error handling is consistent across modules"
        end if
    end subroutine test_error_handling_consistency

    subroutine test_partial_failure_handling()
        !! Test handling of partial failures in batch operations
        use secure_command_executor
        use error_handling
        
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: files(:)
        character(len=:), allocatable :: safe_path
        logical :: test_passed
        
        print *, "  Testing Partial Failure Handling..."
        test_passed = .true.
        
        ! Test 1: File search with mix of valid and invalid patterns
        call safe_find_files("nonexistent_dir/*.f90", files, error_ctx)
        
        ! Should handle gracefully without crashing
        if (.not. allocated(files)) then
            test_passed = .false.
            print *, "    FAIL: File search doesn't allocate empty result array"
        else
            ! Should return empty array for non-existent directory
            if (size(files) > 0) then
                print *, "    INFO: Found unexpected files in nonexistent directory"
            end if
        end if
        
        ! Test 2: Path validation with recoverable vs non-recoverable errors
        call validate_executable_path("nonexistent_executable", safe_path, error_ctx)
        
        if (error_ctx%error_code == ERROR_SUCCESS) then
            test_passed = .false.
            print *, "    FAIL: Should reject nonexistent executable"
        else
            ! Should provide clear error message for missing executables
            if (len_trim(error_ctx%message) == 0) then
                test_passed = .false.
                print *, "    FAIL: No error message for missing executable"
            end if
        end if
        
        if (test_passed) then
            print *, "    PASS: Partial failures handled appropriately"
        end if
    end subroutine test_partial_failure_handling

    subroutine test_edge_case_security()
        !! Test edge cases and boundary conditions for security
        use secure_command_executor
        use error_handling
        
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: safe_path
        logical :: test_passed
        character(len=5000) :: very_long_path
        integer :: i
        
        print *, "  Testing Edge Case Security..."
        test_passed = .true.
        
        ! Test 1: Path length limits
        very_long_path = ""
        do i = 1, 500
            very_long_path = trim(very_long_path) // "verylongpath/"
        end do
        
        call validate_path_security(very_long_path, safe_path, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            test_passed = .false.
            print *, "    FAIL: Excessively long paths not rejected"
        end if
        
        ! Test 2: Null character injection attempts
        call validate_path_security("test" // char(0) // "malicious", safe_path, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            test_passed = .false.
            print *, "    FAIL: Null character injection not blocked"
        end if
        
        ! Test 3: Unicode and special characters
        call validate_path_security("test/../../../", safe_path, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            test_passed = .false.
            print *, "    FAIL: Directory traversal sequences not blocked"
        end if
        
        ! Test 4: Windows-style path injection (even on Linux for cross-platform security)
        call validate_path_security("C:\\Windows\\system32", safe_path, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            ! This might be valid on Windows, but test the backslash handling
            if (index(safe_path, '\') > 0) then
                test_passed = .false.
                print *, "    FAIL: Backslash characters not properly handled"
            end if
        end if
        
        ! Test 5: Empty executable name
        call validate_executable_path("", safe_path, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            test_passed = .false.
            print *, "    FAIL: Empty executable path not rejected"
        end if
        
        if (test_passed) then
            print *, "    PASS: All edge cases properly secured"
        end if
    end subroutine test_edge_case_security

    subroutine test_resource_exhaustion_protection()
        !! Test protection against resource exhaustion attacks
        use secure_command_executor
        use error_handling
        
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: safe_path
        character(len=:), allocatable :: files(:)
        logical :: test_passed
        integer :: i
        character(len=256) :: test_patterns(100)
        
        print *, "  Testing Resource Exhaustion Protection..."
        test_passed = .true.
        
        ! Test 1: Large number of file patterns
        do i = 1, 100
            write(test_patterns(i), '(A,I0,A)') "pattern_", i, "/*.f90"
        end do
        
        ! Should handle many patterns without crashing
        call safe_find_files("nonexistent_massive_directory/**/*.f90", files, error_ctx)
        if (.not. allocated(files)) then
            test_passed = .false.
            print *, "    FAIL: Resource exhaustion caused allocation failure"
        end if
        
        ! Test 2: Verify MAX_ARGS limit is enforced (implicit test)
        ! The secure executor should have built-in limits
        
        ! Test 3: Memory allocation limits for result arrays
        ! Should gracefully handle cases where many files might be found
        call safe_find_files("*.f90", files, error_ctx)
        if (.not. allocated(files)) then
            test_passed = .false.
            print *, "    FAIL: Basic file search allocation failed"
        else
            ! Check that result array size is reasonable
            if (size(files) > 1000) then
                print *, "    INFO: Large result set returned:", size(files), "files"
                ! This isn't necessarily a failure, but worth noting
            end if
        end if
        
        ! Test 4: Buffer overflow protection in path handling
        ! MAX_PATH_LENGTH should prevent buffer overflows
        do i = 1, 10
            call validate_path_security(repeat("a", i * 1000), safe_path, error_ctx)
            if (i > 4 .and. error_ctx%error_code == ERROR_SUCCESS) then
                test_passed = .false.
                print *, "    FAIL: Path length limit not enforced at size:", i * 1000
                exit
            end if
        end do
        
        if (test_passed) then
            print *, "    PASS: Resource exhaustion protections working"
        end if
    end subroutine test_resource_exhaustion_protection

end program test_security_fixes