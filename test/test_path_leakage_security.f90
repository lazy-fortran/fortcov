program test_path_leakage_security
    !! 
    !! Given-When-Then Test Documentation:
    !! 
    !! GIVEN: Error handling in secure_command_executor may leak path information
    !! WHEN: Security violations or file operations fail 
    !! THEN: Error messages should NOT contain sensitive path information
    !! 
    !! This test demonstrates Issue #310: Path leakage in error messages
    !!
    use file_ops_secure, only: safe_find_files, safe_mkdir
    use path_security_core, only: validate_path_security, validate_executable_path
    use error_handling_core
    implicit none
    
    integer :: test_count = 0
    integer :: failed_count = 0
    
    print *, "=================================================================="
    print *, "SECURITY: Path Information Leakage Test Suite (Issue #310)"
    print *, "=================================================================="
    print *, ""
    print *, "Testing error messages for sensitive path information disclosure"
    print *, "Expected: Error messages should be sanitized and generic"
    print *, ""
    
    ! Test path leakage scenarios
    call test_temp_file_deletion_path_leakage()
    call test_system_path_error_leakage()
    call test_sensitive_directory_path_leakage()
    call test_security_assessment_path_leakage()
    
    ! Report results
    print *, ""
    print *, "=================================================================="
    print *, "Path Leakage Security Test Results"
    print *, "=================================================================="
    write(*, '(A, I0, A, I0, A)') "Tests run: ", test_count, ", Failed: ", &
        failed_count, " (failures indicate path leakage vulnerabilities)"
    
    if (failed_count > 0) then
        print *, ""
        print *, "🚨 PATH LEAKAGE VULNERABILITIES FOUND"
        print *, "   Error messages contain sensitive path information"
        print *, "   CRITICAL: Implement path sanitization in error handling"
        print *, ""
    else
        print *, ""
        print *, "✅ PATH LEAKAGE PROTECTION ACTIVE"
        print *, "   Error messages properly sanitized"
        print *, ""
    end if
    
contains

    subroutine test_temp_file_deletion_path_leakage()
        !! 
        !! Given-When-Then: Test temp file deletion error message leakage
        !! 
        !! GIVEN: Temp file deletion failures in safe_close_and_delete
        !! WHEN: Deletion fails and error messages are generated
        !! THEN: Full file paths should NOT appear in error messages
        !!
        character(len=:), allocatable :: files(:)
        type(error_context_t) :: error_ctx
        
        call start_test("Temp File Deletion Path Leakage")
        
        ! Trigger temp file deletion scenario with sensitive path
        call safe_find_files("/home/sensitive/.ssh/nonexistent", files, error_ctx)
        
        ! Check if error message contains sensitive path information
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            if (index(error_ctx%message, "/home/sensitive/.ssh") > 0 .or. &
                index(error_ctx%message, "fortcov_1234_temp.txt") > 0) then
                call fail_test("CRITICAL: Error message leaks sensitive temp file path")
            else
                call pass_test("Temp file path properly sanitized in error message")
            end if
        else
            call fail_test("Security error not triggered - test inconclusive")
        end if
        
    end subroutine test_temp_file_deletion_path_leakage

    subroutine test_system_path_error_leakage()
        !! 
        !! Given-When-Then: Test system path error message leakage
        !! 
        !! GIVEN: System path validation failures
        !! WHEN: Access to system directories is blocked
        !! THEN: Error messages should not reveal specific system paths
        !!
        character(len=:), allocatable :: safe_path
        type(error_context_t) :: error_ctx
        
        call start_test("System Path Error Message Leakage")
        
        ! Test with sensitive system path
        call validate_path_security("/etc/shadow", safe_path, error_ctx)
        
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            if (index(error_ctx%message, "/etc/shadow") > 0 .or. &
                index(error_ctx%message, "/etc/") > 0) then
                call fail_test("CRITICAL: Error message leaks system file path")
            else
                call pass_test("System path properly sanitized in error message")
            end if
        else
            call fail_test("System path not blocked - different issue")
        end if
        
    end subroutine test_system_path_error_leakage

    subroutine test_sensitive_directory_path_leakage()
        !! 
        !! Given-When-Then: Test sensitive directory path leakage
        !! 
        !! GIVEN: Directory creation failures with sensitive paths
        !! WHEN: mkdir operations fail on restricted paths  
        !! THEN: Error messages should not reveal the attempted directory path
        !!
        type(error_context_t) :: error_ctx
        
        call start_test("Sensitive Directory Path Leakage")
        
        ! Test directory creation with sensitive path
        call safe_mkdir("/root/.ssh/malicious_directory", error_ctx)
        
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            if (index(error_ctx%message, "/root/.ssh/malicious_directory") > 0 .or. &
                index(error_ctx%message, "/root/") > 0) then
                call fail_test("CRITICAL: Error message leaks sensitive directory path")
            else
                call pass_test("Sensitive directory path sanitized in error message")
            end if
        else
            call fail_test("Sensitive directory access not blocked")
        end if
        
    end subroutine test_sensitive_directory_path_leakage

    subroutine test_security_assessment_path_leakage()
        !! 
        !! Given-When-Then: Test security assessment message path leakage
        !! 
        !! GIVEN: Security assessment functions that report on file operations
        !! WHEN: Security issues are detected and reported
        !! THEN: Security messages should not leak sensitive file paths
        !!
        character(len=:), allocatable :: files(:)
        type(error_context_t) :: error_ctx
        
        call start_test("Security Assessment Message Path Leakage")
        
        ! Trigger security assessment with sensitive pattern
        call safe_find_files("/home/user/.ssh/*.key", files, error_ctx)
        
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            if (index(error_ctx%message, "/home/user/.ssh") > 0 .or. &
                index(error_ctx%message, ".key") > 0) then
                call fail_test("CRITICAL: Security assessment leaks sensitive path info")
            else
                call pass_test("Security assessment properly sanitizes path information")
            end if
        else
            call fail_test("Security assessment not triggered")
        end if
        
    end subroutine test_security_assessment_path_leakage

    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A, I0, A, A)') "Test ", test_count, ": ", test_name
    end subroutine start_test
    
    subroutine pass_test(message)
        character(len=*), intent(in) :: message
        print *, "   ✅ PASS: " // trim(message)
        print *, ""
    end subroutine pass_test
    
    subroutine fail_test(message)
        character(len=*), intent(in) :: message
        failed_count = failed_count + 1
        print *, "   🚨 FAIL: " // trim(message)
        print *, ""
    end subroutine fail_test

end program test_path_leakage_security
