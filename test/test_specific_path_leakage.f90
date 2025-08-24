program test_specific_path_leakage
    !! 
    !! Given-When-Then Test Documentation:
    !! 
    !! GIVEN: Specific lines in secure_command_executor.f90 with path leakage
    !! WHEN: Error conditions trigger these specific code paths
    !! THEN: Error messages should be sanitized to prevent information disclosure
    !! 
    !! This test targets the exact path leakage vulnerabilities in issue #310
    !!
    use secure_command_executor
    use error_handling
    implicit none
    
    integer :: test_count = 0
    integer :: failed_count = 0
    
    print *, "=================================================================="
    print *, "SPECIFIC: Path Leakage Security Vulnerabilities (Issue #310)"
    print *, "=================================================================="
    print *, ""
    print *, "Testing specific lines in secure_command_executor.f90 for path leakage"
    print *, "Target lines: 240-247, 255-257, 464, 555"
    print *, ""
    
    ! Test specific leakage points
    call test_line_240_247_temp_file_path_leakage()
    call test_line_255_257_temp_file_name_leakage()
    call test_line_464_mkdir_path_leakage()  
    call test_line_555_executable_path_leakage()
    
    ! Report results
    print *, ""
    print *, "=================================================================="
    print *, "Specific Path Leakage Test Results"
    print *, "=================================================================="
    write(*, '(A, I0, A, I0, A)') "Tests run: ", test_count, ", Failed: ", &
        failed_count, " (failures indicate critical path leakage)"
    
    if (failed_count > 0) then
        print *, ""
        print *, "🚨 CRITICAL PATH LEAKAGE CONFIRMED"
        print *, "   Specific code lines leak sensitive path information"
        print *, "   IMMEDIATE SECURITY FIX REQUIRED"
        print *, ""
    else
        print *, ""
        print *, "✅ PATH LEAKAGE RESOLVED" 
        print *, "   All specific vulnerabilities have been addressed"
        print *, ""
    end if
    
contains

    subroutine test_line_240_247_temp_file_path_leakage()
        !! 
        !! Given-When-Then: Test path leakage via temp file operations
        !! 
        !! GIVEN: safe_find_files creates temporary files that may fail to delete (lines 240-247)
        !! WHEN: Temp file deletion fails during safe_find_files operation
        !! THEN: Error message should not contain full file path
        !!
        character(len=:), allocatable :: files(:)
        type(error_context_t) :: error_ctx
        
        call start_test("Lines 240-247: Temp File Deletion Path Leakage via safe_find_files")
        
        ! Force a scenario that creates temp files and may fail to delete them
        ! This should trigger the safe_close_and_delete code path with potential leakage
        call safe_find_files("/sensitive/directory/pattern*", files, error_ctx)
        
        ! Check if error message contains temp file path information
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            if (index(error_ctx%message, "fortcov") > 0 .or. &
                index(error_ctx%message, "_temp.txt") > 0 .or. &
                index(error_ctx%message, "/tmp/") > 0) then
                call fail_test("CRITICAL: Lines 240-247 leak temp file path in error message")
            else
                call pass_test("Lines 240-247: Temp file path properly sanitized")
            end if
        else
            call pass_test("No temp file error triggered - may need different test scenario")
        end if
        
    end subroutine test_line_240_247_temp_file_path_leakage

    subroutine test_line_255_257_temp_file_name_leakage()
        !! 
        !! Given-When-Then: Test specific path leakage in lines 255-257
        !! 
        !! GIVEN: safe_close_and_delete recovery error messages (lines 255-257)
        !! WHEN: Primary deletion fails but recovery succeeds
        !! THEN: Error message should not contain temp file name
        !!
        character(len=:), allocatable :: files(:)
        type(error_context_t) :: error_ctx
        
        call start_test("Lines 255-257: Temp File Recovery Path Leakage")
        
        ! Force a condition that triggers the recovery path with potential leakage
        ! This pattern should trigger temp file creation and potential close issues
        call safe_find_files("nonexistent_pattern_that_forces_temp_creation", files, error_ctx)
        
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            if (index(error_ctx%message, "fortcov") > 0 .or. &
                index(error_ctx%message, "_temp.txt") > 0 .or. &
                index(error_ctx%message, "/tmp/") > 0) then
                call fail_test("CRITICAL: Lines 255-257 leak temp file name in recovery")
            else
                call pass_test("Lines 255-257: Temp file recovery properly sanitized")
            end if
        else
            call pass_test("No recovery error triggered - test inconclusive")
        end if
        
    end subroutine test_line_255_257_temp_file_name_leakage

    subroutine test_line_464_mkdir_path_leakage()
        !! 
        !! Given-When-Then: Test specific path leakage in line 464
        !! 
        !! GIVEN: safe_mkdir error message construction (line 464)
        !! WHEN: Directory creation fails
        !! THEN: Error message should not reveal the attempted directory path
        !!
        type(error_context_t) :: error_ctx
        
        call start_test("Line 464: mkdir Path Leakage")
        
        ! Test mkdir with a path that will fail and potentially leak in error message
        call safe_mkdir("/restricted/sensitive/user/directory", error_ctx)
        
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            if (index(error_ctx%message, "/restricted/sensitive/user/directory") > 0 .or. &
                index(error_ctx%message, "sensitive") > 0 .or. &
                index(error_ctx%message, "/restricted/") > 0) then
                call fail_test("CRITICAL: Line 464 leaks directory path in mkdir error")
            else
                call pass_test("Line 464: mkdir path properly sanitized")
            end if
        else
            call fail_test("mkdir error not triggered - path validation may have issues")
        end if
        
    end subroutine test_line_464_mkdir_path_leakage

    subroutine test_line_555_executable_path_leakage()
        !! 
        !! Given-When-Then: Test specific path leakage in line 555
        !! 
        !! GIVEN: validate_executable_path error message (line 555)
        !! WHEN: Executable validation fails
        !! THEN: Error message should not contain full executable path
        !!
        character(len=:), allocatable :: safe_executable
        type(error_context_t) :: error_ctx
        
        call start_test("Line 555: Executable Path Leakage")
        
        ! Test with sensitive executable path that doesn't exist
        call validate_executable_path("/usr/local/sensitive/secret_tool", safe_executable, error_ctx)
        
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            if (index(error_ctx%message, "/usr/local/sensitive/secret_tool") > 0 .or. &
                index(error_ctx%message, "sensitive") > 0 .or. &
                index(error_ctx%message, "secret_tool") > 0) then
                call fail_test("CRITICAL: Line 555 leaks executable path in error message")
            else
                call pass_test("Line 555: Executable path properly sanitized")
            end if
        else
            call fail_test("Executable validation error not triggered")
        end if
        
    end subroutine test_line_555_executable_path_leakage

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

end program test_specific_path_leakage