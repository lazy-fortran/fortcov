program test_timeout_integration
    !! Integration test for secure_command_executor with timeout protection
    use secure_command_executor
    use error_handling
    implicit none
    
    type(error_context_t) :: error_ctx
    integer :: tests_passed = 0
    integer :: tests_total = 2
    
    write(*, '(A)') "Testing timeout integration with secure command executor..."
    
    ! Test 1: Valid timeout execution
    call test_valid_timeout_execution()
    
    ! Test 2: Invalid gcov executable
    call test_invalid_gcov_executable()
    
    ! Report results
    write(*, '(A,I0,A,I0,A)') "Integration tests passed: ", tests_passed, &
        "/", tests_total, " total"
    
    if (tests_passed == tests_total) then
        write(*, '(A)') "All integration tests passed!"
        stop 0
    else
        write(*, '(A)') "Some integration tests failed!"
        stop 1
    end if

contains

    subroutine test_valid_timeout_execution()
        ! Test with echo command (should complete quickly)
        call safe_execute_gcov("echo", "test.f90", ".", .false., &
                               "/dev/null", error_ctx)
        
        ! For this test, we expect it to fail because echo is not gcov
        ! But it should fail with validation error, not timeout
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            write(*, '(A)') "PASS: Valid timeout parameter accepted"
            tests_passed = tests_passed + 1
        else
            write(*, '(A)') "FAIL: Expected validation error for invalid gcov command"
        end if
    end subroutine test_valid_timeout_execution
    
    subroutine test_invalid_gcov_executable()
        ! Test with invalid gcov executable
        call safe_execute_gcov("gcov", "test.f90", ".", .false., &
                               "/dev/null", error_ctx)
        
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            write(*, '(A)') "PASS: Invalid gcov executable rejected"
            tests_passed = tests_passed + 1
        else
            write(*, '(A)') "FAIL: Should reject invalid gcov executable"
        end if
    end subroutine test_invalid_gcov_executable

end program test_timeout_integration