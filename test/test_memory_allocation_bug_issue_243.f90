program test_memory_allocation_bug_issue_243
    !! Memory allocation test orchestrator for Issue #243
    !! Delegates to focused memory safety test modules
    
    use iso_fortran_env, only: error_unit
    implicit none
    
    integer :: status
    integer :: total_failures = 0
    
    print *, ""
    print *, "============================================================="
    print *, "Issue #243: Memory Allocation Safety Test Suite"
    print *, "============================================================="
    print *, ""
    
    ! Run core allocation tests
    print *, "Running core allocation tests..."
    call execute_command_line("./test_memory_allocation_core", &
        exitstat=status, wait=.true.)
    if (status /= 0) then
        total_failures = total_failures + 1
        write(error_unit, *) "Core allocation tests failed with status:", status
    end if
    
    ! Run error path tests
    print *, ""
    print *, "Running error path tests..."
    call execute_command_line("./test_memory_error_paths", &
        exitstat=status, wait=.true.)
    if (status /= 0) then
        total_failures = total_failures + 1
        write(error_unit, *) "Error path tests failed with status:", status
    end if
    
    ! Print overall summary
    print *, ""
    print *, "============================================================="
    if (total_failures == 0) then
        print *, "✅ ALL MEMORY ALLOCATION TESTS PASSED"
        print *, "Issue #243: Memory Safety VERIFIED"
        print *, "============================================================="
        call exit(0)
    else
        write(error_unit, '(A,I0,A)') "❌ ", total_failures, &
            " MEMORY TEST SUITES FAILED"
        print *, "============================================================="
        call exit(1)
    end if
    
end program test_memory_allocation_bug_issue_243