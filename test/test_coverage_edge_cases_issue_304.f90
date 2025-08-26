program test_coverage_edge_cases_issue_304
    !! Coverage edge cases test orchestrator
    !! Delegates to focused test modules for edge case scenarios
    
    use iso_fortran_env, only: error_unit
    implicit none
    
    integer :: status
    integer :: total_failures = 0
    
    print *, ""
    print *, "============================================================="
    print *, "Issue #304: Coverage Edge Case Testing Suite"
    print *, "============================================================="
    print *, ""
    
    ! Run core edge case tests
    print *, "Running core edge case tests..."
    call execute_command_line("./test_coverage_edge_cases_core", &
        exitstat=status, wait=.true.)
    if (status /= 0) then
        total_failures = total_failures + 1
        write(error_unit, *) "Core edge case tests failed with status:", status
    end if
    
    ! Run precision tests
    print *, ""
    print *, "Running precision tests..."
    call execute_command_line("./test_coverage_precision", &
        exitstat=status, wait=.true.)
    if (status /= 0) then
        total_failures = total_failures + 1
        write(error_unit, *) "Precision tests failed with status:", status
    end if
    
    ! Run scalability tests
    print *, ""
    print *, "Running scalability tests..."
    call execute_command_line("./test_coverage_scalability", &
        exitstat=status, wait=.true.)
    if (status /= 0) then
        total_failures = total_failures + 1
        write(error_unit, *) "Scalability tests failed with status:", status
    end if
    
    ! Print overall summary
    print *, ""
    print *, "============================================================="
    if (total_failures == 0) then
        print *, "✅ ALL COVERAGE EDGE CASE TESTS PASSED"
        print *, "Issue #304 Edge Cases: COMPREHENSIVE COVERAGE ACHIEVED"
        print *, "============================================================="
        call exit(0)
    else
        write(error_unit, '(A,I0,A)') "❌ ", total_failures, &
            " EDGE CASE TEST SUITES FAILED"
        print *, "============================================================="
        call exit(1)
    end if
    
end program test_coverage_edge_cases_issue_304