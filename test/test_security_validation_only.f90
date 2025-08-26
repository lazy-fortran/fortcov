program test_security_validation_only
    !! Security validation test orchestrator
    !! Delegates to focused security test modules
    
    use iso_fortran_env, only: error_unit
    implicit none
    
    integer :: status
    integer :: total_failures = 0
    
    print *, ""
    print *, "======================================================================"
    print *, "SECURITY: Path Validation Test Suite (Issue #235)"
    print *, "======================================================================"
    print *, ""
    
    ! Run core security tests
    print *, "Running core security validation tests..."
    call execute_command_line("./test_security_validation_core", &
        exitstat=status, wait=.true.)
    if (status /= 0) then
        total_failures = total_failures + 1
        write(error_unit, *) "Core security tests failed with status:", status
    end if
    
    ! Run attack vector tests
    print *, ""
    print *, "Running attack vector tests..."
    call execute_command_line("./test_security_attack_vectors", &
        exitstat=status, wait=.true.)
    if (status /= 0) then
        total_failures = total_failures + 1
        write(error_unit, *) "Attack vector tests failed with status:", status
    end if
    
    ! Print overall summary
    print *, ""
    print *, "======================================================================"
    if (total_failures == 0) then
        print *, "✅ ALL SECURITY VALIDATION TESTS PASSED"
        print *, "======================================================================"
        call exit(0)
    else
        write(error_unit, '(A,I0,A)') "❌ ", total_failures, &
            " SECURITY TEST SUITES FAILED"
        print *, "======================================================================"
        call exit(1)
    end if
    
end program test_security_validation_only