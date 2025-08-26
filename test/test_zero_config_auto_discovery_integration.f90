program test_zero_config_auto_discovery_integration
    !! Integration test orchestrator for zero-configuration mode
    !! Delegates to focused test modules for specific functionality
    
    use iso_fortran_env, only: error_unit
    implicit none
    
    integer :: status
    integer :: total_failures = 0
    
    print *, ""
    print *, "====================================================="
    print *, "ZERO-CONFIGURATION AUTO-DISCOVERY INTEGRATION TESTS"
    print *, "====================================================="
    print *, ""
    
    ! Run core functionality tests
    print *, "Running core tests..."
    call execute_command_line("./test_zero_config_core", &
        exitstat=status, wait=.true.)
    if (status /= 0) then
        total_failures = total_failures + 1
        write(error_unit, *) "Core tests failed with status:", status
    end if
    
    ! Run build system integration tests
    print *, ""
    print *, "Running build integration tests..."
    call execute_command_line("./test_zero_config_build_integration", &
        exitstat=status, wait=.true.)
    if (status /= 0) then
        total_failures = total_failures + 1
        write(error_unit, *) "Build integration tests failed with status:", status
    end if
    
    ! Run workflow tests
    print *, ""
    print *, "Running workflow tests..."
    call execute_command_line("./test_zero_config_workflow", &
        exitstat=status, wait=.true.)
    if (status /= 0) then
        total_failures = total_failures + 1
        write(error_unit, *) "Workflow tests failed with status:", status
    end if
    
    ! Print overall summary
    print *, ""
    print *, "====================================================="
    if (total_failures == 0) then
        print *, "✅ ALL INTEGRATION TEST SUITES PASSED"
        print *, "====================================================="
        call exit(0)
    else
        write(error_unit, '(A,I0,A)') "❌ ", total_failures, &
            " TEST SUITES FAILED"
        print *, "====================================================="
        call exit(1)
    end if

end program test_zero_config_auto_discovery_integration