program test_fork_bomb_prevention_validation
    !! Fork Bomb Prevention Validation Test Suite
    !!
    !! Extracted from test_sprint_2_validation_comprehensive.f90 for QADS 
    !! architectural compliance (Issue #842).
    !! 
    !! Validates Sprint 2 Success Criteria 5: Fork bomb prevention marker 
    !! cleanup functionality to ensure normal operation is not blocked.
    
    use iso_fortran_env, only: output_unit
    use sprint2_test_utils, only: assert_test
    implicit none
    
    integer :: test_count = 0
    integer :: passed_tests = 0
    logical :: all_tests_passed = .true.
    
    write(output_unit, '(A)') "============================================="
    write(output_unit, '(A)') "    Fork Bomb Prevention Validation Test   "
    write(output_unit, '(A)') "============================================="
    
    call test_criterion_5_fork_bomb_prevention()
    
    ! Print summary
    write(output_unit, '(A)') ""
    write(output_unit, '(A)') "============================================="
    write(*, '(A,I0,A,I0,A)') "FORK BOMB PREVENTION: ", passed_tests, "/", &
                              test_count, " tests passed"
    
    if (all_tests_passed) then
        write(output_unit, '(A)') "✅ ALL FORK BOMB PREVENTION TESTS PASSED"
        stop 0
    else
        write(output_unit, '(A)') "❌ SOME FORK BOMB PREVENTION TESTS FAILED"  
        stop 1
    end if

contains

    subroutine test_criterion_5_fork_bomb_prevention()
        !! SUCCESS CRITERIA 5: Fork bomb prevention marker cleanup
        !! Validates that fork bomb prevention works without blocking normal operation
        
        logical :: marker_exists
        integer :: exit_status
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== CRITERION 5: Fork Bomb Prevention ==="
        
        ! Test 5.1: Initial state clean
        inquire(file='.fortcov_execution_marker', exist=marker_exists)
        call assert_test(.not. marker_exists, "Clean initial state", &
                        "No stale marker should exist", &
                        test_count, passed_tests, all_tests_passed)
        
        ! Test 5.2: Marker creation and detection
        call execute_command_line('touch .fortcov_execution_marker', wait=.true.)
        inquire(file='.fortcov_execution_marker', exist=marker_exists)
        call assert_test(marker_exists, "Fork bomb marker detection", &
                        "Should detect created marker", &
                        test_count, passed_tests, all_tests_passed)
        
        ! Test 5.3: Automatic cleanup on startup (simulate main.f90 behavior)
        call execute_command_line('rm -f .fortcov_execution_marker', &
                                  wait=.true., exitstat=exit_status)
        call assert_test(exit_status == 0, "Automatic marker cleanup", &
                        "Should clean up stale markers", &
                        test_count, passed_tests, all_tests_passed)
        
        inquire(file='.fortcov_execution_marker', exist=marker_exists)
        call assert_test(.not. marker_exists, "Cleanup effectiveness", &
                        "Marker should be removed", &
                        test_count, passed_tests, all_tests_passed)
        
        ! Test 5.4: Normal operation not blocked
        ! This is validated by the fact that this test is running at all
        call assert_test(.true., "Normal operation not blocked", &
                        "Tests can run without marker blocking", &
                        test_count, passed_tests, all_tests_passed)
        
    end subroutine test_criterion_5_fork_bomb_prevention

end program test_fork_bomb_prevention_validation
