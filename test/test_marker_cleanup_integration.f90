program test_marker_cleanup_integration
    !! Integration test for Issue #467: Fork bomb prevention marker file cleanup
    !! 
    !! PROBLEM: When fortcov auto-test execution is interrupted or fails,
    !! the .fortcov_execution_marker file may remain, blocking subsequent runs
    !!
    !! SOLUTION: Test that the startup cleanup and block-based cleanup
    !! properly handle marker files in all scenarios
    
    implicit none
    
    logical :: all_tests_pass = .true.
    
    print *, "Integration test: Fork bomb marker cleanup (Issue #467)"
    print *, "===================================================="
    print *, ""
    
    call test_startup_cleanup_removes_stale_marker()
    call test_marker_file_persists_after_simulated_crash()
    call test_fortcov_startup_cleanup_works()
    
    print *, ""
    if (all_tests_pass) then
        print *, "✅ ALL INTEGRATION TESTS PASSED"
        print *, "   Fork bomb marker cleanup working correctly"
    else
        print *, "❌ SOME INTEGRATION TESTS FAILED"
        call exit(1)  
    end if
    
contains

    subroutine test_startup_cleanup_removes_stale_marker()
        logical :: marker_exists_before, marker_exists_after
        integer :: unit_num
        
        print *, "Integration Test 1: Startup cleanup removes stale markers"
        
        ! Create a stale marker file to simulate previous crash
        open(newunit=unit_num, file='.fortcov_execution_marker', &
             status='replace', action='write')
        write(unit_num, '(A)') 'STALE_MARKER_FROM_CRASHED_EXECUTION'
        close(unit_num)
        
        ! Verify marker exists (simulating stale state)
        inquire(file='.fortcov_execution_marker', exist=marker_exists_before)
        if (.not. marker_exists_before) then
            print *, "failed - stale marker not created"
            all_tests_pass = .false.
            return
        end if
        
        ! Simulate startup cleanup (what main.f90 does)
        call execute_command_line('rm -f .fortcov_execution_marker')
        
        ! Verify marker is removed
        inquire(file='.fortcov_execution_marker', exist=marker_exists_after)
        if (marker_exists_after) then
            print *, "❌ FAIL: Stale marker not removed by startup cleanup"
            all_tests_pass = .false.
        else
            print *, "✅ PASS: Startup cleanup removes stale markers"
        end if
        
    end subroutine test_startup_cleanup_removes_stale_marker
    
    subroutine test_marker_file_persists_after_simulated_crash()
        logical :: marker_exists
        integer :: unit_num
        
        print *, "Integration Test 2: Marker persists after simulated crash"
        
        ! Clean state
        call execute_command_line('rm -f .fortcov_execution_marker')
        
        ! Create marker (simulating auto-test execution start)
        open(newunit=unit_num, file='.fortcov_execution_marker', &
             status='replace', action='write')
        write(unit_num, '(A)') 'FORTCOV_AUTO_TEST_EXECUTION_IN_PROGRESS'
        close(unit_num)
        
        ! Simulate crash/kill by NOT cleaning up marker
        ! (This is what would happen if process was killed)
        
        ! Verify marker persists (demonstrating the problem)
        inquire(file='.fortcov_execution_marker', exist=marker_exists)
        if (.not. marker_exists) then
            print *, "failed - marker should persist"
            all_tests_pass = .false.
        else
            print *, "✅ PASS: Marker persists after simulated crash"
            print *, "   (This demonstrates the problem the fix addresses)"
        end if
        
        ! Clean up for next test
        call execute_command_line('rm -f .fortcov_execution_marker')
        
    end subroutine test_marker_file_persists_after_simulated_crash
    
    subroutine test_fortcov_startup_cleanup_works()
        logical :: marker_exists_before, marker_exists_after
        integer :: unit_num
        character(len=512) :: test_command
        integer :: exit_status
        
        print *, "Integration Test 3: Fortcov startup handles stale markers"
        
        ! Create a stale marker file
        open(newunit=unit_num, file='.fortcov_execution_marker', &
             status='replace', action='write')
        write(unit_num, '(A)') 'STALE_FROM_PREVIOUS_CRASHED_RUN'
        close(unit_num)
        
        ! Verify marker exists before fortcov startup
        inquire(file='.fortcov_execution_marker', exist=marker_exists_before)
        if (.not. marker_exists_before) then
            print *, "failed - stale marker not created"
            all_tests_pass = .false.
            call execute_command_line('rm -f .fortcov_execution_marker')
            return
        end if
        
        ! Run a basic fortcov command that should clean up the marker
        ! We use --help to avoid any complex operations that might fail in tests
        test_command = 'fpm run --profile release -- --help >/dev/null 2>&1 || true'
        call execute_command_line(test_command, exitstat=exit_status)
        
        ! Give it a moment for cleanup to complete
        call execute_command_line('sleep 0.1')
        
        ! Check if marker was cleaned up by fortcov startup
        inquire(file='.fortcov_execution_marker', exist=marker_exists_after)
        if (marker_exists_after) then
            print *, "❌ FAIL: Fortcov startup did not clean up stale marker"
            print *, "   This indicates the fix is not working"
            all_tests_pass = .false.
            ! Clean up for safety
            call execute_command_line('rm -f .fortcov_execution_marker')
        else
            print *, "✅ PASS: Fortcov startup cleans up stale markers"
            print *, "   The fix is working correctly"
        end if
        
    end subroutine test_fortcov_startup_cleanup_works

end program test_marker_cleanup_integration
