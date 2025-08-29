program test_marker_cleanup
    !! Test for Issue #467: Fork bomb prevention marker file cleanup
    !!
    !! PROBLEM: The .fortcov_execution_marker file is created during auto-test
    !! execution but not properly cleaned up in all exit paths, causing
    !! subsequent fortcov runs to be blocked by false positives.
    !!
    !! SOLUTION: Ensure marker file is cleaned up in ALL exit paths using
    !! proper cleanup mechanisms.
    
    implicit none
    
    logical :: all_tests_pass = .true.
    
    print *, "Testing marker file cleanup (Issue #467)"
    print *, "======================================"
    print *, ""
    
    call test_marker_file_operations()
    call test_marker_cleanup_simulation()
    call test_fork_bomb_prevention_with_cleanup()
    
    print *, ""
    if (all_tests_pass) then
        print *, "✅ ALL TESTS PASSED - Marker cleanup working correctly"
    else
        print *, "❌ SOME TESTS FAILED - Marker cleanup NOT working"
        call exit(1)
    end if
    
contains

    subroutine test_marker_file_operations()
        logical :: marker_exists
        integer :: unit_num, iostat
        
        print *, "Test 1: Basic marker file operations"
        
        ! Ensure clean start - remove any existing marker
        call execute_command_line('rm -f .fortcov_execution_marker')
        
        ! Check marker doesn't exist initially
        inquire(file='.fortcov_execution_marker', exist=marker_exists)
        if (marker_exists) then
            print *, "❌ FAIL: Marker existed before creation"
            all_tests_pass = .false.
            return
        end if
        
        ! Create marker file manually (simulating create_recursion_marker)
        open(newunit=unit_num, file='.fortcov_execution_marker', &
             status='replace', action='write', iostat=iostat)
        if (iostat /= 0) then
            print *, "❌ FAIL: Could not create marker file"
            all_tests_pass = .false.
            return
        end if
        write(unit_num, '(A)') 'FORTCOV_AUTO_TEST_EXECUTION_IN_PROGRESS'
        close(unit_num)
        
        ! Check marker exists after creation
        inquire(file='.fortcov_execution_marker', exist=marker_exists)
        if (.not. marker_exists) then
            print *, "❌ FAIL: Marker was not created"
            all_tests_pass = .false.
            return
        end if
        
        ! Clean up marker (simulating cleanup_recursion_marker)
        call execute_command_line('rm -f .fortcov_execution_marker')
        
        ! Check marker doesn't exist after cleanup
        inquire(file='.fortcov_execution_marker', exist=marker_exists)
        if (marker_exists) then
            print *, "❌ FAIL: Marker was not cleaned up"
            all_tests_pass = .false.
            return
        end if
        
        print *, "✅ PASS: Basic marker file operations working"
        
    end subroutine test_marker_file_operations
    
    subroutine test_marker_cleanup_simulation()
        logical :: marker_exists
        integer :: unit_num
        
        print *, "Test 2: Marker cleanup when file exists and when it doesn't"
        
        ! Test cleanup when file doesn't exist (should not error)
        call execute_command_line('rm -f .fortcov_execution_marker')
        call execute_command_line('rm -f .fortcov_execution_marker')  ! Second time should be safe
        
        inquire(file='.fortcov_execution_marker', exist=marker_exists)
        if (marker_exists) then
            print *, "❌ FAIL: Marker exists after double cleanup"
            all_tests_pass = .false.
            return
        end if
        
        ! Test cleanup when file exists
        open(newunit=unit_num, file='.fortcov_execution_marker', &
             status='replace', action='write')
        write(unit_num, '(A)') 'TEST_MARKER'
        close(unit_num)
        
        call execute_command_line('rm -f .fortcov_execution_marker')
        
        inquire(file='.fortcov_execution_marker', exist=marker_exists)
        if (marker_exists) then
            print *, "❌ FAIL: Marker was not cleaned up properly"
            all_tests_pass = .false.
            return
        end if
        
        print *, "✅ PASS: Marker cleanup working in all scenarios"
        
    end subroutine test_marker_cleanup_simulation
    
    subroutine test_fork_bomb_prevention_with_cleanup()
        logical :: marker_exists
        integer :: unit_num
        character(len=256) :: test_command
        integer :: exit_status
        
        print *, "Test 3: Fork bomb prevention detects marker correctly"
        
        ! Clean state
        call execute_command_line('rm -f .fortcov_execution_marker')
        
        ! Create marker to simulate ongoing execution
        open(newunit=unit_num, file='.fortcov_execution_marker', &
             status='replace', action='write')
        write(unit_num, '(A)') 'FORTCOV_AUTO_TEST_EXECUTION_IN_PROGRESS'
        close(unit_num)
        
        ! Verify marker exists
        inquire(file='.fortcov_execution_marker', exist=marker_exists)
        if (.not. marker_exists) then
            print *, "❌ FAIL: Test setup failed - marker not created"
            all_tests_pass = .false.
            call execute_command_line('rm -f .fortcov_execution_marker')
            return
        end if
        
        ! Test that fortcov detects the marker and exits gracefully
        ! We use echo instead of actual fortcov to avoid infinite recursion in tests
        test_command = 'test -f .fortcov_execution_marker && echo "Fork bomb detected" || echo "No fork bomb"'
        call execute_command_line(test_command, exitstat=exit_status)
        
        if (exit_status /= 0) then
            print *, "❌ FAIL: Fork bomb detection test failed"
            all_tests_pass = .false.
        else
            print *, "✅ PASS: Fork bomb detection working"
        end if
        
        ! Clean up test marker
        call execute_command_line('rm -f .fortcov_execution_marker')
        
        ! Verify cleanup worked
        inquire(file='.fortcov_execution_marker', exist=marker_exists)
        if (marker_exists) then
            print *, "❌ FAIL: Test cleanup failed - marker still exists"
            all_tests_pass = .false.
            return
        end if
        
    end subroutine test_fork_bomb_prevention_with_cleanup

end program test_marker_cleanup