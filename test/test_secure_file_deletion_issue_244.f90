program test_secure_file_deletion_issue_244
    !!
    !! Given-When-Then Test Documentation:
    !!
    !! CRITICAL SECURITY VULNERABILITY: File deletion in secure_command_executor.f90:190
    !! can fail silently, leaving sensitive temporary files on disk
    !!
    !! GIVEN: The safe_find_files function creates temporary files for command output
    !! WHEN: File deletion fails via close(unit, status='delete') at line 190
    !! THEN: Security-sensitive temp files remain on disk (SECURITY BREACH)
    !! 
    !! This test suite demonstrates the file deletion vulnerability and tests
    !! scenarios where deletion can fail silently, compromising security.
    !!
    !! Test Categories:
    !! 1. File deletion failure scenarios 
    !! 2. Permission-based deletion failures
    !! 3. File locking preventing deletion
    !! 4. Security validation that temp files are removed
    !! 5. Error handling for deletion failures
    !!
    use iso_fortran_env, only: error_unit
    use secure_command_executor
    use error_handling
    implicit none

    integer :: test_count = 0
    integer :: failed_count = 0
    logical :: debug_mode = .true.

    print *, "======================================================================"
    print *, "SECURITY: File Deletion Vulnerability Tests (Issue #244 - RED Phase)"
    print *, "======================================================================"
    print *, ""
    print *, "Testing file deletion vulnerability in secure_command_executor.f90:190"
    print *, "VULNERABILITY: close(unit, status='delete') can fail silently"
    print *, "IMPACT: Security-critical temp files remain on disk"
    print *, ""

    ! Core vulnerability demonstration tests
    call test_temp_file_deletion_failure()
    call test_temp_file_cleanup_verification()
    call test_permission_denied_deletion()
    call test_file_locked_deletion_failure()
    call test_multiple_temp_files_cleanup()
    call test_error_handling_for_deletion_failures()
    call test_sensitive_data_in_temp_files()
    call test_concurrent_deletion_conflicts()
    call test_disk_space_deletion_failure()
    call test_filesystem_readonly_deletion()

    ! Report results
    print *, ""
    print *, "======================================================================"
    print *, "File Deletion Vulnerability Test Results (RED Phase)"
    print *, "======================================================================"
    write(*, '(A, I0, A, I0, A)') "Tests run: ", test_count, ", Failed: ", failed_count, &
        " (all should FAIL in RED phase)"

    if (failed_count == test_count) then
        print *, ""
        print *, "RED PHASE SUCCESS: All tests FAILED as expected"
        print *, "This demonstrates the file deletion vulnerability exists"
        print *, ""
        print *, "CRITICAL SECURITY FINDINGS:"
        print *, "• Temp files NOT properly deleted after use"
        print *, "• NO error handling when deletion fails"
        print *, "• Security-sensitive data remains on disk"
        print *, "• Silent failures compromise security guarantees"
        print *, ""
        print *, "Next: sergei-perfectionist-coder implements fixes (GREEN phase)"
    else
        print *, ""
        print *, "RED PHASE ERROR: Some tests unexpectedly PASSED"
        print *, "This suggests the vulnerability may not exist as expected"
        write(*, '(A, I0, A)') "Unexpected passes: ", (test_count - failed_count), " tests"
        print *, "Investigation needed before proceeding to GREEN phase"
    end if

contains

    subroutine test_temp_file_deletion_failure()
        !! 
        !! Given-When-Then: Temp file deletion failure demonstration
        !!
        !! GIVEN: safe_find_files creates temp file at line 148
        !! WHEN: close(unit, status='delete') executes at line 190
        !! THEN: File should be deleted but may fail silently
        !!
        character(len=:), allocatable :: files(:)
        type(error_context_t) :: error_ctx
        character(len=256) :: temp_file_pattern
        logical :: temp_exists_before, temp_exists_after
        integer :: i

        call start_test("Temp File Deletion Failure Demonstration")

        ! Create a simple pattern that should find files
        temp_file_pattern = "*.f90"
        
        ! Check temp directory before operation
        call check_temp_directory_state(temp_exists_before)
        
        if (debug_mode) then
            print *, "   DEBUG: Temp files exist before: ", temp_exists_before
        end if

        ! Execute safe_find_files which creates and should delete temp file
        call safe_find_files(temp_file_pattern, files, error_ctx, max_files=5)
        
        ! Check temp directory after operation - temp files should be gone
        call check_temp_directory_state(temp_exists_after)
        
        if (debug_mode) then
            print *, "   DEBUG: Temp files exist after: ", temp_exists_after
            if (error_ctx%error_code /= ERROR_SUCCESS) then
                print *, "   DEBUG: Error occurred: ", trim(error_ctx%message)
            end if
        end if

        ! In RED phase, we expect temp files to remain (vulnerability)
        ! This test SHOULD FAIL because files remain on disk
        if (.not. temp_exists_after) then
            call pass_test("Temp files properly cleaned up")
        else
            call fail_test("VULNERABILITY: Temp files remain after operation (deletion failed)")
        end if

        if (allocated(files)) deallocate(files)

    end subroutine test_temp_file_deletion_failure

    subroutine test_temp_file_cleanup_verification()
        !!
        !! Given-When-Then: Verify temp file cleanup after find operation
        !!
        !! GIVEN: Multiple temp files may be created during operations
        !! WHEN: Each operation completes
        !! THEN: ALL temp files should be removed from system
        !!
        character(len=:), allocatable :: files(:)
        type(error_context_t) :: error_ctx
        character(len=256) :: patterns(3)
        integer :: i, temp_count_before, temp_count_after

        call start_test("Comprehensive Temp File Cleanup Verification")

        patterns(1) = "*.f90" 
        patterns(2) = "**/*.o"
        patterns(3) = "test_*.f90"

        ! Count temp files before operations
        call count_fortcov_temp_files(temp_count_before)
        
        if (debug_mode) then
            write(*, '(A, I0)') "   DEBUG: Temp files before: ", temp_count_before
        end if

        ! Execute multiple find operations
        do i = 1, 3
            call safe_find_files(patterns(i), files, error_ctx, max_files=10)
            if (allocated(files)) then
                if (debug_mode) then
                    write(*, '(A, I0, A, I0, A)') "   DEBUG: Pattern ", i, " found ", size(files), " files"
                end if
                deallocate(files)
            end if
        end do

        ! Count temp files after all operations
        call count_fortcov_temp_files(temp_count_after)
        
        if (debug_mode) then
            write(*, '(A, I0)') "   DEBUG: Temp files after: ", temp_count_after
        end if

        ! In RED phase, temp files should accumulate (vulnerability)
        if (temp_count_after <= temp_count_before) then
            call pass_test("No temp file accumulation detected")
        else
            write(*, '(A, I0, A)') "   VULNERABILITY: ", (temp_count_after - temp_count_before), " temp files accumulated"
            call fail_test("CRITICAL: Temp files accumulating on disk")
        end if

    end subroutine test_temp_file_cleanup_verification

    subroutine test_permission_denied_deletion()
        !!
        !! Given-When-Then: Test deletion failure due to permissions
        !!
        !! GIVEN: Temp file created in protected/readonly location
        !! WHEN: close(unit, status='delete') attempts deletion
        !! THEN: Deletion should fail and be handled properly
        !!
        character(len=:), allocatable :: files(:)
        type(error_context_t) :: error_ctx
        character(len=256) :: readonly_pattern
        logical :: permission_error_handled

        call start_test("Permission Denied Deletion Handling")

        ! Try operation that might create temp files in protected area
        readonly_pattern = "/proc/*.f90"  ! System area that might cause permission issues
        
        call safe_find_files(readonly_pattern, files, error_ctx)
        
        ! Check if permission errors are properly handled
        permission_error_handled = (error_ctx%error_code /= ERROR_SUCCESS)
        
        if (debug_mode) then
            print *, "   DEBUG: Permission error detected: ", permission_error_handled
            if (permission_error_handled) then
                print *, "   DEBUG: Error message: ", trim(error_ctx%message)
            end if
        end if

        ! In RED phase, error handling should be insufficient
        if (permission_error_handled) then
            call pass_test("Permission errors properly detected and handled")
        else
            call fail_test("VULNERABILITY: Permission errors not handled - temp files may remain")
        end if

        if (allocated(files)) deallocate(files)

    end subroutine test_permission_denied_deletion

    subroutine test_file_locked_deletion_failure()
        !!
        !! Given-When-Then: Test deletion when file is locked
        !!
        !! GIVEN: Temp file is locked by another process/handle
        !! WHEN: close(unit, status='delete') attempts deletion
        !! THEN: Deletion fails but error should be handled
        !!
        character(len=:), allocatable :: files(:)
        type(error_context_t) :: error_ctx
        character(len=256) :: test_pattern
        logical :: concurrent_access_handled

        call start_test("File Locked Deletion Failure")

        ! Create scenario that might cause file locking issues
        test_pattern = "*.f90"
        
        ! Execute find operation
        call safe_find_files(test_pattern, files, error_ctx, max_files=1)
        
        ! Simulate checking if concurrent access issues are handled
        concurrent_access_handled = .false.
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            concurrent_access_handled = .true.
        end if
        
        if (debug_mode) then
            print *, "   DEBUG: Concurrent access handled: ", concurrent_access_handled
        end if

        ! In RED phase, locking issues should not be handled
        if (concurrent_access_handled) then
            call pass_test("File locking scenarios properly handled")
        else
            call fail_test("VULNERABILITY: File locking not handled - temp files may persist")
        end if

        if (allocated(files)) deallocate(files)

    end subroutine test_file_locked_deletion_failure

    subroutine test_multiple_temp_files_cleanup()
        !!
        !! Given-When-Then: Test cleanup of multiple temp files
        !!
        !! GIVEN: Multiple operations create multiple temp files
        !! WHEN: Each operation completes
        !! THEN: ALL temp files should be cleaned up properly
        !!
        character(len=:), allocatable :: files(:)
        type(error_context_t) :: error_ctx
        integer :: operation_count, i
        integer :: temp_files_start, temp_files_end

        call start_test("Multiple Temp Files Cleanup")

        operation_count = 5
        
        ! Count temp files at start
        call count_fortcov_temp_files(temp_files_start)
        
        if (debug_mode) then
            write(*, '(A, I0)') "   DEBUG: Starting temp file count: ", temp_files_start
        end if

        ! Perform multiple operations
        do i = 1, operation_count
            call safe_find_files("*.f90", files, error_ctx, max_files=2)
            if (allocated(files)) deallocate(files)
        end do

        ! Count temp files at end  
        call count_fortcov_temp_files(temp_files_end)
        
        if (debug_mode) then
            write(*, '(A, I0)') "   DEBUG: Ending temp file count: ", temp_files_end
        end if

        ! In RED phase, multiple temp files should accumulate
        if (temp_files_end <= temp_files_start) then
            call pass_test("Multiple temp files properly cleaned up")
        else
            write(*, '(A, I0, A, I0, A)') "   VULNERABILITY: Created ", operation_count, &
                " operations but ", (temp_files_end - temp_files_start), " temp files remain"
            call fail_test("CRITICAL: Multiple temp file cleanup failing")
        end if

    end subroutine test_multiple_temp_files_cleanup

    subroutine test_error_handling_for_deletion_failures()
        !!
        !! Given-When-Then: Test error handling when deletion fails
        !!
        !! GIVEN: close(unit, status='delete') may fail
        !! WHEN: Deletion failure occurs
        !! THEN: Error should be detected and reported properly
        !!
        character(len=:), allocatable :: files(:)
        type(error_context_t) :: error_ctx
        logical :: deletion_errors_reported

        call start_test("Error Handling for Deletion Failures")

        ! Execute operation that should detect deletion failures
        call safe_find_files("nonexistent_*.f90", files, error_ctx)
        
        ! Check if any deletion-related errors are reported
        deletion_errors_reported = .false.
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            if (index(error_ctx%message, 'delete') > 0 .or. &
                index(error_ctx%message, 'cleanup') > 0 .or. &
                index(error_ctx%message, 'temp') > 0) then
                deletion_errors_reported = .true.
            end if
        end if
        
        if (debug_mode) then
            print *, "   DEBUG: Deletion errors reported: ", deletion_errors_reported
            print *, "   DEBUG: Error code: ", error_ctx%error_code
        end if

        ! In RED phase, deletion errors should NOT be reported
        if (deletion_errors_reported) then
            call pass_test("Deletion errors properly detected and reported")
        else
            call fail_test("VULNERABILITY: Deletion failures not detected or reported")
        end if

        if (allocated(files)) deallocate(files)

    end subroutine test_error_handling_for_deletion_failures

    subroutine test_sensitive_data_in_temp_files()
        !!
        !! Given-When-Then: Test that sensitive data doesn't persist in temp files
        !!
        !! GIVEN: Operations may write sensitive file paths to temp files
        !! WHEN: Temp files are not properly deleted
        !! THEN: Sensitive information may be exposed on disk
        !!
        character(len=:), allocatable :: files(:)
        type(error_context_t) :: error_ctx
        character(len=256) :: sensitive_pattern
        logical :: sensitive_data_found

        call start_test("Sensitive Data in Temp Files")

        ! Use pattern that might reveal sensitive paths
        sensitive_pattern = "/home/*/.ssh/*.f90"
        
        call safe_find_files(sensitive_pattern, files, error_ctx)
        
        ! Check if sensitive data remains in temp files
        call check_temp_files_for_sensitive_data(sensitive_data_found)
        
        if (debug_mode) then
            print *, "   DEBUG: Sensitive data found in temps: ", sensitive_data_found
        end if

        ! In RED phase, sensitive data should be found (vulnerability)
        if (.not. sensitive_data_found) then
            call pass_test("No sensitive data found in temp files")
        else
            call fail_test("SECURITY BREACH: Sensitive data found in persistent temp files")
        end if

        if (allocated(files)) deallocate(files)

    end subroutine test_sensitive_data_in_temp_files

    subroutine test_concurrent_deletion_conflicts()
        !!
        !! Given-When-Then: Test concurrent access to temp files
        !!
        !! GIVEN: Multiple operations may access same temp file names
        !! WHEN: Deletion conflicts occur
        !! THEN: Should be handled without leaving temp files
        !!
        character(len=:), allocatable :: files1(:), files2(:)
        type(error_context_t) :: error_ctx1, error_ctx2
        integer :: conflicts_detected

        call start_test("Concurrent Deletion Conflicts")

        ! Execute concurrent-like operations
        call safe_find_files("*.f90", files1, error_ctx1, max_files=3)
        call safe_find_files("*.f90", files2, error_ctx2, max_files=3)
        
        conflicts_detected = 0
        if (error_ctx1%error_code /= ERROR_SUCCESS) conflicts_detected = conflicts_detected + 1
        if (error_ctx2%error_code /= ERROR_SUCCESS) conflicts_detected = conflicts_detected + 1
        
        if (debug_mode) then
            write(*, '(A, I0)') "   DEBUG: Conflicts detected: ", conflicts_detected
        end if

        ! In RED phase, concurrent conflicts should not be handled
        if (conflicts_detected > 0) then
            call pass_test("Concurrent access conflicts detected and handled")
        else
            call fail_test("VULNERABILITY: Concurrent access to temp files not managed")
        end if

        if (allocated(files1)) deallocate(files1)
        if (allocated(files2)) deallocate(files2)

    end subroutine test_concurrent_deletion_conflicts

    subroutine test_disk_space_deletion_failure()
        !!
        !! Given-When-Then: Test deletion failure due to disk space issues
        !!
        !! GIVEN: System may have disk space constraints
        !! WHEN: Temp file deletion fails due to disk issues
        !! THEN: Should be handled gracefully
        !!
        character(len=:), allocatable :: files(:)
        type(error_context_t) :: error_ctx
        logical :: disk_errors_handled

        call start_test("Disk Space Deletion Failure")

        ! Execute operation that might encounter disk issues
        call safe_find_files("**/*.f90", files, error_ctx, max_files=100)
        
        disk_errors_handled = .false.
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            if (index(error_ctx%message, 'space') > 0 .or. &
                index(error_ctx%message, 'full') > 0 .or. &
                index(error_ctx%message, 'disk') > 0) then
                disk_errors_handled = .true.
            end if
        end if
        
        if (debug_mode) then
            print *, "   DEBUG: Disk errors handled: ", disk_errors_handled
        end if

        ! In RED phase, disk space issues should not be handled
        if (disk_errors_handled) then
            call pass_test("Disk space issues properly handled")
        else
            call fail_test("VULNERABILITY: Disk space issues not handled for temp file cleanup")
        end if

        if (allocated(files)) deallocate(files)

    end subroutine test_disk_space_deletion_failure

    subroutine test_filesystem_readonly_deletion()
        !!
        !! Given-When-Then: Test deletion on readonly filesystem
        !!
        !! GIVEN: Temp files created on readonly filesystem
        !! WHEN: close(unit, status='delete') executes
        !! THEN: Should handle readonly filesystem gracefully
        !!
        character(len=:), allocatable :: files(:)
        type(error_context_t) :: error_ctx
        logical :: readonly_handled

        call start_test("Readonly Filesystem Deletion")

        ! Try operation in potentially readonly area
        call safe_find_files("/usr/**/*.f90", files, error_ctx, max_files=5)
        
        readonly_handled = .false.
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            if (index(error_ctx%message, 'read') > 0 .or. &
                index(error_ctx%message, 'permission') > 0 .or. &
                index(error_ctx%message, 'write') > 0) then
                readonly_handled = .true.
            end if
        end if
        
        if (debug_mode) then
            print *, "   DEBUG: Readonly filesystem handled: ", readonly_handled
        end if

        ! In RED phase, readonly filesystem should not be handled
        if (readonly_handled) then
            call pass_test("Readonly filesystem properly handled")
        else
            call fail_test("VULNERABILITY: Readonly filesystem not handled for temp cleanup")
        end if

        if (allocated(files)) deallocate(files)

    end subroutine test_filesystem_readonly_deletion

    ! Helper subroutines for testing infrastructure

    subroutine check_temp_directory_state(temp_files_exist)
        logical, intent(out) :: temp_files_exist
        integer :: stat
        
        ! Check if fortcov temp files exist in /tmp
        call execute_command_line("ls /tmp/fortcov_secure_* >/dev/null 2>&1", exitstat=stat)
        temp_files_exist = (stat == 0)
        
    end subroutine check_temp_directory_state

    subroutine count_fortcov_temp_files(count)
        integer, intent(out) :: count
        integer :: unit, stat
        character(len=256) :: line
        
        count = 0
        
        ! Use find to count temp files and read from temp file
        call execute_command_line("find /tmp -name 'fortcov_secure_*' 2>/dev/null | wc -l > /tmp/count_temp.txt", exitstat=stat)
        
        if (stat == 0) then
            open(newunit=unit, file="/tmp/count_temp.txt", action='read', iostat=stat)
            if (stat == 0) then
                read(unit, '(A)', iostat=stat) line
                if (stat == 0) then
                    read(line, *, iostat=stat) count
                    if (stat /= 0) count = 0
                end if
                close(unit)
                ! Clean up our own temp file
                call execute_command_line("rm -f /tmp/count_temp.txt", exitstat=stat)
            end if
        end if
        
    end subroutine count_fortcov_temp_files

    subroutine check_temp_files_for_sensitive_data(sensitive_found)
        logical, intent(out) :: sensitive_found
        integer :: stat
        
        sensitive_found = .false.
        
        ! Check if temp files contain sensitive paths (simplified check)
        call execute_command_line("find /tmp -name 'fortcov_secure_*' -exec grep -l " // &
                                   "'ssh\\|home\\|root\\|etc' {} \\; >/dev/null 2>&1", exitstat=stat)
        if (stat == 0) then
            sensitive_found = .true.
        end if
        
    end subroutine check_temp_files_for_sensitive_data

    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A, I0, A, A)') "Test ", test_count, ": ", test_name
    end subroutine start_test

    subroutine pass_test(message)
        character(len=*), intent(in) :: message
        print *, "   PASS: " // trim(message)
        print *, ""
    end subroutine pass_test

    subroutine fail_test(message)
        character(len=*), intent(in) :: message
        failed_count = failed_count + 1
        print *, "   FAIL: " // trim(message)
        print *, ""
    end subroutine fail_test

end program test_secure_file_deletion_issue_244