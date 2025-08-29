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
    !! DECOMPOSED: Security test logic extracted to focused modules for 
    !! architecture size compliance (<500 lines per file)
    !!
    use test_framework_utilities
    use test_environment_utilities
    use test_file_deletion_vulnerabilities
    use test_sensitive_data_handling
    implicit none

    type(test_counter_t) :: test_counter
    logical :: debug_mode = .true.

    print *, "======================================================================"
    print *, "SECURITY: File Deletion Vulnerability Tests (Issue #244 - RED Phase)"
    print *, "======================================================================"
    print *, ""
    print *, "Testing file deletion vulnerability in secure_command_executor.f90:190"
    print *, "VULNERABILITY: close(unit, status='delete') can fail silently"
    print *, "IMPACT: Security-critical temp files remain on disk"
    print *, ""

    ! Initialize test framework
    call init_test_counter(test_counter)

    ! Setup security test environment
    call setup_security_test_environment()

    ! Core vulnerability demonstration tests
    call test_temp_file_deletion_failure(test_counter)
    call test_temp_file_cleanup_verification(test_counter)
    call test_permission_denied_deletion(test_counter)
    call test_file_locked_deletion_failure(test_counter)
    call test_multiple_temp_files_cleanup(test_counter)
    call test_error_handling_for_deletion_failures(test_counter)
    call test_sensitive_data_in_temp_files(test_counter)
    call test_concurrent_deletion_conflicts(test_counter)

    ! Additional security edge cases
    call test_disk_space_deletion_failure(test_counter)
    call test_filesystem_readonly_failure(test_counter)

    ! Cleanup security test environment
    call cleanup_security_test_environment()

    ! Report results
    call print_test_summary(test_counter, "Secure File Deletion Issue #244")

contains

    subroutine setup_security_test_environment()
        !! Setup security test environment
        call setup_basic_test_environment("secure file deletion")
        
        ! Create secure test directory using Fortran I/O (CI-friendly)
        ! Note: directory creation using makedirs intrinsic when available
        ! For CI compatibility, we skip directory creation as it's not essential for the test
    end subroutine setup_security_test_environment
    
    subroutine cleanup_security_test_environment()
        !! Cleanup security test environment
        call cleanup_basic_test_environment("secure file deletion")
        
        ! Clean up any remaining security test files using Fortran I/O (CI-friendly)
        ! Note: file cleanup handled by test framework cleanup routines
        ! Shell commands replaced with CI-compatible approach
    end subroutine cleanup_security_test_environment

end program test_secure_file_deletion_issue_244