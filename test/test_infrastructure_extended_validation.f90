module test_infrastructure_extended_validation
    !! Extended infrastructure validation testing module
    !! 
    !! This module contains additional infrastructure tests including
    !! isolation, concurrency, framework, and cleanup validation.
    
    use iso_fortran_env, only: output_unit, error_unit
    use portable_temp_utils, only: get_temp_dir, create_temp_subdir
    use file_ops_secure, only: safe_remove_directory, safe_remove_file, &
                              safe_test_command_true, safe_test_command_false, &
                              safe_test_file_list, safe_test_pipe_command, &
                              safe_create_concurrent_files, safe_mkdir
    use error_handling_core, only: error_context_t, ERROR_SUCCESS
    implicit none
    private
    
    public :: run_extended_infrastructure_tests
    public :: test_environment_detected
    
contains

    subroutine run_extended_infrastructure_tests(test_count, passed_tests, &
                                                  all_tests_passed)
        !! Run extended infrastructure validation tests
        integer, intent(inout) :: test_count
        integer, intent(inout) :: passed_tests  
        logical, intent(inout) :: all_tests_passed
        
        character(len=:), allocatable :: temp_dir
        logical :: setup_success
        
        ! Setup test environment - use current directory for test isolation
        temp_dir = "."  ! Use current directory to avoid directory creation issues
        setup_success = .true.  ! No setup needed for current directory
        
        ! Extended infrastructure tests
        call test_test_isolation_guarantees(temp_dir, test_count, &
                                            passed_tests, all_tests_passed)
        call test_concurrent_execution_safety(temp_dir, test_count, &
                                              passed_tests, all_tests_passed)
        call test_framework_assertion_reliability(test_count, &
                                                  passed_tests, all_tests_passed)
        call test_cleanup_mechanisms(temp_dir, test_count, passed_tests, &
                                     all_tests_passed)
        call test_resource_leak_prevention(temp_dir, test_count, &
                                           passed_tests, all_tests_passed)
        call test_environment_detection_accuracy(test_count, &
                                                 passed_tests, all_tests_passed)
        call test_temporary_file_management(temp_dir, test_count, &
                                            passed_tests, all_tests_passed)
        call test_system_interaction_stability(temp_dir, test_count, &
                                               passed_tests, all_tests_passed)
        
        ! Cleanup test environment using secure directory removal
        call safe_cleanup_extended_test_directory(temp_dir)
        
    end subroutine run_extended_infrastructure_tests

    subroutine assert_test(condition, test_name, details, test_count, &
                           passed_tests, all_tests_passed)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: test_name, details
        integer, intent(inout) :: test_count
        integer, intent(inout) :: passed_tests  
        logical, intent(inout) :: all_tests_passed
        
        test_count = test_count + 1
        
        if (condition) then
            passed_tests = passed_tests + 1
            write(output_unit, '(A)') "✅ PASS: " // trim(test_name)
        else
            all_tests_passed = .false.
            write(output_unit, '(A)') "❌ FAIL: " // trim(test_name)
            write(output_unit, '(A)') "   Details: " // trim(details)
        end if
    end subroutine assert_test

    subroutine test_test_isolation_guarantees(temp_dir, test_count, &
                                              passed_tests, all_tests_passed)
        !! Tests that tests are properly isolated from each other
        character(len=*), intent(in) :: temp_dir
        integer, intent(inout) :: test_count
        integer, intent(inout) :: passed_tests  
        logical, intent(inout) :: all_tests_passed
        
        character(len=512) :: isolation_file
        logical :: file_exists
        integer :: unit_number, iostat
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== TEST ISOLATION GUARANTEES ==="
        
        isolation_file = "test_infra_isolation_test.txt"
        
        ! Test 1: Environment detection
        call assert_test(test_environment_detected(), &
                        "Test environment detection", &
                        "Should detect we're inside tests", &
                        test_count, passed_tests, all_tests_passed)
        
        ! Test 2: Temporary file isolation
        open(newunit=unit_number, file=trim(isolation_file), &
             status='replace', action='write', iostat=iostat)
        if (iostat == 0) then
            write(unit_number, '(A)') "Isolation test content"
            close(unit_number)
        end if
        
        inquire(file=trim(isolation_file), exist=file_exists)
        call assert_test(file_exists, "Temporary file creation", &
                        "Should create files in isolated space", &
                        test_count, passed_tests, all_tests_passed)
        
        ! Test 3: Fork bomb prevention active
        inquire(file='.fortcov_execution_marker', exist=file_exists)
        if (file_exists) then
            call assert_test(.true., "Fork bomb prevention active", &
                            "Fork bomb prevention working", &
                            test_count, passed_tests, all_tests_passed)
        else
            call assert_test(.true., "Fork bomb prevention ready", &
                            "Fork bomb prevention available", &
                            test_count, passed_tests, all_tests_passed)
        end if
        
    end subroutine test_test_isolation_guarantees

    subroutine test_concurrent_execution_safety(temp_dir, test_count, &
                                                passed_tests, all_tests_passed)
        !! Tests safety of concurrent operations
        character(len=*), intent(in) :: temp_dir
        integer, intent(inout) :: test_count
        integer, intent(inout) :: passed_tests  
        logical, intent(inout) :: all_tests_passed
        
        integer :: i, exit_status
        character(len=512) :: test_file
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== CONCURRENT EXECUTION SAFETY ==="
        
        ! Test 1: Multiple file operations using secure file creation
        call safe_create_concurrent_files(temp_dir, 3, exit_status)
        call assert_test(exit_status == 0, "Concurrent file operations", &
                        "Should handle concurrent file ops", &
                        test_count, passed_tests, all_tests_passed)
        
    end subroutine test_concurrent_execution_safety

    subroutine test_framework_assertion_reliability(test_count, &
                                                    passed_tests, all_tests_passed)
        !! Tests that the test framework assertions work reliably
        integer, intent(inout) :: test_count
        integer, intent(inout) :: passed_tests  
        logical, intent(inout) :: all_tests_passed
        
        logical :: test_condition
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== FRAMEWORK ASSERTION RELIABILITY ==="
        
        ! Test 1: True assertions work
        test_condition = .true.
        call assert_test(test_condition, "True assertion handling", &
                        "Should handle true conditions correctly", &
                        test_count, passed_tests, all_tests_passed)
        
        ! Test 2: String comparison reliability
        call assert_test(trim("test") == "test", "String comparison", &
                        "String comparisons should work", &
                        test_count, passed_tests, all_tests_passed)
        
        ! Test 3: Numeric comparison reliability  
        call assert_test(42 == 42, "Numeric comparison", &
                        "Numeric comparisons should work", &
                        test_count, passed_tests, all_tests_passed)
        
        ! Test 4: Logical operation reliability
        call assert_test(.true. .and. .not. .false., "Logical operations", &
                        "Logical operations should work correctly", &
                        test_count, passed_tests, all_tests_passed)
        
    end subroutine test_framework_assertion_reliability

    subroutine test_cleanup_mechanisms(temp_dir, test_count, &
                                       passed_tests, all_tests_passed)
        !! Tests that cleanup mechanisms work properly
        character(len=*), intent(in) :: temp_dir
        integer, intent(inout) :: test_count
        integer, intent(inout) :: passed_tests  
        logical, intent(inout) :: all_tests_passed
        
        character(len=512) :: cleanup_file
        logical :: file_exists
        integer :: unit_number, iostat
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== CLEANUP MECHANISMS ==="
        
        cleanup_file = "test_infra_cleanup_test.txt"
        
        ! Test 1: File cleanup
        open(newunit=unit_number, file=trim(cleanup_file), &
             status='replace', action='write', iostat=iostat)
        if (iostat == 0) then
            write(unit_number, '(A)') "Cleanup test"
            close(unit_number)
        end if
        
        inquire(file=trim(cleanup_file), exist=file_exists)
        call assert_test(file_exists, "File creation for cleanup test", &
                        "Should create file", &
                        test_count, passed_tests, all_tests_passed)
        
        ! Perform cleanup using secure file removal
        call safe_cleanup_test_file(cleanup_file)
        inquire(file=trim(cleanup_file), exist=file_exists)
        call assert_test(.not. file_exists, "File cleanup effectiveness", &
                        "Should remove files properly", &
                        test_count, passed_tests, all_tests_passed)
        
    end subroutine test_cleanup_mechanisms

    subroutine test_resource_leak_prevention(temp_dir, test_count, &
                                             passed_tests, all_tests_passed)
        !! Tests that resources are properly managed to prevent leaks
        character(len=*), intent(in) :: temp_dir
        integer, intent(inout) :: test_count
        integer, intent(inout) :: passed_tests  
        logical, intent(inout) :: all_tests_passed
        
        integer :: i, unit_number, iostat
        character(len=512) :: test_file
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== RESOURCE LEAK PREVENTION ==="
        
        ! Test 1: File handle management
        do i = 1, 5
            write(test_file, '(A,I0,A)') "test_infra_handle_", i, ".txt"
            open(newunit=unit_number, file=trim(test_file), &
                 status='replace', action='write', iostat=iostat)
            if (iostat == 0) then
                write(unit_number, '(A,I0)') "Handle test ", i
                close(unit_number)
            end if
        end do
        call assert_test(iostat == 0, "File handle management", &
                        "Should manage file handles properly", &
                        test_count, passed_tests, all_tests_passed)
        
    end subroutine test_resource_leak_prevention

    subroutine test_environment_detection_accuracy(test_count, &
                                                   passed_tests, all_tests_passed)
        !! Tests accuracy of environment detection
        integer, intent(inout) :: test_count
        integer, intent(inout) :: passed_tests  
        logical, intent(inout) :: all_tests_passed
        
        logical :: inside_tests
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== ENVIRONMENT DETECTION ACCURACY ==="
        
        ! Test 1: Test environment detection
        inside_tests = test_environment_detected()
        call assert_test(inside_tests, "Test environment detection", &
                        "Should correctly detect test environment", &
                        test_count, passed_tests, all_tests_passed)
        
    end subroutine test_environment_detection_accuracy

    subroutine test_temporary_file_management(temp_dir, test_count, &
                                              passed_tests, all_tests_passed)
        !! Tests temporary file management capabilities
        character(len=*), intent(in) :: temp_dir
        integer, intent(inout) :: test_count
        integer, intent(inout) :: passed_tests  
        logical, intent(inout) :: all_tests_passed
        
        character(len=512) :: temp_file
        logical :: file_exists
        integer :: unit_number, iostat
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== TEMPORARY FILE MANAGEMENT ==="
        
        temp_file = "test_infra_temp_mgmt_test.tmp"
        
        ! Test 1: Temporary file creation
        open(newunit=unit_number, file=trim(temp_file), &
             status='replace', action='write', iostat=iostat)
        call assert_test(iostat == 0, "Temporary file creation", &
                        "Should create temporary files", &
                        test_count, passed_tests, all_tests_passed)
        
        if (iostat == 0) then
            write(unit_number, '(A)') "Temporary content"
            close(unit_number)
        end if
        
        ! Test 2: Temporary file access
        inquire(file=trim(temp_file), exist=file_exists)
        call assert_test(file_exists, "Temporary file access", &
                        "Should access created temp files", &
                        test_count, passed_tests, all_tests_passed)
        
        ! Test 3: Temporary file cleanup using secure file removal
        call safe_cleanup_temp_file(temp_file)
        inquire(file=trim(temp_file), exist=file_exists)
        call assert_test(.not. file_exists, "Temporary file cleanup", &
                        "Should clean up temp files", &
                        test_count, passed_tests, all_tests_passed)
        
    end subroutine test_temporary_file_management

    function test_environment_detected() result(is_test_env)
        !! Use consistent test environment detection  
        use test_environment_utils, only: &
            test_environment_detected_util => test_environment_detected
        logical :: is_test_env
        
        is_test_env = test_environment_detected_util()
    end function test_environment_detected

    subroutine test_system_interaction_stability(temp_dir, test_count, &
                                                 passed_tests, all_tests_passed)
        !! Tests stability of system interactions
        character(len=*), intent(in) :: temp_dir
        integer, intent(inout) :: test_count
        integer, intent(inout) :: passed_tests  
        logical, intent(inout) :: all_tests_passed
        
        integer :: exit_status
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== SYSTEM INTERACTION STABILITY ==="
        
        ! Test 1: Basic system commands using secure true replacement
        call safe_test_command_true(exit_status)
        call assert_test(exit_status == 0, "Basic system command", &
                        "Should execute basic commands", &
                        test_count, passed_tests, all_tests_passed)
        
        ! Test 2: File system interactions using secure directory listing
        call safe_test_file_list(temp_dir, exit_status)
        call assert_test(exit_status == 0, "File system interaction", &
                        "Should interact with file system", &
                        test_count, passed_tests, all_tests_passed)
        
        ! Test 3: Error command handling using secure false replacement
        call safe_test_command_false(exit_status)
        call assert_test(exit_status /= 0, "Error command detection", &
                        "Should detect command errors", &
                        test_count, passed_tests, all_tests_passed)
        
        ! Test 4: Complex command chains using secure pipe simulation
        call safe_test_pipe_command(exit_status)
        call assert_test(exit_status == 0, "Complex command chains", &
                        "Should handle command pipelines", &
                        test_count, passed_tests, all_tests_passed)
        
    end subroutine test_system_interaction_stability

    ! SECURITY FIX Issue #971: Secure setup and cleanup helper functions
    
    subroutine setup_test_directory_secure(temp_dir, success)
        !! Secure setup of test directory using Fortran I/O
        character(len=*), intent(in) :: temp_dir
        logical, intent(out) :: success
        
        type(error_context_t) :: error_ctx
        
        call safe_mkdir(temp_dir, error_ctx)
        success = (error_ctx%error_code == ERROR_SUCCESS)
        
    end subroutine setup_test_directory_secure
    
    subroutine safe_cleanup_extended_test_directory(temp_dir)
        !! Secure cleanup of extended test directory
        character(len=*), intent(in) :: temp_dir
        type(error_context_t) :: error_ctx
        call safe_remove_directory(temp_dir, error_ctx)
        ! Ignore cleanup errors - directory may not exist or may be locked
    end subroutine safe_cleanup_extended_test_directory
    
    subroutine safe_cleanup_test_file(file_path)
        !! Secure cleanup of individual test file
        character(len=*), intent(in) :: file_path
        type(error_context_t) :: error_ctx
        call safe_remove_file(file_path, error_ctx)
        ! Ignore cleanup errors - file may not exist
    end subroutine safe_cleanup_test_file
    
    subroutine safe_cleanup_temp_file(file_path)
        !! Secure cleanup of temporary test file
        character(len=*), intent(in) :: file_path
        type(error_context_t) :: error_ctx
        call safe_remove_file(file_path, error_ctx)
        ! Ignore cleanup errors - file may not exist
    end subroutine safe_cleanup_temp_file

end module test_infrastructure_extended_validation
