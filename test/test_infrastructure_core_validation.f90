module test_infrastructure_core_validation
    !! Core infrastructure validation testing module
    !! 
    !! This module contains the core infrastructure tests broken out
    !! from the monolithic infrastructure stability test to meet size requirements.
    
    use iso_fortran_env, only: output_unit, error_unit, iostat_end
    use portable_temp_utils, only: get_temp_dir, create_temp_subdir
    implicit none
    private
    
    public :: run_core_infrastructure_tests
    
contains

    subroutine run_core_infrastructure_tests(test_count, passed_tests, all_tests_passed)
        !! Run core infrastructure validation tests
        integer, intent(inout) :: test_count
        integer, intent(inout) :: passed_tests  
        logical, intent(inout) :: all_tests_passed
        
        character(len=:), allocatable :: temp_dir
        logical :: setup_success
        
        ! Setup test environment using portable temp utilities
        call create_temp_subdir("fortcov_infra_test", temp_dir, setup_success)
        if (.not. setup_success) then
            write(error_unit, '(A)') "Failed to create test environment"
            return
        end if
        
        ! Core infrastructure stability tests
        call test_command_execution_stability(temp_dir, test_count, &
                                              passed_tests, all_tests_passed)
        call test_file_io_reliability(temp_dir, test_count, passed_tests, &
                                      all_tests_passed)
        call test_memory_management_stability(test_count, passed_tests, &
                                              all_tests_passed)
        call test_error_handling_robustness(test_count, passed_tests, all_tests_passed)
        
        ! Cleanup test environment
        call execute_command_line('rm -rf "' // temp_dir // '"')
        
    end subroutine run_core_infrastructure_tests

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

    subroutine test_command_execution_stability(temp_dir, test_count, &
                                                passed_tests, all_tests_passed)
        !! Tests that command execution is stable and reliable
        character(len=*), intent(in) :: temp_dir
        integer, intent(inout) :: test_count
        integer, intent(inout) :: passed_tests  
        logical, intent(inout) :: all_tests_passed
        
        integer :: exit_status, i
        character(len=512) :: test_file
        logical :: file_exists
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== COMMAND EXECUTION STABILITY ==="
        
        test_file = trim(temp_dir) // "/cmd_test.txt"
        
        ! Test 1: Basic command execution
        call execute_command_line('echo "test" > "' // trim(test_file) // '"', &
                                  wait=.true., exitstat=exit_status)
        call assert_test(exit_status == 0, "Basic command execution", &
                        "Simple echo command should succeed", &
                        test_count, passed_tests, all_tests_passed)
        
        ! Test 2: File creation verification
        inquire(file=trim(test_file), exist=file_exists)
        call assert_test(file_exists, "Command output verification", &
                        "Echo command should create file", &
                        test_count, passed_tests, all_tests_passed)
        
        ! Test 3: Multiple rapid commands
        do i = 1, 5
            call execute_command_line('touch "' // trim(temp_dir) // '/rapid_' // &
                                     char(48 + i) // '.txt"', &
                                     wait=.true., exitstat=exit_status)
            if (exit_status /= 0) exit
        end do
        call assert_test(exit_status == 0, "Rapid command execution", &
                        "Multiple rapid commands should succeed", &
                        test_count, passed_tests, all_tests_passed)
        
        ! Test 4: Error handling in commands
        call execute_command_line('ls /nonexistent/path', &
                                  wait=.true., exitstat=exit_status)
        call assert_test(exit_status /= 0, "Command error detection", &
                        "Failed commands should return non-zero", &
                        test_count, passed_tests, all_tests_passed)
        
    end subroutine test_command_execution_stability

    subroutine test_file_io_reliability(temp_dir, test_count, &
                                        passed_tests, all_tests_passed)
        !! Tests file I/O operations for reliability
        character(len=*), intent(in) :: temp_dir
        integer, intent(inout) :: test_count
        integer, intent(inout) :: passed_tests  
        logical, intent(inout) :: all_tests_passed
        
        integer :: unit_number, iostat, i
        character(len=512) :: test_file, line
        character(len=32) :: test_data
        logical :: file_exists
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== FILE I/O RELIABILITY ==="
        
        test_file = trim(temp_dir) // "/io_test.txt"
        
        ! Test 1: Basic file write
        open(newunit=unit_number, file=trim(test_file), &
             status='replace', action='write', iostat=iostat)
        call assert_test(iostat == 0, "File open for write", &
                        "Should open file for writing", &
                        test_count, passed_tests, all_tests_passed)
        
        if (iostat == 0) then
            write(unit_number, '(A)', iostat=iostat) "Test line 1"
            write(unit_number, '(A)', iostat=iostat) "Test line 2"
            write(unit_number, '(A)', iostat=iostat) "Test line 3"
            close(unit_number)
            call assert_test(iostat == 0, "File write operations", &
                            "Should write lines successfully", &
                            test_count, passed_tests, all_tests_passed)
        end if
        
        ! Test 2: File existence check
        inquire(file=trim(test_file), exist=file_exists)
        call assert_test(file_exists, "File existence after write", &
                        "File should exist after writing", &
                        test_count, passed_tests, all_tests_passed)
        
        ! Test 3: Basic file read
        open(newunit=unit_number, file=trim(test_file), &
             status='old', action='read', iostat=iostat)
        call assert_test(iostat == 0, "File open for read", &
                        "Should open existing file for reading", &
                        test_count, passed_tests, all_tests_passed)
        
        if (iostat == 0) then
            read(unit_number, '(A)', iostat=iostat) line
            call assert_test(iostat == 0 .and. trim(line) == "Test line 1", &
                            "File read verification", &
                            "Should read first line correctly", &
                            test_count, passed_tests, all_tests_passed)
            close(unit_number)
        end if
        
    end subroutine test_file_io_reliability

    subroutine test_memory_management_stability(test_count, &
                                                passed_tests, all_tests_passed)
        !! Tests memory management stability
        integer, intent(inout) :: test_count
        integer, intent(inout) :: passed_tests  
        logical, intent(inout) :: all_tests_passed
        
        character(len=:), allocatable :: dynamic_string
        character(len=256), allocatable :: string_array(:)
        integer :: i, allocation_status
        logical :: allocation_success = .true.
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== MEMORY MANAGEMENT STABILITY ==="
        
        ! Test 1: Dynamic string allocation
        allocate(character(len=100) :: dynamic_string, stat=allocation_status)
        call assert_test(allocation_status == 0, "Dynamic string allocation", &
                        "Should allocate dynamic string", &
                        test_count, passed_tests, all_tests_passed)
        
        if (allocation_status == 0) then
            dynamic_string = "Test dynamic string allocation"
            call assert_test(len(dynamic_string) == 100, &
                            "Dynamic string length", "Should have correct length", &
                            test_count, passed_tests, all_tests_passed)
            deallocate(dynamic_string)
        end if
        
        ! Test 2: Array allocation and deallocation
        allocate(string_array(10), stat=allocation_status)
        call assert_test(allocation_status == 0, "Array allocation", &
                        "Should allocate array", &
                        test_count, passed_tests, all_tests_passed)
        
        if (allocation_status == 0) then
            do i = 1, 10
                write(string_array(i), '(A,I0)') "Element ", i
            end do
            call assert_test(trim(string_array(5)) == "Element 5", &
                            "Array element assignment", &
                            "Should assign array elements correctly", &
                            test_count, passed_tests, all_tests_passed)
            deallocate(string_array)
        end if
        
    end subroutine test_memory_management_stability

    subroutine test_error_handling_robustness(test_count, passed_tests, &
                                              all_tests_passed)
        !! Tests error handling mechanisms for robustness
        integer, intent(inout) :: test_count
        integer, intent(inout) :: passed_tests  
        logical, intent(inout) :: all_tests_passed
        
        integer :: unit_number, iostat
        character(len=256) :: error_message
        logical :: error_detected
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== ERROR HANDLING ROBUSTNESS ==="
        
        ! Test 1: File operation error handling
        open(newunit=unit_number, file="/root/cannot_write_here.txt", &
             status='new', action='write', iostat=iostat, iomsg=error_message)
        call assert_test(iostat /= 0, "File permission error detection", &
                        "Should detect permission errors", &
                        test_count, passed_tests, all_tests_passed)
        
        ! Test 2: Non-existent file handling
        open(newunit=unit_number, file="/nonexistent/path/file.txt", &
             status='old', action='read', iostat=iostat)
        call assert_test(iostat /= 0, "Missing file error detection", &
                        "Should detect missing file errors", &
                        test_count, passed_tests, all_tests_passed)
        
        ! Test 3: Graceful degradation
        error_detected = .false.
        call execute_command_line('invalid_command_that_does_not_exist', &
                                  wait=.true., exitstat=iostat)
        if (iostat /= 0) error_detected = .true.
        
        call assert_test(error_detected, "Command error handling", &
                        "Should handle invalid command gracefully", &
                        test_count, passed_tests, all_tests_passed)
        
    end subroutine test_error_handling_robustness

end module test_infrastructure_core_validation