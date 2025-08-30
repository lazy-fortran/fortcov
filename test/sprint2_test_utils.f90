module sprint2_test_utils
    !! Sprint 2 Test Validation Utilities (Issue #540)
    !!
    !! This module contains extracted utilities from the comprehensive Sprint 2
    !! validation test suite to achieve QADS compliance (<500 lines per file).
    !! Provides common test utilities, assertion helpers, and mock data creation.
    
    use iso_fortran_env, only: output_unit
    implicit none
    
    public :: assert_test, create_mock_gcov_with_coverage, test_environment_detected, &
              check_gcov_files_exist
    
contains

    subroutine assert_test(condition, test_name, details, test_count, &
                          passed_tests, all_tests_passed)
        !! Test assertion utility with comprehensive reporting
        !! Extracted from test_sprint_2_validation_comprehensive.f90
        
        logical, intent(in) :: condition
        character(len=*), intent(in) :: test_name, details
        integer, intent(inout) :: test_count, passed_tests
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

    subroutine create_mock_gcov_with_coverage()
        !! Creates a mock gcov file with realistic coverage data for testing
        !! Extracted from test_sprint_2_validation_comprehensive.f90
        
        integer :: unit_number
        
        open(newunit=unit_number, file='mock_coverage_test.f90.gcov', &
             status='replace', action='write')
        
        write(unit_number, '(A)') "        -:    0:Source:mock_coverage_test.f90"
        write(unit_number, '(A)') "        -:    0:Graph:mock_coverage_test.gcno"
        write(unit_number, '(A)') "        -:    0:Data:mock_coverage_test.gcda"
        write(unit_number, '(A)') "        -:    0:Runs:1"
        write(unit_number, '(A)') "        -:    0:Programs:1"
        write(unit_number, '(A)') "        -:    1:program mock_test"
        write(unit_number, '(A)') "        1:    2:  print *, ""Hello World"""
        write(unit_number, '(A)') "        1:    3:  call test_function()"
        write(unit_number, '(A)') "    #####:    4:  print *, ""Never executed"""
        write(unit_number, '(A)') "        1:    5:end program"
        write(unit_number, '(A)') "        -:    6:"
        write(unit_number, '(A)') "        -:    7:subroutine test_function()"
        write(unit_number, '(A)') "        1:    8:  integer :: x = 42"
        write(unit_number, '(A)') "        1:    9:end subroutine"
        
        close(unit_number)
        
    end subroutine create_mock_gcov_with_coverage

    function test_environment_detected() result(is_test_env)
        !! Wrapper for consistent test environment detection
        !! Extracted from test_sprint_2_validation_comprehensive.f90
        
        use test_environment_utils, only: &
            test_environment_detected_util => test_environment_detected
        logical :: is_test_env
        
        is_test_env = test_environment_detected_util()
    end function test_environment_detected

    function check_gcov_files_exist() result(found_data)
        !! Check if gcov files exist in the current directory
        !! Extracted from test_sprint_2_validation_comprehensive.f90
        
        use portable_temp_utils, only: get_temp_dir
        logical :: found_data
        character(len=:), allocatable :: temp_dir
        character(len=512) :: gcov_files_list
        integer :: unit_number, iostat
        character(len=1000) :: line
        
        found_data = .false.
        temp_dir = get_temp_dir()
        gcov_files_list = temp_dir // '/gcov_files.txt'
        
        call execute_command_line('find . -name "*.gcov" > "' // &
                                  trim(gcov_files_list) // '" 2>/dev/null')
        
        open(newunit=unit_number, file=trim(gcov_files_list), status='old', &
             iostat=iostat, action='read')
        if (iostat == 0) then
            do
                read(unit_number, '(A)', iostat=iostat) line
                if (iostat /= 0) exit
                if (len_trim(line) > 0) then
                    found_data = .true.
                    exit
                end if
            end do
            close(unit_number)
        end if
        
        ! Cleanup
        call execute_command_line('rm -f "' // trim(gcov_files_list) // '"')
    end function check_gcov_files_exist


end module sprint2_test_utils
