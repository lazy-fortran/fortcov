module test_utils_core
    !! Shared Test Utilities Module
    !!
    !! Common test utilities and assertion functions shared across
    !! all test modules to eliminate code duplication and ensure
    !! consistent test reporting.
    !!
    !! DESIGN PRINCIPLES:
    !! - Single responsibility: test utility functions only
    !! - Eliminates 70+ lines of duplicate assert_test code
    !! - Consistent test output format across all modules
    !! - Centralized test counter and status management
    
    use iso_fortran_env, only: output_unit
    implicit none
    
    ! Test state management (shared across tests)
    integer, public :: test_count = 0
    integer, public :: passed_tests = 0
    logical, public :: all_tests_passed = .true.
    
    public :: assert_test, reset_test_counters, print_test_header, &
              print_test_summary
    
contains

    subroutine assert_test(condition, test_name, details)
        !! Assertion function with detailed reporting
        !! 
        !! Args:
        !!   condition: Test condition to evaluate
        !!   test_name: Descriptive name for the test
        !!   details: Additional details for failure cases
        
        logical, intent(in) :: condition
        character(len=*), intent(in) :: test_name, details
        
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

    subroutine reset_test_counters()
        !! Reset test counters for independent test modules
        
        test_count = 0
        passed_tests = 0
        all_tests_passed = .true.
    end subroutine reset_test_counters

    subroutine print_test_header(suite_name)
        !! Print standardized test suite header
        
        character(len=*), intent(in) :: suite_name
        
        write(output_unit, '(A)') "======================================================="
        write(output_unit, '(A)') "            " // trim(suite_name) // " Test Suite"
        write(output_unit, '(A)') "======================================================="
        write(output_unit, '(A)') ""
    end subroutine print_test_header

    subroutine print_test_summary(suite_name, exit_on_completion)
        !! Print standardized test summary and optionally exit
        
        character(len=*), intent(in) :: suite_name
        logical, intent(in), optional :: exit_on_completion
        logical :: should_exit
        
        should_exit = .true.
        if (present(exit_on_completion)) should_exit = exit_on_completion
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "======================================================="
        write(*, '(A,I0,A,I0,A)') trim(suite_name) // ": ", passed_tests, "/", &
                                  test_count, " tests passed"

        if (all_tests_passed) then
            write(output_unit, '(A)') "✅ ALL " // trim(suite_name) // " TESTS PASSED"
            if (should_exit) call exit(0)
        else
            write(output_unit, '(A)') "❌ " // trim(suite_name) // " TESTS FAILED"
            if (should_exit) call exit(1)
        end if
    end subroutine print_test_summary

end module test_utils_core