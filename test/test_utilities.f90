module test_utilities
    !! Common test utilities shared across test programs
    !! Provides standard assertion and reporting infrastructure
    
    use iso_fortran_env, only: error_unit
    implicit none
    private
    
    public :: assert_true, assert_false, assert_equals_int
    public :: assert_equals_str, assert_present, assert_absent
    public :: test_runner_t
    
    type :: test_runner_t
        !! Test runner with consistent reporting
        integer :: test_count = 0
        integer :: passed_tests = 0
        character(len=256) :: suite_name = ""
    contains
        procedure :: init => runner_init
        procedure :: run_test => runner_run_test
        procedure :: print_summary => runner_print_summary
        procedure :: get_pass_rate => runner_get_pass_rate
    end type test_runner_t
    
contains
    
    subroutine runner_init(this, name)
        !! Initialize test runner with suite name
        class(test_runner_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        
        this%test_count = 0
        this%passed_tests = 0
        this%suite_name = trim(name)
        
        print *, "=== " // trim(name) // " ==="
    end subroutine runner_init
    
    subroutine runner_run_test(this, test_name, test_passed)
        !! Record test result and update counters
        class(test_runner_t), intent(inout) :: this
        character(len=*), intent(in) :: test_name
        logical, intent(in) :: test_passed
        
        this%test_count = this%test_count + 1
        
        if (test_passed) then
            this%passed_tests = this%passed_tests + 1
            print *, "  ✅ PASSED: " // test_name
        else
            print *, "  ❌ FAILED: " // test_name
        end if
    end subroutine runner_run_test
    
    subroutine runner_print_summary(this)
        !! Print test execution summary
        class(test_runner_t), intent(in) :: this
        
        print *, ""
        write(*, '(A,I0,A,I0,A)') "Tests completed: ", &
            this%passed_tests, "/", this%test_count, " passed"
        
        if (this%passed_tests == this%test_count) then
            print *, "✅ All tests in " // trim(this%suite_name) // " PASSED"
        else
            print *, "❌ Some tests in " // trim(this%suite_name) // " FAILED"
        end if
    end subroutine runner_print_summary
    
    function runner_get_pass_rate(this) result(rate)
        !! Get test pass rate as percentage
        class(test_runner_t), intent(in) :: this
        real :: rate
        
        if (this%test_count > 0) then
            rate = real(this%passed_tests) / real(this%test_count) * 100.0
        else
            rate = 0.0
        end if
    end function runner_get_pass_rate
    
    subroutine assert_true(condition, message, passed)
        !! Assert condition is true
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        logical, intent(inout) :: passed
        
        if (.not. condition) then
            write(error_unit, *) "  ❌ Assertion failed: " // message
            passed = .false.
        end if
    end subroutine assert_true
    
    subroutine assert_false(condition, message, passed)
        !! Assert condition is false
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        logical, intent(inout) :: passed
        
        if (condition) then
            write(error_unit, *) "  ❌ Assertion failed: " // message
            passed = .false.
        end if
    end subroutine assert_false
    
    subroutine assert_equals_int(actual, expected, message, passed)
        !! Assert integer equality
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: message
        logical, intent(inout) :: passed
        
        if (actual /= expected) then
            write(error_unit, '(A,I0,A,I0)') &
                "  ❌ " // message // " - Expected: ", expected, &
                ", Got: ", actual
            passed = .false.
        end if
    end subroutine assert_equals_int
    
    subroutine assert_equals_str(actual, expected, message, passed)
        !! Assert string equality
        character(len=*), intent(in) :: actual, expected
        character(len=*), intent(in) :: message
        logical, intent(inout) :: passed
        
        if (trim(actual) /= trim(expected)) then
            write(error_unit, *) "  ❌ " // message
            write(error_unit, *) "    Expected: '" // trim(expected) // "'"
            write(error_unit, *) "    Got:      '" // trim(actual) // "'"
            passed = .false.
        end if
    end subroutine assert_equals_str
    
    subroutine assert_present(value, message, passed)
        !! Assert that a file or directory exists
        character(len=*), intent(in) :: value
        character(len=*), intent(in) :: message
        logical, intent(inout) :: passed
        logical :: exists
        
        inquire(file=value, exist=exists)
        if (.not. exists) then
            write(error_unit, *) "  ❌ " // message // &
                " - File not found: " // trim(value)
            passed = .false.
        end if
    end subroutine assert_present
    
    subroutine assert_absent(value, message, passed)
        !! Assert that a file or directory does not exist
        character(len=*), intent(in) :: value
        character(len=*), intent(in) :: message
        logical, intent(inout) :: passed
        logical :: exists
        
        inquire(file=value, exist=exists)
        if (exists) then
            write(error_unit, *) "  ❌ " // message // &
                " - File should not exist: " // trim(value)
            passed = .false.
        end if
    end subroutine assert_absent
    
end module test_utilities