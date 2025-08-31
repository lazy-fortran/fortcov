module test_framework_utilities
    !! Shared test framework infrastructure
    !! Provides common test functionality across all test modules
    
    implicit none
    private
    
    public :: test_counter_t
    public :: init_test_counter, increment_test, increment_pass, increment_fail
    public :: print_test_summary
    
    type :: test_counter_t
        integer :: total = 0
        integer :: passed = 0
        integer :: failed = 0
    end type test_counter_t
    
contains
    
    subroutine init_test_counter(counter)
        type(test_counter_t), intent(out) :: counter
        counter%total = 0
        counter%passed = 0
        counter%failed = 0
    end subroutine init_test_counter
    
    subroutine increment_test(counter)
        type(test_counter_t), intent(inout) :: counter
        counter%total = counter%total + 1
    end subroutine increment_test
    
    subroutine increment_pass(counter)
        type(test_counter_t), intent(inout) :: counter
        call increment_test(counter)
        counter%passed = counter%passed + 1
    end subroutine increment_pass
    
    subroutine increment_fail(counter)
        type(test_counter_t), intent(inout) :: counter
        call increment_test(counter)
        counter%failed = counter%failed + 1
    end subroutine increment_fail
    
    subroutine print_test_summary(counter, suite_name)
        type(test_counter_t), intent(in) :: counter
        character(len=*), intent(in) :: suite_name
        
        print *, ""
        print *, "======================================================================"
        print *, "Test Summary: ", suite_name
        print *, "======================================================================"
        print *, "Total Tests: ", counter%total
        print *, "Passed:      ", counter%passed
        print *, "Failed:      ", counter%failed
        
        if (counter%failed > 0) then
            print *, "Status:      FAILED"
            stop 1
        else
            print *, "Status:      PASSED"
        end if
        print *, ""
    end subroutine print_test_summary
    
end module test_framework_utilities
