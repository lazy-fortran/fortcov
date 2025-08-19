! Test suite for demo calculator
program test_demo
    use demo_calculator
    implicit none
    
    integer :: test_count = 0
    integer :: passed_count = 0
    integer :: failed_count = 0
    
    write(*,*) "Running Demo Calculator Tests"
    write(*,*) "============================="
    
    ! Test addition
    call test_addition()
    
    ! Test multiplication
    call test_multiplication()
    
    ! Test division
    call test_division()
    
    ! Test division by zero
    call test_division_by_zero()
    
    ! Report results
    write(*,*)
    write(*,'(A,I0,A,I0,A,I0,A)') "Tests completed: ", test_count, " (", passed_count, " passed, ", failed_count, " failed)"
    
    if (failed_count > 0) then
        write(*,*) "TESTS FAILED"
        stop 1
    else
        write(*,*) "All tests passed"
    end if

contains

    subroutine test_addition()
        real :: result
        result = add_numbers(2.0, 3.0)
        call assert_equal(result, 5.0, "Addition test 1")
        
        result = add_numbers(-1.0, 1.0)
        call assert_equal(result, 0.0, "Addition test 2")
    end subroutine test_addition

    subroutine test_multiplication()
        real :: result
        result = multiply_numbers(4.0, 5.0)
        call assert_equal(result, 20.0, "Multiplication test 1")
        
        result = multiply_numbers(-2.0, 3.0)
        call assert_equal(result, -6.0, "Multiplication test 2")
    end subroutine test_multiplication

    subroutine test_division()
        real :: result
        result = divide_numbers(10.0, 2.0)
        call assert_equal(result, 5.0, "Division test 1")
        
        result = divide_numbers(-6.0, 3.0)
        call assert_equal(result, -2.0, "Division test 2")
    end subroutine test_division

    subroutine test_division_by_zero()
        real :: result
        result = divide_numbers(10.0, 0.0)
        call assert_equal(result, 0.0, "Division by zero test")
    end subroutine test_division_by_zero

    subroutine assert_equal(actual, expected, test_name)
        real, intent(in) :: actual, expected
        character(len=*), intent(in) :: test_name
        
        test_count = test_count + 1
        
        if (abs(actual - expected) < 1.0e-6) then
            write(*,'(A,A,A)') "✓ ", test_name, " PASSED"
            passed_count = passed_count + 1
        else
            write(*,'(A,A,A,F0.6,A,F0.6)') "✗ ", test_name, " FAILED (expected ", expected, ", got ", actual, ")"
            failed_count = failed_count + 1
        end if
    end subroutine assert_equal

end program test_demo