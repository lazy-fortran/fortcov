! Test suite for CMake demo math module
program test_demo
    use demo_math
    implicit none
    
    integer :: test_count = 0
    integer :: passed_count = 0
    integer :: failed_count = 0
    
    write(*,*) "Running CMake Demo Math Tests"
    write(*,*) "============================"
    
    ! Test factorial function
    call test_factorial()
    
    ! Test fibonacci function
    call test_fibonacci()
    
    ! Test prime checking function
    call test_is_prime()
    
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

    subroutine test_factorial()
        call assert_int_equal(factorial(0), 1, "Factorial of 0")
        call assert_int_equal(factorial(1), 1, "Factorial of 1")
        call assert_int_equal(factorial(5), 120, "Factorial of 5")
        call assert_int_equal(factorial(4), 24, "Factorial of 4")
    end subroutine test_factorial

    subroutine test_fibonacci()
        call assert_int_equal(fibonacci(0), 0, "Fibonacci 0")
        call assert_int_equal(fibonacci(1), 1, "Fibonacci 1")
        call assert_int_equal(fibonacci(8), 21, "Fibonacci 8")
        call assert_int_equal(fibonacci(6), 8, "Fibonacci 6")
    end subroutine test_fibonacci

    subroutine test_is_prime()
        call assert_logical_equal(is_prime(2), .true., "2 is prime")
        call assert_logical_equal(is_prime(3), .true., "3 is prime") 
        call assert_logical_equal(is_prime(17), .true., "17 is prime")
        call assert_logical_equal(is_prime(4), .false., "4 is not prime")
        call assert_logical_equal(is_prime(15), .false., "15 is not prime")
        call assert_logical_equal(is_prime(1), .false., "1 is not prime")
    end subroutine test_is_prime

    subroutine assert_int_equal(actual, expected, test_name)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: test_name
        
        test_count = test_count + 1
        
        if (actual == expected) then
            write(*,'(A,A,A)') "✓ ", test_name, " PASSED"
            passed_count = passed_count + 1
        else
            write(*,'(A,A,A,I0,A,I0)') "✗ ", test_name, " FAILED (expected ", expected, ", got ", actual, ")"
            failed_count = failed_count + 1
        end if
    end subroutine assert_int_equal

    subroutine assert_logical_equal(actual, expected, test_name)
        logical, intent(in) :: actual, expected
        character(len=*), intent(in) :: test_name
        
        test_count = test_count + 1
        
        if (actual .eqv. expected) then
            write(*,'(A,A,A)') "✓ ", test_name, " PASSED"
            passed_count = passed_count + 1
        else
            write(*,'(A,A,A,L1,A,L1)') "✗ ", test_name, " FAILED (expected ", expected, ", got ", actual, ")"
            failed_count = failed_count + 1
        end if
    end subroutine assert_logical_equal

end program test_demo