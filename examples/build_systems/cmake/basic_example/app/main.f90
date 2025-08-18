! Demo application for CMake integration testing
program demo_app
    use demo_math
    implicit none
    
    integer :: n, result
    logical :: prime_result
    
    write(*,*) "CMake Demo Math Application"
    write(*,*) "=========================="
    
    ! Test factorial
    n = 5
    result = factorial(n)
    write(*,'(A,I0,A,I0)') "Factorial of ", n, " = ", result
    
    ! Test fibonacci
    n = 8
    result = fibonacci(n)
    write(*,'(A,I0,A,I0)') "Fibonacci number ", n, " = ", result
    
    ! Test prime checking
    n = 17
    prime_result = is_prime(n)
    if (prime_result) then
        write(*,'(I0,A)') n, " is prime"
    else
        write(*,'(I0,A)') n, " is not prime"
    end if
    
    n = 15
    prime_result = is_prime(n)
    if (prime_result) then
        write(*,'(I0,A)') n, " is prime"
    else
        write(*,'(I0,A)') n, " is not prime"
    end if

end program demo_app