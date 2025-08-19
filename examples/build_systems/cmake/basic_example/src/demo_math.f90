! Demo math module for CMake coverage testing
module demo_math
    implicit none
    private
    
    public :: factorial, fibonacci, is_prime
    
contains

    recursive function factorial(n) result(fact)
        integer, intent(in) :: n
        integer :: fact
        
        if (n <= 1) then
            fact = 1
        else
            fact = n * factorial(n - 1)
        end if
    end function factorial

    function fibonacci(n) result(fib)
        integer, intent(in) :: n
        integer :: fib
        integer :: i, a, b, temp
        
        if (n <= 0) then
            fib = 0
        else if (n == 1) then
            fib = 1
        else
            a = 0
            b = 1
            do i = 2, n
                temp = a + b
                a = b
                b = temp
            end do
            fib = b
        end if
    end function fibonacci

    function is_prime(n) result(prime)
        integer, intent(in) :: n
        logical :: prime
        integer :: i
        
        if (n <= 1) then
            prime = .false.
        else if (n <= 3) then
            prime = .true.
        else if (mod(n, 2) == 0 .or. mod(n, 3) == 0) then
            prime = .false.
        else
            prime = .true.
            i = 5
            do while (i * i <= n)
                if (mod(n, i) == 0 .or. mod(n, i + 2) == 0) then
                    prime = .false.
                    exit
                end if
                i = i + 6
            end do
        end if
    end function is_prime

end module demo_math