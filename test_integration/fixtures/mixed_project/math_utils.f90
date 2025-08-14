module math_utils
    implicit none
    private
    
    public :: add_numbers
    public :: multiply_numbers
    public :: unused_divide  ! This will remain uncovered
    
contains
    
    function add_numbers(a, b) result(sum)
        integer, intent(in) :: a, b
        integer :: sum
        
        sum = a + b
    end function add_numbers
    
    function multiply_numbers(a, b) result(product)
        integer, intent(in) :: a, b
        integer :: product
        
        product = a * b
    end function multiply_numbers
    
    function unused_divide(a, b) result(quotient)
        integer, intent(in) :: a, b
        real :: quotient
        
        ! This function is never called - creates uncovered code
        if (b /= 0) then
            quotient = real(a) / real(b)
        else
            quotient = 0.0
        end if
    end function unused_divide
    
end module math_utils