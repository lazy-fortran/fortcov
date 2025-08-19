! Demo calculator module for coverage testing
module demo_calculator
    implicit none
    private
    
    public :: add_numbers, multiply_numbers, divide_numbers
    
contains

    pure function add_numbers(a, b) result(sum_result)
        real, intent(in) :: a, b
        real :: sum_result
        sum_result = a + b
    end function add_numbers

    pure function multiply_numbers(a, b) result(product_result)
        real, intent(in) :: a, b
        real :: product_result
        product_result = a * b
    end function multiply_numbers

    function divide_numbers(a, b) result(division_result)
        real, intent(in) :: a, b
        real :: division_result
        
        if (abs(b) < 1.0e-10) then
            division_result = 0.0  ! Simple error handling
        else
            division_result = a / b
        end if
    end function divide_numbers

end module demo_calculator