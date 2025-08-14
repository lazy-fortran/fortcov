module simple_module
    implicit none
    private
    
    public :: calculate_sum
    public :: calculate_product
    
contains
    
    function calculate_sum(a, b) result(sum_result)
        integer, intent(in) :: a, b
        integer :: sum_result
        
        sum_result = a + b
    end function calculate_sum
    
    function calculate_product(a, b) result(product_result)
        integer, intent(in) :: a, b
        integer :: product_result
        
        product_result = a * b
    end function calculate_product
    
end module simple_module