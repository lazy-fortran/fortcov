module test_module
    implicit none
contains
    function add_numbers(a, b) result(sum)
        integer, intent(in) :: a, b
        integer :: sum
        sum = a + b
    end function add_numbers
end module test_module
