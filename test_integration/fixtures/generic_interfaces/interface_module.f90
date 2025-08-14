module interface_module
    implicit none
    private
    
    public :: compute
    
    interface compute
        module procedure compute_integer
        module procedure compute_real
    end interface compute
    
contains
    
    function compute_integer(x, y) result(z)
        integer, intent(in) :: x, y
        integer :: z
        
        z = x + y
    end function compute_integer
    
    function compute_real(x, y) result(z)
        real, intent(in) :: x, y
        real :: z
        
        z = x * y
    end function compute_real
    
end module interface_module