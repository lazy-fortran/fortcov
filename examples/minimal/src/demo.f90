module demo
    implicit none
    private
    public :: add
contains
    integer function add(a, b)
        integer, intent(in) :: a, b
        add = a + b
    end function add
end module demo

