module array_module
    implicit none
    private
    
    public :: test_array_assignment
    public :: test_array_constructor
    public :: test_where_construct
    public :: test_array_slicing
    
contains
    
    subroutine test_array_assignment(arr1, arr2, n)
        integer, intent(in) :: n
        integer, intent(in) :: arr1(n)
        integer, intent(out) :: arr2(n)
        
        ! Whole array assignment
        arr2 = arr1 * 2
        
        ! Element-wise operations
        arr2 = arr2 + 1
    end subroutine test_array_assignment
    
    function test_array_constructor() result(result_array)
        integer :: result_array(5)
        integer :: i
        
        ! Array constructor with implied do
        result_array = [(i*i, i = 1, 5)]
    end function test_array_constructor
    
    subroutine test_where_construct(array, n)
        integer, intent(in) :: n
        real, intent(inout) :: array(n)
        
        ! Where construct for conditional array operations
        where (array > 0.0)
            array = sqrt(array)
        elsewhere (array < -10.0)
            array = -1.0
        elsewhere
            array = 0.0
        end where
    end subroutine test_where_construct
    
    subroutine test_array_slicing(matrix, rows, cols)
        integer, intent(in) :: rows, cols
        real, intent(inout) :: matrix(rows, cols)
        integer :: i
        
        ! Array slicing and sections
        matrix(1, :) = 1.0                    ! First row
        matrix(:, 1) = 2.0                    ! First column
        matrix(2:rows-1, 2:cols-1) = 3.0      ! Interior elements
        
        ! Stride operations
        do i = 1, rows, 2
            matrix(i, ::2) = matrix(i, ::2) + 10.0
        end do
    end subroutine test_array_slicing
    
end module array_module