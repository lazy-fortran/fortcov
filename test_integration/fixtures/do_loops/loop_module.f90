module loop_module
    implicit none
    private
    
    public :: test_do_loop
    public :: test_do_while
    public :: test_do_concurrent
    public :: test_named_loop
    
contains
    
    subroutine test_do_loop(array, n)
        integer, intent(in) :: n
        integer, intent(inout) :: array(n)
        integer :: i
        
        ! Standard do loop
        do i = 1, n
            array(i) = array(i) * 2
        end do
    end subroutine test_do_loop
    
    subroutine test_do_while(sum_result)
        integer, intent(out) :: sum_result
        integer :: i
        
        ! Do while loop
        sum_result = 0
        i = 1
        do while (i <= 10)
            sum_result = sum_result + i
            i = i + 1
        end do
    end subroutine test_do_while
    
    subroutine test_do_concurrent(array, n)
        integer, intent(in) :: n
        integer, intent(inout) :: array(n)
        integer :: i
        
        ! Do concurrent loop (modern Fortran)
        do concurrent (i = 1:n)
            array(i) = array(i) + 10
        end do
    end subroutine test_do_concurrent
    
    subroutine test_named_loop(matrix, rows, cols)
        integer, intent(in) :: rows, cols
        integer, intent(inout) :: matrix(rows, cols)
        integer :: i, j
        
        ! Named nested loops
        outer_loop: do i = 1, rows
            inner_loop: do j = 1, cols
                if (matrix(i, j) < 0) then
                    cycle inner_loop
                end if
                if (matrix(i, j) > 100) then
                    exit outer_loop
                end if
                matrix(i, j) = matrix(i, j) + 1
            end do inner_loop
        end do outer_loop
    end subroutine test_named_loop
    
end module loop_module