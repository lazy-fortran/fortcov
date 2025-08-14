program test_loops
    use loop_module
    implicit none
    
    integer, parameter :: n = 5
    integer :: array(n) = [1, 2, 3, 4, 5]
    integer :: sum_result
    integer :: matrix(3, 3)
    integer :: i, j
    
    ! Test standard do loop
    call test_do_loop(array, n)
    if (array(1) /= 2 .or. array(5) /= 10) then
        print *, "ERROR: test_do_loop failed"
        call exit(1)
    end if
    
    ! Test do while loop
    call test_do_while(sum_result)
    if (sum_result /= 55) then  ! Sum of 1 to 10
        print *, "ERROR: test_do_while failed"
        call exit(1)
    end if
    
    ! Test do concurrent
    array = [1, 2, 3, 4, 5]  ! Reset array
    call test_do_concurrent(array, n)
    if (array(1) /= 11 .or. array(5) /= 15) then
        print *, "ERROR: test_do_concurrent failed"
        call exit(1)
    end if
    
    ! Test named loops with various values
    ! Initialize matrix
    do i = 1, 3
        do j = 1, 3
            matrix(i, j) = i * j
        end do
    end do
    
    call test_named_loop(matrix, 3, 3)
    if (matrix(1, 1) /= 2) then  ! 1*1 + 1 = 2
        print *, "ERROR: test_named_loop failed"
        call exit(1)
    end if
    
    print *, "Test passed - various do loop constructs covered"
    
end program test_loops