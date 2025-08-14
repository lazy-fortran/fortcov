program test_arrays
    use array_module
    implicit none
    
    integer, parameter :: n = 5
    integer :: arr1(n) = [1, 2, 3, 4, 5]
    integer :: arr2(n)
    integer :: constructed_array(5)
    real :: real_array(n) = [4.0, -2.0, -15.0, 9.0, 0.5]
    real :: matrix(4, 4)
    integer :: i, j
    
    ! Test whole array assignment
    call test_array_assignment(arr1, arr2, n)
    if (arr2(1) /= 3 .or. arr2(5) /= 11) then  ! (1*2)+1=3, (5*2)+1=11
        print *, "ERROR: test_array_assignment failed"
        call exit(1)
    end if
    
    ! Test array constructor
    constructed_array = test_array_constructor()
    if (constructed_array(1) /= 1 .or. constructed_array(3) /= 9) then  ! 1^2=1, 3^2=9
        print *, "ERROR: test_array_constructor failed"
        call exit(1)
    end if
    
    ! Test where construct
    call test_where_construct(real_array, n)
    if (abs(real_array(1) - 2.0) > 0.001) then  ! sqrt(4.0) = 2.0
        print *, "ERROR: test_where_construct failed"
        call exit(1)
    end if
    if (real_array(3) /= -1.0) then  ! -15.0 < -10.0, so becomes -1.0
        print *, "ERROR: test_where_construct failed"
        call exit(1)
    end if
    
    ! Test array slicing
    matrix = 0.0  ! Initialize
    call test_array_slicing(matrix, 4, 4)
    if (matrix(1, 1) /= 12.0) then  ! 1.0 (first row) + 2.0 (first col) + 10.0 (stride) - 1.0 (overlap)
        print *, "ERROR: test_array_slicing failed, got ", matrix(1, 1)
        call exit(1)
    end if
    
    print *, "Test passed - array operations covered"
    
end program test_arrays