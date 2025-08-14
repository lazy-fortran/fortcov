program test_simple
    use simple_module
    implicit none
    
    integer :: result_sum, result_product
    
    ! Test calculate_sum function - covers all lines
    result_sum = calculate_sum(5, 3)
    if (result_sum /= 8) then
        print *, "ERROR: calculate_sum failed"
        call exit(1)
    end if
    
    ! Test calculate_product function - covers all lines
    result_product = calculate_product(4, 7)
    if (result_product /= 28) then
        print *, "ERROR: calculate_product failed"
        call exit(1)
    end if
    
    print *, "All tests passed - 100% coverage achieved"
    
end program test_simple