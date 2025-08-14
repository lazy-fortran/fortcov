program test_interfaces
    use interface_module
    implicit none
    
    integer :: int_result
    real :: real_result
    
    ! Test generic interface with integer arguments
    int_result = compute(5, 3)
    if (int_result /= 8) then
        print *, "ERROR: compute(integer) failed"
        call exit(1)
    end if
    
    ! Test generic interface with real arguments
    real_result = compute(2.5, 4.0)
    if (abs(real_result - 10.0) > 0.001) then
        print *, "ERROR: compute(real) failed"
        call exit(1)
    end if
    
    print *, "Test passed - generic interfaces covered"
    
end program test_interfaces