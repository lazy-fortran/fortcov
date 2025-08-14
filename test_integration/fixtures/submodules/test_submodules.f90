program test_submodules
    use parent_module
    implicit none
    
    real :: area_result, volume_result
    
    ! Test calculate_area function from submodule
    area_result = calculate_area(5.0, 3.0)
    if (abs(area_result - 15.0) > 0.001) then
        print *, "ERROR: calculate_area failed"
        call exit(1)
    end if
    
    ! Test calculate_volume function from submodule
    volume_result = calculate_volume(4.0, 2.0, 3.0)
    if (abs(volume_result - 24.0) > 0.001) then
        print *, "ERROR: calculate_volume failed"
        call exit(1)
    end if
    
    ! Test edge case with negative values
    area_result = calculate_area(-2.0, 3.0)
    if (abs(area_result - 0.0) > 0.001) then
        print *, "ERROR: negative area handling failed"
        call exit(1)
    end if
    
    print *, "Test passed - submodules covered"
    
end program test_submodules