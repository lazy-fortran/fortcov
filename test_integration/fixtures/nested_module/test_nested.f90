program test_nested
    use nested_module
    implicit none
    
    integer :: test_value, result_val
    
    ! Test module level function
    result_val = module_level_function(5)
    if (result_val /= 6) then
        print *, "ERROR: module_level_function failed"
        call exit(1)
    end if
    
    ! Test outer procedure which exercises nested contains blocks
    test_value = 4
    call outer_procedure(test_value)
    
    ! Expected calculation:
    ! inner_procedure: 4 * 3 = 12
    ! nested_function: 10 / 2 = 5
    ! deeply_nested_procedure: +1 inside function
    ! final: 12 + 5 = 17
    if (test_value /= 17) then
        print *, "ERROR: Expected 17, got ", test_value
        call exit(1)
    end if
    
    print *, "Test passed - nested contains blocks covered"
    
end program test_nested