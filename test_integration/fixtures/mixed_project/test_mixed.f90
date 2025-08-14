program test_mixed
    use math_utils
    use string_utils
    use io_utils
    implicit none
    
    integer :: sum_result, product_result
    character(len=20) :: str_result, reversed
    
    ! Test math_utils (partial coverage - unused_divide not called)
    sum_result = add_numbers(10, 5)
    if (sum_result /= 15) then
        print *, "ERROR: add_numbers failed"
        call exit(1)
    end if
    
    product_result = multiply_numbers(4, 7)
    if (product_result /= 28) then
        print *, "ERROR: multiply_numbers failed"
        call exit(1)
    end if
    
    ! Note: unused_divide is intentionally not called
    
    ! Test string_utils (full coverage)
    str_result = concatenate_strings("Hello", "World")
    if (trim(str_result) /= "HelloWorld") then
        print *, "ERROR: concatenate_strings failed"
        call exit(1)
    end if
    
    reversed = reverse_string("test")
    if (trim(reversed) /= "tset") then
        print *, "ERROR: reverse_string failed"
        call exit(1)
    end if
    
    ! Test io_utils (partial coverage - unused_read_file not called)
    call write_message("Integration test successful")
    
    ! Note: unused_read_file is intentionally not called
    
    print *, "Test passed - mixed project with varying coverage"
    
end program test_mixed