program test_basic
    use test_module
    implicit none
    integer :: result
    result = add_numbers(2, 3)
    if (result == 5) then
        print *, "Test passed"
        stop 0
    else
        print *, "Test failed"
        stop 1
    end if
end program test_basic
