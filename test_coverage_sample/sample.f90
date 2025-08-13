program sample_coverage
    implicit none
    integer :: x, y
    logical :: condition
    
    ! Line coverage test
    x = 42
    y = 0
    
    ! Branch coverage test
    condition = (x > 40)
    if (condition) then
        y = x + 10
    else
        y = x - 10
    end if
    
    ! Loop coverage test
    do x = 1, 3
        y = y + x
    end do
    
    print *, "Result:", y
end program sample_coverage