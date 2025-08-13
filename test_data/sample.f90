program sample
    implicit none
    integer :: i, result
    
    result = 0
    
    do i = 1, 10
        if (i <= 5) then
            result = result + i
        else
            result = result + i * 2
        end if
    end do
    
    write(*,*) "Result:", result
end program sample