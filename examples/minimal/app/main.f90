program main
    use demo, only: add
    implicit none
    integer :: x
    x = add(2, 3)
    if (x /= 5) then
        print *, 'unexpected'
        stop 1
    end if
    print *, 'demo ok: ', x
end program main

