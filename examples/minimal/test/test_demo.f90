program test_demo
    use demo, only: add
    implicit none
    if (add(1, 1) /= 2) then
        print *, 'FAIL: add(1,1) /= 2'
        stop 1
    end if
    print *, 'OK: add works'
end program test_demo

