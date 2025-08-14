program test_uncovered
    use module_with_uncovered
    implicit none
    
    integer :: test_value
    
    ! Test only the used_procedure - leaving unused_procedure uncovered
    test_value = 5
    call used_procedure(test_value)
    
    if (test_value /= 10) then
        print *, "ERROR: used_procedure failed"
        call exit(1)
    end if
    
    ! Note: unused_procedure is intentionally not called
    ! This should result in partial coverage
    
    print *, "Test passed - unused_procedure remains uncovered"
    
end program test_uncovered