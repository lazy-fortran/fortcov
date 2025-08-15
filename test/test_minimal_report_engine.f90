program test_minimal_report_engine
    use report_engine
    implicit none
    
    type(report_engine_t) :: engine
    logical :: success
    character(len=:), allocatable :: error_msg
    
    print *, "Testing minimal report engine initialization..."
    
    ! This should not crash
    call engine%init(success, error_msg)
    
    if (success) then
        print *, "PASS: Report engine initialized successfully"
        stop 0
    else
        print *, "FAIL: Report engine initialization failed: ", error_msg
        stop 1
    end if
    
end program test_minimal_report_engine