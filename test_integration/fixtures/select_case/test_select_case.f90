program test_select_case
    use select_case_module
    implicit none
    
    character(len=20) :: result
    character(len=10) :: category
    
    ! Test some cases but not all - creates partial branch coverage
    
    ! Test case 1
    result = process_value(1)
    if (trim(result) /= "one") then
        print *, "ERROR: process_value(1) failed"
        call exit(1)
    end if
    
    ! Test case 2:5 range
    result = process_value(3)
    if (trim(result) /= "few") then
        print *, "ERROR: process_value(3) failed"
        call exit(1)
    end if
    
    ! Test case 11: range
    result = process_value(15)
    if (trim(result) /= "many") then
        print *, "ERROR: process_value(15) failed"
        call exit(1)
    end if
    
    ! Note: We intentionally do NOT test:
    ! - case (6:10) "several"
    ! - case default "none"
    ! This creates partial coverage for select case
    
    ! Test categorize_number with some cases
    call categorize_number(50, category)
    if (trim(category) /= "small") then
        print *, "ERROR: categorize_number(50) failed"
        call exit(1)
    end if
    
    call categorize_number(500, category)
    if (trim(category) /= "medium") then
        print *, "ERROR: categorize_number(500) failed"
        call exit(1)
    end if
    
    ! Note: We intentionally do NOT test:
    ! - negative numbers
    ! - zero
    ! - large numbers (>1000)
    ! This creates partial branch coverage
    
    print *, "Test passed - partial select case coverage achieved"
    
end program test_select_case