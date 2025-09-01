program test_issue_996_memory_allocation_standardization
    !! Standardization tests for Issue #996: Inconsistent memory allocation handling
    use safe_allocation
    use coverage_model_core, only: coverage_line_t
    implicit none

    integer, allocatable :: int_arr(:)
    character(len=:), allocatable :: char_arr(:)
    type(coverage_line_t), allocatable :: lines(:)
    logical :: ok
    integer :: passed, failed

    passed = 0
    failed = 0

    print *, '========================================================='
    print *, ' Issue #996: Memory Allocation Standardization Tests'
    print *, '========================================================='

    ! Test 1: Integer allocation positive size
    call safe_allocate_int_array(int_arr, 5, ok)
    if (ok .and. allocated(int_arr) .and. size(int_arr) == 5) then
        print *, '  PASS: Integer allocation (5)'
        passed = passed + 1
    else
        print *, '  FAIL: Integer allocation (5)'
        failed = failed + 1
    end if

    ! Test 2: Integer allocation zero size
    call safe_deallocate_int_array(int_arr, ok)
    call safe_allocate_int_array(int_arr, 0, ok)
    if (ok .and. allocated(int_arr) .and. size(int_arr) == 0) then
        print *, '  PASS: Integer allocation (0)'
        passed = passed + 1
    else
        print *, '  FAIL: Integer allocation (0)'
        failed = failed + 1
    end if

    ! Test 3: Integer allocation negative size should fail
    call safe_deallocate_int_array(int_arr, ok)
    call safe_allocate_int_array(int_arr, -1, ok)
    if (.not. ok) then
        print *, '  PASS: Integer allocation (-1) rejected'
        passed = passed + 1
    else
        print *, '  FAIL: Integer allocation (-1) accepted (BUG)'
        failed = failed + 1
    end if

    ! Test 4: Lines allocation zero size
    call safe_allocate_lines_array(lines, 0, ok)
    if (ok .and. allocated(lines) .and. size(lines) == 0) then
        print *, '  PASS: Lines allocation (0)'
        passed = passed + 1
    else
        print *, '  FAIL: Lines allocation (0)'
        failed = failed + 1
    end if

    ! Test 5: Lines allocation positive size
    if (allocated(lines)) deallocate(lines)
    call safe_allocate_lines_array(lines, 3, ok)
    if (ok .and. allocated(lines) .and. size(lines) == 3) then
        print *, '  PASS: Lines allocation (3)'
        passed = passed + 1
    else
        print *, '  FAIL: Lines allocation (3)'
        failed = failed + 1
    end if

    print *, '---------------------------------------------------------'
    print *, ' Tests passed:', passed
    print *, ' Tests failed:', failed
    if (failed == 0) then
        print *, ' MEMORY ALLOCATION STANDARDIZATION: OK'
    else
        print *, ' MEMORY ALLOCATION STANDARDIZATION: FAILURES'
    end if

end program test_issue_996_memory_allocation_standardization
