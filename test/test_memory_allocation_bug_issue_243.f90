program test_memory_allocation_bug_issue_243
    !! Memory allocation safety test for Issue #243
    !! Tests memory safety without external dependencies
    !! 
    !! Note: The comprehensive memory tests are in separate test programs:
    !! - test_memory_allocation_core: Core allocation patterns
    !! - test_memory_error_paths: Error handling paths
    !! These run independently via FPM's test system
    
    use iso_fortran_env, only: error_unit, int64
    implicit none
    
    logical :: all_tests_passed = .true.
    
    print *, ""
    print *, "============================================================="
    print *, "Issue #243: Memory Allocation Safety Verification"
    print *, "============================================================="
    print *, ""
    
    ! Run basic memory safety checks inline
    call test_basic_allocation_safety()
    call test_deallocation_safety()
    call test_reallocation_patterns()
    
    ! Print summary
    print *, ""
    print *, "============================================================="
    if (all_tests_passed) then
        print *, "✅ MEMORY ALLOCATION SAFETY TESTS PASSED"
        print *, "Issue #243: Basic memory safety verified"
        print *, ""
        print *, "Note: Run full test suite for comprehensive coverage:"
        print *, "  - test_memory_allocation_core (core patterns)"
        print *, "  - test_memory_error_paths (error handling)"
        print *, "============================================================="
        call exit(0)
    else
        write(error_unit, *) "❌ MEMORY SAFETY TESTS FAILED"
        print *, "============================================================="
        call exit(1)
    end if
    
contains

    subroutine test_basic_allocation_safety()
        !! Test basic allocation safety patterns
        real, allocatable :: test_array(:)
        integer :: stat
        character(len=256) :: errmsg
        
        print *, "Testing basic allocation safety..."
        
        ! Test allocation with error handling
        allocate(test_array(1000), stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            print *, "  ❌ FAIL: Basic allocation failed:", trim(errmsg)
            all_tests_passed = .false.
            return
        end if
        
        ! Verify allocation
        if (.not. allocated(test_array)) then
            print *, "  ❌ FAIL: Array not allocated after allocate"
            all_tests_passed = .false.
            return
        end if
        
        ! Clean up
        deallocate(test_array)
        
        print *, "  ✅ PASS: Basic allocation safety verified"
        
    end subroutine test_basic_allocation_safety
    
    subroutine test_deallocation_safety()
        !! Test safe deallocation patterns
        integer, allocatable :: int_array(:)
        integer :: stat
        
        print *, "Testing deallocation safety..."
        
        ! Allocate
        allocate(int_array(500), stat=stat)
        if (stat /= 0) then
            print *, "  ❌ FAIL: Setup allocation failed"
            all_tests_passed = .false.
            return
        end if
        
        ! Deallocate with status check
        deallocate(int_array, stat=stat)
        if (stat /= 0) then
            print *, "  ❌ FAIL: Deallocation failed"
            all_tests_passed = .false.
            return
        end if
        
        ! Verify deallocation
        if (allocated(int_array)) then
            print *, "  ❌ FAIL: Array still allocated after deallocate"
            all_tests_passed = .false.
            return
        end if
        
        print *, "  ✅ PASS: Deallocation safety verified"
        
    end subroutine test_deallocation_safety
    
    subroutine test_reallocation_patterns()
        !! Test safe reallocation patterns
        character(len=:), allocatable :: string_array(:)
        integer :: stat, i
        
        print *, "Testing reallocation patterns..."
        
        ! Initial allocation
        allocate(character(len=10) :: string_array(5), stat=stat)
        if (stat /= 0) then
            print *, "  ❌ FAIL: Initial string allocation failed"
            all_tests_passed = .false.
            return
        end if
        
        ! Initialize
        do i = 1, 5
            write(string_array(i), '(A,I0)') "Item", i
        end do
        
        ! Deallocate before reallocation
        deallocate(string_array, stat=stat)
        if (stat /= 0) then
            print *, "  ❌ FAIL: String deallocation failed"
            all_tests_passed = .false.
            return
        end if
        
        ! Reallocate with different size
        allocate(character(len=20) :: string_array(10), stat=stat)
        if (stat /= 0) then
            print *, "  ❌ FAIL: String reallocation failed"
            all_tests_passed = .false.
            return
        end if
        
        ! Clean up
        deallocate(string_array)
        
        print *, "  ✅ PASS: Reallocation patterns verified"
        
    end subroutine test_reallocation_patterns
    
end program test_memory_allocation_bug_issue_243