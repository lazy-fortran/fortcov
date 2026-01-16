program test_memory_allocation_safety
    !! Memory allocation safety patterns.
    !! Regression test for issue 243.

    use, intrinsic :: iso_fortran_env, only: dp => real64, error_unit, output_unit
    implicit none

    character(len=*), parameter :: separator = repeat('=', 70)
    logical :: all_tests_passed = .true.

    write (output_unit, '(A)') ''
    write (output_unit, '(A)') separator
    write (output_unit, '(A)') 'Memory Allocation Safety Verification'
    write (output_unit, '(A)') separator
    write (output_unit, '(A)') ''

    ! Run basic memory safety checks inline
    call test_basic_allocation_safety()
    call test_deallocation_safety()
    call test_reallocation_patterns()

    ! Print summary
    write (output_unit, '(A)') ''
    write (output_unit, '(A)') separator
    if (all_tests_passed) then
        write (output_unit, '(A)') 'MEMORY ALLOCATION SAFETY TESTS PASSED'
        write (output_unit, '(A)') separator
        stop 0
    else
        write (error_unit, '(A)') 'MEMORY SAFETY TESTS FAILED'
        write (output_unit, '(A)') separator
        stop 1
    end if

contains

    subroutine test_basic_allocation_safety()
        !! Test basic allocation safety patterns
        real(dp), allocatable :: test_array(:)
        integer :: stat
        character(len=256) :: errmsg

        write (output_unit, '(A)') 'Testing basic allocation safety...'

        ! Test allocation with error handling
        allocate (test_array(1000), stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            write (output_unit, '(A,A)') '  failed: ', trim(errmsg)
            all_tests_passed = .false.
            return
        end if

        ! Verify allocation
        if (.not. allocated(test_array)) then
            write (output_unit, '(A)') '  FAIL: Array not allocated after allocate'
            all_tests_passed = .false.
            return
        end if

        ! Clean up
        deallocate (test_array)

        write (output_unit, '(A)') '  PASS: Basic allocation safety verified'

    end subroutine test_basic_allocation_safety

    subroutine test_deallocation_safety()
        !! Test safe deallocation patterns
        integer, allocatable :: int_array(:)
        integer :: stat

        write (output_unit, '(A)') 'Testing deallocation safety...'

        ! Allocate
        allocate (int_array(500), stat=stat)
        if (stat /= 0) then
            write (output_unit, '(A)') '  failed'
            all_tests_passed = .false.
            return
        end if

        ! Deallocate with status check
        deallocate (int_array, stat=stat)
        if (stat /= 0) then
            write (output_unit, '(A)') '  failed'
            all_tests_passed = .false.
            return
        end if

        ! Verify deallocation
        if (allocated(int_array)) then
            write (output_unit, '(A)') '  FAIL: Array still allocated after deallocate'
            all_tests_passed = .false.
            return
        end if

        write (output_unit, '(A)') '  PASS: Deallocation safety verified'

    end subroutine test_deallocation_safety

    subroutine test_reallocation_patterns()
        !! Test safe reallocation patterns
        character(len=:), allocatable :: string_array(:)
        integer :: stat, i

        write (output_unit, '(A)') 'Testing reallocation patterns...'

        ! Initial allocation
        allocate (character(len=10) :: string_array(5), stat=stat)
        if (stat /= 0) then
            write (output_unit, '(A)') '  failed'
            all_tests_passed = .false.
            return
        end if

        ! Initialize
        do i = 1, 5
            write (string_array(i), '(A,I0)') 'Item', i
        end do

        ! Deallocate before reallocation
        deallocate (string_array, stat=stat)
        if (stat /= 0) then
            write (output_unit, '(A)') '  failed'
            all_tests_passed = .false.
            return
        end if

        ! Reallocate with different size
        allocate (character(len=20) :: string_array(10), stat=stat)
        if (stat /= 0) then
            write (output_unit, '(A)') '  failed'
            all_tests_passed = .false.
            return
        end if

        ! Clean up
        deallocate (string_array)

        write (output_unit, '(A)') '  PASS: Reallocation patterns verified'

    end subroutine test_reallocation_patterns

end program test_memory_allocation_safety
