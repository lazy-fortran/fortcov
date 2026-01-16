program test_discovery_deduplicate_files
    !! Coverage file list deduplication during discovery.
    !! Regression test for issue 1238.
    use coverage_workflows_discovery, only: deduplicate_files
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    implicit none

    integer :: tests = 0
    integer :: passed = 0

    write (output_unit, '(A)') ''
    write (output_unit, '(A)') '=============================================='
    write (output_unit, '(A)') 'Issue #1238: Duplicate File Deduplication Test'
    write (output_unit, '(A)') '=============================================='
    write (output_unit, '(A)') ''

    call test_exact_duplicates()
    call test_no_duplicates()
    call test_empty_input()
    call test_single_file()
    call test_multiple_duplicates()

    write (output_unit, '(A)') ''
    write (output_unit, '(A,I0,A,I0,A)') 'Test Results: ', passed, ' / ', &
        tests, ' passed'

    if (passed /= tests) then
        write (error_unit, '(A)') 'TESTS FAILED'
        stop 1
    end if

    write (output_unit, '(A)') 'ALL TESTS PASSED'
    stop 0

contains

    subroutine test_exact_duplicates()
        character(len=:), allocatable :: input(:), output(:)

        tests = tests + 1
        write (output_unit, '(A)') 'Testing exact duplicate removal...'

        allocate (character(len=256) :: input(3))
        input(1) = 'demo.f90.gcov'
        input(2) = 'demo.f90.gcov'
        input(3) = 'other.f90.gcov'

        call deduplicate_files(input, output)

        if (allocated(output)) then
            if (size(output) == 2) then
                passed = passed + 1
                write (output_unit, '(A)') '  [PASS] Exact duplicates removed'
            else
                write (output_unit, '(A)') '  [FAIL] Expected 2 files after dedup'
                write (output_unit, '(A,I0)') '    Got: ', size(output)
            end if
        else
            write (output_unit, '(A)') '  [FAIL] Expected 2 files after dedup'
            write (output_unit, '(A)') '    Output not allocated'
        end if
    end subroutine test_exact_duplicates

    subroutine test_no_duplicates()
        character(len=:), allocatable :: input(:), output(:)

        tests = tests + 1
        write (output_unit, '(A)') 'Testing no duplicates case...'

        allocate (character(len=256) :: input(3))
        input(1) = 'file1.gcov'
        input(2) = 'file2.gcov'
        input(3) = 'file3.gcov'

        call deduplicate_files(input, output)

        if (allocated(output)) then
            if (size(output) == 3) then
                passed = passed + 1
                write (output_unit, '(A)') '  [PASS] All unique files preserved'
            else
                write (output_unit, '(A)') '  [FAIL] Expected 3 unique files'
            end if
        else
            write (output_unit, '(A)') '  [FAIL] Expected 3 unique files'
        end if
    end subroutine test_no_duplicates

    subroutine test_empty_input()
        character(len=:), allocatable :: input(:), output(:)

        tests = tests + 1
        write (output_unit, '(A)') 'Testing empty input...'

        allocate (character(len=256) :: input(0))

        call deduplicate_files(input, output)

        if (allocated(output)) then
            if (size(output) == 0) then
                passed = passed + 1
                write (output_unit, '(A)') '  [PASS] Empty input handled correctly'
            else
                write (output_unit, '(A)') '  [FAIL] Empty input not handled correctly'
            end if
        else
            write (output_unit, '(A)') '  [FAIL] Empty input not handled correctly'
        end if
    end subroutine test_empty_input

    subroutine test_single_file()
        character(len=:), allocatable :: input(:), output(:)

        tests = tests + 1
        write (output_unit, '(A)') 'Testing single file...'

        allocate (character(len=256) :: input(1))
        input(1) = 'single.gcov'

        call deduplicate_files(input, output)

        if (allocated(output)) then
            if (size(output) == 1) then
                passed = passed + 1
                write (output_unit, '(A)') '  [PASS] Single file preserved'
            else
                write (output_unit, '(A)') '  [FAIL] Single file not preserved'
            end if
        else
            write (output_unit, '(A)') '  [FAIL] Single file not preserved'
        end if
    end subroutine test_single_file

    subroutine test_multiple_duplicates()
        character(len=:), allocatable :: input(:), output(:)

        tests = tests + 1
        write (output_unit, '(A)') 'Testing multiple duplicates of same file...'

        allocate (character(len=256) :: input(5))
        input(1) = 'same.gcov'
        input(2) = 'same.gcov'
        input(3) = 'same.gcov'
        input(4) = 'same.gcov'
        input(5) = 'same.gcov'

        call deduplicate_files(input, output)

        if (allocated(output)) then
            if (size(output) == 1) then
                passed = passed + 1
                write (output_unit, '(A)') '  [PASS] Multiple duplicates reduced to one'
            else
                write (output_unit, '(A)') '  [FAIL] Expected 1 file after dedup'
                write (output_unit, '(A,I0)') '    Got: ', size(output)
            end if
        else
            write (output_unit, '(A)') '  [FAIL] Expected 1 file after dedup'
            write (output_unit, '(A)') '    Output not allocated'
        end if
    end subroutine test_multiple_duplicates

end program test_discovery_deduplicate_files
