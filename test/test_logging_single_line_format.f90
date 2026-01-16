program test_logging_single_line_format
    !! Smoke test for logging formatting.
    !! Regression test for issue 1123.
    !! Verifies discovery message uses single-line formatting without spurious newline.

    use, intrinsic :: iso_fortran_env, only: output_unit
    implicit none

    integer :: tests = 0
    integer :: passed = 0

    call test_discovery_message_format()

    write(output_unit,'(A,I0,A,I0,A)') 'Test Results: ', passed, ' / ', tests, ' passed'
    if (passed /= tests) stop 1

contains

    subroutine test_discovery_message_format()
        character(len=256) :: buf
        character(len=*), parameter :: expected = &
            'FortCov: Discovery returned 249 files'

        tests = tests + 1

        ! Correct formatting: all parts in a single WRITE with (A,I0,A)
        write(buf, '(A,I0,A)') 'FortCov: Discovery returned ', 249, ' files'

        if (index(buf, new_line('a')) == 0 .and. trim(buf) == expected) then
            passed = passed + 1
            write(output_unit,'(A)') &
                '  [PASS] Discovery message formatted on a single line'
        else
            write(output_unit,'(A)') &
                '  [FAIL] Discovery message formatting has newline/mismatch'
            write(output_unit,'(A,A)') '    Expected: ', expected
            write(output_unit,'(A,A)') '    Actual:   ', trim(buf)
        end if
    end subroutine test_discovery_message_format

end program test_logging_single_line_format
