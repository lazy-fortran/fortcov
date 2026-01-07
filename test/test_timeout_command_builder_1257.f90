program test_timeout_command_builder_1257
    !! Test for Issue #1257: timeout command selection is portable.

    use, intrinsic :: iso_fortran_env, only: output_unit
    use test_executor_core, only: build_timeout_command
    implicit none

    integer :: tests = 0
    integer :: passed = 0

    call test_wraps_timeout_command()
    call test_fallback_without_timeout()
    call test_disable_timeout_seconds()

    write (output_unit, '(A,I0,A,I0,A)') 'Test Results: ', passed, ' / ', &
        tests, ' passed'
    if (passed /= tests) stop 1

contains

    subroutine test_wraps_timeout_command()
        character(len=:), allocatable :: actual
        character(len=*), parameter :: expected = &
                                       'timeout -k 5 120 fpm test'

        tests = tests + 1
        actual = build_timeout_command('fpm test', 120, 'timeout', .true.)
        call assert_equal(expected, actual, 'wraps with timeout when available')
    end subroutine test_wraps_timeout_command

    subroutine test_fallback_without_timeout()
        character(len=:), allocatable :: actual
        character(len=*), parameter :: expected = 'fpm test'

        tests = tests + 1
        actual = build_timeout_command('fpm test', 120, '', .false.)
        call assert_equal(expected, actual, 'runs without timeout when missing')
    end subroutine test_fallback_without_timeout

    subroutine test_disable_timeout_seconds()
        character(len=:), allocatable :: actual
        character(len=*), parameter :: expected = 'ctest'

        tests = tests + 1
        actual = build_timeout_command('ctest', 0, 'timeout', .true.)
        call assert_equal(expected, actual, 'runs without timeout when disabled')
    end subroutine test_disable_timeout_seconds

    subroutine assert_equal(expected, actual, label)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: label

        if (trim(actual) == trim(expected)) then
            passed = passed + 1
            write (output_unit, '(A)') '  [PASS] '//trim(label)
        else
            write (output_unit, '(A)') '  [FAIL] '//trim(label)
            write (output_unit, '(A)') '    Expected: '//trim(expected)
            write (output_unit, '(A)') '    Actual:   '//trim(actual)
        end if
    end subroutine assert_equal

end program test_timeout_command_builder_1257
