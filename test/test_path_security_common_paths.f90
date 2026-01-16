program test_path_security_common_paths
    !! Path security should not block common local workflows.
    !! Regression test for issue 1261.

    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use error_handling_core, only: error_context_t, ERROR_INVALID_PATH, &
                                   ERROR_SUCCESS
    use path_security, only: validate_path_security
    implicit none

    integer :: tests = 0
    integer :: passed = 0

    call test_allows_relative_parent_paths()
    call test_allows_absolute_paths()
    call test_allows_windows_like_paths()
    call test_rejects_empty_path()
    call test_rejects_overlong_path()

    write (output_unit, '(A,I0,A,I0,A)') 'Test Results: ', passed, ' / ', &
        tests, ' passed'
    if (passed /= tests) then
        write (error_unit, '(A)') 'TESTS FAILED'
        stop 1
    end if

contains

    subroutine test_allows_relative_parent_paths()
        character(len=:), allocatable :: safe
        type(error_context_t) :: ctx

        tests = tests + 1

        call validate_path_security('../src', safe, ctx)
        if (ctx%error_code == ERROR_SUCCESS .and. trim(safe) == '../src') then
            passed = passed + 1
            write (output_unit, '(A)') '  [PASS] allows ../ relative paths'
        else
            write (output_unit, '(A)') '  [FAIL] rejects ../ relative paths'
            write (output_unit, '(A,I0)') '    Code: ', ctx%error_code
            write (output_unit, '(A,A)') '    Msg:  ', trim(ctx%message)
        end if
    end subroutine test_allows_relative_parent_paths

    subroutine test_allows_absolute_paths()
        character(len=:), allocatable :: safe
        type(error_context_t) :: ctx

        tests = tests + 1

        call validate_path_security('/home/user/project/src/main.f90', safe, ctx)
        if (ctx%error_code == ERROR_SUCCESS) then
            passed = passed + 1
            write (output_unit, '(A)') '  [PASS] allows / absolute paths'
        else
            write (output_unit, '(A)') '  [FAIL] rejects / absolute paths'
            write (output_unit, '(A,I0)') '    Code: ', ctx%error_code
            write (output_unit, '(A,A)') '    Msg:  ', trim(ctx%message)
        end if
    end subroutine test_allows_absolute_paths

    subroutine test_allows_windows_like_paths()
        character(len=:), allocatable :: safe
        character(len=:), allocatable :: path
        type(error_context_t) :: ctx
        character(len=1), parameter :: backslash = achar(92)

        tests = tests + 1

        path = 'C:'//backslash//'Users'//backslash//'me'//backslash// &
               'project'//backslash//'src'
        call validate_path_security(path, safe, ctx)
        if (ctx%error_code == ERROR_SUCCESS) then
            passed = passed + 1
            write (output_unit, '(A)') '  [PASS] allows Windows-like paths'
        else
            write (output_unit, '(A)') '  [FAIL] rejects Windows-like paths'
            write (output_unit, '(A,I0)') '    Code: ', ctx%error_code
            write (output_unit, '(A,A)') '    Msg:  ', trim(ctx%message)
        end if
    end subroutine test_allows_windows_like_paths

    subroutine test_rejects_empty_path()
        character(len=:), allocatable :: safe
        type(error_context_t) :: ctx

        tests = tests + 1

        call validate_path_security('', safe, ctx)
        if (ctx%error_code == ERROR_INVALID_PATH) then
            passed = passed + 1
            write (output_unit, '(A)') '  [PASS] rejects empty path'
        else
            write (output_unit, '(A)') '  [FAIL] empty path not rejected'
            write (output_unit, '(A,I0)') '    Code: ', ctx%error_code
            write (output_unit, '(A,A)') '    Msg:  ', trim(ctx%message)
        end if
    end subroutine test_rejects_empty_path

    subroutine test_rejects_overlong_path()
        character(len=:), allocatable :: safe
        character(len=:), allocatable :: long_path
        type(error_context_t) :: ctx
        integer :: n

        tests = tests + 1

        n = 5000
        allocate (character(len=n) :: long_path)
        long_path = repeat('a', n)

        call validate_path_security(long_path, safe, ctx)
        if (ctx%error_code == ERROR_INVALID_PATH) then
            passed = passed + 1
            write (output_unit, '(A)') '  [PASS] rejects overly long path'
        else
            write (output_unit, '(A)') '  [FAIL] long path not rejected'
            write (output_unit, '(A,I0)') '    Code: ', ctx%error_code
            write (output_unit, '(A,A)') '    Msg:  ', trim(ctx%message)
        end if
    end subroutine test_rejects_overlong_path

end program test_path_security_common_paths

