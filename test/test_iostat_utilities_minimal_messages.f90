program test_iostat_utilities_minimal_messages
    use, intrinsic :: iso_fortran_env, only: output_unit
    use error_handling_core, only: error_context_t
    use iostat_utilities, only: interpret_iostat_open_error
    implicit none

    integer :: tests
    integer :: passed

    tests = 0
    passed = 0

    call test_open_no_suggestion()
    call test_open_permission_no_suggestion()

    write (output_unit, '(A,I0,A,I0,A)') 'Test Results: ', passed, ' / ', &
        tests, ' passed'
    if (passed /= tests) stop 1

contains

    subroutine test_open_no_suggestion()
        type(error_context_t) :: error_ctx
        character(len=*), parameter :: path = 'missing_file.gcov'

        tests = tests + 1

        call interpret_iostat_open_error(29, path, error_ctx)

        if (len_trim(error_ctx%suggestion) == 0 .and. &
            index(error_ctx%message, 'Could not open file:') > 0) then
            passed = passed + 1
            write (output_unit, '(A)') &
                '  [PASS] Open error has minimal message and no suggestion'
        else
            write (output_unit, '(A)') &
                '  [FAIL] Open error message should be minimal with no suggestion'
            write (output_unit, '(A,A)') '    Message:    ', trim(error_ctx%message)
            write (output_unit, '(A,A)') '    Suggestion: ', trim(error_ctx%suggestion)
        end if
    end subroutine test_open_no_suggestion

    subroutine test_open_permission_no_suggestion()
        type(error_context_t) :: error_ctx
        character(len=*), parameter :: path = 'no_permission.gcov'

        tests = tests + 1

        call interpret_iostat_open_error(13, path, error_ctx)

        if (len_trim(error_ctx%suggestion) == 0 .and. &
            index(error_ctx%message, 'Permission denied opening:') > 0) then
            passed = passed + 1
            write (output_unit, '(A)') &
                '  [PASS] Permission error has minimal message and no suggestion'
        else
            write (output_unit, '(A)') &
                '  [FAIL] Permission error message should be minimal with no suggestion'
            write (output_unit, '(A,A)') '    Message:    ', trim(error_ctx%message)
            write (output_unit, '(A,A)') '    Suggestion: ', trim(error_ctx%suggestion)
        end if
    end subroutine test_open_permission_no_suggestion

end program test_iostat_utilities_minimal_messages
