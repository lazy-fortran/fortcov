program test_negative_fail_under_1236
    !! Test for Issue #1236: Confusing error when using negative value with --fail-under
    !! Verifies that negative numbers are recognized as values (not flags) and that
    !! threshold validation provides a clear range error message.

    use iso_fortran_env, only: output_unit
    use config_parser_flags, only: is_flag_argument
    use config_parser_string, only: parse_threshold_with_error
    implicit none

    integer :: tests = 0
    integer :: passed = 0

    call test_is_flag_argument_with_flags()
    call test_is_flag_argument_with_negative_numbers()
    call test_threshold_validation_negative_value()

    write (output_unit, '(A,I0,A,I0,A)') 'Test Results: ', passed, ' / ', &
        tests, ' passed'
    if (passed /= tests) stop 1

contains

    subroutine test_is_flag_argument_with_flags()
        !! Ensure normal flags are still recognized
        tests = tests + 1

        if (is_flag_argument("--help") .and. &
            is_flag_argument("-h") .and. &
            is_flag_argument("--fail-under") .and. &
            is_flag_argument("-v")) then
            passed = passed + 1
            write (output_unit, '(A)') '  [PASS] Normal flags correctly identified'
        else
            write (output_unit, '(A)') '  [FAIL] Normal flags not recognized'
        end if
    end subroutine test_is_flag_argument_with_flags

    subroutine test_is_flag_argument_with_negative_numbers()
        !! Ensure negative numbers are NOT treated as flags
        tests = tests + 1

        if (.not. is_flag_argument("-10") .and. &
            .not. is_flag_argument("-3.14") .and. &
            .not. is_flag_argument("-0") .and. &
            .not. is_flag_argument("-99999")) then
            passed = passed + 1
            write (output_unit, '(A)') '  [PASS] Negative numbers not treated as flags'
        else
            write (output_unit, '(A)') &
                '  [FAIL] Negative numbers incorrectly treated as flags'
        end if
    end subroutine test_is_flag_argument_with_negative_numbers

    subroutine test_threshold_validation_negative_value()
        !! Ensure negative threshold gives clear range error
        real :: value
        logical :: success
        character(len=256) :: error_message

        tests = tests + 1

        call parse_threshold_with_error("-10", value, "fail-under threshold", &
                                        success, error_message)

        if (.not. success .and. &
            index(error_message, "must be between 0.0 and 100.0") > 0) then
            passed = passed + 1
            write (output_unit, '(A)') &
                '  [PASS] Negative threshold gives clear range error'
        else
            write (output_unit, '(A)') &
                '  [FAIL] Negative threshold error message unclear'
            write (output_unit, '(A,L1)') '    Success: ', success
            write (output_unit, '(A,A)') '    Error: ', trim(error_message)
        end if
    end subroutine test_threshold_validation_negative_value

end program test_negative_fail_under_1236
