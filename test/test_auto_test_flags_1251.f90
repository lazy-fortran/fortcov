program test_auto_test_flags_1251
    !! Test for Issue #1251: auto-test flags are supported and configurable.

    use, intrinsic :: iso_fortran_env, only: dp => real64, output_unit
    use config_defaults_core, only: initialize_default_config
    use config_parser_flags, only: process_single_flag
    use config_types, only: config_t
    implicit none

    integer :: tests = 0
    integer :: passed = 0

    call test_no_auto_test_flag()
    call test_auto_test_flag()

    write (output_unit, '(A,I0,A,I0,A)') 'Test Results: ', passed, ' / ', &
        tests, ' passed'
    if (passed /= tests) stop 1

contains

    subroutine test_no_auto_test_flag()
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message

        tests = tests + 1

        call initialize_default_config(config)
        call process_single_flag("--no-auto-test", config, success, error_message)

        if (success .and. .not. config % auto_test_execution) then
            passed = passed + 1
            write (output_unit, '(A)') '  [PASS] --no-auto-test disables auto-test'
        else
            write (output_unit, '(A)') '  [FAIL] --no-auto-test did not apply'
            write (output_unit, '(A,L1)') '    Success: ', success
            write (output_unit, '(A,A)') '    Error: ', trim(error_message)
        end if
    end subroutine test_no_auto_test_flag

    subroutine test_auto_test_flag()
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message

        tests = tests + 1

        call initialize_default_config(config)
        config % auto_test_execution = .false.
        call process_single_flag("--auto-test", config, success, error_message)

        if (success .and. config % auto_test_execution) then
            passed = passed + 1
            write (output_unit, '(A)') '  [PASS] --auto-test enables auto-test'
        else
            write (output_unit, '(A)') '  [FAIL] --auto-test did not apply'
            write (output_unit, '(A,L1)') '    Success: ', success
            write (output_unit, '(A,A)') '    Error: ', trim(error_message)
        end if
    end subroutine test_auto_test_flag

end program test_auto_test_flags_1251
