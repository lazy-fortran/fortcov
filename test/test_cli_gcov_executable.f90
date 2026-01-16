program test_cli_gcov_executable
    !! CLI gcov executable flag and validation.
    !! Regression test for issue 1256.

    use, intrinsic :: iso_fortran_env, only: output_unit
    use config_defaults_core, only: initialize_default_config
    use config_parser_flags, only: process_single_flag
    use config_types, only: config_t
    use config_validators, only: validate_gcov_executable
    implicit none

    integer :: tests = 0
    integer :: passed = 0

    call test_default_executable()
    call test_flag_executable()
    call test_validation()

    write (output_unit, '(A,I0,A,I0,A)') 'Test Results: ', passed, ' / ', &
        tests, ' passed'
    if (passed /= tests) stop 1

contains

    subroutine test_default_executable()
        type(config_t) :: config

        tests = tests + 1
        call initialize_default_config(config)

        if (trim(config%gcov_executable) == 'gcov') then
            passed = passed + 1
            write (output_unit, '(A)') '  [PASS] Default gcov executable set'
        else
            write (output_unit, '(A)') '  [FAIL] Default gcov executable mismatch'
            write (output_unit, '(A,A)') '    Got: ', trim(config%gcov_executable)
        end if
    end subroutine test_default_executable

    subroutine test_flag_executable()
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message

        tests = tests + 1

        call initialize_default_config(config)
        call process_single_flag("--gcov-executable=gcov-12", config, success, &
                                 error_message)

        if (success .and. trim(config%gcov_executable) == 'gcov-12') then
            passed = passed + 1
            write (output_unit, '(A)') '  [PASS] --gcov-executable applies'
        else
            write (output_unit, '(A)') '  [FAIL] --gcov-executable did not apply'
            write (output_unit, '(A,L1)') '    Success: ', success
            write (output_unit, '(A,A)') '    Error: ', trim(error_message)
            write (output_unit, '(A,A)') '    Got: ', trim(config%gcov_executable)
        end if
    end subroutine test_flag_executable

    subroutine test_validation()
        logical :: ok
        character(len=256) :: error_message

        tests = tests + 1

        call validate_gcov_executable('gcov-wrapper', ok, error_message)
        if (.not. ok) then
            write (output_unit, '(A)') '  [FAIL] gcov-wrapper should be valid'
            write (output_unit, '(A,A)') '    Error: ', trim(error_message)
            return
        end if

        call validate_gcov_executable('gcov wrapper', ok, error_message)
        if (ok) then
            write (output_unit, '(A)') '  [FAIL] gcov wrapper should be invalid'
            return
        end if

        passed = passed + 1
        write (output_unit, '(A)') '  [PASS] gcov executable validation'
    end subroutine test_validation

end program test_cli_gcov_executable
