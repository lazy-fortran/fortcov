program test_cli_validate_and_zero_config_flags
    !! CLI --validate and --zero-config flags.
    !! Regression tests for issue 1312.

    use, intrinsic :: iso_fortran_env, only: output_unit
    use config_parser, only: parse_command_line_config
    use config_types, only: config_t
    implicit none

    integer :: tests = 0
    integer :: passed = 0

    call test_validate_flag()
    call test_zero_config_flag()

    write (output_unit, '(A,I0,A,I0,A)') 'Test Results: ', passed, ' / ', &
        tests, ' passed'
    if (passed /= tests) stop 1

contains

    subroutine test_validate_flag()
        type(config_t) :: config
        logical :: success
        character(len=512) :: error_message
        character(len=32) :: args(1)

        tests = tests + 1

        args(1) = "--validate"
        call parse_command_line_config(args, config, success, error_message)

        if (success .and. config%validate_config_only) then
            passed = passed + 1
            write (output_unit, '(A)') '  [PASS] --validate sets validate_config_only'
        else
            write (output_unit, '(A)') '  [FAIL] --validate did not set '// &
                'validate_config_only'
            write (output_unit, '(A,L1)') '    Success: ', success
            write (output_unit, '(A,L1)') '    validate_config_only: ', &
                config%validate_config_only
            write (output_unit, '(A,A)') '    Error: ', trim(error_message)
        end if
    end subroutine test_validate_flag

    subroutine test_zero_config_flag()
        type(config_t) :: config
        logical :: success
        character(len=512) :: error_message
        character(len=32) :: args(1)

        tests = tests + 1

        args(1) = "--zero-config"
        call parse_command_line_config(args, config, success, error_message)

        if (success .and. config%zero_configuration_mode) then
            passed = passed + 1
            write (output_unit, '(A)') '  [PASS] --zero-config sets '// &
                'zero_configuration_mode'
        else
            write (output_unit, '(A)') '  [FAIL] --zero-config did not set '// &
                'zero_configuration_mode'
            write (output_unit, '(A,L1)') '    Success: ', success
            write (output_unit, '(A,L1)') '    zero_configuration_mode: ', &
                config%zero_configuration_mode
            write (output_unit, '(A,A)') '    Error: ', trim(error_message)
        end if
    end subroutine test_zero_config_flag

end program test_cli_validate_and_zero_config_flags
