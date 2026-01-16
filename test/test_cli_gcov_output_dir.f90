program test_cli_gcov_output_dir
    !! CLI gcov output directory flag.
    !! Regression test for issue 1260.

    use, intrinsic :: iso_fortran_env, only: output_unit
    use config_defaults_core, only: initialize_default_config
    use config_parser, only: process_single_flag
    use config_types, only: config_t
    implicit none

    integer :: tests = 0
    integer :: passed = 0

    call test_default_output_dir()
    call test_flag_output_dir()

    write (output_unit, '(A,I0,A,I0,A)') 'Test Results: ', passed, ' / ', &
        tests, ' passed'
    if (passed /= tests) stop 1

contains

    subroutine test_default_output_dir()
        type(config_t) :: config

        tests = tests + 1

        call initialize_default_config(config)

        if (trim(config%gcov_output_dir) == 'build/gcov') then
            passed = passed + 1
            write (output_unit, '(A)') '  [PASS] Default gcov output dir set'
        else
            write (output_unit, '(A)') '  [FAIL] Default gcov output dir mismatch'
            write (output_unit, '(A,A)') '    Got: ', trim(config%gcov_output_dir)
        end if
    end subroutine test_default_output_dir

    subroutine test_flag_output_dir()
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message

        tests = tests + 1

        call initialize_default_config(config)
        call process_single_flag("--gcov-output-dir=out/gcov", config, success, &
                                 error_message)

        if (success .and. trim(config%gcov_output_dir) == 'out/gcov') then
            passed = passed + 1
            write (output_unit, '(A)') '  [PASS] --gcov-output-dir applies'
        else
            write (output_unit, '(A)') '  [FAIL] --gcov-output-dir did not apply'
            write (output_unit, '(A,L1)') '    Success: ', success
            write (output_unit, '(A,A)') '    Error: ', trim(error_message)
            write (output_unit, '(A,A)') '    Got: ', trim(config%gcov_output_dir)
        end if
    end subroutine test_flag_output_dir

end program test_cli_gcov_output_dir
