program test_cli_flags_rejection_minimal
    use config_core, only: parse_config
    use config_types, only: config_t
    implicit none

    type(config_t) :: cfg
    logical :: ok
    character(len=256) :: err
    integer :: failures

    failures = 0

    call check_flag('--config=fortcov.nml', .false., 'Configuration files are no longer supported', failures)
    call check_flag('--diff', .false., 'Flag no longer supported', failures)
    call check_flag('--include=src/**', .false., 'Flag no longer supported', failures)
    call check_flag('--auto-test', .false., 'Flag no longer supported', failures)
    call check_flag('--zero-config', .false., 'Flag no longer supported', failures)

    if (failures == 0) then
        print *, 'OK: cli flags rejection minimal'
    else
        error stop 1
    end if

contains

    subroutine check_flag(flag, expect_ok, expect_substr, failures)
        character(len=*), intent(in) :: flag
        logical, intent(in) :: expect_ok
        character(len=*), intent(in) :: expect_substr
        integer, intent(inout) :: failures

        type(config_t) :: cfg
        logical :: ok
        character(len=256) :: err
        character(len=:), allocatable :: args(:)

        allocate(character(len=64) :: args(1))
        args = [ character(len=64) :: trim(flag) ]

        call parse_config(args, cfg, ok, err)
        if (ok .neqv. expect_ok) then
            print *, 'FAIL: ', trim(flag), ' ok=', ok, ' expected=', expect_ok
            failures = failures + 1
        else
            if (index(err, trim(expect_substr)) == 0) then
                print *, 'FAIL: ', trim(flag), ' missing expected error: ', trim(expect_substr)
                failures = failures + 1
            else
                print *, 'OK: ', trim(flag), ' rejected: ', trim(err)
            end if
        end if
    end subroutine check_flag

end program test_cli_flags_rejection_minimal
