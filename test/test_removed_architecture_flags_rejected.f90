program test_removed_architecture_flags_rejected
    !! Ensure removed architecture/size flags are rejected by parser
    use iso_fortran_env, only: output_unit
    use config_core,      only: config_t, parse_config
    use test_utils_core,  only: assert_test, reset_test_counters, &
                               print_test_header, print_test_summary
    implicit none

    call reset_test_counters()
    call print_test_header("Removed Flags Rejection")

    call test_unknown_flag("--validate-architecture")
    call test_unknown_flag("--fail-on-size-warnings")

    call print_test_summary("REMOVED FLAGS", .false.)

contains

    subroutine test_unknown_flag(flag)
        character(len=*), intent(in) :: flag
        type(config_t) :: config
        character(len=256) :: err
        logical :: ok
        character(len=64), allocatable :: args(:)

        allocate(character(len=64) :: args(1))
        args(1) = flag

        call parse_config(args, config, ok, err)

        call assert_test(.not. ok, &
            "Unknown flag rejected: " // trim(flag), &
            "Parser should reject removed flag")

        if (.not. ok) then
            call assert_test(index(err, "Unknown flag") > 0, &
                "Error message mentions unknown flag", &
                trim(err))
        end if
    end subroutine test_unknown_flag

end program test_removed_architecture_flags_rejected

