program test_cli_flag_gcov_mode
    !! Tests for the --gcov / --discover-and-gcov CLI flags
    !! Ensures flags route discovery to gcov processor by disabling
    !! generic auto-discovery.

    use iso_fortran_env, only: output_unit
    use config_core, only: config_t, parse_config
    use test_utils_core, only: assert_test, reset_test_counters, &
                               print_test_header, print_test_summary
    implicit none

    call reset_test_counters()
    call print_test_header("CLI --gcov flag")

    call test_gcov_flag_disables_auto_discovery()
    call test_gcov_alias_disables_auto_discovery()
    call test_default_keeps_auto_discovery()

    call print_test_summary("CLI --gcov flag")

contains

    subroutine test_gcov_flag_disables_auto_discovery()
        type(config_t) :: config
        character(len=32), allocatable :: args(:)
        logical :: success
        character(len=256) :: error_message

        allocate(character(len=32) :: args(1))
        args(1) = "--gcov"

        call parse_config(args, config, success, error_message)
        call assert_test(success, "--gcov parses", "Should parse: " // trim(error_message))
        if (success) then
            call assert_test(.not. config%auto_discovery, "--gcov disables auto-discovery", "Should be false")
        end if
    end subroutine test_gcov_flag_disables_auto_discovery

    subroutine test_gcov_alias_disables_auto_discovery()
        type(config_t) :: config
        character(len=32), allocatable :: args(:)
        logical :: success
        character(len=256) :: error_message

        allocate(character(len=32) :: args(1))
        args(1) = "--discover-and-gcov"

        call parse_config(args, config, success, error_message)
        call assert_test(success, "--discover-and-gcov parses", "Should parse: " // trim(error_message))
        if (success) then
            call assert_test(.not. config%auto_discovery, "alias disables auto-discovery", "Should be false")
        end if
    end subroutine test_gcov_alias_disables_auto_discovery

    subroutine test_default_keeps_auto_discovery()
        type(config_t) :: config
        character(len=1), allocatable :: no_args(:)
        logical :: success
        character(len=256) :: error_message

        allocate(character(len=1) :: no_args(0))
        call parse_config(no_args, config, success, error_message)
        call assert_test(success, "no-arg parses", "Should parse: " // trim(error_message))
        if (success) then
            call assert_test(config%auto_discovery, "default auto-discovery true", "Should be true")
        end if
    end subroutine test_default_keeps_auto_discovery

end program test_cli_flag_gcov_mode

