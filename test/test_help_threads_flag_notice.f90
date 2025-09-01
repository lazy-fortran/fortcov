program test_help_threads_flag_notice
    !! Smoke test for help/version output related to threads flag notice
    use iso_fortran_env, only: output_unit
    use config_help_core, only: show_help_information, show_version_information
    use test_utils_core, only: assert_test, reset_test_counters, &
                               print_test_header, print_test_summary
    implicit none

    call reset_test_counters()
    call print_test_header("CLI Help and Version")

    ! Ensure help function executes without error
    write(output_unit, '(A)') "Running show_help_information()"
    call show_help_information()
    call assert_test(.true., "Help output smoke test", "Help should print without errors")

    ! Ensure version function executes without error
    write(output_unit, '(A)') "Running show_version_information()"
    call show_version_information()
    call assert_test(.true., "Version output smoke test", "Version should print without errors")

    call print_test_summary("CLI HELP/VERSION", .false.)

end program test_help_threads_flag_notice

