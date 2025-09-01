program test_tui_threads_warning
    !! Exercise TUI statistics with threads>1 to trigger warning path
    use config_types, only: config_t
    use config_defaults_core, only: initialize_default_config
    use test_utils_core, only: assert_test, reset_test_counters, &
                               print_test_header, print_test_summary
    use coverage_tui, only: display_tui_statistics
    implicit none

    type(config_t) :: cfg

    call reset_test_counters()
    call print_test_header("TUI Threads Warning")

    call initialize_default_config(cfg)
    cfg%quiet = .false.
    cfg%threads = 4

    ! Should print a warning message; this is a smoke test for execution.
    call display_tui_statistics(cfg)
    call assert_test(.true., "display_tui_statistics executes with threads>1", &
                     "Should not error when threads>1")

    call print_test_summary("TUI THREADS WARNING", .false.)

end program test_tui_threads_warning

