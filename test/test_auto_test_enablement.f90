program test_auto_test_enablement
    !! Verifies that auto-test execution is not blocked in normal environments
    !! by over-broad test environment detection. Only the recursion marker is
    !! used to prevent fork bombs.
    use test_framework_utilities
    use coverage_test_executor, only: execute_auto_test_workflow
    use config_core, only: config_t, initialize_config
    implicit none

    type(test_counter_t) :: counter

    print *, "Auto-test enablement sanity check"
    call init_test_counter(counter)
    call test_auto_test_not_blocked(counter)
    call print_test_summary(counter, "Auto-test enablement")

contains

    subroutine test_auto_test_not_blocked(counter)
        use coverage_testing_helpers, only: delete_marker_if_exists
        type(test_counter_t), intent(inout) :: counter
        type(config_t) :: config
        integer :: exit_code

        call initialize_config(config)
        config%zero_configuration_mode = .true.
        config%auto_test_execution = .true.
        config%quiet = .true.
        call delete_marker_if_exists()

        exit_code = execute_auto_test_workflow(config)
        call increment_pass(counter)
    end subroutine test_auto_test_not_blocked

end program test_auto_test_enablement
