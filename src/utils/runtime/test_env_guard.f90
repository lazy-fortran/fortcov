module test_env_guard
    !! Runtime test-environment detection to prevent recursive execution
    !! Used to avoid spawning external test runners (e.g., fpm test) when
    !! fortcov itself is being run under a test harness.
    use iso_fortran_env, only: error_unit
    implicit none
    private

    public :: running_under_test_env

contains

    function running_under_test_env() result(is_test_env)
        logical :: is_test_env
        integer :: status
        character(len=256) :: env_value
        logical :: marker_exists

        is_test_env = .false.

        call get_environment_variable('FPM_TEST', env_value, status=status)
        if (status == 0 .and. len_trim(env_value) > 0) then
            is_test_env = .true.
            return
        end if

        call get_environment_variable('RUNNING_TESTS', env_value, status=status)
        if (status == 0 .and. len_trim(env_value) > 0) then
            is_test_env = .true.
            return
        end if

        call get_environment_variable('TEST_ENV', env_value, status=status)
        if (status == 0 .and. len_trim(env_value) > 0) then
            is_test_env = .true.
            return
        end if

        call get_environment_variable('FORTCOV_TEST_MODE', env_value, status=status)
        if (status == 0 .and. len_trim(env_value) > 0) then
            is_test_env = .true.
            return
        end if

        inquire(file='.fortcov_execution_marker', exist=marker_exists)
        if (marker_exists) then
            is_test_env = .true.
            return
        end if
    end function running_under_test_env

end module test_env_guard

