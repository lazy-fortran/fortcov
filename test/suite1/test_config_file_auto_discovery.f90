program test_config_file_auto_discovery
    !! Test suite for auto-discovery configuration file parsing
    !! 
    !! Tests config file namelist parsing for auto-discovery options

    use config_types, only: config_t
    use config_defaults_core, only: initialize_default_config
    use config_parser, only: process_config_file_option
    implicit none

    integer :: total_tests = 0
    integer :: passed_tests = 0

    call run_all_tests()
    call print_test_summary()

contains

    subroutine run_all_tests()
        !! Run all config file parsing tests
        call test_config_auto_discovery_true()
        call test_config_auto_discovery_false()
        call test_config_auto_test_true()
        call test_config_auto_test_false()
        call test_config_timeout_value()
        call test_config_underscore_variants()
        call test_config_dash_variants()
    end subroutine run_all_tests

    subroutine test_config_auto_discovery_true()
        !! Test auto-discovery = true in config file
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message

        call test_start("Config auto-discovery = true")
        call initialize_default_config(config)
        
        call process_config_file_option("auto_discovery", "true", config, &
                                         success, error_message)

        call assert_true(success, "parse succeeded")
        call assert_true(config%auto_discovery, "auto_discovery set to true")
        call test_pass()
    end subroutine test_config_auto_discovery_true

    subroutine test_config_auto_discovery_false()
        !! Test auto-discovery = false in config file
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message

        call test_start("Config auto-discovery = false")
        call initialize_default_config(config)
        
        call process_config_file_option("auto_discovery", "false", config, &
                                         success, error_message)

        call assert_true(success, "parse succeeded")
        call assert_false(config%auto_discovery, "auto_discovery set to false")
        call test_pass()
    end subroutine test_config_auto_discovery_false

    subroutine test_config_auto_test_true()
        !! Test auto-test-execution = yes in config file
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message

        call test_start("Config auto-test-execution = yes")
        call initialize_default_config(config)
        
        call process_config_file_option("auto-test-execution", "yes", config, &
                                         success, error_message)

        call assert_true(success, "parse succeeded")
        call assert_true(config%auto_test_execution, "auto_test_execution set to true")
        call test_pass()
    end subroutine test_config_auto_test_true

    subroutine test_config_auto_test_false()
        !! Test auto-test = no in config file
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message

        call test_start("Config auto-test = no")
        call initialize_default_config(config)
        
        call process_config_file_option("auto-test", "no", config, &
                                         success, error_message)

        call assert_true(success, "parse succeeded")
        call assert_false(config%auto_test_execution, "auto_test_execution set to false")
        call test_pass()
    end subroutine test_config_auto_test_false

    subroutine test_config_timeout_value()
        !! Test test-timeout-seconds = 600 in config file
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message

        call test_start("Config test-timeout-seconds = 600")
        call initialize_default_config(config)
        
        call process_config_file_option("test-timeout-seconds", "600", config, &
                                         success, error_message)

        call assert_true(success, "parse succeeded")
        call assert_equals_int(config%test_timeout_seconds, 600, &
                               "test_timeout_seconds set correctly")
        call test_pass()
    end subroutine test_config_timeout_value

    subroutine test_config_underscore_variants()
        !! Test underscore variants work
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message

        call test_start("Config underscore variants")
        call initialize_default_config(config)
        
        call process_config_file_option("test_timeout_seconds", "120", config, &
                                         success, error_message)

        call assert_true(success, "parse succeeded")
        call assert_equals_int(config%test_timeout_seconds, 120, &
                               "underscore variant works")
        call test_pass()
    end subroutine test_config_underscore_variants

    subroutine test_config_dash_variants()
        !! Test dash variants work
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message

        call test_start("Config dash variants")
        call initialize_default_config(config)
        
        call process_config_file_option("test-timeout", "240", config, &
                                         success, error_message)

        call assert_true(success, "parse succeeded")
        call assert_equals_int(config%test_timeout_seconds, 240, &
                               "dash variant works")
        call test_pass()
    end subroutine test_config_dash_variants

    ! Test utility functions
    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        total_tests = total_tests + 1
        write(*, '(A)', advance='no') "Testing " // trim(test_name) // "... "
    end subroutine test_start

    subroutine test_pass()
        passed_tests = passed_tests + 1
        print '(A)', "PASS"
    end subroutine test_pass

    subroutine assert_true(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description
        if (.not. condition) then
            print '(A)', "FAIL: " // trim(description) // " expected true but got false"
            error stop 1
        end if
    end subroutine assert_true

    subroutine assert_false(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description
        if (condition) then
            print '(A)', "FAIL: " // trim(description) // " expected false but got true"
            error stop 1
        end if
    end subroutine assert_false

    subroutine assert_equals_int(actual, expected, description)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: description
        if (actual /= expected) then
            print '(A,I0,A,I0)', "FAIL: " // trim(description) // " expected ", expected, " but got ", actual
            error stop 1
        end if
    end subroutine assert_equals_int

    subroutine print_test_summary()
        print '(A)', ""
        print '(A,I0,A,I0,A)', "Test Results: ", passed_tests, "/", total_tests, " passed"
        if (passed_tests /= total_tests) then
            error stop 1
        end if
    end subroutine print_test_summary

end program test_config_file_auto_discovery