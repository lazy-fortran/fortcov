program test_config_auto_discovery
    !! Test suite for auto-discovery configuration options
    !! 
    !! Tests the new auto-discovery configuration fields including:
    !! - auto_discovery boolean flag
    !! - auto_test_execution boolean flag  
    !! - test_timeout_seconds integer value
    !! Tests CLI parsing, defaults, and namelist support.

    use config_types, only: config_t
    use config_defaults, only: initialize_default_config
    use config_command_parser, only: parse_command_line_config
    use config_file_parser, only: parse_config_file
    implicit none

    integer :: total_tests = 0
    integer :: passed_tests = 0

    call run_all_tests()
    call print_test_summary()

contains

    subroutine run_all_tests()
        !! Run all auto-discovery configuration tests
        call test_default_values()
        call test_cli_auto_discovery_enabled()
        call test_cli_auto_discovery_disabled()
        call test_cli_auto_test_enabled()
        call test_cli_auto_test_disabled()
        call test_cli_test_timeout()
        call test_config_file_parsing()
        call test_invalid_timeout_value()
    end subroutine run_all_tests

    subroutine test_default_values()
        !! Test that default values are set correctly
        type(config_t) :: config

        call test_start("Default auto-discovery values")
        call initialize_default_config(config)

        call assert_true(config%auto_discovery, "auto_discovery default")
        call assert_true(config%auto_test_execution, "auto_test_execution default")
        call assert_equals_int(config%test_timeout_seconds, 300, "test_timeout_seconds default")

        call test_pass()
    end subroutine test_default_values

    subroutine test_cli_auto_discovery_enabled()
        !! Test --auto-discovery flag sets value to true
        type(config_t) :: config
        character(len=32) :: args(1)
        logical :: success
        character(len=256) :: error_message

        call test_start("CLI --auto-discovery enables flag")
        
        args(1) = "--auto-discovery"
        call parse_command_line_config(args, config, success, error_message)

        call assert_true(success, "parse succeeded")
        call assert_true(config%auto_discovery, "auto_discovery enabled")

        call test_pass()
    end subroutine test_cli_auto_discovery_enabled

    subroutine test_cli_auto_discovery_disabled()
        !! Test --no-auto-discovery flag sets value to false
        type(config_t) :: config
        character(len=32) :: args(1)
        logical :: success
        character(len=256) :: error_message

        call test_start("CLI --no-auto-discovery disables flag")
        
        args(1) = "--no-auto-discovery"
        call parse_command_line_config(args, config, success, error_message)

        call assert_true(success, "parse succeeded")
        call assert_false(config%auto_discovery, "auto_discovery disabled")

        call test_pass()
    end subroutine test_cli_auto_discovery_disabled

    subroutine test_cli_auto_test_enabled()
        !! Test --auto-test flag sets value to true
        type(config_t) :: config
        character(len=32) :: args(1)
        logical :: success
        character(len=256) :: error_message

        call test_start("CLI --auto-test enables flag")
        
        args(1) = "--auto-test"
        call parse_command_line_config(args, config, success, error_message)

        call assert_true(success, "parse succeeded")
        call assert_true(config%auto_test_execution, "auto_test_execution enabled")

        call test_pass()
    end subroutine test_cli_auto_test_enabled

    subroutine test_cli_auto_test_disabled()
        !! Test --no-auto-test flag sets value to false
        type(config_t) :: config
        character(len=32) :: args(1)
        logical :: success
        character(len=256) :: error_message

        call test_start("CLI --no-auto-test disables flag")
        
        args(1) = "--no-auto-test"
        call parse_command_line_config(args, config, success, error_message)

        call assert_true(success, "parse succeeded")
        call assert_false(config%auto_test_execution, "auto_test_execution disabled")

        call test_pass()
    end subroutine test_cli_auto_test_disabled

    subroutine test_cli_test_timeout()
        !! Test --test-timeout flag sets timeout value
        type(config_t) :: config
        character(len=32) :: args(2)
        logical :: success
        character(len=256) :: error_message

        call test_start("CLI --test-timeout sets timeout")
        
        args(1) = "--test-timeout"
        args(2) = "600"
        call parse_command_line_config(args, config, success, error_message)

        call assert_true(success, "parse succeeded")
        call assert_equals_int(config%test_timeout_seconds, 600, "timeout value set")

        call test_pass()
    end subroutine test_cli_test_timeout

    subroutine test_config_file_parsing()
        !! Test config file namelist parsing (placeholder for now)
        call test_start("Config file namelist parsing")
        ! This will be implemented when config_file_parser.f90 is updated
        call test_pass()
    end subroutine test_config_file_parsing

    subroutine test_invalid_timeout_value()
        !! Test error handling for invalid timeout values
        type(config_t) :: config
        character(len=32) :: args(2)
        logical :: success
        character(len=256) :: error_message

        call test_start("Invalid timeout value produces error")
        
        args(1) = "--test-timeout"
        args(2) = "invalid"
        call parse_command_line_config(args, config, success, error_message)

        call assert_false(success, "parse failed as expected")
        call assert_contains(error_message, "test timeout", "error message mentions timeout")

        call test_pass()
    end subroutine test_invalid_timeout_value

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

    subroutine assert_contains(text, substring, description)
        character(len=*), intent(in) :: text, substring, description
        if (index(text, substring) == 0) then
            print '(A)', "FAIL: " // trim(description) // " expected '" // trim(text) // "' to contain '" // trim(substring) // "'"
            error stop 1
        end if
    end subroutine assert_contains

    subroutine print_test_summary()
        print '(A)', ""
        print '(A,I0,A,I0,A)', "Test Results: ", passed_tests, "/", total_tests, " passed"
        if (passed_tests /= total_tests) then
            error stop 1
        end if
    end subroutine print_test_summary

end program test_config_auto_discovery