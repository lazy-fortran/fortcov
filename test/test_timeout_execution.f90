program test_timeout_execution
    !! Test timeout command execution functionality
    !!
    !! Tests the execute_tests_with_timeout subroutine to ensure proper
    !! timeout handling for test commands
    use iso_fortran_env, only: error_unit, output_unit
    use test_executor_core, only: execute_tests_with_timeout
    use config_types, only: config_t
    use config_defaults_core, only: initialize_default_config
    implicit none

    integer :: total_tests = 0
    integer :: passed_tests = 0
    logical :: all_tests_passed = .true.

    write(output_unit, '(A)') 'Running timeout execution tests...'
    write(output_unit, *)

    call test_successful_command_execution()
    call test_timeout_with_sleep_command()
    call test_failing_command_execution()
    call test_timeout_format_message()

    write(output_unit, *)
    write(output_unit, '(A,I0,A,I0)') 'Test Results: ', passed_tests, ' / ', &
                                      total_tests

    if (all_tests_passed) then
        write(output_unit, '(A)') 'ALL TESTS PASSED'
        stop 0
    else
        write(error_unit, '(A)') 'SOME TESTS FAILED'
        stop 1
    end if

contains

    subroutine test_successful_command_execution()
        !! Test that successful commands execute correctly with timeout
        type(config_t) :: config
        integer :: exit_code
        logical :: success
        
        write(output_unit, '(A)') 'Test 1: Successful command execution'
        
        call initialize_default_config(config)
        config%test_timeout_seconds = 10
        config%quiet = .true.
        
        ! Execute a simple successful command
        call execute_tests_with_timeout('echo "test successful"', config, &
                                       exit_code, success)
        
        call assert_equals_int(exit_code, 0, 'Successful command exit code')
        call assert_equals_logical(success, .true., 'Successful command result')
    end subroutine test_successful_command_execution

    subroutine test_timeout_with_sleep_command()
        !! Test that long-running commands are properly timed out
        type(config_t) :: config
        integer :: exit_code
        logical :: success
        
        write(output_unit, '(A)') 'Test 2: Command timeout handling'
        
        call initialize_default_config(config)
        config%test_timeout_seconds = 2  ! Short timeout
        config%quiet = .true.
        
        ! Execute a command that will timeout
        call execute_tests_with_timeout('sleep 5', config, exit_code, success)
        
        call assert_equals_int(exit_code, 124, 'Timeout exit code')
        call assert_equals_logical(success, .false., 'Timeout command result')
    end subroutine test_timeout_with_sleep_command

    subroutine test_failing_command_execution()
        !! Test that failing commands are handled correctly
        type(config_t) :: config
        integer :: exit_code
        logical :: success
        
        write(output_unit, '(A)') 'Test 3: Failing command execution'
        
        call initialize_default_config(config)
        config%test_timeout_seconds = 10
        config%quiet = .true.
        
        ! Execute a command that will fail
        call execute_tests_with_timeout('false', config, exit_code, success)
        
        call assert_not_equals_int(exit_code, 0, 'Failing command exit code')
        call assert_equals_logical(success, .false., 'Failing command result')
    end subroutine test_failing_command_execution

    subroutine test_timeout_format_message()
        !! Test timeout message formatting
        use test_executor_core, only: format_timeout_message
        character(len=64) :: message
        
        write(output_unit, '(A)') 'Test 4: Timeout message formatting'
        
        ! Test seconds formatting
        message = format_timeout_message(30)
        call assert_contains_string(trim(message), '30 seconds', &
                                   'Seconds formatting')
        
        ! Test minutes formatting
        message = format_timeout_message(90)
        call assert_contains_string(trim(message), '1 minutes', &
                                   'Minutes formatting')
        
        ! Test hours formatting  
        message = format_timeout_message(3665)
        call assert_contains_string(trim(message), '1 hours', &
                                   'Hours formatting')
    end subroutine test_timeout_format_message

    ! Test utilities
    subroutine assert_equals_int(actual, expected, message)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: message
        
        total_tests = total_tests + 1
        if (actual == expected) then
            passed_tests = passed_tests + 1
            write(output_unit, '(A,A)') '  PASS: ', message
        else
            all_tests_passed = .false.
            write(error_unit, '(A,A)') '  FAIL: ', message
            write(error_unit, '(A,I0,A,I0)') '    Expected: ', expected, &
                                             ', Got: ', actual
        end if
    end subroutine assert_equals_int

    subroutine assert_not_equals_int(actual, not_expected, message)
        integer, intent(in) :: actual, not_expected
        character(len=*), intent(in) :: message
        
        total_tests = total_tests + 1
        if (actual /= not_expected) then
            passed_tests = passed_tests + 1
            write(output_unit, '(A,A)') '  PASS: ', message
        else
            all_tests_passed = .false.
            write(error_unit, '(A,A)') '  FAIL: ', message
            write(error_unit, '(A,I0,A,I0)') '    Expected not: ', not_expected, &
                                             ', Got: ', actual
        end if
    end subroutine assert_not_equals_int

    subroutine assert_equals_logical(actual, expected, message)
        logical, intent(in) :: actual, expected
        character(len=*), intent(in) :: message
        
        total_tests = total_tests + 1
        if (actual .eqv. expected) then
            passed_tests = passed_tests + 1
            write(output_unit, '(A,A)') '  PASS: ', message
        else
            all_tests_passed = .false.
            write(error_unit, '(A,A)') '  FAIL: ', message
            write(error_unit, '(A,L1,A,L1)') '    Expected: ', expected, &
                                             ', Got: ', actual
        end if
    end subroutine assert_equals_logical

    subroutine assert_contains_string(haystack, needle, message)
        character(len=*), intent(in) :: haystack, needle, message
        
        total_tests = total_tests + 1
        if (index(haystack, needle) > 0) then
            passed_tests = passed_tests + 1
            write(output_unit, '(A,A)') '  PASS: ', message
        else
            all_tests_passed = .false.
            write(error_unit, '(A,A)') '  FAIL: ', message
            write(error_unit, '(A,A,A,A)') '    Expected "', trim(haystack), &
                                           '" to contain "', trim(needle)
        end if
    end subroutine assert_contains_string

end program test_timeout_execution
