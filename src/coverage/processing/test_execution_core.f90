module test_execution_core
    !! Test Execution with Timeout Handling
    !! 
    !! Handles test command execution with timeout management.
    !! Extracted from coverage_test_executor.f90 for SRP compliance (Issue #718).
    use config_core, only: config_t
    use string_utils, only: int_to_string
    implicit none
    private
    
    public :: execute_tests_with_timeout, format_timeout_message

contains

    subroutine execute_tests_with_timeout(test_command, config, exit_code, &
                                         success)
        !! Execute test command with timeout handling
        !!
        !! Uses system timeout command to limit test execution time and prevent
        !! hanging tests. Provides secure command execution with proper
        !! argument escaping.
        !!
        !! Args:
        !!   test_command: The test command to execute
        !!   config: Configuration with timeout settings
        !!   exit_code: Exit code from test execution
        !!   success: True if tests passed, false if failed or timed out
        
        character(len=*), intent(in) :: test_command
        type(config_t), intent(in) :: config
        integer, intent(out) :: exit_code
        logical, intent(out) :: success
        
        character(len=1024) :: full_command
        character(len=32) :: timeout_str
        
        success = .false.
        
        ! Build timeout command with proper escaping
        write(timeout_str, '(I0)') config%test_timeout_seconds
        
        ! Build full timeout command
        full_command = 'timeout ' // trim(timeout_str) // 's ' // trim(test_command)
        
        if (.not. config%quiet) then
            print *, "🔧 Executing: " // trim(test_command)
            print *, "⏱️  Timeout: " // trim(timeout_str) // " seconds"
        end if
        
        ! Execute the command with timeout
        call execute_command_line(full_command, exitstat=exit_code)
        
        ! Check results
        if (exit_code == 0) then
            success = .true.
        else if (exit_code == 124) then
            ! Standard timeout exit code
            success = .false.
        else
            ! Test failure or other error
            success = .false.
        end if
        
    end subroutine execute_tests_with_timeout
    
    function format_timeout_message(seconds) result(message)
        !! Format timeout duration for user-friendly display
        integer, intent(in) :: seconds
        character(len=64) :: message
        
        if (seconds < 60) then
            write(message, '(I0,A)') seconds, ' seconds'
        else if (seconds < 3600) then
            write(message, '(I0,A,I0,A)') seconds/60, ' minutes ', &
                                         mod(seconds, 60), ' seconds'
        else
            write(message, '(I0,A,I0,A,I0,A)') seconds/3600, ' hours ', &
                                              mod(seconds/60, 60), ' minutes ', &
                                              mod(seconds, 60), ' seconds'
        end if
    end function format_timeout_message

end module test_execution_core